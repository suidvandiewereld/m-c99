-- | Semantic analysis (src/sema.c).
--
-- Walks the parse tree and returns a rewritten one in which every expression
-- carries a type ('exTy') and every name resolves to a 'SymId'. It also
-- rewrites: enum constants become integer literals, @sizeof@ becomes its
-- value, and @__int128@ operators become calls into the u128 runtime.
--
-- The C mutates nodes and symbols in place. Here the tree is rebuilt, and
-- symbols live in a table keyed by 'SymId' — which is why identifiers carry an
-- id rather than a Symbol: @&x@ marks x address-taken long after x's
-- declaration was walked, and a by-value copy would miss it.
module C99.Sema
  ( SemaResult (..)
  , semaCheck
  , symbolOf
  , foldConst
  ) where

import Control.Monad (forM, unless, when)
import Control.Monad.State.Strict
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isNothing)

import C99.Ast
import C99.Common
import C99.CType
import C99.Diag (closestCandidate)

data SemaResult = SemaResult
  { srProgram :: Program
  , -- | A tag unique to this object, mixed into the names of the globals
    -- lowering invents. Without it two separately compiled objects both call
    -- their first string literal @.str0@ and the link silently keeps one.
    srTag :: String
  , srSyms :: M.Map SymId Symbol
  , -- | Globals in declaration order: what the lowerer must emit.
    srGlobals :: [SymId]
  , srTc :: TypeContext
  , srMsgs :: [Message]
  }

symbolOf :: SemaResult -> SymId -> Symbol
symbolOf r sid = case M.lookup sid (srSyms r) of
  Just s -> s
  Nothing -> error ("C99.Sema: unknown symbol id " ++ show sid)

data SemaState = SemaState
  { ssTc :: TypeContext
  , -- | Innermost scope first.
    ssScopes :: [M.Map String SymId]
  , ssSyms :: M.Map SymId Symbol
  , ssNextSym :: !SymId
  , ssGlobals :: [SymId] -- reversed
  , ssRetTy :: Maybe Type
  , ssLoopDepth :: !Int
  , ssSwitchDepth :: !Int
  , ssMsgs :: [Message] -- reversed
  , -- | Where each symbol was declared, and whether anything has read it.
    -- Kept beside the symbols rather than on 'Symbol' so the AST stays the
    -- shape 'Lower' expects.
    ssDeclLoc :: M.Map SymId SrcLoc
  , ssUsed :: S.Set SymId
  }

type Sema a = State SemaState a

semaCheck :: String -> TypeContext -> Program -> SemaResult
semaCheck tag tc prog =
  let st0 =
        SemaState
          { ssTc = tc
          , ssScopes = [M.empty] -- file scope
          , ssSyms = M.empty
          , ssNextSym = 0
          , ssGlobals = []
          , ssRetTy = Nothing
          , ssLoopDepth = 0
          , ssSwitchDepth = 0
          , ssMsgs = []
          , ssDeclLoc = M.empty
          , ssUsed = S.empty
          }
      (prog', st) = runState (checkProgram prog) st0
   in SemaResult
        { srProgram = prog'
        , srTag = tag
        , srSyms = ssSyms st
        , srGlobals = reverse (ssGlobals st)
        , srTc = ssTc st
        , srMsgs = reverse (ssMsgs st)
        }

-- ---- diagnostics ----

errAt :: SrcLoc -> String -> Sema ()
errAt loc text = modify' $ \s -> s {ssMsgs = diag Error loc text : ssMsgs s}

-- | Record a message that has already been given a code, a span or notes.
emit :: Message -> Sema ()
emit m = modify' $ \s -> s {ssMsgs = m : ssMsgs s}

-- | Every name visible from here, nearest scope first, for "did you mean".
visibleNames :: Sema [String]
visibleNames = gets (concatMap M.keys . ssScopes)

-- ---- flow warnings ----

-- | Whether control can leave this statement by falling off its end.
--
-- Everything unsure answers 'False' (\"cannot fall through\"), because both
-- callers only warn when this says 'True'. A wrong 'True' is a false positive;
-- a wrong 'False' just costs a warning nobody gets. @goto@ and @switch@ are
-- the interesting cases: this does not follow a jump, and a switch without a
-- @default@ can fall through but proving it is not worth the risk.
fallsThrough :: Stmt -> Bool
fallsThrough s = case stNode s of
  SReturn _ -> False
  SGoto _ -> False
  SBreak -> False
  SContinue -> False
  SExpr e | isNoReturnCall e -> False
  SCompound items -> all fallsThroughItem items
  SIf _ a (Just b) -> fallsThrough a || fallsThrough b
  -- a one-armed if is skipped when the condition is false
  SIf _ _ Nothing -> True
  -- while (1) / for (;;) with nothing to break out of it never finishes
  SWhile c b -> not (isAlwaysTrue c && not (hasBreak b))
  -- SFor is (init, condition, increment, body)
  SFor _ c _ b -> not (forever c && not (hasBreak b))
    where
      forever Nothing = True
      forever (Just e) = isAlwaysTrue e
  SDo b c -> fallsThrough b && not (isAlwaysTrue c && not (hasBreak b))
  SLabel _ b -> fallsThrough b
  SCase _ b -> fallsThrough b
  SDefault b -> fallsThrough b
  SSwitch _ b -> switchFallsThrough b
  _ -> True
  where
    fallsThroughItem (BIStmt x) = fallsThrough x
    fallsThroughItem (BIDecl _) = True

-- | A switch completes normally when any of three things is true: no
-- @default@ (so an unmatched value skips the whole statement), a @break@ that
-- belongs to it, or a last group that runs off the end.
--
-- Without this, every function ending in a fully-returning @switch@ with a
-- @default@ looked like it could fall out. That is the commonest shape in a
-- compiler, and it produced 118 false positives on the Mettle sources against
-- 0 real ones.
switchFallsThrough :: Stmt -> Bool
switchFallsThrough body =
  not (hasDefault body) || hasBreak body || lastGroupFallsThrough body
  where
    lastGroupFallsThrough s = case stNode s of
      SCompound items -> case reverse items of
        (BIStmt x : _) -> fallsThrough x
        (BIDecl _ : _) -> True
        [] -> True
      _ -> fallsThrough s

-- | Does this switch body have a @default@ of its own? A @default@ inside a
-- nested switch belongs to that one.
hasDefault :: Stmt -> Bool
hasDefault s = case stNode s of
  SDefault _ -> True
  SSwitch {} -> False
  SCompound items -> any item items
  SIf _ a mb -> hasDefault a || maybe False hasDefault mb
  SLabel _ b -> hasDefault b
  SCase _ b -> hasDefault b
  SWhile _ b -> hasDefault b
  SDo b _ -> hasDefault b
  SFor _ _ _ b -> hasDefault b
  _ -> False
  where
    item (BIStmt x) = hasDefault x
    item (BIDecl _) = False

-- | A @break@ that belongs to *this* loop: one nested inside an inner loop or
-- a switch binds there instead, so it does not keep this loop alive.
hasBreak :: Stmt -> Bool
hasBreak s = case stNode s of
  SBreak -> True
  SWhile {} -> False
  SDo {} -> False
  SFor {} -> False
  SSwitch {} -> False
  SCompound items -> any item items
  SIf _ a mb -> hasBreak a || maybe False hasBreak mb
  SLabel _ b -> hasBreak b
  SCase _ b -> hasBreak b
  SDefault b -> hasBreak b
  _ -> False
  where
    item (BIStmt x) = hasBreak x
    item (BIDecl _) = False

isAlwaysTrue :: Expr -> Bool
isAlwaysTrue e = case foldConst e of
  Just n -> n /= 0
  Nothing -> False

-- | A call that never comes back, so the statements after it are not reached.
--
-- There is no @_Noreturn@ or @__attribute__((noreturn))@ in the type system
-- yet, so this is a name list. It only has to cover the functions people
-- actually end a function with; anything missing costs a false warning, which
-- is why 'ExitProcess' is here (it was the last false positive across the
-- 112-file Mettle build).
isNoReturnCall :: Expr -> Bool
isNoReturnCall e = case exNode e of
  ECall f _ _ -> case exNode f of
    EIdent n _ -> baseName n `elem` noReturnNames
    _ -> False
  _ -> False
  where
    noReturnNames =
      [ "exit"
      , "_exit"
      , "_Exit"
      , "abort"
      , "longjmp"
      , "ExitProcess"
      , "ExitThread"
      , "TerminateProcess"
      , "__builtin_unreachable"
      , "__builtin_trap"
      ]

-- | Undo the file-scope static mangling for display.
--
-- 'C99.StaticRename' rewrites a static to @__\<prefix\>\<tu\>_\<name\>@ before sema
-- runs, so a diagnostic that names a function would otherwise print
-- @__st35_helper@ at someone who wrote @helper@.
baseName :: String -> String
baseName n = case n of
  ('_' : '_' : rest) -> case break (== '_') rest of
    (tag, '_' : real)
      | not (null tag)
      , not (null real)
      , any (`elem` ['0' .. '9']) tag ->
          real
    _ -> n
  _ -> n

-- | Warn on the first statement after one that cannot fall through.
--
-- Only the first, and only in a straight run of statements: reporting every
-- dead statement in a block turns one mistake into a wall of output. A label
-- ends the run, because a @goto@ can land on it.
warnUnreachable :: [BlockItem] -> Sema ()
warnUnreachable items = go items
  where
    go (BIStmt a : rest@(next : _))
      | not (fallsThrough a)
      , Just loc <- itemLoc next
      , not (isLabelled next) =
          emit
            . withLabel "this will never run"
            . withHelp "remove it, or move it above the statement that exits"
            $ (diag Warning loc "unreachable statement")
              {msgGroup = Just WUnreachable}
      | otherwise = go rest
    go (_ : rest) = go rest
    go [] = pure ()

    itemLoc (BIStmt x) = Just (stLoc x)
    itemLoc (BIDecl (d : _)) = Just (dLoc d)
    itemLoc (BIDecl []) = Nothing

    -- a goto can jump here, so it is not dead
    isLabelled (BIStmt x) = case stNode x of
      SLabel {} -> True
      SCase {} -> True
      SDefault {} -> True
      _ -> False
    isLabelled _ = False

-- | Warn when a value-returning function can run off its end.
--
-- C99 6.9.1p12 makes the caller's use of that value undefined, and it is
-- nearly always a missing @return@ on one branch rather than a deliberate
-- choice. @main@ is exempt: 6.5.2.2p5 defines falling off the end of @main@ as
-- @return 0@.
warnMissingReturn :: FuncDef -> Stmt -> Sema ()
warnMissingReturn fd body
  | baseName (fdName fd) == "main" = pure ()
  | ret <- funcRet (fdType fd)
  , ret /= TVoid
  , fallsThrough body =
      let shown = baseName (fdName fd)
       in
      emit
        . withLabel "control can reach here without returning a value"
        . withHelp ("add a return, or make '" ++ shown ++ "' return void")
        . withSnap shown
        . withLen (length shown)
        $ (diag Warning (fdLoc fd) ("non-void function '" ++ shown ++ "' can end without returning a value"))
          {msgGroup = Just WMissingReturn}
  | otherwise = pure ()

-- ---- scopes and symbols ----

pushScope :: Sema ()
pushScope = modify' $ \s -> s {ssScopes = M.empty : ssScopes s}

popScope :: Sema ()
popScope = modify' $ \s -> s {ssScopes = drop 1 (ssScopes s)}

-- | Close a block scope, warning about the locals nothing read.
--
-- The false-positive budget here is zero, so this only fires on a plain
-- block-scope variable: never a parameter (an unused one is often required by
-- a signature), never anything extern, static or global (another unit may
-- read it), and never a name starting with @_@, which is the established way
-- to say "declared on purpose, not used".
popScopeChecked :: Sema ()
popScopeChecked = do
  s <- get
  case ssScopes s of
    [] -> pure ()
    (sc : _) -> do
      let unused =
            [ (sid, name)
            | (name, sid) <- M.toList sc
            , not (S.member sid (ssUsed s))
            , not ("_" `isPrefixOf` name)
            , Just sym <- [M.lookup sid (ssSyms s)]
            , symKind sym == SymVar
            , not (symIsExtern sym)
            , not (symIsGlobal sym)
            , not (symIsStatic sym)
            ]
      mapM_ warnUnused unused
      popScope
  where
    warnUnused (sid, name) = do
      mloc <- gets (M.lookup sid . ssDeclLoc)
      case mloc of
        Nothing -> pure ()
        Just loc ->
          emit
            . withHelp
              ( "remove it, or rename it to '_"
                  ++ name
                  ++ "' to keep it and say so on purpose"
              )
            . withLabel "declared here and never read"
            . withSnap name
            . withLen (length name)
            $ (diag Warning loc ("unused variable '" ++ name ++ "'"))
              {msgGroup = Just WUnused}

lookupSym :: String -> Sema (Maybe SymId)
lookupSym name = do
  r <- gets (go . ssScopes)
  -- Reading a name is what "used" means. Marking here catches every read,
  -- including the ones inside sizeof and initializers.
  case r of
    Just sid -> modify' (\s -> s {ssUsed = S.insert sid (ssUsed s)}) >> pure r
    Nothing -> pure r
  where
    go [] = Nothing
    go (sc : rest) = case M.lookup name sc of
      Just sid -> Just sid
      Nothing -> go rest

lookupLocal :: String -> Sema (Maybe SymId)
lookupLocal name = gets (\s -> case ssScopes s of
  (sc : _) -> M.lookup name sc
  [] -> Nothing)

getSym :: SymId -> Sema Symbol
getSym sid = gets (\s -> ssSyms s M.! sid)

modifySym :: SymId -> (Symbol -> Symbol) -> Sema ()
modifySym sid f = modify' $ \s -> s {ssSyms = M.adjust f sid (ssSyms s)}

-- | Insert into the current scope. A redeclaration that C tolerates (a second
-- prototype, an extern re-declaration, a repeated enum constant or typedef)
-- returns the existing symbol instead of erroring.
insertSym :: SymKind -> String -> Type -> SrcLoc -> Bool -> Sema SymId
insertSym kind name ty loc declIsExtern = do
  existing <- lookupLocal name
  case existing of
    Just sid -> do
      old <- getSym sid
      let ok = case (kind, symKind old) of
            (SymFunc, SymFunc) -> True
            (SymVar, SymVar) -> symIsExtern old || declIsExtern
            (SymEnumConst, SymEnumConst) -> True
            (SymTypedef, SymTypedef) -> True
            _ -> False
      unless ok $ do
        prev <- gets (M.lookup sid . ssDeclLoc)
        emit
          . withNotes
            [ withSnap name (withLen (length name) (note p ("previous declaration of '" ++ name ++ "' is here")))
            | Just p <- [prev]
            , p /= loc
            ]
          . withLabel "redefined here"
          . withCode "E0100"
          . withSnap name
          . withLen (length name)
          $ diag Error loc ("redefinition of '" ++ name ++ "'")
      pure sid
    Nothing -> do
      s <- get
      let sid = ssNextSym s
          sym =
            Symbol
              { symId = sid
              , symKind = kind
              , symName = name
              , symLinkName = name
              , symType = ty
              , symEnumVal = 0
              , symIsExtern = False
              , symIsGlobal = False
              , symIsStatic = False
              , symIsDefined = False
              , symAddrTaken = False
              }
      put
        s
          { ssNextSym = sid + 1
          , ssSyms = M.insert sid sym (ssSyms s)
          , ssDeclLoc = M.insert sid loc (ssDeclLoc s)
          , ssScopes = case ssScopes s of
              (sc : rest) -> M.insert name sid sc : rest
              [] -> [M.singleton name sid]
          }
      pure sid

addGlobal :: SymId -> Sema ()
addGlobal sid = modify' $ \s -> s {ssGlobals = sid : ssGlobals s}

-- ---- helpers ----

typeOf :: Expr -> Type
typeOf e = fromMaybe TInt (exTy e)

setTy :: Type -> Expr -> Expr
setTy t e = e {exTy = Just t}

tcGet :: Sema TypeContext
tcGet = gets ssTc

sizeOfType :: Type -> Sema Int
sizeOfType t = do
  tc <- tcGet
  pure (typeSize tc t)

-- | The two-u64 struct that @__int128@ desugars to. Recognized by tag name,
-- exactly as the C does, because that is the only thing distinguishing it from
-- any other two-word struct.
isU128 :: TypeContext -> Type -> Bool
isU128 tc (TStruct tid) = tagName (tagInfo tc tid) == Just "__c99m_u128"
isU128 _ _ = False

isLvalue :: Expr -> Bool
isLvalue e = case exNode e of
  EIdent _ _ -> True
  EIndex _ _ -> True
  EMember {} -> True
  EUnary Deref _ -> True
  _ -> False

-- | Integer constant folding over a *checked* expression (C99 6.6). Enum
-- constants and sizeof have already been rewritten to literals by then, so
-- this only has to handle literals, casts, and arithmetic.
foldConst :: Expr -> Maybe Integer
foldConst e = case exNode e of
  EInt n _ -> Just n
  EChar n -> Just n
  ECast _ x -> foldConst x
  EUnary op x -> do
    v <- foldConst x
    case op of
      Neg -> Just (negate v)
      UPlus -> Just v
      BNot -> Just (complement v)
      Not -> Just (if v == 0 then 1 else 0)
      _ -> Nothing
  EBinary op l r -> do
    a <- foldConst l
    b <- foldConst r
    case op of
      Add -> Just (a + b)
      Sub -> Just (a - b)
      Mul -> Just (a * b)
      Div -> if b == 0 then Nothing else Just (a `quot` b)
      Mod -> if b == 0 then Nothing else Just (a `rem` b)
      Shl -> Just (a `shiftL` fromIntegral (b .&. 63))
      Shr -> Just (a `shiftR` fromIntegral (b .&. 63))
      BAnd -> Just (a .&. b)
      BOr -> Just (a .|. b)
      BXor -> Just (a `xor` b)
      _ -> Nothing
  _ -> Nothing

-- ---- expressions ----

checkExpr :: Expr -> Sema Expr
checkExpr e = case exNode e of
  EInt _ suf -> pure (setTy (intLitType suf) e)
  EFloat _ isF -> pure (setTy (if isF then TFloat else TDouble) e)
  EChar _ -> pure (setTy TInt e) -- C promotes character literals to int
  EString s ->
    -- char[N], not char*: sizeof("ab") is 3. Use sites that want a pointer
    -- get one from the usual decay.
    pure (setTy (TArray TChar (length s + 1) AFixed) e)
  EIdent name _ -> checkIdent e name
  EBinary op l r -> checkBinary e op l r
  EUnary op x -> checkUnary e op x
  EPostfix op x -> checkPostfix e op x
  EAssign op l r -> checkAssign e op l r
  ECall f args mty -> checkCall e f args mty
  EIndex l r -> checkIndex e l r
  EMember obj name arrow -> checkMember e obj name arrow
  ECast ty x -> checkCast e ty x
  ESizeofExpr x -> do
    -- sizeof does not decay its operand
    x' <- checkExpr x
    -- A variably modified operand has no size until its bounds have been
    -- read, so the node survives to lowering instead of folding here.
    if typeIsVM (typeOf x')
      then pure (setTy TULLong e {exNode = ESizeofExpr x'})
      else do
        n <- sizeOfType (typeOf x')
        pure (litULL (exLoc e) (fromIntegral n))
  ESizeofType ty
    -- A type name carries no declaration, so nothing evaluates its bounds and
    -- lowering has nothing to scale by. Say so rather than answer wrongly.
    | typeIsVM ty -> do
        errAt (exLoc e) "sizeof a variably modified type name is not supported"
        pure (litULL (exLoc e) 0)
    | otherwise -> do
        n <- sizeOfType ty
        pure (litULL (exLoc e) (fromIntegral n))
  ECond c l r -> do
    c' <- checkExpr c
    l' <- decayed <$> checkExpr l
    r' <- decayed <$> checkExpr r
    tc <- tcGet
    let lt = typeOf l'
        rt = typeOf r'
        t
          | typeIsArithmetic lt && typeIsArithmetic rt = typeUsualArith tc lt rt
          | TPtr _ <- lt = lt
          | otherwise = rt
    pure (setTy t e {exNode = ECond c' l' r'})
  EComma l r -> do
    l' <- checkExpr l
    r' <- checkExpr r
    let r'' = decayed r'
    pure (setTy (typeOf r'') e {exNode = EComma l' r''})
  ECompoundLit ty i -> do
    i' <- checkInit i
    -- an unsized array literal takes its bound from the initializer, or it
    -- would be laid out as zero bytes and every element past the first would
    -- land outside its storage
    let ty' = case ty of
          TArray b 0 vla | Just n <- initArrayLen i' -> TArray b (max 1 n) vla
          _ -> ty
    pure (setTy ty' e {exNode = ECompoundLit ty' i'})
  EBuiltin name args mty -> do
    args' <- mapM checkExpr args
    let t
          | name == "__c99m_I" = TComplex TDouble
          | otherwise = fromMaybe TInt mty
    pure (setTy t e {exNode = EBuiltin name args' mty})

intLitType :: IntSuffix -> Type
intLitType (IntSuffix uns lng) = case lng of
  2 -> if uns then TULLong else TLLong
  1 -> if uns then TULong else TLong
  _ -> if uns then TUInt else TInt

litULL :: SrcLoc -> Integer -> Expr
litULL loc n = (mkExpr loc (EInt n (IntSuffix True 2))) {exTy = Just TULLong}

litInt :: SrcLoc -> Integer -> Expr
litInt loc n = (mkExpr loc (EInt n noSuffix)) {exTy = Just TInt}

-- | Apply array-to-pointer / function-to-pointer decay to an expression's
-- recorded type. Lowering reads exTy, so the decay has to be written down.
decayed :: Expr -> Expr
decayed e = setTy (typeDecay (typeOf e)) e

checkIdent :: Expr -> String -> Sema Expr
checkIdent e name
  | name == "__c99m_I" =
      pure (setTy (TComplex TDouble) e {exNode = EBuiltin "__c99m_I" [] Nothing})
  | otherwise = do
      msid <- lookupSym name
      case msid of
        Nothing -> do
          names <- visibleNames
          let base =
                withLabel "not found in this scope" $
                  withCode "E0102" $
                    withLen (length name) $
                      diag Error (exLoc e) ("undeclared identifier '" ++ name ++ "'")
          emit $ case closestCandidate name names of
            Just c -> withHelp ("did you mean '" ++ c ++ "'?") base
            Nothing -> base
          pure (setTy TInt e)
        Just sid -> do
          sym <- getSym sid
          case symKind sym of
            SymEnumConst -> pure (litInt (exLoc e) (symEnumVal sym))
            SymFunc ->
              pure (setTy (TPtr (symType sym)) e {exNode = EIdent name (Just sid)})
            _ ->
              pure (setTy (symType sym) e {exNode = EIdent name (Just sid)})

checkBinary :: Expr -> BinOp -> Expr -> Expr -> Sema Expr
checkBinary e op l0 r0 = do
  l <- decayed <$> checkExpr l0
  r <- decayed <$> checkExpr r0
  tc <- tcGet
  let lt = typeOf l
      rt = typeOf r
  if isU128 tc lt || isU128 tc rt
    then u128Binary e op l r
    else do
      let arith = typeUsualArith tc lt rt
          t = case op of
            Add
              | TPtr _ <- lt, typeIsInteger rt -> lt
              | TPtr _ <- rt, typeIsInteger lt -> rt
              | otherwise -> arith
            Sub
              | TPtr _ <- lt, TPtr _ <- rt -> TLLong -- ptrdiff_t
              | TPtr _ <- lt, typeIsInteger rt -> lt
              | otherwise -> arith
            Mul -> arith
            Div -> arith
            Mod -> arith
            Eq -> TInt
            Ne -> TInt
            Lt -> TInt
            Le -> TInt
            Gt -> TInt
            Ge -> TInt
            LAnd -> TInt
            LOr -> TInt
            BAnd -> arith
            BOr -> arith
            BXor -> arith
            -- C99 6.5.7p3: each operand is promoted separately and the result
            -- type is the promoted LEFT operand. Using the usual arithmetic
            -- conversions here let an unsigned right operand make the result
            -- unsigned, which turned `someInt >> someUnsigned` into a logical
            -- shift and gave -8 >> 1u as 2147483644.
            Shl -> typePromote lt
            Shr -> typePromote lt
          -- complex arithmetic: the result is complex unless it is a comparison
          t' = case (lt, rt) of
            _
              | isComplex lt || isComplex rt ->
                  let elem' = case (lt, rt) of
                        (TComplex b, _) -> b
                        (_, TComplex b) -> b
                        _ -> TDouble
                   in if op == Eq || op == Ne then TInt else TComplex elem'
              | otherwise -> t
      pure (setTy t' e {exNode = EBinary op l r})
  where
    isComplex (TComplex _) = True
    isComplex _ = False

-- | @a op b@ where either side is a u128 becomes a call to the corresponding
-- __c99m_u128_* helper, with any narrow operand widened by
-- __c99m_u128_from_u64. Shifts are the exception: their count stays scalar.
u128Binary :: Expr -> BinOp -> Expr -> Expr -> Sema Expr
u128Binary e op l r = do
  tc <- tcGet
  case helperFor op of
    Nothing -> do
      errAt (exLoc e) "unsupported operator on __int128"
      pure (setTy TInt e)
    Just (fn, isShift) -> do
      let lt = typeOf l
          rt = typeOf r
      a <- if isU128 tc lt then pure l else u128Wrap l
      b <-
        if isShift
          then
            if isU128 tc rt
              then -- take the shift count from the low word
                checkExpr (mkExpr (exLoc r) (EMember r "lo" False))
              else pure r
          else
            if isU128 tc rt
              then pure r
              else u128Wrap r
      checkExpr (mkExpr (exLoc e) (ECall (identExpr (exLoc e) fn) [a, b] Nothing))
  where
    helperFor o = case o of
      Add -> Just ("__c99m_u128_add", False)
      Sub -> Just ("__c99m_u128_sub", False)
      Mul -> Just ("__c99m_u128_mul", False)
      Div -> Just ("__c99m_u128_div", False)
      Mod -> Just ("__c99m_u128_mod", False)
      BAnd -> Just ("__c99m_u128_and", False)
      BOr -> Just ("__c99m_u128_or", False)
      BXor -> Just ("__c99m_u128_xor", False)
      Shl -> Just ("__c99m_u128_shl", True)
      Shr -> Just ("__c99m_u128_shr", True)
      Lt -> Just ("__c99m_u128_lt", False)
      Le -> Just ("__c99m_u128_le", False)
      Gt -> Just ("__c99m_u128_gt", False)
      Ge -> Just ("__c99m_u128_ge", False)
      Eq -> Just ("__c99m_u128_eq", False)
      Ne -> Just ("__c99m_u128_ne", False)
      _ -> Nothing

identExpr :: SrcLoc -> String -> Expr
identExpr loc name = mkExpr loc (EIdent name Nothing)

-- | @__c99m_u128_from_u64((unsigned long long) e)@
u128Wrap :: Expr -> Sema Expr
u128Wrap x = do
  let loc = exLoc x
      cast = mkExpr loc (ECast TULLong x)
  checkExpr (mkExpr loc (ECall (identExpr loc "__c99m_u128_from_u64") [cast] Nothing))

checkUnary :: Expr -> UnOp -> Expr -> Sema Expr
checkUnary e op x0 = do
  x <- decayed <$> checkExpr x0
  tc <- tcGet
  let t = typeOf x
  case op of
    Not -> pure (setTy TInt e {exNode = EUnary Not x})
    PreInc | isU128 tc t -> incDecAsAssign Add
    PreDec | isU128 tc t -> incDecAsAssign Sub
    PreInc -> pure (setTy (typePromote t) e {exNode = EUnary op x})
    PreDec -> pure (setTy (typePromote t) e {exNode = EUnary op x})
    UPlus | isU128 tc t -> pure x -- +x is x
    Neg | isU128 tc t -> u128Unary "__c99m_u128_neg" x
    BNot | isU128 tc t -> u128Unary "__c99m_u128_not" x
    Neg -> pure (setTy (typePromote t) e {exNode = EUnary op x})
    UPlus -> pure (setTy (typePromote t) e {exNode = EUnary op x})
    BNot -> pure (setTy (typePromote t) e {exNode = EUnary op x})
    AddrOf -> do
      -- Force addressable storage: libmtlc's address_of is local/param
      -- oriented, so a global whose address escapes must be laid out in memory.
      x' <- checkExpr x0 -- re-check WITHOUT decay: &arr is not &(arr[0])
      case exNode x' of
        EIdent _ (Just sid) -> modifySym sid (\s -> s {symAddrTaken = True})
        _ -> pure ()
      pure (setTy (TPtr (typeOf x')) e {exNode = EUnary AddrOf x'})
    Deref -> case t of
      TPtr b -> pure (setTy b e {exNode = EUnary Deref x})
      TArray b _ _ -> pure (setTy b e {exNode = EUnary Deref x})
      _ -> do
        errAt (exLoc e) "indirection requires pointer operand"
        pure (setTy TInt e {exNode = EUnary Deref x})
  where
    u128Unary fn x =
      checkExpr (mkExpr (exLoc e) (ECall (identExpr (exLoc e) fn) [x] Nothing))
    -- ++x on a u128 becomes x = x + 1
    incDecAsAssign bop = do
      let loc = exLoc e
          one = mkExpr loc (EInt 1 noSuffix)
          bin = mkExpr loc (EBinary bop x0 one)
      checkExpr (mkExpr loc (EAssign Assign x0 bin))

checkPostfix :: Expr -> PostOp -> Expr -> Sema Expr
checkPostfix e op x0 = do
  x <- checkExpr x0
  tc <- tcGet
  let t = typeOf x
  if isU128 tc t
    then do
      -- x++ on a u128 becomes x = x + 1: the frontend only supports it in
      -- statement position, where the result value is discarded anyway.
      let loc = exLoc e
          bop = if op == PostInc then Add else Sub
          one = mkExpr loc (EInt 1 noSuffix)
          bin = mkExpr loc (EBinary bop x0 one)
      checkExpr (mkExpr loc (EAssign Assign x0 bin))
    else pure (setTy (typePromote (typeDecay t)) e {exNode = EPostfix op x})

checkAssign :: Expr -> AssignOp -> Expr -> Expr -> Sema Expr
checkAssign e op l0 r0 = do
  l <- checkExpr l0
  r <- decayed <$> checkExpr r0
  tc <- tcGet
  unless (isLvalue l) $
    errAt (exLoc e) "lvalue required as left operand of assignment"
  case op of
    AssignOp bop
      | isU128 tc (typeOf l) || isU128 tc (typeOf r) -> do
          -- a op= b becomes a = a op b, so the u128 binary rewrite handles it
          let bin = mkExpr (exLoc e) (EBinary bop l0 r0)
          checkExpr (mkExpr (exLoc e) (EAssign Assign l0 bin))
    _ -> pure (setTy (typeOf l) e {exNode = EAssign op l r})

checkCall :: Expr -> Expr -> [Expr] -> Maybe Type -> Sema Expr
checkCall e f args mty
  | EIdent name _ <- exNode f
  , name `elem` ["__builtin_va_start", "__builtin_va_end"] = do
      args' <- mapM checkExpr args
      pure (setTy TVoid e {exNode = EBuiltin name args' Nothing})
  | EIdent "__builtin_va_arg" _ <- exNode f = do
      args' <- mapM checkExpr args
      let t = fromMaybe TInt mty
      pure (setTy t e {exNode = EBuiltin "__builtin_va_arg" args' mty})
  | EIdent name _ <- exNode f
  , name `elem` ["__real__", "__imag__"] = do
      args' <- mapM checkExpr args
      let t = case args' of
            (a : _) | TComplex b <- typeOf a -> b
            _ -> TDouble
      pure (setTy t e {exNode = EBuiltin name args' Nothing})
  | otherwise = do
      f' <- decayed <$> checkExpr f
      args' <- map decayed <$> mapM checkExpr args
      let ft = case typeOf f' of
            TPtr inner@TFunc {} -> Just inner
            inner@TFunc {} -> Just inner
            _ -> Nothing
      case ft of
        Nothing -> do
          errAt (exLoc e) "called object is not a function"
          pure (setTy TInt e {exNode = ECall f' args' mty})
        Just fnty -> do
          let np = length (funcParams fnty)
              na = length args'
          when (not (funcVariadic fnty) && not (funcOldStyle fnty) && na /= np) $
            errAt (exLoc e) $
              "wrong number of arguments (got "
                ++ show na
                ++ ", expected "
                ++ show np
                ++ ")"
          pure (setTy (funcRet fnty) e {exNode = ECall f' args' mty})

checkIndex :: Expr -> Expr -> Expr -> Sema Expr
checkIndex e l0 r0 = do
  l <- decayed <$> checkExpr l0
  r <- checkExpr r0
  case typeOf l of
    TPtr b -> pure (setTy b e {exNode = EIndex l r})
    _ -> do
      errAt (exLoc e) "subscripted value is not a pointer"
      pure (setTy TInt e {exNode = EIndex l r})

checkMember :: Expr -> Expr -> String -> Bool -> Sema Expr
checkMember e obj0 name arrow = do
  obj <- checkExpr obj0
  tc <- tcGet
  let base
        | arrow = case typeDecay (typeOf obj) of
            TPtr b -> Right b
            _ -> Left "-> on non-pointer"
        | otherwise = Right (typeOf obj)
  case base of
    Left msg -> do
      errAt (exLoc e) msg
      pure (setTy TInt e {exNode = EMember obj name arrow})
    Right t@(TStruct _) -> case findMember tc t name of
      Just m -> pure (setTy (memType m) e {exNode = EMember obj name arrow})
      Nothing -> do
        errAt (exLoc e) ("no member named '" ++ name ++ "'")
        pure (setTy TInt e {exNode = EMember obj name arrow})
    Right _ -> do
      errAt (exLoc e) "member reference on non-struct"
      pure (setTy TInt e {exNode = EMember obj name arrow})

checkCast :: Expr -> Type -> Expr -> Sema Expr
checkCast e ty x0 = do
  x <- decayed <$> checkExpr x0
  tc <- tcGet
  when (typeIsVM ty) $
    errAt (exLoc e) "cast to a variably modified type is not supported"
  let it = typeOf x
      loc = exLoc e
  case () of
    _
      | isU128 tc ty && not (isU128 tc it) ->
          -- (u128)x becomes __c99m_u128_from_u64((unsigned long long)x)
          checkExpr
            ( mkExpr
                loc
                ( ECall
                    (identExpr loc "__c99m_u128_from_u64")
                    [mkExpr loc (ECast TULLong x0)]
                    Nothing
                )
            )
      | not (isU128 tc ty) && isU128 tc it && typeIsInteger ty -> do
          -- (int)u128 becomes (int)__c99m_u128_to_u64(x)
          call <-
            checkExpr
              ( mkExpr
                  loc
                  (ECall (identExpr loc "__c99m_u128_to_u64") [x0] Nothing)
              )
          pure (setTy ty e {exNode = ECast ty call})
      | otherwise -> pure (setTy ty e {exNode = ECast ty x})

checkInit :: Init -> Sema Init
checkInit (IExpr x) = IExpr <$> checkExpr x
checkInit (IList items) = IList <$> mapM item items
  where
    item (mdes, i) = do
      mdes' <- case mdes of
        Just (DIndex ix) -> Just . DIndex <$> checkExpr ix
        other -> pure other
      i' <- checkInit i
      pure (mdes', i')

-- ---- statements ----

checkStmt :: Stmt -> Sema Stmt
checkStmt st = case stNode st of
  SNull -> pure st
  SExpr x -> node . SExpr <$> checkExpr x
  SCompound items -> do
    pushScope
    warnUnreachable items
    items' <- mapM checkBlockItem items
    popScopeChecked
    pure (node (SCompound items'))
  SIf c b me -> do
    c' <- checkExpr c
    b' <- checkStmt b
    me' <- mapM checkStmt me
    pure (node (SIf c' b' me'))
  SWhile c b -> do
    c' <- checkExpr c
    b' <- inLoop (checkStmt b)
    pure (node (SWhile c' b'))
  SDo b c -> do
    c' <- checkExpr c
    b' <- inLoop (checkStmt b)
    pure (node (SDo b' c'))
  SFor i c inc b -> do
    pushScope
    i' <- mapM checkBlockItem i
    c' <- mapM checkExpr c
    inc' <- mapM checkExpr inc
    b' <- inLoop (checkStmt b)
    popScopeChecked
    pure (node (SFor i' c' inc' b'))
  SBreak -> do
    d <- gets ssLoopDepth
    sw <- gets ssSwitchDepth
    when (d == 0 && sw == 0) $ errAt (stLoc st) "break outside loop or switch"
    pure st
  SContinue -> do
    d <- gets ssLoopDepth
    when (d == 0) $ errAt (stLoc st) "continue outside loop"
    pure st
  SReturn Nothing -> do
    rt <- gets ssRetTy
    when (maybe False (/= TVoid) rt) $
      errAt (stLoc st) "return with no value in non-void function"
    pure st
  SReturn (Just x) -> node . SReturn . Just <$> checkExpr x
  SSwitch c b -> do
    c' <- checkExpr c
    b' <- inSwitch (checkStmt b)
    pure (node (SSwitch c' b'))
  SCase x b -> do
    sw <- gets ssSwitchDepth
    when (sw == 0) $ errAt (stLoc st) "case label not in switch"
    x' <- checkExpr x
    b' <- checkStmt b
    pure (node (SCase x' b'))
  SDefault b -> do
    sw <- gets ssSwitchDepth
    when (sw == 0) $ errAt (stLoc st) "default label not in switch"
    node . SDefault <$> checkStmt b
  SGoto _ -> pure st
  SLabel l b -> node . SLabel l <$> checkStmt b
  where
    node n = st {stNode = n}
    inLoop m = do
      modify' (\s -> s {ssLoopDepth = ssLoopDepth s + 1})
      r <- m
      modify' (\s -> s {ssLoopDepth = ssLoopDepth s - 1})
      pure r
    inSwitch m = do
      modify' (\s -> s {ssSwitchDepth = ssSwitchDepth s + 1})
      r <- m
      modify' (\s -> s {ssSwitchDepth = ssSwitchDepth s - 1})
      pure r

checkBlockItem :: BlockItem -> Sema BlockItem
checkBlockItem (BIStmt s) = BIStmt <$> checkStmt s
checkBlockItem (BIDecl ds) = BIDecl <$> mapM (checkDecl False) ds

-- ---- declarations ----

-- | Complete @T x[] = {...}@ / @char s[] = "..."@ from the initializer
-- (C99 6.7.8p22).
inferArrayLen :: Decl -> Decl
inferArrayLen d = case (dType d, dInit d) of
  (TArray b 0 vla, Just ini)
    | Just n <- initArrayLen ini -> d {dType = TArray b (max 1 n) vla}
  _ -> d

-- | The element count an initializer implies for an unsized array.
initArrayLen :: Init -> Maybe Int
initArrayLen (IExpr x) | EString s <- exNode x = Just (length s + 1)
initArrayLen (IList items) = Just (foldl' step 0 (zip [0 ..] items))
  where
    -- a designated [k] = ... resets the running index, as in C
    step n (seqIdx, (mdes, _)) =
      let idx = case mdes of
            Just (DIndex ix) | Just k <- foldConst ix -> fromIntegral k
            _ -> seqIdx
       in max n (idx + 1)
initArrayLen _ = Nothing

checkDecl :: Bool -> Decl -> Sema Decl
checkDecl isGlobal d0 = case dStorage d0 of
  ScTypedef -> do
    sid <- insertSym SymTypedef (dName d0) (dType d0) (dLoc d0) False
    pure d0 {dSym = Just sid}
  _
    | TFunc {} <- dType d0 -> do
        -- a prototype
        sid <- insertSym SymFunc (dName d0) (dType d0) (dLoc d0) False
        let isStatic = dStorage d0 == ScStatic
        modifySym sid $ \s ->
          s
            { symIsStatic = symIsStatic s || isStatic
            , symIsExtern = not (symIsStatic s || isStatic) && not (symIsDefined s)
            , symIsGlobal = True
            , symLinkName = fromMaybe (dName d0) (dAsmLabel d0)
            }
        when isGlobal (addGlobal sid)
        pure d0 {dSym = Just sid}
  _ -> do
    let d1 = inferArrayLen d0
    -- A VLA bound that folds to a constant (common when it is sizeof
    -- arithmetic the parser could not fold) demotes to a fixed array.
    (d2, vla') <- do
      bs <- mapM (\(i, sz) -> (,) i <$> checkExpr sz) (dVlaBounds d1)
      let fixed =
            [ (i, fromIntegral n)
            | (i, sz') <- bs
            , Just n <- [foldConst sz']
            , n > 0
            ]
          keep = [b | b@(i, _) <- bs, isNothing (lookup i fixed)]
      pure (d1 {dType = demoteVla fixed (dType d1)}, keep)
    existing <- if isGlobal then pure (dSym d2) else pure Nothing
    sid <- case existing of
      Just sid -> do
        modifySym sid (\s -> s {symType = dType d2})
        pure sid
      Nothing -> do
        let isStatic = dStorage d2 == ScStatic
            isExtern = not isStatic && dStorage d2 == ScExtern
        sid <- insertSym SymVar (dName d2) (dType d2) (dLoc d2) isExtern
        modifySym sid $ \s ->
          s
            { symType = dType d2
            , symIsStatic = isStatic
            , symIsExtern = isExtern
            , symIsGlobal = isGlobal || isStatic
            , -- A block-scope static has static duration but no linkage, and
              -- two functions may each declare one by the same name. Key its
              -- link name to the symbol so they cannot collide.
              symLinkName =
                if isStatic && not isGlobal
                  then "__bs" ++ show sid ++ "_" ++ dName d2
                  else dName d2
            }
        when (isGlobal || isStatic) (addGlobal sid)
        pure sid
    ini <- mapM checkInit (dInit d2)
    pure d2 {dSym = Just sid, dInit = ini, dVlaBounds = vla'}

-- | Replace each VLA bound that turned out to be constant with a fixed one.
demoteVla :: [(Int, Int)] -> Type -> Type
demoteVla m t = case t of
  TArray b _ (AVla i) | Just c <- lookup i m -> TArray (demoteVla m b) c AFixed
  TArray b n k -> TArray (demoteVla m b) n k
  TPtr b -> TPtr (demoteVla m b)
  _ -> t

-- ---- program ----

checkProgram :: Program -> Sema Program
checkProgram prog = do
  -- Pass 1: register every global name, so forward references resolve.
  prog1 <- mapM declarePass prog
  -- Pass 2: check bodies and initializers.
  mapM checkPass prog1

declarePass :: TopDecl -> Sema TopDecl
declarePass td = case td of
  TDFunc fd -> do
    sid <- insertSym SymFunc (fdName fd) (fdType fd) (fdLoc fd) False
    let isStatic = fdStorage fd == ScStatic
    modifySym sid $ \s ->
      s
        { symIsGlobal = True
        , symIsStatic = symIsStatic s || isStatic
        , symIsDefined = True
        , symIsExtern = False
        , symType = fdType fd
        }
    addGlobal sid
    pure (TDFunc fd {fdSym = Just sid})
  TDDecl d -> TDDecl <$> declareDecl d
  TDEnumConst name val ty -> do
    sid <- insertSym SymEnumConst name ty noLoc False
    modifySym sid (\s -> s {symEnumVal = val})
    pure td
  TDTag _ -> pure td

declareDecl :: Decl -> Sema Decl
declareDecl d = case dStorage d of
  ScTypedef -> do
    sid <- insertSym SymTypedef (dName d) (dType d) (dLoc d) False
    pure d {dSym = Just sid}
  _
    | TFunc {} <- dType d -> do
        sid <- insertSym SymFunc (dName d) (dType d) (dLoc d) False
        let isStatic = dStorage d == ScStatic
        modifySym sid $ \s ->
          s
            { symIsGlobal = True
            , symIsStatic = symIsStatic s || isStatic
            , symIsExtern = not (symIsStatic s || isStatic) && not (symIsDefined s)
            , -- an __asm__("name") label renames the symbol at link time
              symLinkName = fromMaybe (dName d) (dAsmLabel d)
            }
        addGlobal sid
        pure d {dSym = Just sid}
    | otherwise -> do
        let d' = inferArrayLen d
            isStatic = dStorage d' == ScStatic
            isExtern = not isStatic && dStorage d' == ScExtern
            defining = not isExtern
        sid <- insertSym SymVar (dName d') (dType d') (dLoc d') isExtern
        old <- getSym sid
        tc <- tcGet
        -- The same object is typically declared `extern` in a header, included
        -- by many units, and defined once. The definition is authoritative:
        -- it decides both linkage and the type, since the header's declaration
        -- may leave an array bound off (`extern int t[];`).
        let defined = symIsDefined old || defining
            ty
              | defining = dType d'
              | typeIsComplete tc (symType old) = symType old
              | otherwise = dType d'
        modifySym sid $ \s ->
          s
            { symIsGlobal = True
            , symIsStatic = isStatic
            , symIsDefined = defined
            , symIsExtern = not defined
            , symType = ty
            , symLinkName = dName d'
            }
        addGlobal sid
        pure d' {dSym = Just sid, dType = ty}

checkPass :: TopDecl -> Sema TopDecl
checkPass td = case td of
  TDFunc fd -> do
    pushScope
    modify' (\s -> s {ssRetTy = Just (funcRet (fdType fd))})
    params <- forM (fdParams fd) $ \p -> do
      let pt = typeDecay (pType p)
      if null (pName p)
        then pure p {pType = pt}
        else do
          sid <- insertSym SymVar (pName p) pt (fdLoc fd) False
          pure p {pType = pt, pSym = Just sid}
    -- A parameter's array bounds name earlier parameters, so they can only be
    -- checked once every parameter is in scope.
    params' <- forM params $ \p -> do
      bs <- mapM (\(i, x) -> (,) i <$> checkExpr x) (pVlaBounds p)
      pure p {pVlaBounds = bs}
    body <- checkStmt (fdBody fd)
    warnMissingReturn fd body
    modify' (\s -> s {ssRetTy = Nothing})
    popScope
    pure (TDFunc fd {fdParams = params', fdBody = body})
  TDDecl d
    | dStorage d /= ScTypedef
    , not (isFuncTy (dType d)) ->
        TDDecl <$> checkDecl True d
  _ -> pure td
  where
    isFuncTy TFunc {} = True
    isFuncTy _ = False
