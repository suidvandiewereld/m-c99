-- | File-scope static mangling (src/main.c).
--
-- Translation units are merged into one module before lowering, so a
-- file-scope @static@ in one unit would collide with a same-named symbol in
-- another. Renaming it to @__<prefix><tu>_<name>@ restores internal linkage.
--
-- Only /free/ uses of the name are rewritten. A local, parameter, or typedef
-- that shadows the static keeps its own meaning, so the walk carries the map
-- of still-visible renames and drops a name once a block re-binds it.
-- Everything happens in one traversal, however many statics the unit has.
module C99.StaticRename
  ( mangleStatics
  ) where

import qualified Data.Map.Strict as M

import C99.Ast

type Renames = M.Map String String

-- | Mangle every file-scope static in one translation unit, rewriting its free
-- uses across the whole unit. @prefix@ and @tu@ form the mangled name.
mangleStatics :: String -> Int -> Program -> Program
mangleStatics prefix tu prog
  | M.null renames = prog
  | otherwise = map decl prog
  where
    renames =
      M.fromList
        [ (name, "__" ++ prefix ++ show tu ++ "_" ++ name)
        | td <- prog
        , Just name <- [staticName td]
        ]

    staticName (TDFunc fd)
      | fdStorage fd == ScStatic, not (null (fdName fd)) = Just (fdName fd)
    staticName (TDDecl d)
      | dStorage d == ScStatic
      , not (null (dName d))
      , dStorage d /= ScTypedef =
          Just (dName d)
    staticName _ = Nothing

    decl td = case td of
      TDFunc fd ->
        -- Function parameters shadow for the body.
        let rm = foldr (M.delete . pName) renames (fdParams fd)
            fd' = fd {fdBody = stmt rm (fdBody fd)}
         in TDFunc fd' {fdName = rename (fdName fd')}
      TDDecl d ->
        let d' = d {dInit = fmap (initz renames) (dInit d)}
         in TDDecl d' {dName = rename (dName d')}
      _ -> td

    rename n = M.findWithDefault n n renames

-- The walk takes the coarse view the C frontend took — once any declaration in
-- a block binds a name, the whole block is shadowed for it.
stmt :: Renames -> Stmt -> Stmt
stmt rm0 st
  | M.null rm0 = st
  | otherwise = case stNode st of
      SCompound items ->
        let rm = foldr unbind rm0 items
         in node (SCompound (map (blockItem rm) items))
      SExpr e -> node (SExpr (expr rm0 e))
      SIf c b me -> node (SIf (expr rm0 c) (stmt rm0 b) (fmap (stmt rm0) me))
      SWhile c b -> node (SWhile (expr rm0 c) (stmt rm0 b))
      SDo b c -> node (SDo (stmt rm0 b) (expr rm0 c))
      SFor i c inc b ->
        let rm = maybe rm0 (`unbind` rm0) i
         in node
              ( SFor
                  (fmap (blockItem rm) i)
                  (fmap (expr rm) c)
                  (fmap (expr rm) inc)
                  (stmt rm b)
              )
      SReturn me -> node (SReturn (fmap (expr rm0) me))
      SSwitch c b -> node (SSwitch (expr rm0 c) (stmt rm0 b))
      SCase v b -> node (SCase (expr rm0 v) (stmt rm0 b))
      SDefault b -> node (SDefault (stmt rm0 b))
      SLabel l b -> node (SLabel l (stmt rm0 b))
      _ -> st
  where
    node n = st {stNode = n}

unbind :: BlockItem -> Renames -> Renames
unbind (BIDecl ds) rm = foldr (M.delete . dName) rm ds
unbind _ rm = rm

blockItem :: Renames -> BlockItem -> BlockItem
blockItem rm (BIStmt s) = BIStmt (stmt rm s)
blockItem rm (BIDecl ds) = BIDecl (map (localDecl rm) ds)

-- | The declarator's own name is never rewritten; its initializer still is.
localDecl :: Renames -> Decl -> Decl
localDecl rm d =
  d
    { dInit = fmap (initz rm) (dInit d)
    , dVlaSize = fmap (expr rm) (dVlaSize d)
    }

initz :: Renames -> Init -> Init
initz rm (IExpr e) = IExpr (expr rm e)
initz rm (IList items) = IList (map item items)
  where
    item (mdes, i) = (fmap des mdes, initz rm i)
    des (DIndex e) = DIndex (expr rm e)
    des other = other

expr :: Renames -> Expr -> Expr
expr rm e
  | M.null rm = e
  | otherwise = e {exNode = node (exNode e)}
  where
    node n = case n of
      EIdent name msid
        | Just new <- M.lookup name rm -> EIdent new msid
        | otherwise -> n
      EBinary op l r -> EBinary op (expr rm l) (expr rm r)
      EUnary op x -> EUnary op (expr rm x)
      EPostfix op x -> EPostfix op (expr rm x)
      EAssign op l r -> EAssign op (expr rm l) (expr rm r)
      ECall f as mt -> ECall (expr rm f) (map (expr rm) as) mt
      EIndex l r -> EIndex (expr rm l) (expr rm r)
      EMember o m a -> EMember (expr rm o) m a
      ECast t x -> ECast t (expr rm x)
      ESizeofExpr x -> ESizeofExpr (expr rm x)
      ECond c l r -> ECond (expr rm c) (expr rm l) (expr rm r)
      EComma l r -> EComma (expr rm l) (expr rm r)
      ECompoundLit t i -> ECompoundLit t (initz rm i)
      EBuiltin name as mt -> EBuiltin name (map (expr rm) as) mt
      _ -> n
