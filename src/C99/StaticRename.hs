-- | File-scope static mangling (src/main.c).
--
-- Translation units are merged into one module before lowering, so a
-- file-scope @static@ in one unit would collide with a same-named symbol in
-- another. Renaming it to @__<prefix><tu>_<name>@ restores internal linkage.
--
-- Only /free/ uses of the name are rewritten. A local, parameter, or typedef
-- that shadows the static keeps its own meaning, so the walk tracks whether the
-- name is currently shadowed and stops rewriting when it is.
module C99.StaticRename
  ( mangleStatics
  ) where

import C99.Ast

-- | Mangle every file-scope static in one translation unit, rewriting its free
-- uses across the whole unit. @prefix@ and @tu@ form the mangled name.
mangleStatics :: String -> Int -> Program -> Program
mangleStatics prefix tu prog = foldl renameOne prog statics
  where
    statics =
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

renameOne :: Program -> (String, String) -> Program
renameOne prog (old, new) = map decl prog
  where
    decl td = case td of
      TDFunc fd ->
        let shadowed = any ((== old) . pName) (fdParams fd)
            fd' = fd {fdBody = stmt shadowed (fdBody fd)}
         in TDFunc (if fdName fd' == old then fd' {fdName = new} else fd')
      TDDecl d ->
        let d' = d {dInit = fmap (initz False) (dInit d)}
         in TDDecl (if dName d' == old then d' {dName = new} else d')
      _ -> td

    -- A declaration in this block shadows the static from that point on. The C
    -- frontend takes the coarser view — once any declaration in the block binds
    -- the name, the whole block is shadowed — so match it.
    stmt sh st = case stNode st of
      SCompound items ->
        let sh' = sh || any bindsOld items
         in st {stNode = SCompound (map (blockItem sh') items)}
      SExpr e -> st {stNode = SExpr (expr sh e)}
      SIf c b me ->
        st {stNode = SIf (expr sh c) (stmt sh b) (fmap (stmt sh) me)}
      SWhile c b -> st {stNode = SWhile (expr sh c) (stmt sh b)}
      SDo b c -> st {stNode = SDo (stmt sh b) (expr sh c)}
      SFor i c inc b ->
        let sh' = sh || maybe False bindsOld i
         in st
              { stNode =
                  SFor
                    (fmap (blockItem sh') i)
                    (fmap (expr sh') c)
                    (fmap (expr sh') inc)
                    (stmt sh' b)
              }
      SReturn me -> st {stNode = SReturn (fmap (expr sh) me)}
      SSwitch c b -> st {stNode = SSwitch (expr sh c) (stmt sh b)}
      SCase v b -> st {stNode = SCase (expr sh v) (stmt sh b)}
      SDefault b -> st {stNode = SDefault (stmt sh b)}
      SLabel l b -> st {stNode = SLabel l (stmt sh b)}
      _ -> st

    bindsOld (BIDecl ds) = any ((== old) . dName) ds
    bindsOld _ = False

    blockItem sh (BIStmt s) = BIStmt (stmt sh s)
    blockItem sh (BIDecl ds) = BIDecl (map (localDecl sh) ds)

    -- The declarator's own name is never rewritten; its initializer still is.
    localDecl sh d =
      d
        { dInit = fmap (initz sh) (dInit d)
        , dVlaSize = fmap (expr sh) (dVlaSize d)
        }

    initz sh (IExpr e) = IExpr (expr sh e)
    initz sh (IList items) = IList (map item items)
      where
        item (mdes, i) = (fmap des mdes, initz sh i)
        des (DIndex e) = DIndex (expr sh e)
        des other = other

    expr sh e
      | sh = e
      | otherwise = e {exNode = node (exNode e)}
      where
        node n = case n of
          EIdent name msid
            | name == old -> EIdent new msid
            | otherwise -> n
          EBinary op l r -> EBinary op (expr sh l) (expr sh r)
          EUnary op x -> EUnary op (expr sh x)
          EPostfix op x -> EPostfix op (expr sh x)
          EAssign op l r -> EAssign op (expr sh l) (expr sh r)
          ECall f as mt -> ECall (expr sh f) (map (expr sh) as) mt
          EIndex l r -> EIndex (expr sh l) (expr sh r)
          EMember o m a -> EMember (expr sh o) m a
          ECast t x -> ECast t (expr sh x)
          ESizeofExpr x -> ESizeofExpr (expr sh x)
          ECond c l r -> ECond (expr sh c) (expr sh l) (expr sh r)
          EComma l r -> EComma (expr sh l) (expr sh r)
          ECompoundLit t i -> ECompoundLit t (initz sh i)
          EBuiltin name as mt -> EBuiltin name (map (expr sh) as) mt
          _ -> n
