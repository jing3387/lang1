module Language.Schminke.Frontend.Type where

type Name = String

newtype TVar =
  TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TPro Type
         Type
  | TSum Type
         Type
  | TArr Type
         Type
  deriving (Show, Eq, Ord)

data Scheme =
  Forall [TVar]
         Type
  deriving (Show, Eq, Ord)

typeInt :: Type
typeInt = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

typeOps :: [String]
typeOps = ["->", "+", "*"]

intBinop :: Scheme
intBinop = Forall [] (TArr typeInt (TArr typeInt typeInt))

prims :: [(Name, Scheme)]
prims =
  [ ( "eq"
    , Forall
        [(TV "a"), (TV "b")]
        (TArr
           typeInt
           (TArr
              typeInt
              (TArr
                 (TVar (TV "a"))
                 (TArr (TVar (TV "b")) (TSum (TVar (TV "a")) (TVar (TV "b"))))))))
  , ("add", intBinop)
  , ("sub", intBinop)
  , ("mul", intBinop)
  , ("sdiv", intBinop)
  , ("srem", intBinop)
  ]
