{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Schminke.Codegen where

import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.String
import Data.Word

import Control.Applicative
import Control.Monad.State

import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.AST.Global

import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as T

newtype LLVM a = LLVM
  { unLLVM :: State AST.Module a
  } deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule {moduleName = label}

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s {moduleDefinitions = defs ++ [d]}

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body =
  addDefn $
  GlobalDefinition $
  functionDefaults
  { name = Name label
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType = retty
  , basicBlocks = body
  }

declare :: Type -> String -> [(Type, Name)] -> LLVM ()
declare retty label argtys =
  addDefn $
  GlobalDefinition $
  functionDefaults
  { name = Name label
  , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType = retty
  , basicBlocks = []
  }

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState
  { currentBlock :: Name
  , blocks :: Map.Map Name BlockState
  , symtab :: SymbolTable
  , blockCount :: Int
  , count :: Word
  , names :: Names
  } deriving (Show)

data BlockState = BlockState
  { idx :: Int -- Block index
  , stack :: [Named Instruction] -- Stack of instructions
  , term :: Maybe (Named Terminator) -- Block terminator
  } deriving (Show)

newtype Codegen a = Codegen
  { runCodegen :: State CodegenState a
  } deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s {count = 1 + i}
  return $ i + 1

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = i ++ [ref := ins]})
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk {term = Just trm})
  return trm

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s ->
    s
    { blocks = Map.insert (Name qname) new bls
    , blockCount = ix + 1
    , names = supply
    }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s {currentBlock = bname}
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s {blocks = Map.insert active new (blocks s)}

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s {symtab = (var, x) : lcls}

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "local variable not in scope: " ++ show var

local :: Name -> Operand
local = LocalReference T.void

global :: Name -> C.Constant
global = C.GlobalReference T.void

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference T.void

add :: Operand -> Operand -> Codegen Operand
add a b = instr $ Add False False a b []

sub :: Operand -> Operand -> Codegen Operand
sub a b = instr $ Sub False False a b []

mul :: Operand -> Operand -> Codegen Operand
mul a b = instr $ Mul False False a b []

sdiv :: Operand -> Operand -> Codegen Operand
sdiv a b = instr $ SDiv False a b []

srem :: Operand -> Operand -> Codegen Operand
srem a b = instr $ SRem a b []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr $ ICmp cond a b []

eq :: Operand -> Operand -> Codegen Operand
eq = icmp IP.EQ

trunc :: Operand -> Type -> Codegen Operand
trunc val ty = instr $ Trunc val ty []

cons :: C.Constant -> Operand
cons = ConstantOperand

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []
