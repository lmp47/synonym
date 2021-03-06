{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------
-- Module    :  Main
-- Copyright :  (c) 2015 Marcelo Sousa
--                  2018 Lauren Pick
-------------------------------------------------------------------------------
module Main where

import Analysis.Consolidation
import Analysis.Properties
import Analysis.Types
import Analysis.Util
import Analysis.LockstepAS
import Analysis.SBP
import Analysis.Symmetry
import Data.Maybe
import Language.Java.Parser hiding (opt)
import Language.Java.Pretty hiding (opt)
import Language.Java.Syntax
import System.Console.CmdArgs hiding (opt)
import System.Directory
import System.FilePath.Posix
import Z3.Monad

import qualified Debug.Trace as T

_program, _summary :: String
_summary = unlines ["Synonym","Cartersian Hoare Logic Verifiers","Copyright 2015 @ Marcelo Sousa, 2018 @ Lauren Pick"]
_program = "dss"
_help    = "The input parameter is a Java file."

data Property = P1 | P2 | P3
  deriving (Show, Data, Typeable, Eq)
  
data Option =
  Verify {input :: FilePath
         ,prop :: Int
         ,mode :: String
         ,logLevel :: Int}
  deriving (Show, Data, Typeable, Eq)

verifyMode :: Option
verifyMode =
  Verify {input = def &= args
         ,prop = def
         ,mode = def
         ,logLevel = def} &= help "verify a input Java file"

progModes :: Mode (CmdArgs Option)
progModes = cmdArgsMode $ modes [verifyMode]
         &= help _help
         &= program _program
         &= summary _summary

-- | 'main' function 
main :: IO ()
main = do options <- cmdArgsRun progModes
          runOption options

runOption :: Option -> IO ()
runOption (Verify path p mode logLevel) =
  synonym_main logLevel mode path (arity p) (toProp p) (showProp p)

arity :: Int -> Int
arity 1 = 2 
arity 2 = 3
arity 3 = 3
arity 4 = 3
arity 5 = 2
arity 6 = 2
arity 7 = 2
arity 8 = 2
arity 9 = 4
arity 10 = 2
arity 11 = 4
arity 12 = 2
arity 13 = 2
arity 14 = 4

toProp :: Int -> Prop
toProp 1 = prop1
toProp 2 = prop2
toProp 3 = prop3
toProp 4 = prop4
toProp 5 = prop5
toProp 6 = prop6
toProp 7 = prop7
toProp 8 = prop8
toProp 9 = prop9
toProp 10 = prop10
toProp 11 = prop11
toProp 12 = prop12
toProp 13 = prop13
toProp 14 = prop14

showProp :: Int -> String
showProp 1 = "[Anti-symmetry] (compare): forall x and y, sgn(compare(x,y)) == −sgn(compare(y,x))"
showProp 2 = "[Transitivity] (compare): for all x, y and z,"
           ++ " compare(x, y) > 0 and compare/equals(y, z) > 0 implies compare/equals(x, z) > 0."
showProp 3 = "Property 3: for all x, y and z, compare(x,y) == 0 implies that sgn(compare(x, z)) == sgn(compare(y, z))."
showProp 4 = "[Transitivity] (equals): for all x, y and z,"
           ++ " equals(x, y) and equals(y, z) implies equals(x, z)."
showProp 5 = "[Symmetry] (equals): for any non-null reference values x and y,"
          ++ " x.equals(y) should return true if and only if y.equals(x) returns true."
showProp 6 = "[Consistency] (equals): for any non-null reference values x and y,"
          ++ " multiple invocations of x.equals(y) consistently return true or consistently return false."
showProp 7 = "[False] (misc): for all x and y, if x = y then f(x) < f(y)."
showProp 8 = "[Commutativity] (misc): for all x and y, f(x,y) = f(y,x)."
showProp 9 = "[Symmetry Test] (misc): for all w, x, y, z, if w = x and y = z then f(w) = f(x) and f(y) = f(z)."
showProp 10 = "[Monotonicity] (misc): for all obj1, obj2, obj3, if obj2.in < obj3.in then f(obj1, obj2) < f(obj1, obj3)."
showProp 11 = "[Associativity] (misc): for all x, y, z, f(f(x,y),z) = f(x,f(y,z))"
showProp 12 = "[Cycle] (misc): for all x, y, z, f(x,y,z) = f(z,x,y)"
showProp 13 = "[Swap] (misc): for all x, y, z, f(x,y,z) = f(y,x,z)"
showProp 14 = "[Permutation] (misc): for all x, y, z, f(x,y,z) = f(y,x,z) and for all x, y, z, f(x,y,z) = f(z,y,x)"
  
front_end :: FilePath -> IO ()
front_end file = do
  ast <- parser compilationUnit `fmap` readFile file 
  case ast of 
    Left e -> print $ file ++ ": " ++ show e
    Right cu -> print cu
    
synonym_main :: Int -> String -> FilePath -> Int -> Prop -> String -> IO ()
synonym_main logLevel mode file arity prop propName = do 
  ast <- parser compilationUnit `fmap` readFile file 
  case ast of 
    Left e -> print $ file ++ ": " ++ show e
    Right cu -> do
      let classMap = getInfo cu
          comps = getComps cu
          comparators = map (\c -> map (\idx -> rename idx c) [1..arity]) comps
      if logLevel > 0
      then do
--        putStrLn $ show classMap
--        putStrLn $ show comps
        mapM_ (\cs -> mapM_ (\(Comp _ f) -> putStrLn $ prettyPrint f) cs) comparators
        synonym mode classMap (head comparators) prop propName
      else synonym mode classMap (head comparators) prop propName

synonym mode classMap comparator prop propName = do 
  (vals, models) <- case mode of 
    "descartes" -> evalZ3 $ verify True classMap comparator prop
    "syn" -> evalZ3 $ verifyLsAs True classMap comparator prop
    "synonym" -> evalZ3 $ verifySBP True classMap comparator prop
  case vals of
    Unsat -> putStrLn $ "Unsat: OBEYS " ++ propName
    Sat -> do
      putStrLn $ "Sat: VIOLATES " ++ propName --is buggy! " ++ propName ++ " fails!\nCounter-example:"
  --    putStrLn $ fromJust models
