-------------------------------------------------------------------------------
-- Module    :  Analysis.Properties
-- Copyright :  (c) 2015 Marcelo Sousa
--                  2018 Lauren Pick
-- Defines the properties of comparators:
--  https://docs.oracle.com/javase/8/docs/api/java/util/Comparator.html
-- Also contains other properties (prop7 - prop14)
-------------------------------------------------------------------------------

module Analysis.Properties where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Java.Syntax

import Z3.Monad

import Analysis.Types
import Analysis.Util

-- new props
prop7 :: Prop
prop7 (args, [res1, res2], fields) = do
  let o1 = safeLookup "false" (Ident "o1") args
      o2 = safeLookup "false" (Ident "o2") args
  -- o1 = o2
  pre <- mkEq o1 o2
  -- fn(o1) <= fn(o2)
  pos <- mkLt res1 res2
  return (pre, pos)

-- [Misc] Commutativity
prop8 :: Prop 
prop8 (args, [res1,res2], fields) = do
    let o11 = safeLookup "symm" (Ident "o11") args
        o12 = safeLookup "symm" (Ident "o12") args
        o21 = safeLookup "symm" (Ident "o21") args
        o22 = safeLookup "symm" (Ident "o22") args
    eq1 <- mkEq o11 o22
    eq2 <- mkEq o21 o12
    pre <- mkAnd [eq1,eq2]
    pos <- mkEq res1 res2
    return (pre, pos)

prop9 :: Prop
prop9 (args, [res1, res2, res3, res4], fields) = do
  let o1 = safeLookup "symm_toy" (Ident "o1") args
      o2 = safeLookup "symm_toy" (Ident "o2") args
      o3 = safeLookup "symm_toy" (Ident "o3") args
      o4 = safeLookup "symm_toy" (Ident "o4") args
  -- o1 = o2
  pre12 <- mkEq o1 o2
  -- o3 = o4
  pre34 <- mkEq o3 o4
  -- precondition
  pre <- mkAnd [pre12, pre34]
  -- fn(o1) = fn(o2) and fn(o3) = fn(o4)
  pos12 <- mkEq res1 res2 
  pos34 <- mkEq res3 res4
  pos <- mkAnd [pos12, pos34]
  return (pre, pos)

prop10 :: Prop
prop10 (args, [res1, res2, res3, res4], fields) = do
  let o11 = safeLookup "monotone" (Ident "o11") args
      o12 = safeLookup "monotone" (Ident "o12") args
      o21 = safeLookup "monotone" (Ident "o21") args
      o22 = safeLookup "monotone" (Ident "o22") args
      fn = safeLookup "processName: Field"  (Ident "in") fields
  o11app <- mkApp fn [o11]
  o12app <- mkApp fn [o12]
  o21app <- mkApp fn [o21]
  o22app <- mkApp fn [o22]
  -- o11.in = o12.in
  eq <- mkEq o11app o12app
  lt <- mkLt o21app o22app
  pre <- mkAnd [eq, lt]
  -- fn(o1) < fn(o2)
  pos <- mkLt res1 res2
  return (pre, pos)

prop11 :: Prop
prop11 (args, [res1, res2, res3, res4], fields) = do
  let o11 = safeLookup "assoc" (Ident "o11") args
      o12 = safeLookup "assoc" (Ident "o12") args
      o13 = safeLookup "assoc" (Ident "o13") args
      o14 = safeLookup "assoc" (Ident "o14") args
      o21 = safeLookup "assoc" (Ident "o21") args
      o22 = safeLookup "assoc" (Ident "o22") args
      o23 = safeLookup "assoc" (Ident "o23") args
      o24 = safeLookup "assoc" (Ident "o24") args
  -- res1 = f(x1, x2) and res2 = f(x2, x3) and
  -- res3 = f(res1, x3) and res4 = f(x1, res2) and
  -- res3 = res4
  eqx1 <- mkEq o11 o14
  eqx2 <- mkEq o21 o12
  eqx3 <- mkEq o23 o22
  eqres1 <- mkEq res1 o13
  eqres2 <- mkEq res2 o24
  pre <- mkAnd [eqx1, eqx2, eqx3, eqres1, eqres2]
  pos <- mkEq res3 res4
  return (pre, pos)

prop12 :: Prop
prop12 (args, [res1, res2], fields) = do
    let o11 = safeLookup "perm" (Ident "o11") args
        o12 = safeLookup "perm" (Ident "o12") args
        o21 = safeLookup "perm" (Ident "o21") args
        o22 = safeLookup "perm" (Ident "o22") args
        o31 = safeLookup "perm" (Ident "o31") args
        o32 = safeLookup "perm" (Ident "o32") args
    eq1 <- mkEq o11 o22
    eq2 <- mkEq o21 o32
    eq3 <- mkEq o31 o12
    pre <- mkAnd [eq1, eq2, eq3]
    pos <- mkEq res1 res2
    return (pre, pos)
    
prop13 :: Prop
prop13 (args, [res1, res2], fields) = do
    let o11 = safeLookup "swap" (Ident "o11") args
        o12 = safeLookup "swap" (Ident "o12") args
        o21 = safeLookup "swap" (Ident "o21") args
        o22 = safeLookup "swap" (Ident "o22") args
        o31 = safeLookup "swap" (Ident "o31") args
        o32 = safeLookup "swap" (Ident "o32") args
    eq1 <- mkEq o11 o22
    eq2 <- mkEq o21 o12
    eq3 <- mkEq o31 o32
    pre <- mkAnd [eq1, eq2, eq3]
    pos <- mkEq res1 res2
    return (pre, pos)

prop14 :: Prop
prop14 (args, [res1, res2, res3, res4], fields) = do
    let o11 = safeLookup "swap" (Ident "o11") args
        o12 = safeLookup "swap" (Ident "o12") args
        o13 = safeLookup "swap" (Ident "o13") args
        o14 = safeLookup "swap" (Ident "o14") args
        o21 = safeLookup "swap" (Ident "o21") args
        o22 = safeLookup "swap" (Ident "o22") args
        o23 = safeLookup "swap" (Ident "o23") args
        o24 = safeLookup "swap" (Ident "o24") args
        o31 = safeLookup "swap" (Ident "o31") args
        o32 = safeLookup "swap" (Ident "o32") args
        o33 = safeLookup "swap" (Ident "o33") args
        o34 = safeLookup "swap" (Ident "o34") args
    eq1 <- mkEq o11 o22
    eq2 <- mkEq o21 o12
    eq3 <- mkEq o31 o32
    eq4 <- mkEq o13 o34
    eq5 <- mkEq o23 o24
    eq6 <- mkEq o33 o14
    pre <- mkAnd [eq1, eq2, eq3, eq4, eq5, eq6]
    pos1 <- mkEq res1 res2
    pos2 <- mkEq res3 res4
    pos <- mkAnd [pos1, pos2]
    return (pre, pos)
--

prop1 :: Prop
prop1 (args, [res1,res2], fields) = do
    let o11 = safeLookup "trans" (Ident "o11") args
        o12 = safeLookup "trans" (Ident "o12") args
        o21 = safeLookup "trans" (Ident "o21") args
        o22 = safeLookup "trans" (Ident "o22") args
    eq1 <- mkEq o11 o22
    eq2 <- mkEq o21 o12
    pre <- mkAnd [eq1,eq2]
    i0 <- mkIntNum (0 :: Integer)
    -- compare(x,y) = 0 iff compare (y,x) = 0 
    r1eq <- mkEq res1 i0
    r2eq <- mkEq res2 i0
    poseq <- mkIff r1eq r2eq
    -- compare(x,y) > 0 iff compare(y,x) < 0
    r1gt <- mkGt res1 i0
    r2lt <- mkLt res2 i0
    posas <- mkIff r1gt r2lt
{-
    r1lt <- mkLt res1 i0
    r2gt <- mkGt res2 i0
    posas' <- mkIff r2gt r1lt
-}
    pos <- mkAnd [poseq,posas]
    return (pre, poseq)

-- transitivity
prop2 :: Prop
prop2 (args, [res1,res2,res3], fields) = do
    let o11 = safeLookup "trans" (Ident "o11") args
        o12 = safeLookup "trans" (Ident "o12") args
        o13 = safeLookup "trans" (Ident "o13") args
        o21 = safeLookup "trans" (Ident "o21") args
        o22 = safeLookup "trans" (Ident "o22") args
        o23 = safeLookup "trans" (Ident "o23") args    
    eq1 <- mkEq o11 o13
    eq2 <- mkEq o21 o12
    eq3 <- mkEq o22 o23
    pre <- mkAnd [eq1,eq2,eq3]
    i0 <- mkIntNum (0 :: Integer)
    -- compare(x,y) > 0 and compare(y,z) > 0 => compare(x,z) > 0
    r1gt <- mkGt res1 i0
    r2gt <- mkGt res2 i0
    r3gt <- mkGt res3 i0
    r12gt <- mkAnd [r1gt,r2gt]
    pos <- mkImplies r12gt r3gt
    return (pre, pos)
        
prop3 :: Prop 
prop3 (args, [res1,res2,res3], fields) = do
    let o11 = safeLookup "trans" (Ident "o11") args
        o12 = safeLookup "trans" (Ident "o12") args
        o13 = safeLookup "trans" (Ident "o13") args
        o21 = safeLookup "trans" (Ident "o21") args
        o22 = safeLookup "trans" (Ident "o22") args
        o23 = safeLookup "trans" (Ident "o23") args    
    eq1 <- mkEq o11 o12
    eq2 <- mkEq o21 o13
    eq3 <- mkEq o22 o23
    pre <- mkAnd [eq1,eq2,eq3]
    i0 <- mkIntNum (0 :: Integer)
    -- compare(x,y) = 0
    r1 <- mkEq res1 i0
    -- compare(x,z) > 0 iff compare (y,z) > 0
    r2gt <- mkGt res2 i0
    r3gt <- mkGt res3 i0
    posgt <- mkIff r2gt r3gt
    -- compare(x,z) = 0 iff compare (y,z) = 0
    r2eq <- mkEq res2 i0
    r3eq <- mkEq res3 i0
    poseq <- mkIff r2eq r3eq    
    pos' <- mkAnd [posgt,poseq]
    pos <- mkImplies r1 pos'
    return (pre, pos)

-- [Equals] Transitivity
prop4 :: Prop
prop4 (args, [res1,res2,res3], fields) = do
    let o11 = safeLookup "trans" (Ident "o11") args
        o12 = safeLookup "trans" (Ident "o12") args
        o13 = safeLookup "trans" (Ident "o13") args
        o21 = safeLookup "trans" (Ident "o21") args
        o22 = safeLookup "trans" (Ident "o22") args
        o23 = safeLookup "trans" (Ident "o23") args    
    eq1 <- mkEq o11 o13
    eq2 <- mkEq o21 o12
    eq3 <- mkEq o22 o23
    pre <- mkAnd [eq1,eq2,eq3]
    i1 <- mkIntNum (1 :: Integer)
    -- equals(x,y) == 1 and equals(y,z) == 1 => equals(x,z) == 1
    r1gt <- mkEq res1 i1
    r2gt <- mkEq res2 i1
    r3gt <- mkEq res3 i1
    r12gt <- mkAnd [r1gt,r2gt]
    pos <- mkImplies r12gt r3gt
    return (pre, pos)

-- [Equals] Symmetry
prop5 :: Prop 
prop5 (args, [res1,res2], fields) = do
    let o11 = safeLookup "symm" (Ident "o11") args
        o12 = safeLookup "symm" (Ident "o12") args
        o21 = safeLookup "symm" (Ident "o21") args
        o22 = safeLookup "symm" (Ident "o22") args
    eq1 <- mkEq o11 o22
    eq2 <- mkEq o21 o12
    pre <- mkAnd [eq1,eq2]
    i1 <- mkIntNum (1 :: Integer)
    -- equals(x,y) = 1 iff equals(y,x) = 1
    r1eq <- mkEq res1 i1
    r2eq <- mkEq res2 i1
    pos <- mkIff r1eq r2eq
    return (pre, pos)

-- [Equals] Consistency
prop6 :: Prop 
prop6 (args, [res1,res2], fields) = do
    let o11 = safeLookup "symm" (Ident "o11") args
        o12 = safeLookup "symm" (Ident "o12") args
        o21 = safeLookup "symm" (Ident "o21") args
        o22 = safeLookup "symm" (Ident "o22") args
    eq1 <- mkEq o11 o12
    eq2 <- mkEq o21 o22
    pre <- mkAnd [eq1,eq2]
    -- equals(x,y) = equals(x,y)
    pos <- mkEq res1 res2
    return (pre, pos)

-- transitivity
-- forall (o11,o21,o12,o22,o13,o23). 
--      o11 = o13 and o12 = o21 and o22 = o23
--  =>  ((res1 > 0 and res2 > 0) => res3 > 0)
transitivity :: Prop
transitivity (args, [res1,res2,res3], fields) = do
    let o11 = safeLookup "trans" (Ident "o11") args
        o12 = safeLookup "trans" (Ident "o12") args
        o13 = safeLookup "trans" (Ident "o13") args
        o21 = safeLookup "trans" (Ident "o21") args
        o22 = safeLookup "trans" (Ident "o22") args
        o23 = safeLookup "trans" (Ident "o23") args    
    eq1 <- mkEq o11 o13
    eq2 <- mkEq o21 o12
    eq3 <- mkEq o22 o23
    pre <- mkAnd [eq1,eq2,eq3]
    i0 <- mkIntNum (0 :: Integer)
    -- compare(x,y) = 0 and compare (y,z) = 0 => compare(x,z) = 0
    r1eq <- mkEq res1 i0
    r2eq <- mkEq res2 i0
    r3eq <- mkEq res3 i0
    r12eq <- mkAnd [r1eq,r2eq]
    poseq <- mkImplies r12eq r3eq
    -- compare(x,y) < 0 and compare(y,z) < 0 => compare(x,z) < 0
    r1lt <- mkLt res1 i0
    r2lt <- mkLt res2 i0
    r3lt <- mkLt res3 i0
    r12lt <- mkAnd [r1lt,r2lt]
    poslt <- mkImplies r12lt r3lt
    -- compare(x,y) > 0 and compare(y,z) > 0 => compare(x,z) > 0
    r1gt <- mkGt res1 i0
    r2gt <- mkGt res2 i0
    r3gt <- mkGt res3 i0
    r12gt <- mkAnd [r1gt,r2gt]
    posgt <- mkImplies r12gt r3gt
    pos <- mkAnd [poseq,poslt,posgt]
    return (pre, pos)

antisymmetry :: Prop
antisymmetry (args, [res1, res2], fields) = do
    let o11 = safeLookup "antisymmetry" (Ident "o11") args
        o12 = safeLookup "antisymmetry" (Ident "o12") args
        o21 = safeLookup "antisymmetry" (Ident "o21") args
        o22 = safeLookup "antisymmetry" (Ident "o22") args
    eq1 <- mkEq o11 o22
    eq2 <- mkEq o21 o12
   -- neq1 <- mkNot =<< mkEq o11 o21
   -- lneqs <- mapM (\field -> mkFieldApp o11 o21 field fields) $ M.keys fields
    pre <- mkAnd $ [eq1,eq2] --,neq1] ++ lneqs
    i0 <- mkIntNum (0 :: Integer)
    r1 <- mkLt res1 i0
    r2 <- mkLt res2 i0
    r12 <- mkOr [r1,r2]
    r3 <- mkGt res1 i0
    r4 <- mkGt res2 i0
    r34 <- mkOr [r3,r4]
--    pos <- mkAnd [r12, r34]
    pos <- mkIff r3 r2
    return (pre,pos)

equality :: Prop
equality (args, [res1], fields) = do
    let o11 = safeLookup "eq" (Ident "o11") args
        o21 = safeLookup "eq" (Ident "o21") args
    eq1 <- mkEq o11 o21
    lneqs <- mapM (\field -> mkFieldApp o11 o21 field fields) $ M.keys fields
    pre <- mkAnd $ eq1:lneqs
    i0 <- mkIntNum (0 :: Integer)
    pos <- mkEq res1 i0
    return (pre,pos)

mkFieldApp o1 o2 field fields = do
    let fn = safeLookup "processName: Field"  field fields
    o1app <- mkApp fn [o1]
    o2app <- mkApp fn [o2]
    mkNot =<< mkEq o1app o2app

testProp :: Prop
testProp (args, [res], fields) = do
    i0 <- mkIntNum (0 :: Integer)
    pre <- mkTrue
    pos <- mkEq res i0
    return (pre,pos)
