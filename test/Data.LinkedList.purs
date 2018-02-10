module Test.Main where

import Control.Monad.Eff.Console

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Data.LinkedList (fold, length, make, prepend)
import Prelude (Unit, discard, map, show, ($), (+), (<>))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: ∀ t1. Eff ( console ∷ CONSOLE , testOutput ∷ TESTOUTPUT , avar ∷ AVAR | t1 ) Unit
main = runTest do

  suite "LinkedList" do
  
    test "show" do
      Assert.shouldEqual (show $ prepend 4 $ prepend 3 $ make 1) "[4,3,1]"
      Assert.shouldEqual (show $ make 2) "[2]"
        
    test "make" do
      Assert.shouldEqual (show $ make 1) "[1]"
    
    test "prepend" do
      Assert.shouldEqual (show $ prepend 2 $ make 1) "[2,1]"

    test "length" do
      Assert.shouldEqual (length $ make 1) 1
    
    test "fold" do
      Assert.shouldEqual (fold (<>) "" $ prepend "b" $ make "a") "ba"
    
    test "map instance" do
      Assert.shouldEqual (show $ map ((+) 1) $ prepend 2 $ make 1) "[3,2]"
