{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module DiagramLib where

import Diagrams.Prelude
import Data.List(foldl')

type BR2 b = Backend b R2
type QD b = QDiagram b R2 Any 
type QDTrans b = (BR2 b) => QD b -> QD b
type QDCat b = (BR2 b) => [QD b] -> QD b

-- Layout operations

myframe :: QDTrans b
myframe = padX 1.1 . padY 1.1 . centerXY

--pack horizontally, with no gaps. Assumes adjacent components have parallel edges
hcatSnug :: QDCat b
hcatSnug = catSnug snugR snugL strutX
               
--pack vertically, with no gaps. Assumes adjacent components have parallel edges
vcatSnug :: QDCat b
vcatSnug = catSnug snugB snugT strutY

--pack in given direction, with no gaps. Assumes adjacent components have parallel edges
catSnug :: (BR2 b) => QDTrans b -> QDTrans b -> (Double -> QD b) -> QDCat b
catSnug snugFirst snugNext strut =
    foldl'
      (\acc new -> ((acc # centerXY # snugFirst) `atop` (new # centerXY # snugNext)))
      (strut 0)


type ModifyFn = forall b v m . (HasLinearMap v, InnerSpace v, Floating (Scalar v), Ord (Scalar v), Semigroup m) => 
      (Subdiagram b v m -> QDiagram b v m -> QDiagram b v m) 

modifyByName :: (BR2 b, IsName n) => (n -> ModifyFn) -> [n] -> QDTrans b 
modifyByName modifyFn names d = foldl' (\d name -> withName name (modifyFn name) d) d names
