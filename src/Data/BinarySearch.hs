{-# LANGUAGE MultiParamTypeClasses #-}
module Data.BinarySearch where

import Data.Monoid (Monoid, Sum(Sum), mappend, mempty)
import Data.MinMax
import Data.Maybe(isJust, fromJust)
import Control.Monad(liftM)


emptyBinary :: MinMaxable n => Binary n
emptyBinary = mempty

-- loosely based on http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WordNumbers3/
-- A search tree, where all real values are at nodes, that can be queried by index or value
data Binary n = Binary {
    binarySize :: Sum Int, -- count of leaf nodes (possible including self)
    binaryRange :: MinMax n, -- range of values in tree (single value only if this is a leaf node)
    indexedTreeNode :: Maybe n, 
    indexTreeSubtree :: (Maybe (Binary n), Maybe (Binary n))
} deriving (Eq, Show)

instance MinMaxable n => Semigroup (Binary n) where
  (<>) b1@(Binary i1 r1 n1 _)
       b2@(Binary i2 r2 m2 _) =
      Binary
        (i1 `mappend` i2)
        (r1 `mappend` r2)
        Nothing
        (Just b1, Just b2)

instance MinMaxable n => Monoid (Binary n) where
    mempty =
      Binary
        mempty
        mempty
        Nothing
        (Nothing, Nothing)
        

singleton :: n -> Binary n 
singleton node =
  Binary 
    (Sum 1)
    (MinMax (node, node))
    (Just node)
    (Nothing, Nothing)

get :: Binary n -> Int ->  Maybe n
get (Binary (Sum 1) _ node _) 0 = node
get (Binary (Sum 1) _ node _) _ = Nothing
get (Binary (Sum size) _ node (lt, gt)) needle =
    let ltSize = case lt of
                   Nothing -> 0
                   Just (Binary (Sum ltSize) _ _ _) -> ltSize
    in
    let rightNeedle = needle - ltSize in
    let comp = compare rightNeedle 0 in
    let (subNeedle, subTree) = case comp of
                              LT -> (needle, lt)
                              GT -> (rightNeedle, gt)
                              EQ -> (rightNeedle, gt)
    in case subTree of
         Nothing -> Nothing
         Just b -> get b subNeedle

-- Description of how to mutate an old tree to create a new one;
-- root is first element of list, but with Left or Right branch replaced
-- by "tree described by tail of list"
-- This is like a zipper
data TreePath n = TreePath {
   treePathOriginal :: Binary n,
   treePathAlteration :: TreePathAlteration n
} deriving Show
   
data TreePathAlteration n = None | LeftBranch (TreePath n) | RightBranch (TreePath n)
  deriving Show

leaf :: (MinMaxable n) => TreePath n -> Maybe n
leaf (TreePath original@(Binary s r mNode st) None) =
    case mNode of
        Nothing ->
            if original == mempty 
            then Nothing
            else error ("Corrupt mempty TreePath: " ++ show original) undefined
        Just node ->
            if original == singleton node
            then Just node
            else error ("Corrupt singleton TreePath: " ++ show original) undefined
leaf tb@(TreePath original@(Binary _ _ _ (lt, rt)) branch) =
    case branch of 
        LeftBranch tb -> leaf tb
        RightBranch tb -> leaf tb
        None -> error ("Corrupt TreeBPath " ++ show tb) undefined

search :: MinMaxable n => n -> Binary n -> Maybe n
search node tree = 
      searchPath Find node tree
      >>= (indexedTreeNode . treePathOriginal)

insert :: MinMaxable n => n -> Binary n -> Binary n
insert = adjust Insert

remove :: MinMaxable n => n -> Binary n -> Binary n
remove = adjust Remove

adjust :: MinMaxable n => Operation -> n -> Binary n -> Binary n
adjust operation node tree =
  let maybePath = searchPath operation node tree in 
  case maybePath of 
      Just path -> build path
      Nothing -> tree
    
build :: MinMaxable n => TreePath n -> Binary n
build (TreePath original None) = original
build (TreePath (original@(Binary s r n (lt, rt))) branch) =
    let (lt, rt) = case branch of
           LeftBranch tb -> (Just (build tb), rt)
           RightBranch tb -> (lt, Just (build tb))
    in mergeTrees n lt rt

mergeTrees :: MinMaxable n => Maybe n -> Maybe (Binary n) -> Maybe (Binary n) -> Binary n
mergeTrees Nothing Nothing Nothing = mempty
mergeTrees (Just node) Nothing Nothing = singleton node
mergeTrees _ lt rt = 
    Binary (mappend (maybe mempty binarySize lt)
                    (maybe mempty binarySize rt))
           (mappend (maybe mempty binaryRange lt)
                    (maybe mempty binaryRange rt))
           Nothing
           (lt, rt)

data Operation = Find | Remove | Insert

-- find path to an element of tree, and do something with path
searchPath :: MinMaxable n => Operation -> n -> Binary n -> Maybe (TreePath n)
searchPath operation needle t@(Binary _ _ maybeNode (Nothing, Nothing)) =
        case maybeNode of
            Nothing -> -- tree was empty!
                case operation of
                    Find -> Nothing
                    Remove -> Just (TreePath mempty None)
                    Insert -> Just (TreePath (singleton needle) None)
            Just node ->
                let comp = compare needle node in
                case comp of
                  EQ ->
                    case operation of
                      Find -> Just (TreePath t None)
                      Remove -> Just (TreePath mempty None)
                      Insert -> Nothing
                  _ -> 
                    let branch = case comp of
                                   LT -> LeftBranch 
                                   GT -> RightBranch 
                    in case operation of
                      Find -> Nothing
                      Remove -> Nothing
                      Insert -> Just (TreePath t (branch (TreePath (singleton needle) None)))
searchPath operation needle t@(Binary _ _ node (lt, gt)) =
    let ltRange = case lt of
                   Nothing -> mempty
                   Just (Binary _ ltRange _ _) -> ltRange
    in
    let (MinMax (_, ltMax)) = ltRange in
    let comp = compare needle ltMax in
    let (branch, subTree) = case comp of
                              LT -> (LeftBranch, lt)
                              EQ -> (LeftBranch, lt) -- since a range *includes* its maximum
                              GT -> (RightBranch, gt)
    in case subTree of
         Nothing -> 
             case operation of
               Find -> Nothing
               Remove -> Nothing
               Insert -> Just (TreePath t (branch (TreePath (singleton needle) None)))
         Just b -> liftM (TreePath t . branch) (searchPath operation needle b)

   
{--
insert :: (Measurable a b) => Binary a -> a ->  Maybe a

--}
