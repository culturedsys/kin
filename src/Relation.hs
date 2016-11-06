module Relation where 

import Control.Monad

import Data.Maybe (catMaybes)
import Data.List(intercalate)
import qualified Data.Map as M
import Data.Set((\\))
import qualified Data.Set as S

type RelationName = String
type AttributeName = String
type Value = String
-- Note that what relational algebra calls "tuples" are not the same as what
-- are usually called tuples, in that they are not ordered, but are labelled.
-- i.e., they are maps.
type Tuple = M.Map AttributeName Value

data Relation = Relation {
    relHeading :: S.Set AttributeName,
    relBody :: S.Set Tuple
} deriving(Show,Eq)

type Predicate = Tuple -> Maybe Bool


-- Convert a list of AttributeNames, and a list of tuples (represented as a 
-- list with values ordered the same as the AttributeName list) to a Relation.
fromList :: [AttributeName] -> [[Value]] -> Relation 
fromList attributeNames tuples = 
    Relation heading body where
        heading = S.fromList attributeNames
        body = S.fromList $ map toTuple tuples
        toTuple t = M.fromList $ zip attributeNames t


prettyPrint :: Relation -> String
prettyPrint (Relation attributeNames tuples) = unlines $ (header : separator : body) where
    header = intercalate " | " $ S.toList attributeNames
    separator = replicate (length header + 1) '-'
    body = map printTuple $ S.toList tuples
    printTuple tuple = intercalate "| " . map printCell $ M.toList tuple
    printCell (name, value) = pad (length name + 1) value
    pad l v = take l (v ++ repeat ' ')


-- Produces a Map that contains only keys in a given set (an equivalent 
-- function is in newer version of the standard Data.Map package)
restrictKeys :: Ord k => S.Set k -> M.Map k a -> M.Map k a
restrictKeys ks = M.filterWithKey (\k _ -> k `S.member` ks)

project :: S.Set AttributeName -> Relation -> Maybe Relation
project projectedNames (Relation originalNames body) = 
    if not (projectedNames `S.isSubsetOf` originalNames) then
        Nothing
    else 
        Just . Relation projectedNames $ S.map (restrictKeys projectedNames) body

predicate :: AttributeName -> (Value -> Bool) -> Predicate
predicate name p tuple = p <$> M.lookup name tuple

select :: Predicate -> Relation -> Maybe Relation
select p (Relation names body) = Relation names <$> S.fromList <$> filterM p (S.toList body) 

rename :: (M.Map AttributeName AttributeName) -> Relation -> Maybe Relation
rename nameChanges (Relation originalNames body) =
    let changedNames = M.keysSet nameChanges
        resultNames = (originalNames \\ changedNames) `S.union` (S.fromList $ M.elems nameChanges)
        resultBody = S.map (M.mapKeys changeName) body
        changeName name = case M.lookup name nameChanges of
                            Nothing -> name
                            Just newName -> newName
    in
        if not (changedNames `S.isSubsetOf` originalNames) 
            || (S.size resultNames /= S.size originalNames) then
            Nothing
        else
            Just $ Relation resultNames resultBody

-- This uses a restrictive definition of "union-compatible" where the
-- two relations need to have the same attribute names.
union :: Relation -> Relation -> Maybe Relation
union (Relation rHead rBody) (Relation sHead sBody) =
    if rHead /= sHead then
        Nothing
    else
        Just $ Relation rHead (rBody `S.union` sBody)

intersection :: Relation -> Relation -> Maybe Relation
intersection (Relation rHead rBody) (Relation sHead sBody) =
    if rHead /= sHead then
        Nothing
    else
        Just $ Relation rHead (rBody `S.intersection` sBody)

difference :: Relation -> Relation -> Maybe Relation
difference (Relation rHead rBody) (Relation sHead sBody) =
    if rHead /= sHead then
        Nothing
    else
        Just $ Relation rHead (rBody `S.difference` sBody)

-- This requires that the two relations have no attribute names in common.
-- This is specified in some formalisations of relational algebra, but others
-- specify that the Cartesian product renames relations as necessary.
product :: Relation -> Relation -> Maybe Relation
product (Relation rHead rBody) (Relation sHead sBody) =
    if rHead `S.isSubsetOf` sHead || sHead `S.isSubsetOf` rHead then
        Nothing
    else
        Just $ Relation (rHead `S.union` sHead) (S.foldr S.union S.empty $ S.map (\r -> S.map (M.union r) sBody) rBody)

-- This allows joins between tables with no common attributes (in which case
-- join is equivalent to product).
join :: Relation -> Relation -> Maybe Relation
join (Relation rHead rBody) (Relation sHead sBody) =
    let sharedAttributes = rHead `S.intersection` sHead
        sUniques = sHead `S.difference` rHead
        combineTuples :: Tuple -> Tuple -> Maybe Tuple
        combineTuples r s = 
            if (restrictKeys sharedAttributes r) /= (restrictKeys sharedAttributes s) then
                Nothing
            else
                Just $ r `M.union` (restrictKeys sUniques s)
        combinedTuples = S.fromList $ catMaybes [combineTuples r s | r <- S.toList rBody, s <- S.toList sBody]
    in
        Just $ Relation (rHead `S.union` sHead) combinedTuples

-- An alternative implementation of join in terms of the other relational operators
join' :: Relation -> Relation -> Maybe Relation
join' r@(Relation rHead _) s@(Relation sHead _) = do 
    let sharedAttributes = rHead `S.intersection` sHead
        -- The concatenation of all attribute names in both tables cannot
        -- be a prefix of any attribute name, so I can use it as a prefix
        -- to create unique names for the shared attributes.
        prefix = concat $ S.toList (rHead `S.union` sHead)
        renameList = M.fromList . zip (S.toList sharedAttributes) $ map (prefix ++) $ S.toList sharedAttributes 
        matchAll :: (M.Map AttributeName AttributeName) -> Predicate
        matchAll am t = Just $ all (\(orig, renamed) -> (M.lookup orig t) == (M.lookup renamed t)) (M.toList am)
    t <- rename renameList s
    u <- (r `Relation.product` t)
    p <- select (matchAll renameList) u  
    project (rHead `S.union` sHead) p

-- This allows division where there are no common attributes; such divisions
-- are always empty.
division :: Relation -> Relation -> Maybe Relation
division r@(Relation rHead _) s@(Relation sHead _) = do
    let rUniques = (rHead `S.difference` sHead)
    ru <- (project rUniques r)
    t <-  ru  `Relation.product` s
    u <- t `difference` r
    v <- project rUniques u
    ru `difference` v 
