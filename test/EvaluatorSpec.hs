module EvaluatorSpec (
    spec
    ) where


import qualified Data.Map as M
import qualified Data.Set as S
import Evaluator
import Expression
import Relation
import Test.Hspec

relation1 = fromList ["attribute1", "attribute2"] [["a", "b"], ["c", "d"]]
relation2 = fromList ["attribute1", "attribute2"] [["c", "d"], ["e", "f"]]
relation3 = fromList ["attribute1", "attribute3"] [["e", "f"], ["g", "h"]]
relation4 = fromList ["attribute3", "attribute4"] [["e", "f"], ["g", "h"]]

environment = M.fromList [
        ("relation1", relation1),
        ("relation2", relation2),
        ("relation3", relation3),
        ("relation4", relation4)
    ]

spec = do
    it "can evaluate a RelationVariable" $ do
        evaluate (RelationVariable "relation1") environment
            `shouldBe` Just relation1 

    it "can evaluate a Projection" $ do
        evaluate (Projection ["attribute1"] (RelationVariable "relation1")) environment
            `shouldBe` (project (S.singleton "attribute1") relation1)

    it "can evaluate a Selection" $ do
        evaluate (Selection 
                    (Equals (NameTerm "attribute1") (LiteralTerm "a")) 
                    (RelationVariable "relation1")) environment
            `shouldBe` (select (predicate "attribute1" (== "a")) relation1 )
        evaluate (Selection 
                    (NotEquals (NameTerm "attribute1") (LiteralTerm "a")) 
                    (RelationVariable "relation1")) environment
            `shouldBe` (select (predicate "attribute1" (/= "a")) relation1)

    it "can evaluate a Rename" $ do
        evaluate (Rename [("attribute1", "attribute4")] (RelationVariable "relation1")) environment
            `shouldBe` (rename (M.fromList [("attribute1", "attribute4")]) relation1)

    it "can evaluate a Union" $ do
        evaluate (Union (RelationVariable "relation1") (RelationVariable "relation2")) environment
            `shouldBe` (union relation1 relation2)

    it "can evaluate an Intersection" $ do
        evaluate (Intersection (RelationVariable "relation1") (RelationVariable "relation2")) environment
            `shouldBe` (intersection relation1 relation2)

    it "can evaluate a difference" $ do
        evaluate (Difference (RelationVariable "relation1") (RelationVariable "relation2")) environment
            `shouldBe` (difference relation1 relation2)

    it "can evaluate a product" $ do
        evaluate (Product (RelationVariable "relation1") (RelationVariable "relation4")) environment
            `shouldBe` (Relation.product relation1 relation4)

    it "can evaluate a join" $ do
        evaluate (Join (RelationVariable "relation1") (RelationVariable "relation3")) environment
            `shouldBe` (join relation1 relation3)

    it "can evaluate a division" $ do
        evaluate (Division (RelationVariable "relation1") (RelationVariable "relation3")) environment
            `shouldBe` (division relation1 relation3)

    