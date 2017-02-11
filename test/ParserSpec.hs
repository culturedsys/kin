module ParserSpec (
    spec
    ) where

import Data.Either
import Expression
import Parser
import Test.Hspec
import Text.Parsec

spec = do
    it "gives a parse error for an empty string" $ do
        parse expression "()" "" `shouldSatisfy` isLeft

    it "can parse a relation name" $ do
        parse expression "()" "relation1" 
            `shouldBe` (Right (RelationVariable "relation1"))

    it "can parse a quoted relation name" $ do
        parse expression "()" "{a && confusing -> ! = name}"
            `shouldBe` (Right (RelationVariable "a && confusing -> ! = name"))

    it "can parse a simple projection" $ do
        parse expression "()" "P[attribute1](relation1)"
            `shouldBe` Right (Projection ["attribute1"] (RelationVariable "relation1"))

    it "can parse a projection with multiple attributes" $ do
        parse expression "()" "P[attribute1, attribute2](relation1)"
            `shouldBe` Right (Projection ["attribute1", "attribute2"] (RelationVariable "relation1"))


    it "can parse a rename" $ do
        parse expression "()" "R[old->new](relation1)"
            `shouldBe` Right (Rename [("old", "new")] (RelationVariable "relation1"))

    it "can parse a rename with multiple attributes" $ do
        parse expression "()" "R[old->new , name -> another](relation1)"
            `shouldBe` Right (Rename [("old", "new"), ("name", "another")] (RelationVariable "relation1"))

    it "can parse a selection" $ do
        parse expression "()" "S[attribute='value'](relation1)"
            `shouldBe` Right (Selection (Equals (NameTerm "attribute")
                                                (LiteralTerm "value"))
                                        (RelationVariable "relation1"))
    
    it "can parse an equals condition" $ do
        parse condition "()" "attribute='value'"
            `shouldBe` Right (Equals (NameTerm "attribute") (LiteralTerm "value"))

    it "can parse an equals condition with literal first" $ do
        parse condition "()" "'value'=attribute"
            `shouldBe` Right (Equals (LiteralTerm "value") (NameTerm "attribute"))

    it "can parse an equals term with two attributes" $ do
        parse condition "()" "attribute1=attribute2" 
            `shouldBe` Right (Equals (NameTerm "attribute1") (NameTerm "attribute2"))            

    it "can parse an equals term with two values" $ do
        parse condition "()" "'value1'='value2'" 
            `shouldBe` Right (Equals (LiteralTerm "value1") (LiteralTerm "value2"))

    it "can parse a not equals term" $ do 
         parse condition "()" "attribute!='value'"
            `shouldBe` Right (NotEquals (NameTerm "attribute") (LiteralTerm "value"))

    it "can parse a not equals condition with literal first" $ do
        parse condition "()" "'value'!=attribute"
            `shouldBe` Right (NotEquals (LiteralTerm "value") (NameTerm "attribute"))

    it "can parse a not equals term with two attributes" $ do
        parse condition "()" "attribute1!=attribute2" 
            `shouldBe` Right (NotEquals (NameTerm "attribute1") (NameTerm "attribute2"))            

    it "can parse a not equals term with two values" $ do
        parse condition "()" "'value1'!='value2'" 
            `shouldBe` Right (NotEquals (LiteralTerm "value1") (LiteralTerm "value2"))

    it "can parse a not clause" $ do
        parse condition "()" "NOT(attribute='value')"
            `shouldBe` Right (NotCondition (Equals (NameTerm "attribute") (LiteralTerm "value")))

    it "can parse an and clause" $ do
        parse condition "()" "(attribute1='value1') AND (attribute2='value2')"
            `shouldBe` Right (AndCondition (Equals (NameTerm "attribute1") (LiteralTerm "value1"))
                                            (Equals (NameTerm "attribute2") (LiteralTerm "value2")))

    it "can parse an or clause" $ do                                                                    
        parse condition "()" "(attribute1='value1') OR (attribute2='value2')"
            `shouldBe` Right (OrCondition (Equals (NameTerm "attribute1") (LiteralTerm "value1"))
                                            (Equals (NameTerm "attribute2") (LiteralTerm "value2")))

    it "can parse a nested and/or clause" $ do                                                                    
        parse condition "()" "((attribute1='value1') OR (attribute2='value2')) AND ((attribute3='value3') OR (attribute4='value4'))"
            `shouldBe` Right (AndCondition (OrCondition (Equals (NameTerm "attribute1") (LiteralTerm "value1"))
                                                        (Equals (NameTerm "attribute2") (LiteralTerm "value2")))
                                           (OrCondition (Equals (NameTerm "attribute3") (LiteralTerm "value3"))
                                                        (Equals (NameTerm "attribute4") (LiteralTerm "value4"))))

    it "can parse a simple union" $ do
        parse expression "()" "left U right" 
            `shouldBe` Right (Union (RelationVariable "left") (RelationVariable "right"))

    it "can parse a simple intersection" $ do
        parse expression "()" "left N right"
            `shouldBe` Right (Intersection (RelationVariable "left") (RelationVariable "right"))

    it "can parse a simple difference" $ do
        parse expression "()" "left - right"
            `shouldBe` Right (Difference (RelationVariable "left") (RelationVariable "right"))
    
    it "can parse a simple product" $ do
        parse expression "()" "left * right" 
            `shouldBe` Right (Product (RelationVariable "left") (RelationVariable "right"))

    it "can parse a simple join" $ do
        parse expression "()" "left >< right" 
            `shouldBe` Right (Join (RelationVariable "left") (RelationVariable "right"))

    it "can parse a simple division" $ do
        parse expression "()" "left / right" 
            `shouldBe` Right (Division (RelationVariable "left") (RelationVariable "right"))

    it "can parse a union of projections" $ do
        parse expression "()" "P[attribute1](relation1) U P[attribute2](relation2)"
            `shouldBe` Right (Union (Projection ["attribute1"] (RelationVariable "relation1"))
                                    (Projection ["attribute2"] (RelationVariable "relation2")))

    it "can parse a nested union/intersection" $ do
        parse expression "()" "(a N b) U (c N d)"
            `shouldBe` Right (Union (Intersection (RelationVariable "a") 
                                                  (RelationVariable "b"))
                                    (Intersection (RelationVariable "c") 
                                                  (RelationVariable "d")))                                    