module PrinterSpec (
    spec
    ) where

import Test.Hspec

import Printer
import Expression

relationVariable = RelationVariable "relation"
projection = Projection ["attribute", "otherattribute"] relationVariable

spec = do
    it "can print a RelationVariable" $ do
        asciiPrinter relationVariable `shouldBe` "relation"

    it "can print a Projection" $ do
        asciiPrinter projection `shouldBe` "P[attribute,otherattribute](relation)"

    it "can print an Equals Selection" $ do
        asciiPrinter (Selection (Equals (NameTerm "attribute") (LiteralTerm "value")) relationVariable) 
            `shouldBe` "S[attribute='value'](relation)"

    it "can print a NotEquals Selection" $ do
        asciiPrinter (Selection (NotEquals (NameTerm "attribute") (LiteralTerm "value")) relationVariable) 
            `shouldBe` "S[attribute!='value'](relation)"

    it "can print a Not Selection" $ do
        asciiPrinter (Selection (NotCondition (Equals (NameTerm "attribute") (LiteralTerm "value"))) relationVariable) 
            `shouldBe` "S[NOT (attribute='value')](relation)"

    it "can print an And Selection" $ do
        asciiPrinter (Selection (AndCondition (Equals (NameTerm "attribute") (LiteralTerm "value")) (Equals (NameTerm "attribute") (LiteralTerm "value"))) relationVariable) 
            `shouldBe` "S[(attribute='value') AND (attribute='value')](relation)"

    it "can print an Or Selection" $ do
        asciiPrinter (Selection (OrCondition (Equals (NameTerm "attribute") (LiteralTerm "value")) (Equals (NameTerm "attribute") (LiteralTerm "value"))) relationVariable) 
            `shouldBe` "S[(attribute='value') OR (attribute='value')](relation)"

    it "can print a Rename" $ do
        asciiPrinter (Rename [("old", "new")] relationVariable) 
            `shouldBe` "R[old->new](relation)"

    it "can print a Union" $ do
        asciiPrinter (Union (RelationVariable "left") (RelationVariable "right"))
            `shouldBe` "left U right"
    
    it "can print an Intersection" $ do
        asciiPrinter (Intersection (RelationVariable "left") (RelationVariable "right"))
            `shouldBe` "left N right"

    it "can print a Difference" $ do
        asciiPrinter (Difference (RelationVariable "left") (RelationVariable "right"))
            `shouldBe` "left - right"

    it "can print a Product" $ do
        asciiPrinter (Product (RelationVariable "left") (RelationVariable "right"))
            `shouldBe` "left * right"

    it "can print a Join" $ do
        asciiPrinter (Join (RelationVariable "left") (RelationVariable "right"))
            `shouldBe` "left >< right"

    it "can print a Division" $ do
        asciiPrinter (Division (RelationVariable "left") (RelationVariable "right"))
            `shouldBe` "left / right"

    it "can print a compound Union" $ do
        asciiPrinter (Union (Intersection 
                                (RelationVariable "a") (RelationVariable "b"))
                            (Intersection (RelationVariable "c") (RelationVariable "d")))
            `shouldBe` "(a N b) U (c N d)"

    it "can print a compound Projection" $ do
        asciiPrinter (Projection ["attribute"] (Union (RelationVariable "left") (RelationVariable "right")))
            `shouldBe` "P[attribute](left U right)"
