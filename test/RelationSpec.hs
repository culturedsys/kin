module RelationSpec (
    spec
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Test.Hspec

import Relation

spec = do
    it "can be empty" $ do
        (fromList [] []) `shouldBe` 
            (Relation (S.fromList []) (S.fromList []))

    it "can be empty but have named attributes" $ do
        (fromList ["name1", "name2"] []) `shouldBe` 
            (Relation (S.fromList ["name1", "name2"]) (S.fromList []))

    it "can contain multiple tuples" $ do
        (fromList ["name1", "name2"] [["1", "2"], ["3", "4"]]) `shouldBe`
            (Relation (S.fromList ["name1", "name2"]) 
                      (S.fromList [(M.fromList [("name1", "1"), ("name2", "2")]),
                                   (M.fromList [("name1", "3"), ("name2", "4")])]))


    let sample1 = fromList ["name1", "name2"] [["1", "2"], ["3", "4"]]

    it "can be projected" $ do
        project (S.fromList ["name1"]) sample1 `shouldBe`
            Just (fromList ["name1"] [["1"], ["3"]])
        project (S.fromList ["name2"]) sample1 `shouldBe`
            Just (fromList ["name2"] [["2"], ["4"]])

    it "returns Nothing if asked to project a non-existent attribute" $ do
        project (S.fromList ["name3"]) sample1 `shouldBe` Nothing

    it "can be renamed" $ do
        rename (M.fromList [("name1", "name3")]) sample1 `shouldBe`
            Just (fromList ["name3", "name2"] [["1", "2"], ["3", "4"]])
        rename (M.fromList [("name2", "name3")]) sample1 `shouldBe`
            Just (fromList ["name1", "name3"] [["1", "2"], ["3", "4"]])

    it "returns Nothing if asked to rename a non-existent attribute" $ do
        rename (M.fromList [("name3", "name4")]) sample1 `shouldBe` Nothing

    it "returns Nothing if asked to rename to an already existing name" $ do
        rename (M.fromList [("name1", "name2")]) sample1 `shouldBe` Nothing

    it "can be selected" $ do
        select (predicate "name1" (== "1")) sample1 `shouldBe`
            Just (fromList ["name1", "name2"] [["1", "2"]])
        select (predicate "name2" (/= "2")) sample1 `shouldBe`
            Just (fromList ["name1", "name2"] [["3", "4"]])

    it "returns Nothing if a selection predicate fails" $ do
        select (\_ -> Nothing) sample1 `shouldBe` Nothing

    it "returns Nothing if asked to select based on a non-existent attribute" $ do
        select (predicate "name3" (== "10")) sample1 `shouldBe` Nothing


    let sample2 = fromList ["name1", "name2"] [["1", "2"], ["5", "6"]]
    
    it "can be union-ed" $ do
        union sample1 sample2 `shouldBe`
            Just (fromList ["name1", "name2"] [["1", "2"], ["3", "4"], ["5", "6"]])

    it "is a no-op when union-ed with itself" $ do
        union sample1 sample1 `shouldBe` Just sample1

    let sample3 = fromList ["name1"] [["1"], ["5"]]
        sample4 = fromList ["name3", "name4"] [["1", "2"], ["3", "4"]]


    it "returns Nothing when asked to union incompatible relations" $ do
        union sample1 sample3 `shouldBe` Nothing
        union sample1 sample4 `shouldBe` Nothing

    it "can be intersection-ed" $ do
        intersection sample1 sample2 `shouldBe` 
            Just (fromList ["name1", "name2"] [["1", "2"]])

    it "is a no-op when intersection-ed with itself" $ do
        intersection sample1 sample1 `shouldBe` Just sample1

    it "returns Nothing when asked to intersection incompatible relations" $ do
        intersection sample1 sample3 `shouldBe` Nothing
        intersection sample1 sample4 `shouldBe` Nothing