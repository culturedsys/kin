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

    it "can be renamed" $ do
        rename (M.fromList [("name1", "name3")]) sample1 `shouldBe`
            Just (fromList ["name3", "name2"] [["1", "2"], ["3", "4"]])
        rename (M.fromList [("name2", "name3")]) sample1 `shouldBe`
            Just (fromList ["name1", "name3"] [["1", "2"], ["3", "4"]])