module LoaderSpec (
    spec
    ) where
 
import Loader
import Relation

import Test.Hspec

spec = do
    it "can load a CSV string" $ do
        basicCsvLoader "name1,name2\n1,2\n3,4" `shouldBe`
            Just (fromList ["name1", "name2"] [["1", "2"], ["3", "4"]])

    it "can load an empty relation" $ do
        basicCsvLoader "name1,name2\n" `shouldBe`
            Just (fromList ["name1", "name2"] [])

    it "returns Nothing when asked to load a relation with missing attributes" $ do
        basicCsvLoader "name1,name2\n1\n3,4" `shouldBe`
            Nothing
