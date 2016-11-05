module Loader where 

import Data.List.Split
import Relation

type Loader = String -> Maybe Relation

-- Load a relation from a simple CSV file, where the first line contains the
-- names of the attributes. Checks that each line contains the same number of
-- elements as there are attributes in the relation. The loader does not 
-- handle any kind of quoting; it just splits lines on commas.
basicCsvLoader :: Loader
basicCsvLoader s = case map (splitOn ",") $ lines s of
    [] -> Nothing
    (x:xs) -> fromList x <$> mapM validate xs
        where validate values = if length values == length x then Just values else Nothing