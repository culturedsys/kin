module Printer where

import Data.List

import Expression

type Printer = Expression -> String

-- Converts a relational algebra expression to my simple text based language
-- that is supposed to approximate the conventional notation in plain ASCII.
asciiPrinter :: Printer
asciiPrinter expr = go expr False where
    
    go (RelationVariable rel) _ = rel
    go (Projection attributes e) _ = 
        function "P" (intercalate "," attributes) e 
    go (Selection cond e) _ =
        function "S" (asciiConditionPrinter cond) e  
    go (Rename renames e) _ =
        let rs = intercalate "," $ map (\(a,b) -> a ++ "->" ++ b) renames
        in 
            function "R" rs e
    go (Union l r) b = binop "U" l r b
    go (Intersection l r ) b = binop "N" l r b
    go (Difference l r) b = binop "-" l r b
    go (Product l r) b = binop "*" l r b
    go (Join l r) b = binop "><" l r b
    go (Division l r) b = binop "/" l r b
    
    function f args e = f ++ "[" ++ args ++ "](" ++ go e False ++ ")"
    binop op l r b = brackets b (go l True ++ " " ++ op ++ " " ++ go r True)
    brackets b s = if b then "(" ++ s ++ ")" else s


asciiConditionPrinter :: Condition -> String
asciiConditionPrinter (Equals l r) = 
    (asciiTermPrinter l) ++ "=" ++ (asciiTermPrinter r)
asciiConditionPrinter (NotEquals l r) =
    (asciiTermPrinter l) ++ "!=" ++ (asciiTermPrinter r)
asciiConditionPrinter (AndCondition l r) = 
    "(" ++ asciiConditionPrinter l ++ ") AND (" ++
        asciiConditionPrinter r ++ ")"
asciiConditionPrinter (OrCondition l r) = 
    "(" ++ asciiConditionPrinter l ++ ") OR (" ++
        asciiConditionPrinter r ++ ")"
asciiConditionPrinter (NotCondition l) =
    "!(" ++ asciiConditionPrinter l ++ ")"

asciiTermPrinter :: Term -> String
asciiTermPrinter (NameTerm n) = n
asciiTermPrinter (LiteralTerm v) = "'" ++ v ++ "'"