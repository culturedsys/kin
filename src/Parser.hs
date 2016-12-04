module Parser where

import Prelude hiding (product)

import Text.Parsec
import Text.Parsec.String

import Relation(AttributeName)
import Expression

expression :: Parser Expression
expression = try $ union <|> 
                   intersection <|> 
                   difference <|> 
                   product <|>
                   join <|> 
                   division <|>
                   unary
                

unary :: Parser Expression
unary = try $ projection <|> 
              rename <|>
              selection <|>
              relationVariable <|>
              char '(' *> expression <* char ')'

projection :: Parser Expression
projection = function Projection 'P' attributeList

function :: (a -> Expression -> Expression) -> Char -> Parser a -> Parser Expression
function cons symbol qualifier = try $ do
    _ <- char symbol >> char '[' >> spaces
    qual <- qualifier
    _ <- spaces >> char ']' >> spaces >> char '(' >> spaces
    expr <- expression 
    _ <- spaces >> char ')'
    return $ cons qual expr

attributeList :: Parser [AttributeName]
attributeList = try $ sepBy1 attribute (spaces >> char ',' >> spaces)

attribute :: Parser AttributeName
attribute = (many1 alphaNum) <|> quotedString

quotedString :: Parser String
quotedString = (char '{' *> many1 (noneOf ['}']) <* char '}')

rename :: Parser Expression
rename = function Rename 'R' renameList

renameList :: Parser [(AttributeName, AttributeName)]
renameList = try $ sepBy1 renameExpr (spaces >> char ',' >> spaces)

renameExpr :: Parser (AttributeName, AttributeName)
renameExpr = try $ do
    from <- attribute
    _ <- spaces >> string "->" >> spaces
    to <- attribute
    return $ (from, to)

relationVariable :: Parser Expression
relationVariable = try $ RelationVariable <$> ((many1 alphaNum) <|> quotedString)

selection :: Parser Expression
selection = function Selection 'S' condition 

condition :: Parser Condition
condition = try $ equals <|> 
                  notEquals <|>
                  andCondition <|>
                  orCondition <|>
                  notCondition

equals :: Parser Condition
equals = try $ Equals <$> (term <* (spaces >> char '=' >> spaces)) <*> term

term :: Parser Term
term = LiteralTerm <$> (char '\'' *> (many (noneOf ['\''])) <* char '\'') <|> 
        NameTerm <$> attribute 

notEquals :: Parser Condition
notEquals = try $ NotEquals <$> (term <* (spaces >> string "!=" >> spaces)) <*> term

andCondition :: Parser Condition
andCondition = try $ do
    left <- (char '(' >> spaces) *> condition <* (spaces >> char ')')
    _ <- spaces >> string "AND" >> spaces
    right <- (char '(' >> spaces) *> condition <* (spaces >> char ')')
    return $ AndCondition left right


orCondition :: Parser Condition
orCondition = try $ do
    left <- (char '(' >> spaces) *> condition <* (spaces >> char ')')
    _ <- spaces >> string "OR" >> spaces
    right <- (char '(' >> spaces) *> condition <* (spaces >> char ')')
    return $ OrCondition left right


notCondition :: Parser Condition
notCondition = try $ do
    _ <- string "NOT" >> spaces
    cond <- (char '(' >> spaces) *> condition <* (spaces >> char ')')
    return $ NotCondition cond

binop :: (Expression -> Expression -> Expression) -> String -> Parser Expression
binop cons op = try $ do
    left <- unary
    _ <- spaces >> string op >> spaces
    right <- unary
    return $ cons left right

union :: Parser Expression
union = binop Union "U"

intersection :: Parser Expression
intersection = binop Intersection "N"

difference :: Parser Expression
difference = binop Difference "-"

product :: Parser Expression
product = binop Product "*"

join :: Parser Expression
join = binop Join "><"

division :: Parser Expression
division = binop Division "/"