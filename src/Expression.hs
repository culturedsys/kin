-- An AST representing relational algebra expressions.

module Expression where

import Relation

data Expression = RelationVariable RelationName |
                  Projection [AttributeName] Expression |
                  Selection Condition Expression |
                  Rename [(AttributeName, AttributeName)] Expression | 
                  Union Expression Expression |
                  Intersection Expression Expression |
                  Difference Expression Expression |
                  Product Expression Expression |
                  Join Expression Expression |
                  Division Expression Expression

-- A condition that can be used in a selection. Note that currently only
-- strings are supported as values, and so the only condition that really
-- makes sense is equality.
data Condition = Equals Term Term |
                 NotEquals Term Term |
                 AndCondition Condition Condition |
                 OrCondition Condition Condition |
                 NotCondition Condition

-- A value that can be used in a condition - either an attribute name or a 
-- literal value.
data Term = NameTerm AttributeName | LiteralTerm Value
