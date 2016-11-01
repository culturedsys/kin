module Evaluator where

import qualified Data.Map as M
import qualified Data.Set as S

import Relation as R
import Expression

type Environment = M.Map RelationName Relation

evaluate :: Expression -> Environment -> Maybe Relation
evaluate (RelationVariable name) e = M.lookup name e
evaluate (Projection atts expr) e =
    function (project $ S.fromList atts) expr e 
evaluate (Selection cond expr) e =
    function (select $ evalCondition cond) expr e
evaluate (Rename renames expr) e =
    function (rename $ M.fromList renames) expr e
evaluate (Union l r) e = binop union l r e
evaluate (Intersection l r) e = binop intersection l r e
evaluate (Difference l r) e = binop difference l r e
evaluate (Product l r) e = binop R.product l r e
evaluate (Join l r) e = binop join l r e
evaluate (Division l r) e = binop division l r e 


function ::
    (Relation -> Maybe Relation) ->
        Expression -> Environment ->
                Maybe Relation
function f expr e = f =<< (evaluate expr e)                

binop :: 
    (Relation -> Relation -> Maybe Relation) ->
        Expression -> Expression -> Environment -> 
            Maybe Relation
binop op l r e = do
    l' <- (evaluate l e)
    r' <- (evaluate r e)
    union l' r'



evalCondition :: Condition -> Predicate
evalCondition (Equals l r) t = 
    (==) <$> (evalTerm l t) <*> (evalTerm r t)
evalCondition (NotEquals l r) t =
    (/=) <$> (evalTerm l t) <*> (evalTerm r t)
evalCondition (AndCondition l r) t = 
    (&&) <$> (evalCondition l t) <*> (evalCondition r t)
evalCondition (OrCondition l r) t = 
    (||) <$> (evalCondition l t) <*> (evalCondition r t)
evalCondition (NotCondition l) t = 
    not <$> (evalCondition l t)

    
evalTerm :: Term -> Tuple -> Maybe Value
evalTerm (LiteralTerm v) t = Just v
evalTerm (NameTerm n) t = M.lookup n t

