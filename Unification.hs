module Unification where

import DataTypes (Type (..), Constraint, Subst)

{- Substitution and Unification -}

-- Apply a substitution to a type
substType :: Subst -> Type -> Type
substType subs t = case t of
    TBool -> TBool
    TInt -> TInt
    TVar v -> 
        case lookup v subs of 
            Just tt -> tt
            Nothing -> TVar v -- No Sub needed
    TFun t1 t2 -> TFun (substType subs t1) (substType subs t2)
    TPair t1 t2 -> TPair (substType subs t1) (substType subs t2)

-- Apply a substitution to a constraint
substConstraint :: Subst -> Constraint -> Constraint
substConstraint subs (t1, t2) = (substType subs t1, substType subs t2)

-- Apply a substitution to a list of constraints
substConstraints :: Subst -> [Constraint] -> [Constraint]
substConstraints subs lst = [substConstraint subs c | c <- lst]

-- Check if a variable name occurs in a type
occursIn :: String -> Type -> Bool
occursIn name t = case t of 
    TBool -> False
    TInt -> False
    TVar v -> v == name
    TFun t1 t2 -> occursIn name t1 || occursIn name t2
    TPair t1 t2 -> occursIn name t1 || occursIn name t2


-- Given a list of constraints, generate the Most General Unifier for the constraints if it exists
unify :: [Constraint] -> Maybe Subst
unify [] = Just [] -- Identity Sub

unify ((l1, r1):tail) =
    if l1 == r1
        then unify tail
        else 
            case (l1, r1) of
                (TFun a b, TFun c d) -> unify ((a,c):(b,d):tail)
                (TPair a b, TPair c d) -> unify ((a,c):(b,d):tail)
                (TVar x, _) ->
                    if occursIn x r1
                        then Nothing
                        else 
                            let subSoln = unify (substConstraints [(x, r1)] tail) 
                            in case subSoln of
                                Nothing -> Nothing
                                Just soln -> 
                                    let newR1 = substType soln r1
                                    in Just ((x, newR1):soln)
                (_, TVar x) ->
                    if occursIn x l1
                        then Nothing
                        else 
                            let subSoln = unify (substConstraints [(x, l1)] tail) 
                            in case subSoln of
                                Nothing -> Nothing
                                Just soln -> 
                                    let newL1 = substType soln l1
                                    in Just ((x, newL1):soln)
                (_, _) -> Nothing