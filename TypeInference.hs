{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module TypeInference where

import DataTypes (Expr(..), Type(..), Env, Constraint, Subst) -- import the data type definitions
import Unification (unify, substType) -- Most General Unification Algorithm

import Data.Maybe (fromJust)
import Control.Monad.Trans.State

-- Types for primitive operations
primitives :: Env
primitives  = [ ("+", TFun TInt (TFun TInt TInt))
              , ("*", TFun TInt (TFun TInt TInt))
              , ("-", TFun TInt TInt)
              , ("<", TFun TInt (TFun TInt TBool))
              , ("not", TFun TBool TBool)
              , ("and", TFun TBool (TFun TBool TBool))
              , ("ite", TFun TBool (TFun TInt (TFun TInt TInt))) ]

{- Simple Inference Algorithm -}

-- Infer the type of an expression from the given environment
simpleInfer :: Env -> Expr -> Maybe Type
simpleInfer env expr = case expr of
    CBool _ -> Just TBool
    CInt _ -> Just TInt
    Var v -> lookup v env
    Pair e1 e2 ->
        let exp1 = simpleInfer env e1
        in case exp1 of
            Nothing -> Nothing
            Just exp1T ->
                let exp2 = simpleInfer env e2
                in case exp2 of
                    Nothing -> Nothing
                    Just exp2T -> Just (TPair exp1T exp2T)

    Ann e1 t1 ->
        let tt = simpleInfer env e1
        in case tt of
            Just t2 ->
                if t1 == t2
                then Just t2
                else Nothing
            Nothing -> Nothing

    App e1 e2 ->
        let funcT = simpleInfer env e1
            argT = simpleInfer env e2
        in case funcT of
            Just (TFun eArgT rest) ->
                case argT of
                    Just accArgT ->
                        if eArgT == accArgT
                        then Just rest
                        else Nothing
                    Nothing -> Nothing
            _ -> Nothing


simpleInferWithPrimitives :: Env -> Expr -> Maybe Type
simpleInferWithPrimitives env expr = simpleInfer (env ++ primitives) expr


{- Type Inference Based on Most General Unifier -}

-- A stateful monad for generating fresh type variables
type FreshM a = State Int a
-- Generate a type variable with a unique name (e.g. _t0, _t1, ...) using the counter in the state
-- You are not required to implement this function, but it may be useful
freshTVar :: FreshM Type
freshTVar = do
    count <- get
    let countStr = show count
    let name = "_t" ++ countStr
    modify (+ 1)
    return (TVar name)

-- Run the FreshM monad with an initial counter value of 0
-- You are not required to implement this function, but it may be useful
runFreshM :: FreshM a -> a
runFreshM freshMonad = evalState freshMonad 0

-- Given an environment and an expression, generate a type and a set of constraints
-- You are free to change the type of this function if you want
-- You may assume that all free variables in the input expression are bound in the environment,
-- and that all types in the input environment are concrete (i.e. no type variables)
getConstraints :: Env -> Expr -> FreshM (Type, [Constraint])
getConstraints env expr = case expr of
    CBool _ -> do
        ft <- freshTVar
        return (ft, [(ft, TBool)])
    CInt _ -> do
        ft <- freshTVar
        return (ft, [(ft, TInt)])
    Pair e1 e2 -> do
        ft <- freshTVar
        (t1, c1) <- getConstraints env e1
        (t2, c2) <- getConstraints env e2
        let constraints = [(ft, TPair t1 t2)] ++ c1 ++ c2
        return (ft, constraints)
    Var v -> case lookup v env of
        Just t -> do 
            ft <- freshTVar
            return (ft, [(ft, t)])
    App e1 e2 -> do
        ft <- freshTVar
        (funcT, fc) <- getConstraints env e1
        (argT, ac) <- getConstraints env e2
        let constraints = [(funcT, TFun argT ft)] ++ fc ++ ac
        return (ft, constraints)
    Ann e t -> do
        ft <- freshTVar
        (et, ec) <- getConstraints env e
        let constraints = [(ft, t), (et, t)] ++ ec
        return (ft, constraints)
    Lam arg e -> do
        lambdaFt <- freshTVar
        argFt <- freshTVar
        let newEnv = (arg, argFt) : env
        (et, ec) <- getConstraints newEnv e
        let constraints = (lambdaFt, TFun argFt et) : ec
        return (lambdaFt, constraints)


-- Infer the type of an expression from the given environment, using getConstraints, unify, and substType
inferType :: Env -> Expr -> Maybe Type
inferType env expr = 
    let (etype, constraints) = runFreshM (getConstraints env expr)
        sub = unify constraints
    in case sub of
        Just omega -> Just (substType omega etype)
        _ -> Nothing

inferTypeWithPrimitives :: Env -> Expr -> Maybe Type
inferTypeWithPrimitives env expr = inferType (env ++ primitives) expr
