{-# LANGUAGE GADTs 
           , DataKinds
           , TypeOperators
           , TypeSynonymInstances
           , KindSignatures
           , StandaloneDeriving
           , FlexibleInstances
           , ScopedTypeVariables
           , TypeFamilies
           , FlexibleContexts
           , EmptyDataDecls
           , DeriveFunctor
           , PolyKinds
           #-}


module StateMachine where


import Control.Applicative
import Control.Arrow
import Data.Monoid
import Data.Function(on)
-- import Control.Category
-- import Prelude hiding ((.),id) -- we use those from Category...
import Internal.MachineComponent
{- Author : Alejandro Durán Pallarés (That's me...) 


-}


-- Represent an StateMachine  in a set of concurrent states of execution.
-- it is feeded with value of type "input" and out values of type" output" when success.
data StateMachine input output where
  Wrap:: (MachineCombinator stm, Input stm ~  input) =>  stm output -> StateMachine input output



instance Functor (StateMachine input) where
  fmap f (Wrap stm)= Wrap$ fmap f stm 

instance Applicative (StateMachine input) where
  pure x                      = Wrap$Step True (Just x) (const Nothing)
  Wrap stm1 <*> Wrap stm2 
     | Just f <- collect stm1 = Wrap$Sequenced stm2 (Left (stm1, f<$>stm2))
     | otherwise              = Wrap$Sequenced stm2 (Right(Left stm1)) 

instance Alternative (StateMachine input) where
  empty                      = Wrap$Step True Nothing (const Nothing) 
  Wrap stm1 <|> Wrap stm2    = Wrap$Parallel(Left (stm1,stm2))
  many (Wrap stm)            = Wrap$Loop stm (Just [])  ((:) <$> stm)
  some (Wrap stm)            = Wrap$Loop stm Nothing    ((:) <$> stm)

----TODO...
---- Add instnces of:
--instance Category    CompleteMachine         where
--instance Arrow       CompleteMachine         where
--instance ArrowChoice CompleteMachine         where

elementWith::(a -> Maybe b) -> StateMachine a b
elementWith f = Wrap$Step True Nothing f

elementSuch::(a -> Bool) -> StateMachine a a 
elementSuch cond = elementWith (\x -> if cond x then Just x else Nothing)

element::(Eq a) => a -> StateMachine a a
element x = elementSuch (==x)

anything::StateMachine a a 
anything = elementWith return 

separateBy::StateMachine a b -> StateMachine a c -> StateMachine a [b]  
separateBy stm sep = (:) <$> stm <*> many (sep *> stm)

enclosed::StateMachine a b -> StateMachine a c -> StateMachine a d -> StateMachine a c 
enclosed lft stm rght = lft *> stm <* rght 

parse::StateMachine a b -> [a] -> Maybe b
parse (Wrap stm) = (collect =<<) . foldr ((=<<).trigger) (Just stm).reverse


string::(Eq a) => [a] -> StateMachine a [a]
string stream =  foldr (\a b -> (:) <$> a <*> b) (pure []) (map element stream)  

tokenize::[StateMachine a b] -> [a] -> ([b],[a])
tokenize = undefined

-- This function is supposse to behave somehow as Flex

stateFullTokenize::(state -> [StateMachine a (b,state)]) -> state -> ([b],[state],[a])
stateFullTokenize = undefined
------------------------------------------------------------------------------------------------
---- TODO: this functions below rely on the Arrow instances not defined yet....

notBeing::StateMachine a b -> StateMachine a c -> StateMachine a b 
notBeing = undefined

whateverBut:: StateMachine a b -> StateMachine a ()
whateverBut = undefined

being::StateMachine a b -> StateMachine a c -> StateMachine a (b,c)
being = undefined -- 

recording::StateMachine a b -> (a -> acc -> acc) -> StateMachine a (Maybe b,acc)
recording = undefined

recordingError::StateMachine a b -> (a -> err -> err) -> StateMachine a (Either b err)
recordingError = undefined

parseText::StateMachine Char b -> String -> Either c (Int,Int,String)
parseText = undefined

