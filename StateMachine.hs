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
           , MultiParamTypeClasses
           #-}


module StateMachine where


import Control.Applicative
import Control.Arrow
import Data.Monoid
import Data.Function(on)
-- import Control.Category
-- import Prelude hiding ((.),id) -- we use those from Category...
import Internal.MachineComponent
import Data.Char
import Data.Time.Clock

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

  some (Wrap stm)             
        | Just x <- collect stm = Wrap$Loop stm ( ((.)(x:).(:)) <$> stm)
        | otherwise             = Wrap$Loop stm ( (:)           <$> stm)

  many v                     = some v <|> pure [] -- I'm wondering why I do need this...

----TODO......
---- Add instnces of:
--instance Category    StateMachine         where
--instance Arrow       StateMachine         where
--instance ArrowChoice StateMachine         where

elementWith::(a -> Maybe b) -> StateMachine a b
elementWith f = Wrap$Step True Nothing f

elementSuch::(a -> Bool) -> StateMachine a a 
elementSuch cond = elementWith (\x -> if cond x then Just x else Nothing)

element::(Eq a) => a -> StateMachine a a
element x = elementSuch (==x)

anything::StateMachine a a 
anything = elementWith return 

separatedBy::StateMachine a c -> StateMachine a b -> StateMachine a [b]  
separatedBy sep stm = (:) <$> stm <*> many (sep *> stm)

enclosed::StateMachine a b -> StateMachine a c -> StateMachine a d -> StateMachine a d 
enclosed lft rght stm = lft *> stm <* rght 

string::(Eq a) => [a] -> StateMachine a [a]
string stream =  foldr (\a b -> (:) <$> a <*> b) (pure []) (map element stream)  

parse::StateMachine a b -> [a] -> Maybe b
parse (Wrap stm) = (collect =<<) . foldr ((=<<).trigger) (Just stm).reverse

tokenize::[StateMachine a b] -> [a] -> ([b],[a])
tokenize = undefined

------------------------------------------------------------------------------------------------
---- TODO: this functions will rely on the Arrowed combinators not defined yet....
notBeing::StateMachine a b -> StateMachine a c -> StateMachine a b 
notBeing = undefined

whateverBut:: StateMachine a b -> StateMachine a ()
whateverBut = undefined

being::StateMachine a b -> StateMachine a c -> StateMachine a (b,c)
being = undefined -- 

recording::StateMachine a b -> (a -> acc -> acc) -> StateMachine a (Maybe b,acc)
recording = undefined

recordingError::StateMachine a b -> (a -> err -> err) -> StateMachine (Either b err) b
recordingError = undefined


-- This function is supposse to behave somehow as Flex

stateFullTokenize::(state -> [StateMachine a (b,state)]) -> state -> ([b],[state],[a])
stateFullTokenize = undefined

parseText::StateMachine Char b -> String -> Either (Int,Int,String) b
parseText = undefined



--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
-----TODO: add more instances...

--TODO, add Standards types, lists, maybes, etc...

------------- Usefull 
class DefinedStateMahine inn out where
  field::StateMachine inn out

instance DefinedStateMahine Char Int where
  field = foldl (\acc c-> (ord c - 48) + 10*acc) 0
      <$> some (elementSuch isDigit)

--Might get this one out of here....is a bit pointless...
instance DefinedStateMahine Char () where
  field = () <$ some (elementSuch isSpace)


--Time can be tedious for parsing...related instances can be quiet usefull....
--
--Okey, true, this one is just for the example...but one extended to more general
-- formats it could be awesome....
instance DefinedStateMahine Char DiffTime where
  field = format <$> digit <*> (digit <* element ':')
                 <*> digit <*> (digit <* element ':')
                 <*> digit <*> (digit <* element ',')
                 <*> digit <*> digit <*> digit
   where

      format a b c d e f g h i  = let [h1, h0, m1, m0, s1, s0, p2, p1, p0] = fmap (\c->fromIntegral (ord c) - 48 )
                                                                             [a, b, c, d, e, f, g, h, i]

                                   in (secondsToDiffTime $ ((h1*10+h0)*60 + m1*10+m0)*60 + s1*10+s0)
                                      + (picosecondsToDiffTime $ (p2*100 + p1*10 + p0)*(10^9))

      digit                     = elementSuch isDigit



