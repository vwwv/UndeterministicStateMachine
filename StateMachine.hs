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
import Control.Category
import Prelude hiding ((.),id) -- we use those from Category...
{- Author : Alejandro Durán Pallarés (That's me...) 


-}


-- Represent an StateMachine  in a set of concurrent states of execution.
-- it is feeded with value of type "input" and out values of type" output" when success.
data StateMachine input output where
  Wrap:: (MachineCombinator stm) => stm output -> StateMachine input output


----TODO: Check the laws.....
----TODO...
--instance Functor     (CompleteMachine input) where
--instance Applicative (CompleteMachine input) where
--instance Alternative (CompleteMachine input) where
--instance Category    CompleteMachine         where
--instance Arrow       CompleteMachine         where
--instance ArrowChoice CompleteMachine         where

--elementSuch  cond
--element  
--filteringOut
-- minus
-- and...
--filteringIn
--elements 
--separateBy 
--enclosed  
--anyone          
--some 
--many 
--empty



---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
-- Combinators to define an State machine:

-- provisional laws:
------ trigger inn (st1 `merge` st2) == (trigger inn st1) `merge` (trigger inn st2) 
------ collect st1 <|> collect st2   == collect (st1 `merge` st2)
------ todo, faltan leyes...

class (Functor stm) => MachineCombinator (stm :: * -> * ) where

    type Input stm  :: * 

    trigger  :: Input stm   -> stm output    -> Maybe (stm output)
    collect  :: stm output  -> Maybe output  
    merge    :: stm out     -> stm out -> stm out


--------------------------------------------------------------------------------------

-- Here there's a list of MachineCombinators, complete enough to define all the instances
-- StateMachine needs. Originally it was define as a big GADT; but now It had been splitted
-- using data/type famillies, so one can extend them for particular prupose 
-- such creating an specific more-eficient particular combination (ex: withLength x)....






--------------------------------------------------------------------------------------
instance MachineCombinator (StepMachine input) where
                                        
    type Input (StepMachine input)          = input
    
    trigger inn (Step b _ f)
             | b                            = Just$Step False (f inn) f 
             | otherwise                    = Nothing

    collect (Step _ x  _)                   = x 
    merge   (Step b x f)  (Step b' x' _)    = Step (b||b') (x <|> x') f

data StepMachine input output = Step Bool (Maybe output) (input -> Maybe output) -- TODO check spell 
deriving instance Functor (StepMachine inn) 
---------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------
instance ( MachineCombinator stm1 
         , MachineCombinator stm2
         , Input stm1 ~ Input stm2 
         ) => MachineCombinator (ParallelMachine stm1 stm2) where

         type Input (ParallelMachine stm1 stm2)     = Input stm1

         trigger inn (Parallel st)    =  case st of
                                          Left  (st1,st2)   -> Parallel <$> toTriState (trigger inn st1)  (trigger inn st2)
                                          Right (Left  st1) -> Parallel <$> toTriState (trigger inn st1)  Nothing
                                          Right (Right st2) -> Parallel <$> toTriState Nothing            (trigger inn st2)

         collect (Parallel st)        = either (uncurry (<|>).(collect***collect))  (either collect collect)
                                      $ st

         merge (Parallel a) (Parallel b) = Parallel $ case a of 
                                                (Left (x,y))     -> left (merge x *** merge y) 
                                                                  . right (left (merge x).right (merge y))
                                                                  $ b 

                                                (Right(Left  x)) -> right (left  (merge x)) $ b 
                                                (Right(Right y)) -> right (right (merge y)) $ b



data ParallelMachine stm1 stm2 out = Parallel (TriState (stm1 out) (stm2 out))
instance (Functor stm1,Functor stm2 ) => Functor (ParallelMachine stm1 stm2) where
  fmap f (Parallel st) = Parallel $ ((fmap f ***fmap f)+++ fmap f +++ fmap f$st )

-----------------------------------------------------------------------------------------




-----------------------------------------------------------------------------------------

instance ( MachineCombinator stm1
         , MachineCombinator stm2
         , Input stm1 ~ Input stm2
         ) => MachineCombinator (SequencedMachine stm1 mid stm2) where

         type Input (SequencedMachine stm1 mid stm2)    = Input stm1

         trigger inn (Sequenced cte st)  = case st of 
                                            
                                            Left  (st1,st2) 
                                              
                                              | Just f <- collect st1  -> Sequenced cte <$> toTriState
                                                                          (trigger inn st1)
                                                                          (trigger inn$ merge (f<$>cte) st2)

                                              | otherwise              -> Sequenced cte <$> toTriState
                                                                          (trigger inn st1)
                                                                          (trigger inn st2)

                                            Right (Left st1)
                                              | Just f <- collect st1  -> Sequenced cte <$> toTriState
                                                                          (trigger inn st1)
                                                                          (trigger inn (f<$>cte))
                                              
                                              | otherwise              ->  Sequenced cte . Right . Left
                                                                       <$> trigger inn st1

                                            Right (Right st2)          ->  Sequenced cte . Right . Right
                                                                       <$> trigger inn st2 
                
         collect (Sequenced _ st)  = case st of 
                                      Left (_,st2)       -> collect st2
                                      Right (Right st2)  -> collect st2
                                      _                  -> Nothing
                                         

         merge (Sequenced st1 st2 )
               (Sequenced st1' st2') = Sequenced (merge st1 st1') $ case st2 of 
                                                    (Left (x,y))     -> left (merge x *** merge y) 
                                                                      . right (left (merge x).right (merge y))
                                                                      $ st2'

                                                    (Right(Left  x)) -> right (left  (merge x)) $ st2'
                                                    (Right(Right y)) -> right (right (merge y)) $ st2'




data  SequencedMachine  stm1 mid stm2 out = Sequenced (stm2 mid) (TriState  (stm1 (mid -> out)) (stm2 out))

instance (Functor stm1,Functor stm2) =>  Functor (SequencedMachine stm1 mid stm2) where
  fmap f (Sequenced cte st) = Sequenced cte   ((update***fmap f)+++ update +++ fmap f$st )
         where
          update = fmap (fmap f)

-----------------------------------------------------------------------------------------


---- change names .... loop -> loopMachine

------------------------------------------------------------------------------------------
instance ( MachineCombinator stm
         ) => MachineCombinator (LoopMachine stm mid) where 

         type Input (LoopMachine stm mid)     = Input stm


         trigger inn (Loop cte x stm)
              | Just f  <- collect stm      = let stm'  = ((f.).(++) . return ) <$> cte
                                                  stm'' = merge stm' stm  

                                               in Loop cte Nothing <$> trigger inn stm

              | otherwise                   = Loop cte Nothing <$> trigger inn stm
                   where
                    update :: ([a]->out) -> a -> ([a]->out)
                    update g =  (g.).(++) . return 

         collect (Loop _ x _)               = x 

         merge  (Loop cte  x  stm )
                (Loop cte' x' stm')         = Loop (merge cte cte') (x<|>x')(merge stm stm') 

data LoopMachine stm (mid :: *) output = Loop (stm mid) (Maybe output) (stm ([mid]->output))
instance (Functor stm) => Functor (LoopMachine stm mid) where
  fmap f (Loop cte x stm) = Loop cte (f<$>x) (fmap f <$> stm) 

------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------
--instance ( StateMachine stm 
--         ) => StateMachine (FilterMachine stm mid inn) where
--         data State (FilterMachine stm mid inn) out = FilterSt (State stm mid)
--         type Input (FilterMachine stm mid inn)     = inn

--         FilterSt st1 `merge` FilterSt st2          = FilterSt $ merge st1 st2 

--         trigger inn (FilterMachine stm inF outF) (FilterSt st)
--                      | Just mid <- inF inn         = (outF=<<) *** (FilterSt<$>) $ trigger mid stm st 
--                      | otherwise                   = (Nothing, Just$FilterSt st)



--data FilterMachine stm mid inn out = FilterMachine (stm mid) (inn -> Maybe (Input stm)) (mid -> Maybe out)
------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------





------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------





-- Isomorphic to Maybe, but we explicitly indicate why we are we using it....
data TakingLeft a = Mempty | Took a deriving Functor
instance Monoid (TakingLeft a) where
  mempty = Mempty
  Mempty `mappend` x = x
  x   `mappend` _    = x 



type TriState a b = Either (a,b) (Either a b) 

toTriState:: Maybe a -> Maybe b -> Maybe (TriState a b)
toTriState Nothing  Nothing  = Nothing
toTriState (Just a) Nothing  = Just $ Right (Left a)
toTriState Nothing  (Just b) = Just $ Right (Right b)
toTriState (Just a) (Just b) = Just $ Left (a,b)


fromTrieState::TriState a b -> (Maybe a,Maybe b)
fromTrieState = undefined


------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------











