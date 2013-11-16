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
--instance MachineCombinator (StepMachine input) where
    
--    data State (StepMachine input) out      = StepSt Bool (Maybe out)
                                        
--    type Input (StepMachine input)          = input
    
--    trigger (Step _ f) inn (StepSt b _)
--             | b                            = Just$StepSt False (f inn)
--             | otherwise                    = Nothing

--    collect (StepSt _ out)                  = out 
--    merge  (StepSt b out) (StepSt b' out')  = StepSt (b||b') (out <|> out')
--    start  (Step x _)                       = StepSt True x

--data StepMachine input output = Step (Maybe output) (input -> Maybe output) -- TODO check spell 
--deriving instance Functor (StepMachine inn) 
---------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------
--instance ( MachineCombinator stm1 
--         , MachineCombinator stm2
--         , Input stm1 ~ Input stm2 
--         ) => MachineCombinator (ParallelMachine stm1 stm2) where
--         data State (ParallelMachine stm1 stm2) out = ParallelSt (TriState (State stm1 out) (State stm2 out))

--         type Input (ParallelMachine stm1 stm2)     = Input stm1

--         trigger (Parallel stm1 stm2)
--                 inn (ParallelSt st)    =  case st of

--                                        Left  (st1,st2)   -> ParallelSt <$> toTriState (trigger stm1 inn st1) 
--                                                                                       (trigger stm2 inn st2)

--                                        Right (Left  st1) -> ParallelSt <$> toTriState (trigger stm1 inn st1) 
--                                                                                       Nothing

--                                        Right (Right st2) -> ParallelSt <$> toTriState Nothing
--                                                                                       (trigger stm2 inn st2)

--         collect (ParallelSt st)        = either (uncurry (<|>).(collect***collect))  (either collect collect)
--                                        $ st

--         merge (ParallelSt a) (ParallelSt b) = ParallelSt $ case a of 
--                                                (Left (x,y))     -> left (merge x *** merge y) 
--                                                                  . right (left (merge x).right (merge y))
--                                                                  $ b 

--                                                (Right(Left  x)) -> right (left  (merge x)) $ b 
--                                                (Right(Right y)) -> right (right (merge y)) $ b

--         start (Parallel stm1 stm2) = ParallelSt (Left (start stm1,start stm2)) 


--data ParallelMachine stm1 stm2 out = Parallel (stm1 out) (stm2 out) 
--deriving instance (Functor stm1,Functor stm2 ) => Functor (ParallelMachine stm1 stm2)
-----------------------------------------------------------------------------------------




-----------------------------------------------------------------------------------------
-- TODO: some code is duplicate!!!
--instance ( MachineCombinator stm1
--         , MachineCombinator stm2
--         , Input stm1 ~ Input stm2
--         ) => MachineCombinator (SequencedMachine stm1 mid stm2) where

--         data State (SequencedMachine stm1 mid stm2) out = SequencedSt ( TriState  (State stm1 (mid -> out))
--                                                                                   (State stm2 out)
--                                                                       ) 
                                                                      
                                                                      
  
--         type Input (SequencedMachine stm1 mid stm2)    = Input stm1


--         trigger inn (Sequenced stm1 stm2) (SequencedSt st) = let (st1,st2) = fromTrieState st
--                                                                  st2'      = do x    <- collect <$> st1
--                                                                                 (fmap ($x) <$> st2) <|> st2 

--                                                               in  SequencedSt . toTriState (trigger inn stm<$>st1)
--                                                                                            (trigger inn stm<$>st2') 

--         collect (SequencedSt st)  = case st of 
--                                      Left (_,st2)       -> collect st2
--                                      Right (Right st2)  -> collect st2
--                                      _                  -> Nothing

--         start (Sequenced stm1 stm2) = let st1 = start stm1
--                                        in case collect st1 of
--                                          Nothing -> SequencedSt $Right (Left st1)
--                                          Just x  -> SequencedSt $Left (st1, start $ ($x) fmap stm2)
                                         

--         merge (SequencedSt a )(SequencedSt b) = SequencedSt $ case a of 
--                                                    (Left (x,y))     -> left (merge x *** merge y) 
--                                                                      . right (left (merge x).right (merge y))
--                                                                      $ b 

--                                                    (Right(Left  x)) -> right (left  (merge x)) $ b 
--                                                    (Right(Right y)) -> right (right (merge y)) $ b


--instance (Functor stm1) => Functor (SequencedMachine stm1 mid stm2) where
--  fmap f (Sequenced stm1 stm2) = Sequenced (fmap f<$>stm1) stm2 
--data SequencedMachine  stm1 mid stm2 output = Sequenced (stm1 (mid -> output)) (stm2 mid)

-----------------------------------------------------------------------------------------


---- change names .... loop -> loopMachine

------------------------------------------------------------------------------------------
--instance ( StateMachine stm
--         , Monoid mid2
--         ) => StateMachine (Loop stm mid1 mid2) where 
--         data State (Loop stm mid1 mid2) out = LoopSt (State stm mid1) mid2  
--         type Input (Loop stm mid1 mid2)     = Input stm

--         trigger inn (Loop stm inc f) 
--                     (LoopSt st acc)         = let (out,st') = trigger inn stm st 
--                                                   acc'      = inc acc <$> out 

--                                                in ( f<$>acc' 
--                                                   , LoopSt <$> st' <*> acc'
--                                                   ) 

--         merge  (LoopSt st  mid2 )
--                (LoopSt st' mid2')            = LoopSt (merge st st') (mid2<>mid2') 

--data Loop stm (mid1:: *) (mid2:: *) output = Loop (stm mid1) 
--                                                  (mid2->mid1->mid2) 
--                                                  (mid2->output)   
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











