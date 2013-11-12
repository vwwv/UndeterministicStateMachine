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

-- TODO: laws...


-- Represent an StateMachine  in a set of concurrent states of execution.
-- it is feeded with value of type "input" and out values of type" output" when success.

class MachineCombinator (stm :: * -> * ) where

    data State stm  :: * -> *
    type Input stm  :: * 

    trigger  :: Input stm  -> stm output -> State stm output -> Maybe (State stm output)
    collect  :: State stm output -> Maybe output
    merge    :: State stm out -> State stm out -> State stm out


--------------------------------------------------------------------------------------

-- Here there's a list of MachineCombinators, complete enough to define all the instances
-- StateMachine needs. Originally it was define as a big GADT; but now It had been splitted
-- using data/type famillies, so one can extend them for particular prupose 
-- such creating an specific more-eficient particular combination (ex: withLength x)....


-- provisional laws:
------ trigger inn (st1 `merge` st2) == (trigger inn st1) `merge` (trigger inn st2) 



--------------------------------------------------------------------------------------
instance MachineCombinator (StepMachine input) where
    
    data State (StepMachine input) out      = StepSt Bool (Maybe out)
                                        
    type Input (StepMachine input)          = input
    
    trigger inn (StepMachine f) (StepSt b _)
             | b                            = Just$StepSt False (f inn)
             | otherwise                    = Nothing

    collect (StepSt _ out)                  = out 
    merge  (StepSt b out) (StepSt b' out')  = StepSt (b||b') (out <|> out')

data StepMachine input output = StepMachine (input -> Maybe output) -- TODO check spell 
---------------------------------------------------------------------------------------



-----------------------------------------------------------------------------------------
--instance ( StateMachine stm1 
--         , StateMachine stm2
--         , Input stm1 ~ Input stm2 
--         ) => StateMachine (Parallel stm1 out1 stm2 out2) where
--         data State (Parallel stm1 out1 stm2 out2 ) out = ParallelSt (TriState (State stm1 out1) (State stm2 out2))  
--         type Input (Parallel stm1 out1 stm2 out2 )     = Input stm1
         
--         trigger inn (Parallel f stm1 stm2) 
--                     (ParallelSt st) = case st of

--                                        Left  (st1,st2)   -> let (out1,st1') = trigger inn stm1 st1
--                                                                 (out2,st2') = trigger inn stm2 st2
                                                              
--                                                              in ( f          <$> toTriState out1 out2
--                                                                 , ParallelSt <$> toTriState st1' st2'
--                                                                 )

--                                        Right (Left  st1) -> let (out1,st1') = trigger inn stm1 st1
                                                              
--                                                              in ( f          <$> toTriState out1 Nothing
--                                                                 , ParallelSt <$> toTriState st1' Nothing
--                                                                 )

--                                        Right (Right st2) -> let (out2,st2') = trigger inn stm2 st2
                                                              
--                                                              in ( f          <$> toTriState Nothing out2
--                                                                 , ParallelSt <$> toTriState Nothing st2'
--                                                                 )


--         merge (ParallelSt a) (ParallelSt b) = ParallelSt $ case a of 
--                                                (Left (x,y))     -> left (merge x *** merge y) 
--                                                                  . right (left (merge x).right (merge y))
--                                                                  $ b 

--                                                (Right(Left  x)) -> right (left  (merge x)) $ b 
--                                                (Right(Right y)) -> right (right (merge y)) $ b

--data Parallel stm1 out1 stm2 out2 output = Parallel (TriState out1 out2 -> output) (stm1 out1) (stm2 out2)
-----------------------------------------------------------------------------------------




-----------------------------------------------------------------------------------------
--instance ( StateMachine stm1
--         , StateMachine stm2
--         , Input stm1 ~ Input stm2
--         , Monoid mid2
--         ) => StateMachine (Sequenced stm1 (mid1:: *) (mid2:: *) stm2) where

--         data State (Sequenced stm1 mid1 mid2 stm2) out = SequencedSt (Maybe (State stm1 (mid1 -> mid2))) 
--                                                                      (Maybe (mid1 -> mid2)) 
--                                                                      (State stm2 mid1)
                                                                      
                                                                      
  
--         type Input (Sequenced stm1 mid1 mid2 stm2)     = Input stm1


--         trigger inn  (Sequenced   stmA f stmB) 
--                      (SequencedSt stA  g  stB)         = let (mid,stmB') = trigger inn stmB stB

--                                                              (g' ,stmA') = maybe (Nothing,Nothing)
--                                                                                  (trigger inn stmA)
--                                                                                  stA 

--                                                           in ( f <$> (g <*> mid)
--                                                              , SequencedSt stmA' g <$> stmB'
--                                                              )

--         merge (SequencedSt stA   g   stB )
--               (SequencedSt stA'  g'  stB')              = SequencedSt (may merge stA stA')
--                                                                       (may mix g g')
--                                                                       (merge stB stB')
--                                    where
--                                      may::(a->a->a) -> Maybe a -> Maybe a -> Maybe a
--                                      may f a b = (uncurry f <$> ((,)<$>a<*>b)) <|> a <|> b

--                                      mix f g x =  f x <> g x

--data Sequenced  stm1 mid mid2 stm2 output = Sequenced (stm1 (mid -> mid2)) (mid2 -> output) (stm2 mid)
---- TODO, merge a new fresh st2 to st2' every time (trigger inn stm1 st1) produce an output!!!!
---- Otherwise this machine is pointless.... 

---- data Sequenced  stm1 mid mid2 stm2 output = Sequenced (stm1 (mid -> mid2)) (mid2 -> output) (State (stm2 mid)) (stm2 mid)

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
--data CompleteMachine input output where
--  Wrap:: (StateMachine stm) => stm output -> State stm output -> CompleteMachine input output



----TODO: Check the laws.....
----TODO...
--instance Functor     (CompleteMachine input) where
--instance Applicative (CompleteMachine input) where
--instance Alternative (CompleteMachine input) where
--instance Category    CompleteMachine         where
--instance Arrow       CompleteMachine         where
--instance ArrowChoice CompleteMachine         where
------------------------------------------------------------------------------------------



---- TODO: change refactor trigger such trigger =  (collects.trigger) &&& trigger' !!


-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
---- Usefull functions to manage CompletMachine only based on its class instances


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



------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------











