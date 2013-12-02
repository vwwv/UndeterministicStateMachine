{-# LANGUAGE GADTs 
           , FlexibleInstances
           , TypeFamilies
           , FlexibleContexts
           , DeriveFunctor
           , StandaloneDeriving
           #-}

module Internal.MachineComponent where



import Control.Applicative
import Control.Arrow
import Data.Monoid
import Data.Function(on)
import Control.Category
import Prelude hiding ((.),id) -- we use those from Category...


---------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------
-- Combinators to define an State machine:
-- The class should be visible, but not the instances...



class (Functor stm) => MachineCombinator (stm :: * -> * ) where

    type Input stm  :: * 
  --type Output stm :: * add the output type also!!
    trigger  :: Input stm   -> stm output    -> Maybe (stm output)
    collect  :: stm output  -> Maybe output  
    merge    :: stm out     -> stm out -> stm out
    debug    :: stm out     -> [String] -- TODO: delete....
--------------------------------------------------------------------------------------

-- Here there's a list of MachineCombinators, complete enough to define all the instances
-- StateMachine needs. Originally it was define as a big GADT; but now It had been splitted
-- using classes, so one can extend them for a particular prupose 
-- such creating an specific more-eficient particular combination (ex: withLength x)....






--------------------------------------------------------------------------------------
data StepMachine input output = Step Bool (Maybe output) (input -> Maybe output) -- TODO check spell

instance MachineCombinator (StepMachine input) where
                                        
    type Input (StepMachine input)          = input
    
    trigger inn (Step b _ f)
             | b                            = Just$Step False (f inn) f 
             | otherwise                    = Nothing

    collect (Step _ x  _)                   = x 
    merge   (Step b x f)  (Step b' x' _)    = Step (b||b') (x <|> x') f
 
    debug (Step b x f) = [ "-> Step:"
                         , "    " ++ show b 
                         , "    " ++ show (x *> pure () )
                         ]


deriving instance Functor (StepMachine inn) 
---------------------------------------------------------------------------------------


inc = map ("   "++)

-----------------------------------------------------------------------------------------
data ParallelMachine stm1 stm2 out = Parallel (TriState (stm1 out) (stm2 out))

instance ( MachineCombinator stm1 
         , MachineCombinator stm2
         , Input stm1 ~ Input stm2 
         ) => MachineCombinator (ParallelMachine stm1 stm2) where

         type Input (ParallelMachine stm1 stm2)     = Input stm1

         trigger inn (Parallel st)    =  case st of --TODO: refactor the code....
                                          Left  (st1,st2)   -> Parallel <$> toTriState (trigger inn st1)  (trigger inn st2)
                                          Right (Left  st1) -> Parallel <$> toTriState (trigger inn st1)  Nothing
                                          Right (Right st2) -> Parallel <$> toTriState Nothing            (trigger inn st2)

         collect (Parallel st)        = either (uncurry (<|>).(collect***collect))  (either collect collect)
                                      $ st


--TODO: Duplicate code...
         merge original@(Parallel a)
                         (Parallel b) = let  (stA , stB ) = fromTriState a
                                             (stA', stB') = fromTriState b

                                         in maybe original Parallel
                                                           (toTriState (stA <||> stA') (stB <||> stB') )

         debug (Parallel s) = let (a,b) = fromTriState s
                               in 
                                  [ "Parallel:"
                                  ] 
                                  ++ inc ["Option 1"]
                                  ++ (inc.inc.inc) (maybe ["-> DOWN"] debug a)
                                  ++ inc ["Option 2"] 
                                  ++ (inc.inc.inc) (maybe ["-> DOWN"] debug b)


instance (Functor stm1,Functor stm2 ) => Functor (ParallelMachine stm1 stm2) where
  fmap f (Parallel st) = Parallel $ ((fmap f ***fmap f)+++ fmap f +++ fmap f$st )

-----------------------------------------------------------------------------------------




-----------------------------------------------------------------------------------------
-- TODO: refactorizar...
data  SequencedMachine  stm1 mid stm2 out = Sequenced (stm2 mid) (TriState  (stm1 (mid -> out)) (stm2 out))

instance ( MachineCombinator stm1
         , MachineCombinator stm2
         , Input stm1 ~ Input stm2
         ) => MachineCombinator (SequencedMachine stm1 mid stm2) where

         type Input (SequencedMachine stm1 mid stm2)    = Input stm1

         --TODO, the code looks horrible...lets just refactor a bit......

         trigger inn (Sequenced cte st)  = let (st1 ,st2 ) = fromTriState st 
                                               (st1',st2') = ( trigger inn =<< st1 
                                                             , trigger inn =<< st2
                                                             )
                                               new         = (($cte).fmap) <$> (collect =<< st1')

                                            in Sequenced cte <$> toTriState st1' 
                                                                 (new <||> st2')

                
         collect (Sequenced _ st)  = collect=<< (snd.fromTriState) st  
                                         
         merge original@(Sequenced st1 st2 )
               (Sequenced st1'  st2') = let  (stA , stB ) = fromTriState st2
                                             (stA', stB') = fromTriState st2'

                                             a <||> b     = (merge<$>a<*>b)<|>a<|>b

                                         in maybe original (Sequenced (merge st1 st1')) 
                                                           (toTriState (stA <||> stA') (stB <||> stB') )




         debug (Sequenced c s) = let (a,b) = fromTriState s
                                 in 
                                      [ "Sequenced:"
                                      ] 
                                      ++ inc ["First"]
                                      ++ (inc.inc.inc) (maybe ["-> DOWN"] debug a)
                                      ++ inc ["Second"] 
                                      ++ (inc.inc.inc) (maybe ["-> DOWN"] debug b)
                                      ++ inc ["Memory"] 
                                      ++ inc ((inc.inc.debug) c)

instance (Functor stm1,Functor stm2) =>  Functor (SequencedMachine stm1 mid stm2) where
  fmap f (Sequenced cte st) = Sequenced cte   ((update***fmap f)+++ update +++ fmap f$st )
         where
          update = fmap (fmap f)

-----------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------
data LoopMachine stm mid  output = Loop (stm mid) (stm ([mid]->output))
instance ( MachineCombinator stm
         ) => MachineCombinator (LoopMachine stm mid) where 

         type Input (LoopMachine stm mid)     = Input stm

         trigger inn (Loop cte stm) = case trigger inn stm of 

                                        Just stm'
                                         | Just f <- collect stm'    -> Just $Loop cte $ merge ( ((.)f.(:)) <$> cte) stm'

                                         | otherwise                 -> Just $Loop cte stm'

                                        Nothing                      -> Nothing


         collect (Loop _ stm)       = ($[]) <$> collect stm  

         merge  (Loop cte  stm )
                (Loop cte' stm')    = Loop (merge cte cte') (merge stm stm') 

         debug (Loop c s) = [ "Loop:"
                            ] 
                            ++ inc ["Inside"] 
                            ++ (inc.inc.inc) (debug s)
                            ++ inc ["Memory"] 
                            ++ inc ((inc.inc.debug) c)

instance (Functor stm) => Functor (LoopMachine stm mid) where
  fmap f (Loop cte stm) = Loop cte (fmap f <$> stm) 

------------------------------------------------------------------------------------------

{-
 TODO documment...
-}
----------------------------------------------------------------------------------------------
data InjectedMachine stm1 mid stm2 out = Injected (stm1 mid) (Maybe out) (stm1 (stm2 out,mid))
----------------------------------------------------------------------------------------------
instance ( MachineCombinator stm1
         , MachineCombinator stm2
         ) => MachineCombinator (InjectedMachine stm1 mid stm2) where 

         type Input (InjectedMachine stm1 mid stm2)     = Input stm1

         trigger inn (Injected cte out stm) = undefined

         collect (Injected cte out stm)     =   undefined

         merge  (Injected cte out stm)
                (Injected cte' out' stm')      = undefined

         debug (Injected cte out stm)       = undefined

instance (Functor stm1,Functor stm2) => Functor (InjectedMachine stm1 mid stm2) where
  fmap f (Injected cte out stm) = undefined

-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-- TODO only used to define first, so we can check if it is working properly....
data SimultaneousMachine stm1 mid1 stm2 mid2 out = Simultaneous (stm1 mid1) (stm2 mid2) (mid1->mid2->out)


instance ( MachineCombinator stm1
         , MachineCombinator stm2
         , Input stm1 ~ Input stm2
         ) => MachineCombinator (SimultaneousMachine stm1 mid1 stm2 mid2) where 

         type Input (SimultaneousMachine stm1 mid1 stm2 mid2)     = Input stm1

         trigger inn (Simultaneous stm1 stm2 f) = undefined

         collect (Simultaneous stm1 stm2 f)     =   undefined

         merge  (Simultaneous stm1  stm2  f )
                (Simultaneous stm1' stm2' f')      = undefined

         debug (Simultaneous stm1 stm2 f)       = undefined

instance (Functor stm1,Functor stm2) => Functor (SimultaneousMachine stm1 mid1 stm2 mid2) where
  fmap f (Simultaneous stm1 stm2 g) = undefined

-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-- TODO only used to define first, so we can check if it is working properly....
data ForkedMachine stm1 mid1 stm2 mid2 out = Forked (stm1 mid1) (stm2 mid2) (mid1->mid2->out)


instance ( MachineCombinator stm1
         , MachineCombinator stm2
         , Input stm1 ~ Input stm2
         ) => MachineCombinator (ForkedMachine stm1 mid1 stm2 mid2) where 

         type Input (ForkedMachine stm1 mid1 stm2 mid2)     = Input stm1

         trigger inn (Forked stm1 stm2 f) = undefined

         collect (Forked stm1 stm2 f)     =   undefined

         merge  (Forked stm1  stm2  f )
                (Forked stm1' stm2' f')      = undefined

         debug (Forked stm1 stm2 f)       = undefined

instance (Functor stm1,Functor stm2) => Functor (ForkedMachine stm1 mid1 stm2 mid2) where
  fmap f (Forked stm1 stm2 g) = undefined
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

--TODO: refactor the common code...
-- TODO: let it full of comments!!

type TriState a b = Either (a,b) (Either a b) 

toTriState:: Maybe a -> Maybe b -> Maybe (TriState a b)
toTriState Nothing  Nothing  = Nothing
toTriState (Just a) Nothing  = Just $ Right (Left a)
toTriState Nothing  (Just b) = Just $ Right (Right b)
toTriState (Just a) (Just b) = Just $ Left (a,b)


fromTriState::TriState a b -> (Maybe a,Maybe b)
fromTriState st = case st of 
                    (Left (a,b))      -> (Just a , Just b )
                    (Right (Left a )) -> (Just a , Nothing)
                    (Right (Right b)) -> (Nothing, Just b )


mergeTriState::( MachineCombinator stm1
               , MachineCombinator stm2
               ) => TriState (stm1 a) (stm2 b) -> TriState (stm1 a) (stm2 b) -> TriState (stm1 a) (stm2 b) 
mergeTriState = undefined 




a <||> b     = (merge<$>a<*>b)<|>a<|>b



------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------












































