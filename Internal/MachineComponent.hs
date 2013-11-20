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


-- provisional laws:
------ trigger inn (st1 `merge` st2) == (trigger inn st1) `merge` (trigger inn st2) 
------ collect st1 <|> collect st2   == collect (st1 `merge` st2)
------ todo, faltan leyes...


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
-- using data/type famillies, so one can extend them for particular prupose 
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
                                             a <||> b     = (merge<$>a<*>b)<|>a<|>b

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
                                               newAndSt2'  = merge <$> new <*> st2'

                                            in Sequenced cte <$> toTriState st1' 
                                                                 (newAndSt2' <|> new <|> st2')

                
         collect (Sequenced _ st)  = case st of 
                                      Left (_,st2)       -> collect st2
                                      Right (Right st2)  -> collect st2
                                      _                  -> Nothing
                                         

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


---- change names .... loop -> loopMachine

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


-- todo: add value cache for all the which may need, and avoid unecesary merging

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
--instance Simultaneos machine:




------------------------------------------------------------------------------------------



-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
--TODO: recheck all the information remains at the bottom...




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


------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------

















































