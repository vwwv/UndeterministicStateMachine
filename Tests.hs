{-# LANGUAGE GADTs 
           , FlexibleInstances
           , TypeFamilies
           , FlexibleContexts
           , DeriveFunctor
           #-}

module Test where



import Control.Applicative
import Control.Arrow
import Data.Monoid
import Data.Function(on)
--import Control.Category
--import Prelude hiding ((.),id) -- we use those from Category...
import StateMachine
import Internal.MachineComponent
import Data.Char

{-
  TODO...
-}



{-
  To check laws:



    As Applicative:

        identity
            pure id <*> v = v 
        composition
            pure (.) <*> u <*> v <*> w = u <*> (v <*> w) 
        homomorphism
            pure f <*> pure x = pure (f x) 
        interchange
            u <*> pure y = pure ($ y) <*> u 

    As Alternative:

        zero element left:
            empty <|> x = x

        zero element right:
            x <|>  empty = x

        associativeness:
            x <|> ( y <|> z) = ( x <|>  y ) <|> z 


    As category:
        
        identity:
            f.id == id.f == f

        associativity:
            (h.f).g = h.(f.g)   

    As Arrow:

            arr id = id

            arr (f >>> g) = arr f >>> arr g

            first (arr f) = arr (first f)

            first (f >>> g) = first f >>> first g

            first f >>> arr fst = arr fst >>> f

            first f >>> arr (id *** g) = arr (id *** g) >>> first f

            first (first f) >>> arr assoc = arr assoc >>> first f
        where
            assoc ((a,b),c) = (a,(b,c))


    As ArrowChoice:

            left (arr f) = arr (left f)

            left (f >>> g) = left f >>> left g

            f >>> arr Left = arr Left >>> left f

            left f >>> arr (id +++ g) = arr (id +++ g) >>> left f

            left (left f) >>> arr assocsum = arr assocsum >>> left f

        where

            assocsum (Left (Left x))  = Left x
            assocsum (Left (Right y)) = Right (Left y)
            assocsum (Right z)        = Right (Right z)

-}







debuging::StateMachine Char String -> String -> String
debuging (Wrap parse) [] = unlines 
                  [ unlines $ debug parse  
                  , "#################################"
                  , "####### Partially got: " ++ show (collect parse)            
                  , "<---------------------------------------> End of the stream: <--------------------------------->"
                  , "#################################################################################################"
                  , ""
                  , ""
                  ] 
                       

debuging (Wrap parse) (c:cs) = unlines 
                  [ unlines $ debug parse  
                  , "#################################"
                  , "####### Partially got: " ++ show (collect parse)            
                  , "#################################################################################################"
                  , "Feeding with: "++ show c
                  , ""
                  , case (trigger c parse) of 
                            (Nothing) -> "<--------------- Suddenly end of parseser :( XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX>"
                            (Just parse')  -> debuging (Wrap parse') cs
                  ] 


--p = Sequenced (Step True Nothing (const (Just "o"))  )  (Left (Left (Step True Nothing  (const $Just ('h':)) )))



