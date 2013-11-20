{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import StateMachine
import Data.Char
import Data.List
import System.Locale
import Data.Time.Clock
import Data.Time
import System.Locale
import Safe
import ReadArgs hiding (parse)

{-
  Toy Example: 
     Have you ever take a look to a subtitle file? They are written in a very human readible way, so you can easily
     edit and modify them. So let say you are watching a movie and find out that because some reason, the first minutes
     of the film are cropped; your German is not good enough by its own so you need subtitles, but the movie player can
     only manage a subtitle delay of +/-30 seg, and it seems you'll need at least several minutes......when you check
     the subtitle file you find out that it has more the 5000 lines to be modify, you just have to delete the first entries
     and then update the time for each of the every single remaining 4990 entries, man, this is not a human work, lets 
     script it up using out library! :)
-}

-------------------------------------------------------------------------------------------------------------------------
-- 1)  Lets start definning the data we are dealing with...pretty complex XD

data Entry = Entry { nSeq     :: Int         -- The sequence number....
                   , starts   :: DiffTime    -- Time when it starts with respect to the beginning of the movie
                   , ends     :: DiffTime    -- The same for the ending....
                   , contents :: String      -- The piece of subtitle itself 
                   } deriving Show

--------------------------------------------------------------------------------------------------------------------------
-- 2)  This is what we want to do, a.k.a., the actual work:

cropped::DiffTime -> [Entry] -> [Entry]
cropped t subtitles = zipWith ($) [ (\i -> Entry i (t0-t) (t1-t)  c) 
                                  |        Entry _ t0     t1      c <- subtitles, t0 >  t
                                  ] [1..] 

--------------------------------------------------------------------------------------------------------------------------
-- 3)  Now the tedious part, serialization/deserialization of the data in a concrete format :(


-- 3) a) An example of some entries:
{-

        539
        00:52:35,520 --> 00:52:39,513
        im Schulalltag, da...
        kann das manchmal problematisch werden.

        540
        00:52:40,320 --> 00:52:43,312
        Da habt ihr sie einfach abserviert.

        541
        00:52:48,520 --> 00:52:52,115
        Sie sind ihr trotzdem was schuldig.

-}


-- 3) b) Defining the pretty printer:
prettyPrint (Entry i t0 t1 content) = unlines [ show i
                                              , show' t0 ++ " --> " ++ show' t1
                                              , content                                            
                                              ]
    where
      show' t = take 12 $ formatTime defaultTimeLocale "%H:%M:%S,%q" (timeToTimeOfDay t)


-- 3) c) Declarative defining a parser using out library :

parser  = separatedBy newline
        $ format <$> field                         <* newline
                 <*> field  <* separator <*> field <* newline
                 <*> content                       <* newline
      where
        separator             = some $ elementSuch (`elem`[' ','-','>'])
        content               = some $ some (elementSuch (/='\n'))  <* element '\n'
        newline               = some $ element '\n'

        format i t0 t1 str    = Entry i t0 t1 (intercalate "\n" str) 

------------------------------------------------------------------------------------------------------------------------
-- 4) And now everything it's done! :) ....well non, we still need to define the program!!
-- This program just read its input from console an execute it....as right today the proper
-- parsing function with error diagnostic (*) its not yet inplemented, in case of error, we
-- just apologise and exist...

-- 4) a) Reading the arguments, we need again to define a parser...

                       

arguments = let format _ a _ b _ c _ = (a,b,c) :: (String,DiffTime,String)  
             in format <$> optional space
                       <*> some (elementSuch isAlphaNum) <*> space 
                       <*> field                         <*> space 
                       <*> some (elementSuch isAlphaNum) <*> optional space
                        where 
                            space = some$elementSuch isSpace


main = do arg <-  parse arguments <$> readArgs
          case arg of
            Just (inn,time,out) -> (parse parser <$> readFile inn) 
                                    >>= maybe ( putStrLn $ unlines
                                               [ "Sorry, a wild error appeared while parsing the file,"
                                               , "maybe you can try manually editing it...."
                                               ]
                                              )
                                              (writeFile out.intercalate "\n".map prettyPrint.cropped time)  >> putStrLn "Done :-)"


            _                   -> putStrLn$unlines 
                                 [ "The argument syntax was incorrect or there was no argument\n"
                                 , "a correct example could be:\n"
                                 , "\tcropper goodByLennin 03:55:19 goodByCutted" -- (**)
                                 ]



 -- (*) as an example of that kind of function would be
 -- (**) We supossed the executable its called "cropper". 
 -- recordingError::StateMachine a b -> (a -> err -> err) -> StateMachine (Either b err) b  















