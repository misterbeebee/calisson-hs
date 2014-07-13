module Data.Entropy where

import qualified Debug.Trace as T

data Entropy bit source = Entropy source (source -> bit) (source -> source)

instance Show bit => Show (Entropy bit source) where
    show e@(Entropy ev curr _) = "(Entropy " ++ show (currEntropyBit e)  ++ " _ _)"

currEntropyBit (Entropy ev curr _) = curr ev
nextEntropy (Entropy ev curr next) = Entropy (next ev) curr next

maxTries = 50

-- Feeds a stream of "random/arbitrary" data to a function (that can fail)
-- retries function (with new entropic value) if it fails
-- todo use state/random/Gen monad
-- fixme stupid name
-- WARNING: can loop forever!
-- Show is just for debugging
withEntropyAndRetry :: Show bit => (bit -> a -> Maybe a) -> (a, Entropy bit e) -> (a, Entropy bit e)
withEntropyAndRetry f (a,e) =
    -- T.trace ("withEntropyAndRetry: ") $
    keepTrying maxTries e f a
    where
        keepTrying 0 e f a =
            T.trace ("keepTrying retries expired; e: " ++ show e)
            (a, nextEntropy e)
        keepTrying triesLeft e f a =
          -- T.trace ("keepTrying: " ++ show e) $
          let bit = (currEntropyBit e) in
          case f bit a of
            Nothing ->
              -- should never happen in Calissons, now that we are maintaing hexagon set
              T.trace ("attempt " ++ show (maxTries - triesLeft) ++ " with entropy: " ++ show bit)
              keepTrying  (pred triesLeft) (nextEntropy e) f a
            Just fresult -> 
              -- T.trace ("attempt " ++ show (maxTries - triesLeft) ++ " succeeded" )
              (fresult, nextEntropy e)
