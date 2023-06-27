module Main where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-------------------------------------------------------------------------------
-- Main -----------------------------------------------------------------------
-------------------------------------------------------------------------------

main :: Effect Unit
main = do
    let aliceID = 1
        bobID = 2
        studentData =
            { names: Map.fromFoldable [Tuple aliceID "Alice", Tuple bobID "Bob"]
            , ages: Map.fromFoldable [Tuple aliceID 30]
            , heights: Map.fromFoldable [Tuple aliceID 165, Tuple bobID 183]
            }

    -- these all print out the same thing: (Just "Alice 30 165")
    log (show (ex1 aliceID studentData))
    log (show (ex2 aliceID studentData))
    log (show (ex3 aliceID studentData))
    log (show (ex4 aliceID studentData))
    log (show (ex5 aliceID studentData))
    log (show (ex6 aliceID studentData))

    -- in the `Maybe` implementation of `bind`, if there's ever a `Nothing`,
    -- then the whole thing becomes `Nothing`. It's like an early-exit.
    -- This prints out "Nothing" because there's no age record for Bob.
    log (show (ex1 bobID studentData))

-------------------------------------------------------------------------------
-- Types and utility functions ------------------------------------------------
-------------------------------------------------------------------------------

type StudentData =
    { names :: Map ID String
    , ages :: Map ID Int
    , heights :: Map ID Int
    }

type ID = Int

getName :: ID -> StudentData -> Maybe String
getName id sd = Map.lookup id sd.names

getAge :: ID -> StudentData -> Maybe Int
getAge id sd = Map.lookup id sd.ages

getHeight :: ID -> StudentData -> Maybe Int
getHeight id sd = Map.lookup id sd.heights

-------------------------------------------------------------------------------
-- All of the examples below are equivalent -----------------------------------
-------------------------------------------------------------------------------

-- The most naive approach, explicitly threading each part together. Messy and repetitive!
ex1 :: ID -> StudentData -> Maybe String
ex1 id sd = case getName id sd of
    Nothing -> Nothing
    Just name -> case getAge id sd of
        Nothing -> Nothing
        Just age -> case getHeight id sd of
            Nothing -> Nothing
            Just height -> Just (name <> " " <> show age <> " " <> show height)

-- Now we're using `bind` (>>=) instead of explicitly threading.
-- Definition: bind :: forall a b. m a -> (a -> m b) -> m b
-- This is better, but still verbose and hard to read.
ex2 :: ID -> StudentData -> Maybe String
ex2 id sd = getName id sd >>= (\name -> getAge id sd >>= (\age -> getHeight id sd >>= (\height -> Just (name <> " " <> show age <> " " <> show height))))

-- Same as ex2 but we've just put each part onto a new line. This is better! Still noisy.
ex3 :: ID -> StudentData -> Maybe String
ex3 id sd =
    getName id sd >>= (\name ->
        getAge id sd >>= (\age ->
            getHeight id sd >>= (\height ->
                Just (name <> " " <> show age <> " " <> show height))))

-- Turns out the parens are un-necessary!
ex4 :: ID -> StudentData -> Maybe String
ex4 id sd =
    getName id sd >>= \name ->
        getAge id sd >>= \age ->
            getHeight id sd >>= \height ->
                Just (name <> " " <> show age <> " " <> show height)

-- Also the indentation can all line up and it's still fine!
ex5 :: ID -> StudentData -> Maybe String
ex5 id sd =
    getName id sd >>= \name ->
    getAge id sd >>= \age ->
    getHeight id sd >>= \height ->
    Just (name <> " " <> show age <> " " <> show height)

-- Do-notation is just the same as ex5, just each line is flipped
-- around to look more like a normal variable assignment.
-- Do-notation gets parsed and transformed (de-sugared) into the ex5 form.
-- It's nothing more than syntactic sugar for functions joined together
-- with the `bind` function!
-- Note: all these examples are using the `Maybe` implementation of the `bind`
-- function. This do-notation would work equivalently well with any other
-- type that implements `bind`. Exercise: Try implement something similar
-- to these examples using `Either`.
ex6 :: ID -> StudentData -> Maybe String
ex6 id sd = do
    name <- getName id sd
    age <- getAge id sd
    height <- getHeight id sd
    Just (name <> " " <> show age <> " " <> show height)

-- The definitions of relevant typeclasses:

-- class Apply m <= Bind m where
--   bind :: forall a b. m a -> (a -> m b) -> m b
-- infixl 1 bind as >>=

-- class Functor f <= Apply f where
--   apply :: forall a b. f (a -> b) -> f a -> f b
-- infixl 4 apply as <*>

-- class Functor f where
--   map :: forall a b. (a -> b) -> f a -> f b
-- infixl 4 map as <$>
