module Main where

import Prelude

import Data.Either (Either(..))
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
        catID = 3
        studentData =
            { names: Map.fromFoldable
                [ Tuple aliceID "Alice"
                , Tuple bobID "Bob"
                , Tuple catID "Cat"
                ]
            , ages: Map.fromFoldable
                [ Tuple aliceID 30
                , Tuple catID 25
                ]
            , heights: Map.fromFoldable
                [ Tuple aliceID 165
                , Tuple bobID 183
                ]
            }

    log (show (getStudentInfo1 aliceID studentData)) -- Just "Alice 30 165"
    log (show (getStudentInfo2 aliceID studentData)) -- Just "Alice 30 165"
    log (show (getStudentInfo3 aliceID studentData)) -- Just "Alice 30 165"
    log (show (getStudentInfo4 aliceID studentData)) -- Just "Alice 30 165"
    log (show (getStudentInfo5 aliceID studentData)) -- Just "Alice 30 165"
    log (show (getStudentInfo6 aliceID studentData)) -- Just "Alice 30 165"
    log (show (getStudentInfo7 aliceID studentData)) -- Right "Alice 30 165"
    log (show (getStudentInfo8 aliceID studentData)) -- Right "Alice 30 165"
    log (show (getStudentInfo9 aliceID studentData)) -- Just "Alice 30 165"
    log (show (getStudentInfo10 aliceID studentData)) -- Right "Alice 30 165"
    log (show (getStudentInfo11 aliceID studentData)) -- Just "Alice 30 165"
    log (show (getStudentInfo12 aliceID studentData)) -- Right "Alice 30 165"
    log (show (getStudentInfo13 aliceID studentData)) -- Just "Alice 30 165"
    log (show (getStudentInfo14 aliceID studentData)) -- Right "Alice 30 165"

    -- in the `Maybe` implementation of `bind`, if there's ever a `Nothing`,
    -- then the whole thing becomes `Nothing`. It's like an early-exit.
    -- This prints out "Nothing" because there's no age record for Bob.
    log (show (getStudentInfo1 bobID studentData))

    -- Similarly, in the `Either` implementation of `bind`, if there's ever a
    -- `Left`, then the whole thing becomes that `Left`. It's also like an
    -- early-exit. These print out various error messages because they fail
    -- at different stages.
    log (show (getStudentInfo7 999 studentData)) -- Left "Name not found"
    log (show (getStudentInfo7 bobID studentData)) -- Left "Age not found"
    log (show (getStudentInfo7 catID studentData)) -- Left "Height not found"

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
-- Keep reading! --------------------------------------------------------------
-------------------------------------------------------------------------------

-- `getStudentInfo1` looks like a function you might see in a real codebase.
-- Given an ID, it returns the name, age, and height of the associated student.
-- For now, let's ignore the fact that this function isn't THAT useful; it's
-- only for the purpose of building towards an intuition for how do-notation works!
-- Notice how this function does the job, but it's a bit messy and verbose:
-- The line `Nothing -> Nothing` is repeated three times, and if you wanted to add
-- more to this function, the indentation level would get out of hand pretty quick.
getStudentInfo1 :: ID -> StudentData -> Maybe String
getStudentInfo1 id sd = case getName id sd of
    Nothing -> Nothing
    Just name -> case getAge id sd of
        Nothing -> Nothing
        Just age -> case getHeight id sd of
            Nothing -> Nothing
            Just height -> Just (name <> " " <> show age <> " " <> show height)

-- Notice how the structure of `getStudentInfo1` is repetitive: if we have `Nothing`
-- we return `Nothing`, otherwise do something else with the value inside the `Just`.
-- This pattern repeats three times. We can extract this pattern out by writing a
-- helper function that handles this piping logic:
pipeMaybe :: forall a b. Maybe a -> (a -> Maybe b) -> Maybe b
pipeMaybe Nothing _ = Nothing
pipeMaybe (Just v) f = f v

-- Now we can re-write `getStudentInfo1` using `pipeMaybe`, giving `getStudentInfo2`
-- below. You may think it looks worse, but we'll gradually improve the syntax over
-- the next few examples. The important bit is that the explicit piping logic is gone.
getStudentInfo2 :: ID -> StudentData -> Maybe String
getStudentInfo2 id sd =
    pipeMaybe (getName id sd) (\name ->
        pipeMaybe (getAge id sd) (\age ->
            pipeMaybe (getHeight id sd) (\height ->
                Just (name <> " " <> show age <> " " <> show height))))

-- This is exactly the same function, except just using infix notation now:
getStudentInfo3 :: ID -> StudentData -> Maybe String
getStudentInfo3 id sd =
    (getName id sd) `pipeMaybe` (\name ->
        (getAge id sd) `pipeMaybe` (\age ->
            (getHeight id sd) `pipeMaybe` (\height ->
                Just (name <> " " <> show age <> " " <> show height))))

-- Now that we're using infix notation, we can actually get rid of the parens!
-- The reason why this is possible is outside the scope of this tutorial, but
-- if you're interested see [here](https://github.com/purescript/documentation/blob/master/language/Syntax.md#functions-as-operators).
getStudentInfo4 :: ID -> StudentData -> Maybe String
getStudentInfo4 id sd =
    getName id sd `pipeMaybe` \name ->
        getAge id sd `pipeMaybe` \age ->
            getHeight id sd `pipeMaybe` \height ->
                Just (name <> " " <> show age <> " " <> show height)

-- Indentation
getStudentInfo5 :: ID -> StudentData -> Maybe String
getStudentInfo5 id sd =
    getName id sd `pipeMaybe` \name ->
    getAge id sd `pipeMaybe` \age ->
    getHeight id sd `pipeMaybe` \height ->
    Just (name <> " " <> show age <> " " <> show height)

infixl 1 pipeMaybe as |>

-- Using an infix operator
getStudentInfo6 :: ID -> StudentData -> Maybe String
getStudentInfo6 id sd =
    getName id sd |> \name ->
    getAge id sd |> \age ->
    getHeight id sd |> \height ->
    Just (name <> " " <> show age <> " " <> show height)

-- Pretty good! The function still behaves as before (ie. it early-exits with `Nothing`
-- if there's a `Nothing` at any stage), but it's easier to read, and it's easily
-- extensible (if we needed to add another stage, just add it on a new line).

-- But it could still be better; currently if something goes wrong,
-- there's no way of telling WHERE it went wrong because only a `Nothing` is returned.
-- The standard approach for returning information in an error case is to use `Either`
-- instead of `Maybe`.
-- Let's introduce some more types and basic functions (same as the ones above, but
-- with an "E" suffix for "Either". Naming stuff is hard):

type Error = String

getNameE :: ID -> StudentData -> Either Error String
getNameE id sd = case Map.lookup id sd.names of
    Nothing -> Left "Name not found"
    Just name -> Right name

getAgeE :: ID -> StudentData -> Either Error Int
getAgeE id sd = case Map.lookup id sd.ages of
    Nothing -> Left "Age not found"
    Just name -> Right name

getHeightE :: ID -> StudentData -> Either Error Int
getHeightE id sd = case Map.lookup id sd.heights of
    Nothing -> Left "Height not found"
    Just name -> Right name

-- Now for the function implementation. Let's start with the naive approach:
getStudentInfo7 :: ID -> StudentData -> Either Error String
getStudentInfo7 id sd = case getNameE id sd of
    Left e -> Left e
    Right name -> case getAgeE id sd of
        Left e -> Left e
        Right age -> case getHeightE id sd of
            Left e -> Left e
            Right height -> Right (name <> " " <> show age <> " " <> show height)

-- Now we can write a pipe function again like we did with Maybe, but this time for Either:
pipeEither :: forall e a b. Either e a -> (a -> Either e b) -> Either e b
pipeEither (Left e) _ = Left e
pipeEither (Right v) f = f v

infixl 1 pipeEither as |>>

-- Now using pipeEither (and applying the newlines, parens, indentation, and infix
-- syntax enhancements):
getStudentInfo8 :: ID -> StudentData -> Either Error String
getStudentInfo8 id sd =
    getNameE id sd |>> \name ->
    getAgeE id sd |>> \age ->
    getHeightE id sd |>> \height ->
    Right (name <> " " <> show age <> " " <> show height)

-- This is better, but you can see how this would get annoying and repetitive quickly.
-- Each time we want to pipe functions together using a new container type, you have
-- to define a new pipe function, and come up with a new symbol combination for the
-- infix operator. If only there was a way to do this piping logic generically for
-- any type...

-- This can be done using type classes! These are a lot like interfaces from Go or other
-- imperative languages.

-- We define a type class called `Pipe`, essentially a contract which requires a
-- single function called `pipe`:
class Pipe m where
    pipe :: forall a b. m a -> (a -> m b) -> m b

-- Notice how the `pipe` function's type signature is similar to the signature of
-- `pipeMaybe` and `pipeEither`:
--
-- pipe       :: forall a b.   m a        -> (a -> m b)        -> m b
-- pipeMaybe  :: forall a b.   Maybe a    -> (a -> Maybe b)    -> Maybe b
-- pipeEither :: forall a b e. Either e a -> (a -> Either e b) -> Either e b
--
-- The only difference is that the concrete types `Maybe` and `Either` have been stripped
-- out, and replaced with a generic type variable `m`. Now the specific behaviour of the
-- `pipe` function is determined by each type's implementation of the `pipe` function:
instance Pipe Maybe where
    pipe Nothing _ = Nothing
    pipe (Just v) f = f v

instance Pipe (Either a) where
    pipe (Left v) _ = (Left v)
    pipe (Right v) f = f v

-- Now we can re-write the implementations using `pipe` instead of the specific pipe
-- functions, and it will work for both `Maybe` and `Either`, using the correct implementation
-- of the `pipe` function as defined in the instances above!

infixl 1 pipe as >>==

getStudentInfo9 :: ID -> StudentData -> Maybe String
getStudentInfo9 id sd =
    getName id sd >>== \name ->
    getAge id sd >>== \age ->
    getHeight id sd >>== \height ->
    Just (name <> " " <> show age <> " " <> show height)

getStudentInfo10 :: ID -> StudentData -> Either Error String
getStudentInfo10 id sd =
    getNameE id sd >>== \name ->
    getAgeE id sd >>== \age ->
    getHeightE id sd >>== \height ->
    Right (name <> " " <> show age <> " " <> show height)

-- It turns out that there's already a function that does the job of `pipe`.
-- It's called `bind`, and it looks like this: `>>=`.
-- Definition: bind :: forall a b. m a -> (a -> m b) -> m b
-- The two examples below work because `Maybe` and `Either` already have a `bind`
-- instance defined!
--
-- https://github.com/purescript/purescript-maybe/blob/v6.0.0/src/Data/Maybe.purs#L125
-- instance bindMaybe :: Bind Maybe where
--   bind (Just x) k = k x
--   bind Nothing  _ = Nothing
--
-- https://github.com/purescript/purescript-either/blob/v3.0.0/src/Data/Either.purs#L125
-- instance bindEither :: Bind (Either e) where
--   bind = either (\e _ -> Left e) (\a f -> f a)
--
-- These next two implementations are pretty much exactly the same as the implementations for
-- `pipe` above, which makes sense.
getStudentInfo11 :: ID -> StudentData -> Maybe String
getStudentInfo11 id sd =
    getName id sd >>= \name ->
    getAge id sd >>= \age ->
    getHeight id sd >>= \height ->
    Just (name <> " " <> show age <> " " <> show height)

getStudentInfo12 :: ID -> StudentData -> Either Error String
getStudentInfo12 id sd =
    getNameE id sd >>= \name ->
    getAgeE id sd >>= \age ->
    getHeightE id sd >>= \height ->
    Right (name <> " " <> show age <> " " <> show height)

-- Do-notation is just the same as these last two examples, just that each line
-- is flipped around to look more like a normal variable assignment.
-- Do-notation gets parsed and transformed (de-sugared) into the form above.
-- It's nothing more than syntactic sugar for functions joined together
-- with the `bind` function! `getStudentInfo13` and getStudentInfo14` use
-- do-notation:
getStudentInfo13 :: ID -> StudentData -> Maybe String
getStudentInfo13 id sd = do
    name <- getName id sd
    age <- getAge id sd
    height <- getHeight id sd
    Just (name <> " " <> show age <> " " <> show height)

getStudentInfo14 :: ID -> StudentData -> Either Error String
getStudentInfo14 id sd = do
    name <- getNameE id sd
    age <- getAgeE id sd
    height <- getHeightE id sd
    Right (name <> " " <> show age <> " " <> show height)
