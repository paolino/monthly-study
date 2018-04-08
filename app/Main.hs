{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

import           April
import           Text.Read

---------------------------------------------------------------------------------
-- free applicative example , consuming words of a phrase
---------------------------------------------------------------------------------


-- grammar for consuming tokens
data Consumer b a = Elem (b -> Maybe a) | BiElem (b -> b -> Maybe a) | ListElem Int ([b] -> Maybe a) deriving Functor

-- ListElem ?


exactly 0 xs       = Just (xs, [])
exactly n []       = Nothing
exactly n (x : xs) = fmap (x :) <$> exactly (n - 1) xs

-- get the string as it is
opt :: LiftAR c => FreeAR c (Consumer b) b
opt = liftAR $ Elem Just

skip :: LiftAR c => FreeAR c (Consumer b) ()
skip = liftAR $ Elem (fmap (const ()) . Just)

-- get a tuple of strings
tuple :: LiftAR c => FreeAR c (Consumer b) (b, b)
tuple = liftAR $ BiElem $ \x y -> Just (x, y)

list :: LiftAR c => Int -> FreeAR c (Consumer b) [b]
list n = liftAR $ ListElem n Just
-- b ~ String
-- parse a word in to any 'a'
optS :: LiftAR c => Read a => FreeAR c (Consumer String) a
optS = liftAR $ Elem readMaybe

-- get a tuple of values
tupleS :: LiftAR c => Read a => Read b => FreeAR c (Consumer String) (a, b)
tupleS = liftAR $ BiElem $ \x y -> (,) <$> readMaybe x <*> readMaybe y

listP :: LiftAR c => Read a => Int -> FreeAR c (Consumer String) [a]
listP n = liftAR $ ListElem n $ sequence . map readMaybe




consumeCap = go
  where
    go :: FreeAR Cap (Consumer b) a -> [b] -> Maybe a
    go (One x                ) _            = Just x
    go (More (Elem   r    ) p) (x     : xs) = r x <*> go p xs
    go (More (BiElem r    ) p) (x : y : xs) = r x y <*> go p xs
    go (More (ListElem n r) p) xs           = do
        (xs', ys) <- exactly n xs
        r ys <*> go p xs'
    go _ _ = Nothing


consumeTwan :: FreeAR Twan (Consumer b) a -> [b] -> Maybe a
consumeTwan = go
  where
    go :: FreeAR Twan (Consumer b) a -> [b] -> Maybe a
    go (One x                ) _            = Just x
    go (More p (Elem   r    )) (x     : xs) = go p xs <*> r x
    go (More p (BiElem r    )) (x : y : xs) = go p xs <*> r x y
    go (More p (ListElem n r)) xs           = do
        (xs', ys) <- exactly n xs
        go p xs' <*> r ys
    go _ _ = Nothing

consumeOrian :: FreeAR Orian (Consumer b) a -> [b] -> Maybe a
consumeOrian = go
  where
    go :: FreeAR Orian (Consumer b) a -> [b] -> Maybe a
    go (One x                ) _            = Just x
    go (More p (Elem   r    )) (x     : xs) = go p xs <*> r x
    go (More p (BiElem r    )) (x : y : xs) = go p xs <*> r y x
    go (More p (ListElem n r)) xs           = do
        (xs', ys) <- exactly n xs
        go p xs' <*> r (reverse ys)
    go _ _ = Nothing



-- example , let's get an A from a phrase
type A = (Int,(String,String),Char,(),(),(Double,Char), [Int],String)


getA
    :: LiftAR c
    => Applicative (FreeAR c (Consumer String))
    => Functor (R c (Consumer String))
    => FreeAR c (Consumer String) A
getA =
    (,,,,,,,)
        <$> optS
        <*> tuple
        <*> optS
        <*> skip
        <*> skip
        <*> tupleS
        <*> listP 3
        <*> opt


anA = words "14 category theory 'c' ignore me 4.5 'w' 1 2 3 tazzo"

main = do
    print $ (consumeCap getA anA :: Maybe A)
    print $ (consumeTwan getA anA :: Maybe A)
    print $ (consumeOrian getA (reverse anA) :: Maybe A)
