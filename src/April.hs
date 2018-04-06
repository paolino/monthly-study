{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Lens
import           Control.Lens.TH
import           Text.Read

data Box f a = Box (f (Box f a)) | Core a

deriving instance (Show a, Show (f (Box f a))) => Show (Box f a)

instance Functor f => Functor (Box f) where
    fmap f (Core x) = Core $ f x
    fmap f (Box g)  = Box $ fmap f <$> g

instance Functor f => Applicative (Box f) where
    pure  = Core
    Core h <*> g = h <$> g
    Box h <*> g = Box $ (<*> g) <$> h

instance Functor f => Monad (Box f) where
    f >>= g = join $ fmap g f
        where
        join (Core x) = x
        join (Box f)  = Box $ join <$> f

data Par (f :: * -> *) a where
    One :: a  -> Par f a
    More ::  f (b -> a) -> Par f b -> Par f a

instance Functor f => Functor (Par f) where
    fmap f (One x)    = One $ f x
    fmap f (More c p) = More (fmap (f .) c) p

instance Functor f => Applicative (Par f) where
    pure = One
    One g <*> One x = One $ g x
    One g <*> x = g <$> x
    {-More (c :: b -> a -> c) (g :: Par f b) <*> One (x :: a)
        = More (uncurry c) $ fmap (, x) g
    More (c :: b -> a -> c) (g :: Par f b)
        <*> More  (c' :: d -> a) (g' :: Par f d)
            = More (uncurry c) $ (,) <$> g <*> (c' <$> g')-}
    More (c :: f (b -> a -> c)) (g :: Par f b) <*> y
            = More (fmap uncurry c) $ (,) <$> g <*> (id <$> y)

one :: Functor f => f a -> Par f a
one x = More (fmap const x) $ One undefined

---------------------------------------------------------------------------------

data Elem a = Elem (String -> Maybe a) deriving Functor

opt :: Read a => Elem a
opt = Elem readMaybe

optS :: Elem String
optS = Elem Just


parse :: String -> Par Elem a -> Maybe a
parse = parseEx . words
    where
    parseEx :: [String] -> Par Elem a -> Maybe a
    parseEx _ (One x)                = Just x
    parseEx (x:xs) (More (Elem r) p) = r x <*> parseEx xs p
