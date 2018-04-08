{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module April where

---------------------------------------------------------------------------------
-- free monad
---------------------------------------------------------------------------------

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

class LiftF f c where
  liftF :: f a -> c f a


instance Functor f => LiftF f Box where
  liftF :: f a -> Box f a
  liftF = Box . fmap Core

---------------------------------------------------------------------------------
-- free applicative, if we have a functor , can we build an applicative structure
-- out of it ? Call it Par :: (* -> *) -> * -> *
-- <*> :: Par f (a -> b) -> Par f a -> Par f b
-- For pure we just intoduce it with an ad hoc constructor: One
-- The idea is to assoc a f (c -> a) * Par f c as type Par f a
-- now the  <*> becomes (q :: f (c -> a -> b)) ->  (g :: Par f c)  -> (y :: Par f a) -> Par f b
-- reusing the <*> we can zip Par f c and Par f a : (,) g <*> y
-- then it's enough tu uncurry our reified applicatin to realize Par f b:
---------------------------------------------------------------------------------


type family R c (f :: * -> *) :: * -> *
type family S c (f :: * -> *) :: * -> *

data FreeAR c (f :: * -> *) a where
    One :: a -> FreeAR c f a
    More :: R c f (b -> a) -> S c f b -> FreeAR c f a

class LiftAR c where
    liftAR
        :: (Functor f)
            => f a -> FreeAR c f a
    lowerAR ::
        (Applicative f)
            =>  FreeAR c f a -> f a

instance (Functor (R c f), Functor f) => Functor (FreeAR c f) where
    fmap f (One x)    = One $ f x
    fmap f (More c p) = More ((f .) <$> c) p

-- capriotti

data Cap
type instance R Cap f = f
type instance S Cap f = FreeAR Cap f


instance (Functor f)
    => Applicative (FreeAR Cap f) where
    pure = One
    One f <*> x = f <$> x
    More c g <*> y
            = More (fmap uncurry c) $ (,) <$> g <*> y

instance LiftAR Cap where
  liftAR x = More (fmap const x) $
    (One $ error "liftF is forgetful" :: FreeAR Cap f a)
  lowerAR (One x)    = pure x
  lowerAR (More g x) = g <*> lowerAR x

--
data Orian

type instance R Orian f = FreeAR Orian f
type instance S Orian f = f

instance Functor f => Applicative (FreeAR Orian f) where
      pure = One
      tx <*> One y = fmap ($ y) tx
      tx <*> More ty az = More ((.) <$> tx <*> ty) az

instance LiftAR Orian where
    liftAR = More $ One id
    lowerAR (One x)      = pure x
    lowerAR (More tx ay) = lowerAR tx <*> ay

--
data Twan

type instance R Twan f = FreeAR Twan f
type instance S Twan f = f

instance Functor f => Applicative (FreeAR Twan f) where
    pure = One
    One f <*> tx = fmap f tx
    More tx ay <*> tz = More (flip <$> tx <*> tz) ay

instance LiftAR Twan where
    liftAR = More $ One id
    lowerAR (One x)      = pure x
    lowerAR (More tx ay) = flip id <$> ay <*> lowerAR tx

