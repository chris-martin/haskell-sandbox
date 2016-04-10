module Aligned where

import           Control.Monad (MonadPlus)
import           Prelude       (Applicative, Bool, Functor, Monad, Monoid,
                                Traversable, undefined)


-- Function application

($)      ::                                     (a ->   b) ->   a ->   b
(<$>)    ::  Functor     f                 =>   (a ->   b) -> f a -> f b
(<*>)    ::  Applicative f                 => f (a ->   b) -> f a -> f b
liftA    ::  Applicative f                 =>   (a ->   b) -> f a -> f b
liftM    ::  Monad       f                 =>   (a ->   b) -> f a -> f b
(=<<)    ::  Monad       f                 =>   (a -> f b) -> f a -> f b
mapM     :: (Monad       f, Traversable t) =>   (a -> f b) -> t a -> f (t b)
traverse :: (Applicative f, Traversable t) =>   (a -> f b) -> t a -> f (t b)
foldMap  ::                 Monoid      m  =>   (a -> m)   -> t a -> m

-- Function composition

(.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

-- Filtering

filter  ::                (a ->   Bool) ->  [a] ->   [a]
filterM :: Monad     m => (a -> m Bool) ->  [a] -> m [a]
mfilter :: MonadPlus m => (a ->   Bool) -> m a  -> m  a

-- Discarding one of two values

const ::                    a ->   b ->   a
(<$)  :: Functor     f =>   a -> f b -> f a
(<*)  :: Applicative f => f a -> f b -> f a
($>)  :: Functor     f => f a ->   b -> f b
(*>)  :: Applicative f => f a -> f b -> f b

-- Restructuring

sequenceA ::  Applicative f                 => t (f a) -> f (t a)
sequence  :: (Monad       f, Traversable t) => t (f a) -> f (t a)
join      ::  Monad       f                 => f (f a) -> f a

(.) = undefined
(<=<) = undefined
($) = undefined
($>) = undefined
(*>) = undefined
(<$) = undefined
(<$>) = undefined
(<*) = undefined
(<*>) = undefined
(=<<) = undefined
const = undefined
filter = undefined
filterM = undefined
foldMap = undefined
join = undefined
liftA = undefined
liftM = undefined
mapM = undefined
mfilter = undefined
sequence = undefined
sequenceA = undefined
traverse = undefined
