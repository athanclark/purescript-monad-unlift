module Control.Monad.Trans.Unlift where

import Prelude
import Data.Functor.Singleton (class SingletonFunctor, getSingleton)
import Control.Monad.Trans.Control (class MonadTransControl, liftWith, class MonadBaseControl, liftBaseWith)


class ( MonadTransControl m t stT
      , SingletonFunctor stT
      , Functor m
      , Functor (t m)
      ) <= MonadTransUnlift m t stT | t -> stT

instance monadTransUnlift ::
           ( MonadTransControl m t stT
           , SingletonFunctor stT
           , Functor m
           , Functor (t m)
           ) => MonadTransUnlift m t stT


mkUnlift :: forall t m a stT
          . SingletonFunctor stT
         => Functor m
         => (forall b. t m b -> m (stT b))
         -> t m a -> m a
mkUnlift run x = getSingleton <$> run x


newtype Unlift t m = Unlift (forall a. t m a -> m a)

unlift :: forall t m. Unlift t m -> (forall a. t m a -> m a)
unlift (Unlift f) = f


askUnlift :: forall t m stT
           . MonadTransUnlift m t stT
          => Functor m
          => t m (Unlift t m)
askUnlift = liftWith \run -> pure (Unlift (mkUnlift run))

askRun :: forall t m a stT
        . MonadTransUnlift m t stT
       => Functor m
       => t m (t m a -> m a)
askRun = unlift <$> askUnlift


class ( MonadBaseControl base m stM
      , SingletonFunctor stM
      , Functor base
      , Functor m
      ) <= MonadBaseUnlift base m stM | m -> stM base

instance monadBaseUnlift ::
           ( MonadBaseControl base m stM
           , SingletonFunctor stM
           , Functor base
           , Functor m
           ) => MonadBaseUnlift base m stM


mkUnliftBase :: forall base m stM a
              . SingletonFunctor stM
             => Functor base
             => (forall b. m b -> base (stM b))
             -> m a -> base a
mkUnliftBase run x = getSingleton <$> run x


newtype UnliftBase base m = UnliftBase (forall a. m a -> base a)

unliftBase :: forall base m. UnliftBase base m -> (forall a. m a -> base a)
unliftBase (UnliftBase f) = f


askUnliftBase :: forall base m stM. MonadBaseUnlift base m stM => Applicative base => m (UnliftBase base m)
askUnliftBase = liftBaseWith \run -> pure (UnliftBase (mkUnliftBase run))

askRunBase :: forall base m stM a. MonadBaseUnlift base m stM => Applicative base => m (m a -> base a)
askRunBase = unliftBase <$> askUnliftBase
