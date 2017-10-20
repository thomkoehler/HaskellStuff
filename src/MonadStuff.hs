
module MonadStuff where

newtype State st a = State { runState :: st -> (a, st) }


instance Functor (State st) where
   fmap f stf = State $ \st0 ->
      let
         (x, st1) = runState stf st0
      in
         (f x, st1)


instance Applicative (State st) where
      pure x = State $ \st -> (x, st)
      af <*> sta = State $ \st ->
         let
            (f, st1) = runState af st
            (x, st2) = runState sta st1
         in
            (f x, st2)


newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
   fmap f mt = MaybeT $ do
      x <- runMaybeT mt
      case x of
         Nothing -> return Nothing
         Just y -> return $ Just $ f y


instance Monad m => Applicative (MaybeT m) where
   pure = MaybeT . return . Just

   mf <*> mx = MaybeT $ do
      mbf <- runMaybeT mf
      case mbf of
         Nothing -> return Nothing
         Just f -> do
            mbx <- runMaybeT mx
            case mbx of
               Nothing -> return Nothing
               Just x -> return $ Just $ f x


instance Monad m => Monad (MaybeT m) where
   return = pure

   ma >>= mf = MaybeT $ do
      mba <- runMaybeT ma
      case mba of
         Nothing -> return Nothing
         Just a -> runMaybeT (mf a)
