module MonadTrans where
-- analog of Control.Monad.Trans.Class

class MonadTrans t where
  lift :: Monad m => m a -> t m a
