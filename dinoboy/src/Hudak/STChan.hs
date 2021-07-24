
module STChan

  ( STChan

  , newSTChan

  , writeSTChan

  , writeSTChanNonBlocking
  
  , readSTChanNonBlock

  , readSTChan

  , readSTChan2

  ,isEmptySTChan
  )

where
-- #if !MIN_VERSION_base(4,8,0)
-- import Control.Applicative ((<$>))
-- #endif



import Control.Concurrent.STM.TBQueue
import Control.Monad.STM (atomically, orElse)


-- | @STChan@ is an abstract type representing a bounded FIFO channel.
data STChan a = STChan (TBQueue a)



-- | Builds and returns a new instance of @STChan@.

newSTChan :: Int   -- ^ maximum number of elements the channel can hold
          -> IO (STChan a)
newSTChan size = atomically $ STChan <$> newTBQueue (fromIntegral size)


-- | Writes a value to a @STChan@; blocks if the channel is full.

writeSTChan :: STChan a -> a -> IO ()
writeSTChan (STChan q) a = atomically $ writeTBQueue q a



-- | Attempts to write a value to a @STChan@. If the channel has room,
-- the value is written and this returns 'True'. Otherwise this returns
-- 'False' and returns immediately.

writeSTChanNonBlocking :: STChan a -> a -> IO Bool
writeSTChanNonBlocking (STChan q) a = atomically $ do
    f <- isFullTBQueue q
    if f
       then return False
       else writeTBQueue q a >> return True

-- | Reads the next value from the @STChan@; blocks if necessary.
readSTChan :: STChan a -> IO a
readSTChan (STChan q) = atomically $ readTBQueue q

-- | Reads the next value from the @STChan@; non block.
readSTChanNonBlock :: STChan a -> IO (Maybe a)
readSTChanNonBlock (STChan q) =  atomically $ do
    empty <- isEmptyTBQueue q
    if empty
      then return Nothing
      else 
        do
          a <- readTBQueue q
          return (Just a)

-- | Reads the next value from either @STChan@, prioritizing the first
-- @STChan@; blocks if necessary.

readSTChan2 :: STChan a -> STChan b -> IO (Either a b)
readSTChan2 (STChan q1) (STChan q2) = atomically $
  (Left <$> readTBQueue q1) `orElse` (Right <$> readTBQueue q2)

-- | check if empty
isEmptySTChan :: STChan a -> IO Bool
isEmptySTChan (STChan q) =  atomically $ do
    emptyState <- isEmptyTBQueue q
    return emptyState