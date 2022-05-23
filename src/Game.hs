{-# LANGUAGE MultiParamTypeClasses #-}
{-|
This module contains the 'Game' type and the 'play' function, which must be understood together.

We see that 'Game s r' is simply a type synonym for 'State s (Maybe r)'. 
However, the 'Maybe' here plays an important role, opposite of how the 'Maybe' monad typically acts.

'play' takes a value of type 's', the initial state, 
a function of type 'a -> Game s r', which we may think of as a function that handles inputs, 
and an object of type 't a' where t is traversable.
We may think of 't a' as a bunch of inputs. 

'play' works much like foldM, working down the traversable with this input-handling function and spitting out a final 'Game s r' value.
The key thing to note is that the instant we get a non-'Nothing' value in our traversal, we are done- the game has reached its conclusion.
On the contrary, if 'foldGame' never encounters a non-'Nothing' value in its traversal, that indicates a game that is not yet finished.
-}
module Game where
import           Control.Monad                  ( foldM )
import           Control.Monad.State            ( State
                                                , evalState
                                                , get
                                                )
import           Debug.Trace                    ( traceShowM )

type Game s r = State s r
type InputHandler a s r = a -> Game s r

class HasElement a where
  defaultElement :: a

instance (HasElement e) => HasElement (Either e a) where
  defaultElement = Left defaultElement

-- | Handle a bunch of inputs in sequence.
play
  :: (Traversable t, HasElement r)
  => s -- ^ The initial state
  -> InputHandler a s r -- ^ An 'input handler'
  -> t a -- ^ A bunch of inputs
  -> r -- ^ The end result
play initial fn inputs =
  evalState (foldM (const fn) defaultElement inputs) initial
--  where
--   fn' last = case last of
--     Right res -> const $ finishedGame res
--     Left _  -> fn

-- | The same as 'play', but traces the internal state before accepting each input in the traversable.
playDebug
  :: (Traversable t, Show s, HasElement r) => (a -> Game s r) -> t a -> Game s r
playDebug fn = foldM fn' defaultElement
 where
  fn' _ = \a -> do
    st <- get
    traceShowM st
    fn a


-- finishedGame :: r -> Game s r
-- finishedGame = return . Right
