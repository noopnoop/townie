{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           Data.Aeson                     ( ToJSON(toJSON)
                                                , Value(Null)
                                                , defaultOptions
                                                , genericToJSON
                                                )
import           Debug.Trace                    ( traceShowM )
import           GHC.Generics                   ( Generic )

data Emission e = Emission e | Start | Finish
  deriving (Eq, Show, Generic)

instance (ToJSON e) => ToJSON (Emission e) where
  toJSON (Emission e) = toJSON e
  toJSON x            = genericToJSON defaultOptions x

type Game s r = State s [Emission r]
type InputHandler a s r = a -> Game s r


-- | Handle a bunch of inputs in sequence.
play
  :: (Traversable t)
  => s -- ^ The initial state
  -> InputHandler a s r -- ^ An 'input handler'
  -> t a -- ^ A bunch of inputs
  -> [Emission r] -- ^ The end result
play initial fn inputs = evalState (foldM fn' [Start] inputs) initial
 where
  fn' emissions = case last emissions of
    Finish -> const $ pure emissions
    _      -> \input -> do
      newEmissions <- fn input
      return $ emissions <> newEmissions


-- | The same as 'play', but traces the internal state before accepting each input in the traversable.
playDebug
  :: (Traversable t, Show s) => s -> (a -> Game s r) -> t a -> [Emission r]
playDebug initial fn inputs = evalState (foldM fn' [Start] inputs) initial
 where
  fn' emissions = case last emissions of
    Finish -> const $ pure emissions
    _      -> \input -> do
      st <- get
      traceShowM st
      newEmissions <- fn input
      return $ emissions <> newEmissions


-- finishedGame :: r -> Game s r
-- finishedGame = return . Right
