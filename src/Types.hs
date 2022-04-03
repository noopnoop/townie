{-# LANGUAGE MultiParamTypeClasses #-}
-- Types.hs
-- Types that are used everywhere in the app, and some of their most important functions
module Types where
import           Control.Monad                  ( foldM )
import           Control.Monad.State            ( State
                                                , evalState
                                                , get
                                                )
import           Debug.Trace                    ( traceShowM )

type Game s r = State s (Maybe r)

play :: s -> Game s r -> Maybe r
play = flip evalState

finishedGame :: r -> Game s r
finishedGame = return . Just

foldGame :: (Traversable t) => (a -> Game s r) -> t a -> Game s r
foldGame fn = foldM fn' Nothing
 where
  fn' last = case last of
    Just res -> const $ finishedGame res
    Nothing  -> fn

foldGameDebug :: (Traversable t, Show s) => (a -> Game s r) -> t a -> Game s r
foldGameDebug fn = foldM fn' Nothing
 where
  fn' last = case last of
    Just res -> const $ finishedGame res
    Nothing  -> \a -> do
      st <- get
      traceShowM st
      fn a
