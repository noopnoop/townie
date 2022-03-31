{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- Types.hs
-- Types that are used everywhere in the app, and some of their most important functions
module Types where
import           Control.Lens                   ( (^.)
                                                , makeLenses
                                                )
import           Control.Monad                  ( foldM
                                                , when
                                                )
import           Control.Monad.Except           ( MonadError(throwError)
                                                , liftEither
                                                )
import           Control.Monad.State            ( MonadState(put)
                                                , State
                                                , StateT(StateT, runStateT)
                                                , evalState
                                                , evalStateT
                                                , get
                                                , gets
                                                , modify
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Debug.Trace                    ( traceShowM, traceShowId )
import           GHC.Generics                   ( Generic )
import           Util                           ( mte
                                                , (|||)
                                                )

-- data GameError = InvalidAction | InProgress deriving (Eq, Show)

data InProgress = InProgress Text
  deriving (Eq, Show)

-- this type will likely get more and more complicated
-- probably should move to like the mafia subgame when i end up making that
data Player = Player
  { _alive :: Bool
  }
  deriving (Eq, Show, Ord, Generic)

type PlayerName = Text
type Players p = Map PlayerName p

type Game s r = State s (Maybe r)

data Action s = Action
  { useable :: s -> Bool
  , action  :: s -> s
  }

type ActionName = Text
type ActionSet s = Map ActionName (Action s)

act :: Action s -> s -> s
act (Action usbl acn) s = if usbl s then acn s else s

-- play :: s -> [Action s] -> Game s r -> Maybe r
-- -- play init acns game = evalStateT (gameWithInputs acns game) init
-- -- play init [] game = evalStateT game init
-- -- play init (acn:acns) = flip evalStateT init $ undefined
-- play init acns game = evalState (gameWithInputs acns game) init

play :: s -> Game s r -> Maybe r
play = flip evalState

-- playDebug :: (Show s) => s -> Game s r -> Maybe r
-- playDebug init game = evalState (get >>= traceShowM >> game) init

finishedGame :: r -> Game s r
finishedGame = return . Just

gameWithInputs :: [Action s] -> Game s r -> Game s r
gameWithInputs []           game = game
gameWithInputs (acn : acns) game = do
  res <- game
  case res of
    Just r  -> return $ Just r
    Nothing -> modify (act acn) >> gameWithInputs acns game

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

-- mkGame :: (Show s) => Text -> (s -> Bool) -> (s -> r) -> Game s r
-- mkGame name winCond result = do
--   isOver <- gets winCond
--   if isOver then gets (Just . result) else return Nothing

-- showState :: (Show s) => Text -> Game s r
-- showState txt =
--   get >>= \st -> throwError $ InProgress $ txt <> ": " <> Text.pack (show st)

getActions :: ActionSet s -> [ActionName] -> [Action s]
getActions acns []       = []
getActions acns (x : xs) = case Map.lookup x acns of
  Nothing  -> getActions acns xs
  Just acn -> acn : getActions acns xs

