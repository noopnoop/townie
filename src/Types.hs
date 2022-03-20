{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
-- Types.hs
-- Types that are used everywhere in the app, and some of their most important functions
module Types where
import           Control.Lens                   ( (^.)
                                                , makeLenses
                                                )
import           Control.Monad                  ( when )
import           Control.Monad.Except           ( liftEither )
import           Control.Monad.State            ( State
                                                , StateT(runStateT)
                                                , evalStateT
                                                , get
                                                , gets
                                                , modify
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Util                           ( mte )

data GameError = InvalidAction | InProgress deriving (Eq, Show)

-- this type will likely get more and more complicated
data Player = Player
  { _alive :: Bool
  }
  deriving (Eq, Show, Ord, Generic)

$(makeLenses ''Player)

type PlayerName = Text
type Players p = Map PlayerName p

data Action s = Action
  { useable :: GameState s -> Bool
  , action  :: GameState s -> GameState s
  }

type ActionName = Text
type ActionSet s = Map ActionName (Action s)

data GameState s = GameState
  { _actions   :: ActionSet s
  , _gameState :: s
  }
  deriving Generic

$(makeLenses ''GameState)

data Game s r = Game
  { _initial :: GameState s
  , _isOver  :: GameState s -> Bool
  , _onWin   :: GameState s -> r
  }
  deriving Generic

$(makeLenses ''Game)

act :: Monad m => Action s -> StateT (GameState s) m ()
act (Action usbl acn) = do
  ok <- gets usbl
  when ok $ modify acn

tryToEnd :: Game s r -> GameState s -> Either GameError r
tryToEnd game st =
  if game ^. isOver $ st then return $ game ^. onWin $ st else Left InProgress

play :: [Action s] -> Game s r -> Either GameError r
play []       game = tryToEnd game $ game ^. initial
play (x : xs) game = flip evalStateT (game ^. initial) $ do
  act x
  st <- get
  case tryToEnd game st of
    Left  _ -> liftEither $ play xs game
    Right r -> return r

getAction :: Text -> GameState s -> Either GameError (Action s)
getAction txt game = mte InvalidAction $ Map.lookup txt (game ^. actions)
