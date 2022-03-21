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
                                                , modify, MonadState (put)
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Util                           ( mte )

data GameError = InvalidAction | InProgress deriving (Eq, Show)

-- this type will likely get more and more complicated
-- probably should move to like the mafia subgame when i end up making that
data Player = Player
  { _alive :: Bool
  }
  deriving (Eq, Show, Ord, Generic)

$(makeLenses ''Player)

type PlayerName = Text
type Players p = Map PlayerName p

data Game s r = Game
  { _actions :: ActionSet s
  , _initial :: s
  , _isOver  :: s -> Bool
  , _onWin   :: s -> r
  }
  deriving Generic

data Action s = Action
  { useable :: s -> Bool
  , action  :: s -> s
  }

type ActionName = Text
type ActionSet s = Map ActionName (Action s)

$(makeLenses ''Game)

act :: Monad m => Action s -> StateT s m ()
act (Action usbl acn) = do
  ok <- gets usbl
  when ok $ modify acn

tryToEnd :: Game s r -> s -> Either GameError r
tryToEnd game st =
  if game ^. isOver $ st then return $ game ^. onWin $ st else Left InProgress

play :: [ActionName] -> Game s r -> Either GameError r
play []       game = tryToEnd game $ game ^. initial
play (x : xs) game = flip evalStateT (game ^. initial) $ do
  acn <- liftEither $ getAction x game
  act acn
  st <- get
  case tryToEnd game st of
    Left  _ -> liftEither $ play xs game
    Right r -> return r

getAction :: ActionName -> Game s r -> Either GameError (Action s)
getAction name game = mte InvalidAction $ Map.lookup name (game ^. actions)
