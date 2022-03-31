{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module BasicMafia where
import           Control.Lens                   ( use )
import           Control.Lens.Operators         ( (%~)
                                                , (&)
                                                , (.=)
                                                , (.~)
                                                , (^.)
                                                )
import           Control.Lens.TH                ( makeClassy )
import           Control.Monad.Except           ( MonadError(catchError) )
import           Control.Monad.State            ( MonadState(get, put)
                                                , gets
                                                , modify
                                                )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Debug.Trace                    ( traceShowM )
import           GHC.Generics                   ( Generic )
import           Types                          ( Game
                                                , InProgress(InProgress)
                                                , PlayerName
                                                , Players
                                                )
import           Voting                         ( HasVotingState(..)
                                                , Vote(..)
                                                , VoteResult(..)
                                                , VotingState(..)
                                                , mkVotingState
                                                , vote
                                                , voting
                                                )

newtype Player = Player { team :: Team } deriving (Show, Eq)
data Team = Town | Mafia deriving (Show, Eq)
type Input = (PlayerName, Vote PlayerName)

data MafiaState = MafiaState
  { _mafiaVotingState :: VotingState PlayerName PlayerName
  , _players     :: Players PlayerName
  }
  deriving (Generic, Show)

$(makeClassy ''MafiaState)

instance HasVotingState MafiaState PlayerName PlayerName where
  votingState = mafiaVotingState

-- teamMembers :: Team -> MafiaState -> Players PlayerName
-- teamMembers t st = Map.filter ((==) t . team) $ st ^. players

-- townMembers :: MafiaState -> Players PlayerName
-- townMembers = teamMembers Town

-- mafiaMembers :: MafiaState -> Players PlayerName
-- mafiaMembers = teamMembers Mafia

handleKill
  :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s)
  => Game s (VoteResult PlayerName)
  -> VoteResult PlayerName
  -> Game s (VoteResult PlayerName)
handleKill next plr = do
  modify $ kill plr
  done <- gets isOver
  if done then gets (Just . handleWin) else next

kill :: (Show s, HasMafiaState s) => VoteResult PlayerName -> s -> s
kill target st = case target of
  Passed    -> st
  Voted plr -> st & players %~ Map.delete plr

isOver :: (HasMafiaState s) => s -> Bool
isOver st = (==) Map.empty $ st ^. players

handleWin :: (HasMafiaState s) => s -> VoteResult PlayerName
handleWin = const Passed

basicMafia
  :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s)
  => Input
  -> Game s (VoteResult PlayerName)
basicMafia input = do
  checkVoting <- vote input
  case checkVoting of
    Nothing     -> return Nothing
    Just result -> handleKill resetVotes result

-- basicMafia :: [(PlayerName, Vote PlayerName)] -> Game MafiaState (Players Player)
-- basicMafia inputs = do
--   voted <- dayPhase inputs
--   st    <- get
--   traceShowM st
--   case voted of
--     Nothing   -> return Nothing
--     Just vote -> handleKill basicMafia vote

-- nightPhase :: Game MafiaState (Maybe PlayerName)
-- nightPhase = do
--   mafia <- gets mafiaMembers
--   votingState .= mkVotingState $ toList mafia
--   voting

resetVotes :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s) => Game s (VoteResult PlayerName)
resetVotes = do
  plrs <- Map.keys <$> use players
  votingState .= mkVotingState plrs
  return Nothing
