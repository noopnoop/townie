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
                                                , (^.)
                                                )
import           Control.Lens.TH                ( makeClassy )
import           Control.Monad.State            ( gets
                                                , modify
                                                )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Types                          ( Game )
import           Voting                         ( HasVotingState(..)
                                                , Vote(..)
                                                , VoteResult(..)
                                                , VotingState(..)
                                                , mkVotingState
                                                , vote
                                                )

newtype Player = Player { team :: Team } deriving (Show, Eq)
type PlayerName = Text
type Players = Map PlayerName Player
data Team = Town | Mafia deriving (Show, Eq, Enum)
data Phase = Night | Day deriving (Show, Eq, Enum)
type Input = (PlayerName, Vote PlayerName)

data MafiaState = MafiaState
  { _mafiaVotingState :: VotingState PlayerName PlayerName
  , _players          :: Players
  , _phase            :: Phase
  }
  deriving (Generic, Show)

$(makeClassy ''MafiaState)

instance HasVotingState MafiaState PlayerName PlayerName where
  votingState = mafiaVotingState

teamMembers :: (HasMafiaState s) => Team -> s -> Players
teamMembers t st = Map.filter ((==) t . team) $ st ^. players

townMembers :: (HasMafiaState s) => s -> Players
townMembers = teamMembers Town

mafiaMembers :: (HasMafiaState s) => s -> Players
mafiaMembers = teamMembers Mafia

mafiaWin :: (HasMafiaState s) => s -> Bool
mafiaWin st = Map.size (mafiaMembers st) >= Map.size (townMembers st)

townWin :: (HasMafiaState s) => s -> Bool
townWin st = Map.size (mafiaMembers st) == 0

nooneWin :: (HasMafiaState s) => s -> Bool
nooneWin st = mafiaWin st && townWin st

handleKill
  :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s)
  => Game s Players
  -> VoteResult PlayerName
  -> Game s Players
handleKill next plr = do
  modify $ kill plr
  checkWin <- gets handleWin
  case checkWin of
    Nothing     -> next
    Just winner -> return $ Just winner

kill :: (Show s, HasMafiaState s) => VoteResult PlayerName -> s -> s
kill target st = case target of
  Passed    -> st
  Voted plr -> st & players %~ Map.delete plr

handleWin :: (HasMafiaState s) => s -> Maybe Players
handleWin st | nooneWin st = Just Map.empty
             | mafiaWin st = Just $ mafiaMembers st
             | townWin st  = Just $ townMembers st
             | otherwise   = Nothing

dayPhase
  :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s)
  => Input
  -> Game s Players
dayPhase input = do
  checkVoting <- vote input
  case checkVoting of
    Nothing     -> return Nothing
    Just result -> handleKill (changePhase Night >> mkMafiaVote) result

basicMafia
  :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s)
  => Input
  -> Game s Players
basicMafia input = do
  time <- use phase
  case time of
    Night -> nightPhase input
    Day   -> dayPhase input

nightPhase
  :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s)
  => Input
  -> Game s Players
nightPhase input = do
  checkVoting <- vote input
  case checkVoting of
    Nothing     -> return Nothing
    Just result -> handleKill (changePhase Day >> mkTownVote) result

changePhase :: (HasMafiaState s) => Phase -> Game s ()
changePhase p = phase .= p >> return Nothing

mkVote
  :: (Show s, HasVotingState s PlayerName r, HasMafiaState s)
  => Players
  -> Game s Players
mkVote plrs = do
  votingState .= mkVotingState (Map.keys plrs)
  return Nothing

mkMafiaVote
  :: (Show s, HasVotingState s PlayerName r, HasMafiaState s) => Game s Players
mkMafiaVote = do
  mafias <- gets mafiaMembers
  mkVote mafias

mkTownVote
  :: (Show s, HasVotingState s PlayerName r, HasMafiaState s) => Game s Players
mkTownVote = do
  plrs <- use players
  mkVote plrs

nightStart :: Players -> MafiaState
nightStart plrs = MafiaState mafVote plrs Night
 where
  mafVote = mkVotingState mafias
  mafias  = Map.keys $ Map.filter ((==) Mafia . team) plrs
