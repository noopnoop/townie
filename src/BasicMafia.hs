{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module BasicMafia where
import           Control.Lens                   ( makeClassyPrisms
                                                , use
                                                )
import           Control.Lens.Operators         ( (#)
                                                , (%~)
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
import           Game                           ( Emission(..)
                                                , Game
                                                , InputHandler
                                                )
import           Util                           ( safeLast )
import           Voting                         ( AsVoteResult(..)
                                                , HasVotingState(..)
                                                , Vote(..)
                                                , VoteResult(..)
                                                , VotingState(..)
                                                , mkVotingState
                                                , vote
                                                )

data Player = Player
  { team  :: Team
  , alive :: Bool
  }
  deriving (Show, Eq)
type PlayerName = Text
type Players = Map PlayerName Player
data Team = Town | Mafia deriving (Show, Eq, Enum)
data Phase = Night | Day deriving (Show, Eq, Enum)
data MafiaResult = Draw | TeamWin Team | VoteRes (VoteResult PlayerName) deriving (Show, Eq)
type MafiaInput = (PlayerName, Vote PlayerName)

-- instance HasElement MafiaResult where
--   defaultElement = MafiaInProgress :: MafiaResult

data MafiaState = MafiaState
  { _mafiaVotingState :: VotingState PlayerName PlayerName
  , _players          :: Players
  , _phase            :: Phase
  }
  deriving (Generic, Show)

$(makeClassy ''MafiaState)
$(makeClassyPrisms ''MafiaResult)

instance HasVotingState MafiaState PlayerName PlayerName where
  votingState = mafiaVotingState

teamMembers :: Team -> Players -> Players
teamMembers t = Map.filter ((==) t . team)

townMembers :: Players -> Players
townMembers = teamMembers Town

mafiaMembers :: Players -> Players
mafiaMembers = teamMembers Mafia

mafiaWin :: (HasMafiaState s) => s -> Bool
mafiaWin st = Map.size (mafiaMembers $ st ^. players)
  >= Map.size (townMembers $ st ^. players)

townWin :: (HasMafiaState s) => s -> Bool
townWin st = Map.size (mafiaMembers $ st ^. players) == 0

nooneWin :: (HasMafiaState s) => s -> Bool
nooneWin st = mafiaWin st && townWin st

checkAlive :: (HasMafiaState s) => PlayerName -> s -> Bool
checkAlive plr st = maybe False alive $ Map.lookup plr (st ^. players)

validVote :: (HasMafiaState s) => MafiaInput -> s -> Bool
validVote (voter, theirVote) st = case theirVote of
  NoVote    -> checkAlive voter st
  Pass      -> checkAlive voter st
  For votee -> checkAlive voter st && checkAlive votee st

handleKill
  :: ( Show s
     , HasVotingState s PlayerName PlayerName
     , HasMafiaState s
     )
  => Game s MafiaResult
  -> VoteResult PlayerName
  -> Game s MafiaResult
handleKill next plr = do
  modify $ kill plr
  checkWin <- gets handleWin
  case checkWin of
    Nothing -> next
    Just winner ->
      return [Emission $ VoteRes plr, Emission winner]

kill :: (Show s, HasMafiaState s) => VoteResult PlayerName -> s -> s
kill target st = case target of
  Passed    -> st
  Voted plr -> st & players %~ Map.delete plr

handleWin :: (HasMafiaState s) => s -> Maybe MafiaResult
handleWin st | nooneWin st = Just Draw
             | mafiaWin st = Just $ TeamWin Mafia
             | townWin st  = Just $ TeamWin Town
             | otherwise   = Nothing

dayPhase
  :: ( Show s
     , HasVotingState s PlayerName PlayerName
     , HasMafiaState s
     )
  => InputHandler MafiaInput s MafiaResult
dayPhase = handleVote $ changePhase Night >> mkMafiaVote

handleVote
  :: ( Show s
     , HasVotingState s PlayerName PlayerName
     , HasMafiaState s
     )
  => Game s MafiaResult
  -> InputHandler MafiaInput s MafiaResult
handleVote next input = do
  checkValid <- gets $ validVote input
  if not checkValid
    then return []
    else do
      checkVoting <- vote input
      case safeLast checkVoting of
        Just (Emission res) -> handleKill next res
        _                   -> return []

basicMafia
  :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s)
  => InputHandler MafiaInput s MafiaResult
basicMafia input = do
  time <- use phase
  case time of
    Night -> nightPhase input
    Day   -> dayPhase input

nightPhase
  :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s)
  => InputHandler MafiaInput s MafiaResult
nightPhase = handleVote $ changePhase Day >> mkTownVote

changePhase :: (HasMafiaState s) => Phase -> Game s ()
changePhase p = phase .= p >> return []

mkMafiaVote
  :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s)
  => Game s MafiaResult
mkMafiaVote = do
  mafias <- Map.keys . mafiaMembers <$> use players
  plrs   <- Map.keys <$> use players
  votingState .= mkVotingState mafias plrs
  return []

mkTownVote
  :: (Show s, HasVotingState s PlayerName PlayerName, HasMafiaState s)
  => Game s MafiaResult
mkTownVote = do
  plrs <- Map.keys <$> use players
  votingState .= mkVotingState plrs plrs
  return []

nightStart :: Players -> MafiaState
nightStart plrs = MafiaState mafVote plrs Night
  where mafVote = mkVotingState (Map.keys $ mafiaMembers plrs) (Map.keys plrs)
