{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
-- Voting.hs
-- Logic for the "voting" subgame that makes up the heart of mafia
module Voting
  ( Vote(..)
  , VoteResult(..)
  , AsVoteResult(..)
  , VotingState
  , mkVotingState
  , vote
  , HasVotingState(..)
  , voting
  ) where
import           Control.Lens                   ( (^.)
                                                , makeClassy, makeClassyPrisms
                                                )
import           Control.Lens.Operators         ( (%~)
                                                , (&)
                                                )
import           Control.Monad.State            ( gets
                                                , modify
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           GHC.Generics                   ( Generic )
import           Game                           ( Emission(..)
                                                , Game
                                                , InputHandler
                                                , play
                                                )
import           Util                           ( tally )
import Data.Aeson (ToJSON)

data Vote r = NoVote
          | Pass
          | For r deriving (Eq, Show)

data VoteResult r = Passed | Voted r deriving (Eq,Show, Generic)

instance (ToJSON r) => ToJSON (VoteResult r)

data VotingState p r = VotingState
  { _votes      :: Map p (Vote r)
  , _candidates :: Set r
  }
  deriving (Generic, Show)

$(makeClassy ''VotingState)
$(makeClassyPrisms ''VoteResult)

mkVotingState :: (Ord p, Ord r) => [p] -> [r] -> VotingState p r
mkVotingState plrs choices =
  VotingState (Map.fromList $ map (, NoVote) plrs) (Set.fromList choices)

votePure :: (Ord p, Ord r, HasVotingState s p r) => p -> Vote r -> s -> s
votePure player ballot st = st & votes %~ Map.adjust newVote player
 where
  newVote = case ballot of
    NoVote -> const NoVote
    Pass   -> const Pass
    For r  -> if Set.member r (st ^. candidates) then const $ For r else id

votingFinished :: (Eq r, HasVotingState s p r) => s -> Maybe (VoteResult r)
votingFinished st = if majVotes > (numPlayers `div` 2)
  then case majority of
    NoVote -> Nothing
    Pass   -> Just Passed
    For r  -> Just $ Voted r
  else Nothing
 where
  (majority, majVotes) = head $ tally $ Map.elems $ st ^. votes
  numPlayers           = Map.size $ st ^. votes

vote
  :: (Show s, Ord p, Ord r, HasVotingState s p r)
  => InputHandler (p, Vote r) s (VoteResult r)
vote (player, ballot) = do
  modify $ votePure player ballot
  res <- gets votingFinished
  case res of
    Nothing -> return []
    Just vt -> return [Emission vt]

voting
  :: (Ord p, Ord r, Show s, HasVotingState s p r)
  => s
  -> [(p, Vote r)]
  -> [Emission (VoteResult r)]
voting st = play st vote
