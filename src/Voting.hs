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
  , VotingState
  , mkVotingState
  , vote
  , HasVotingState(..)
  , voting
  ) where
import           Control.Lens                   ( (^.)
                                                , makeClassy
                                                )
import           Control.Lens.Operators         ( (%~)
                                                , (&)
                                                )
import           Control.Monad.State            ( gets
                                                , modify
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           GHC.Generics                   ( Generic )
import           Types                          ( Game
                                                , foldGame
                                                )
import           Util                           ( tally )

data Vote r = NoVote
          | Pass
          | For r deriving (Eq, Show)

data VoteResult r = Passed | Voted r deriving (Eq,Show)

data VotingState p r = VotingState
  { _votes      :: Map p (Vote r)
  , _candidates :: Set r
  }
  deriving (Generic, Show)

$(makeClassy ''VotingState)

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
  => (p, Vote r)
  -> Game s (VoteResult r)
vote (player, ballot) = do
  modify $ votePure player ballot
  gets votingFinished

voting
  :: (Ord p, Ord r, Show s, HasVotingState s p r)
  => [(p, Vote r)]
  -> Game s (VoteResult r)
voting = foldGame vote
