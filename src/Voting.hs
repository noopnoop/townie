{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections #-}
-- Voting.hs
-- Logic for the "voting" subgame that makes up the heart of mafia
-- Could make this even more general (voting for arbitrary things instead of always for other players)
module Voting where
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
import           GHC.Generics                   ( Generic )
import           Types                          ( Game
                                                , foldGame
                                                )
import           Util                           ( tally )

data Vote r = NoVote
          | Pass
          | For r deriving (Eq, Show)

data VoteResult r = Passed | Voted r deriving (Eq,Show)

newtype VotingState p r = VotingState { _votes :: Map p (Vote r) } deriving (Generic, Show)

$(makeClassy ''VotingState)

mkVotingState :: (Ord p) => [p] -> VotingState p r
mkVotingState = VotingState . Map.fromList . map (, NoVote)

votePure :: (Ord p, HasVotingState s p r) => p -> Vote r -> s -> s
votePure player ballot st = st & votes %~ Map.adjust (const ballot) player

votingFinished :: (Eq r, HasVotingState s p r) => s -> Maybe (VoteResult r)
votingFinished st = case majority of
  NoVote -> Nothing
  Pass   -> Just Passed
  For r  -> Just $ Voted r
  where majority = fst $ head $ tally $ Map.elems $ st ^. votes

vote
  :: (Show s, Ord p, Eq r, HasVotingState s p r)
  => (p, Vote r)
  -> Game s (VoteResult r)
vote (player, ballot) = do
  modify $ votePure player ballot
  gets votingFinished

voting
  :: (Ord p, Eq r, Show s, HasVotingState s p r)
  => [(p, Vote r)]
  -> Game s (VoteResult r)
voting = foldGame vote
