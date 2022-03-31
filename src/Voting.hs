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
                                                , views
                                                )
import           Control.Lens.Operators         ( (%~)
                                                , (&)
                                                )
import           Control.Monad                  ( foldM
                                                , liftM2
                                                )
import           Control.Monad.State            ( MonadState(state, get), modify, gets )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Float                      ( int2Float )
import           GHC.Generics                   ( Generic )
import           Types                          ( Action(Action)
                                                , ActionName
                                                , ActionSet
                                                , Game
                                                , Player
                                                , PlayerName
                                                , Players
                                                , foldGame
                                                )
import           Util                           ( tally
                                                , unQuote
                                                )
import Debug.Trace (traceShowM)

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
  :: (Show s, Ord p, Eq r, HasVotingState s p r) => (p, Vote r) -> Game s (VoteResult r)
vote (player, ballot) = do
  modify $ votePure player ballot
  gets votingFinished

voting
  :: (Ord p, Eq r, Show s, HasVotingState s p r)
  => [(p, Vote r)]
  -> Game s (VoteResult r)
voting = foldGame vote

-- showVote :: (Show r) => Vote r -> Text
-- showVote HasntVoted = "HasntVoted"
-- showVote Pass       = "pass"
-- showVote (For r)    = "for " <> Text.pack (unQuote $ show r)

-- mkInitial :: Players p -> Votes r
-- mkInitial ps = Votes $ fmap (const HasntVoted) ps

-- mkVotingActions :: (Show r, HasVotes s r) => Set r -> Players p -> ActionSet s
-- mkVotingActions rs ps = Map.fromList $ plist >>= mkOnce
--  where
--   plist = Map.toList ps
--   rlist = Set.toList rs
--   mkOnce (name, _) = mkVoteTuple name Pass : map (mkVoteTuple name . For) rlist

-- mkVoteAction :: (HasVotes s r) => PlayerName -> Vote r -> Action s
-- mkVoteAction plr vote =
--   Action (isJust . views getVotes (Map.lookup plr))
--     $  getVotes
--     %~ Map.adjust (const vote) plr

-- mkVoteActionName :: (Show r) => PlayerName -> Vote r -> ActionName
-- mkVoteActionName plr vote = "vote " <> plr <> " " <> showVote vote

-- mkVoteTuple
--   :: (Show r, HasVotes s r) => PlayerName -> Vote r -> (ActionName, Action s)
-- mkVoteTuple nm vt = (mkVoteActionName nm vt, mkVoteAction nm vt)

-- majority :: (Eq r, Ord p, HasVotingState s p r) => s -> (Vote r, Int)
-- majority st = head $ tally $ Map.elems $ st ^. votes

-- overWhen :: (Eq r, Ord p, HasVotingState s p r) => s -> Bool
-- overWhen st = reachedMajority && (numMajority >= numPlayers / 2)
--  where
--   numPlayers      = int2Float $ length $ Map.toList $ st ^. votes
--   numMajority     = int2Float $ snd $ majority $ st ^. votes
--   reachedMajority = NoVote /= fst (majority $ st ^. votes)

-- doWin :: (Eq r, HasVotes s r) => s -> Maybe r
-- doWin st = case majority st of
--   (For name, _) -> Just name
--   _             -> Nothing

-- mkVoting :: (Eq r, HasVotes s r, Show s) => Game s (Maybe r)
-- mkVoting = mkGame "Voting" overWhen doWin
