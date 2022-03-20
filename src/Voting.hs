{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
-- Voting.hs
-- Logic for the "voting" subgame that makes up the heart of mafia
-- Could make this even more general (voting for arbitrary things instead of always for other players)
module Voting
  ( mkVoting
  ) where
import           Control.Lens                   ( (^.)
                                                , makeLenses
                                                )
import           Control.Lens.Operators         ( (%~) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Float                      ( int2Float )
import           GHC.Generics                   ( Generic )
import           SimpleGame                     ( mkSimpleGame )
import           Types                          ( Action(Action)
                                                , ActionName
                                                , ActionSet
                                                , Game(Game)
                                                , GameState(GameState)
                                                , Player
                                                , PlayerName
                                                , Players
                                                , alive
                                                , gameState
                                                )
import           Util                           ( tally )

data Vote = HasntVoted
          | Pass
          | ForPlayer PlayerName
          deriving (Eq)

type Votes = Map PlayerName Vote

data VotingState p = VotingState
  { _players :: Players p
  , _votes   :: Votes
  }
  deriving Generic


$(makeLenses ''VotingState)

type VotingState' p = GameState (VotingState p)

showVote :: Vote -> Text
showVote HasntVoted      = "HasntVoted"
showVote Pass            = "pass"
showVote (ForPlayer plr) = "for " <> plr

mkInitial :: Players p -> VotingState p
mkInitial ps = VotingState ps $ fmap (const HasntVoted) ps

mkActions :: Players p -> ActionSet (VotingState p)
mkActions ps = Map.fromList $ plist >>= mkOnce
 where
  plist = Map.toList ps
  mkOnce (name, _) =
    mkVoteTuple name Pass
      : map (\(opp, _) -> mkVoteTuple name (ForPlayer opp)) plist

mkVoteAction :: PlayerName -> Vote -> Action (VotingState p)
mkVoteAction plr vote =
  Action (const True) $ gameState . votes %~ Map.insert plr vote

mkVoteActionName :: PlayerName -> Vote -> ActionName
mkVoteActionName plr vote = "vote " <> plr <> " " <> showVote vote

mkVoteTuple :: PlayerName -> Vote -> (ActionName, Action (VotingState p))
mkVoteTuple nm vt = (mkVoteActionName nm vt, mkVoteAction nm vt)

majority :: VotingState p -> (Vote, Int)
majority st = head $ tally $ Map.elems $ st ^. votes

overWhen :: VotingState p -> Bool
overWhen st = reachedMajority && (numMajority >= numPlayers / 2)
 where
  numPlayers      = int2Float $ length $ Map.toList $ st ^. votes
  numMajority     = int2Float $ snd $ majority st
  reachedMajority = HasntVoted /= fst (majority st)

doWin :: VotingState p -> Maybe PlayerName
doWin st = case majority st of
  (ForPlayer name, _) -> Just name
  _                   -> Nothing

mkVoting :: Players p -> Game (VotingState p) (Maybe PlayerName)
mkVoting ps = mkSimpleGame (mkActions ps) (mkInitial ps) overWhen doWin
