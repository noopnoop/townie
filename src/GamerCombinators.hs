{-# LANGUAGE OverloadedStrings #-}
module GamerCombinators where
import           Control.Lens                   ( (&) )
import           Control.Lens.Operators         ( (%~)
                                                , (^.)
                                                )
import           Control.Monad                  ( liftM2 )
import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Data.Either                    ( isLeft )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
-- GamerCombinators.hs
-- GameCombinators would make more sense but still be too verbose. i really just wanted to call something gamer combinators
-- Ways of combining Games to make new Games
-- GamerCombinators.hs
-- GameCombinators would make more sense but still be too verbose. i really just wanted to call something gamer combinators
-- Ways of combining Games to make new Games
-- GamerCombinators.hs
-- GameCombinators would make more sense but still be too verbose. i really just wanted to call something gamer combinators
-- Ways of combining Games to make new Games
-- GamerCombinators.hs
-- GameCombinators would make more sense but still be too verbose. i really just wanted to call something gamer combinators
-- Ways of combining Games to make new Games
-- GamerCombinators.hs
-- GameCombinators would make more sense but still be too verbose. i really just wanted to call something gamer combinators
-- Ways of combining Games to make new Games
-- GamerCombinators.hs
-- GameCombinators would make more sense but still be too verbose. i really just wanted to call something gamer combinators
-- Ways of combining Games to make new Games
-- GamerCombinators.hs
-- GameCombinators would make more sense but still be too verbose. i really just wanted to call something gamer combinators
-- Ways of combining Games to make new Games
-- GamerCombinators.hs
-- GameCombinators would make more sense but still be too verbose. i really just wanted to call something gamer combinators
-- Ways of combining Games to make new Games
import           Types                          ( Action(Action)
                                                , ActionSet
                                                , Game(Game)
                                                , GameState(GameState)
                                                , actions
                                                , gameState
                                                )


-- Close but not quite the type signature of bind
--bindGame :: 

-- addAction :: Text -> Action s -> Game s r -> Game s r
-- addAction txt acn game = game & actions %~ Map.insert txt acn

-- addActions :: ActionSet s -> Game s r -> Game s r
-- addActions acns game = game & actions %~ Map.union acns

-- sequenceGames :: Game s r -> (r -> Game t o) -> Game (Either s t) o
-- sequenceGames (Game (GameState acns game) res cond) fn = Game init' res' cond'
--  where
--   init' = GameState acns' (Left game)
--   acns' =
--     fmap (\(Action usbl acn) -> Action (liftM2 (&&) usbl $ init' ^. gameState))
--   cond' = undefined -- \st -> cond st && fn (res st)
--   res'  = undefined

-- mkLeftAction :: Action s -> Action (Either s t)
-- mkLeftAction (Action usbl acn) = Action usbl' acn'
--  where
--   usbl' = \st -> case st ^. gameState of
--     Right _ -> False
--     Left  s -> usbl s
