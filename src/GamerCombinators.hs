{-# LANGUAGE OverloadedStrings #-}
module GamerCombinators where
import           Control.Lens                   ( (&) )
import           Control.Lens.Operators         ( (%~)
                                                , (^.)
                                                )
import           Data.Bifunctor                 ( Bifunctor(bimap) )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
-- GamerCombinators.hs
-- GameCombinators would make more sense but still be too verbose. i really just wanted to call something gamer combinators
-- Ways of combining Games to make new Games
import           Types                          ( Action
                                                , ActionSet
                                                , Game(Game)
                                                , actions
                                                )


-- Close but not quite the type signature of bind
--bindGame :: 

-- addAction :: Text -> Action s -> Game s r -> Game s r
-- addAction txt acn game = game & actions %~ Map.insert txt acn

-- addActions :: ActionSet s -> Game s r -> Game s r
-- addActions acns game = game & actions %~ Map.union acns

-- sequenceGames :: Game s r -> Game t o -> Game (Either s t) o
-- sequenceGames (Game acns1 cond1 res1 init1) (Game acns2 cond2 res2 init2) =
--   Game acns cond res init
--  where
--   init = Left init1
--   res (Right t) = res2 t
--   res _ = error "should never happen... not sure how to make this less ugly"
--   cond (Right t) = cond2 t
--   cond _         = False
--   acns   = acns1' <> acns2'
--   acns1'  = undefined
--   acns2' = undefined
