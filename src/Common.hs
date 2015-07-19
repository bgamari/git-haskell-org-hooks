{-# LANGUAGE OverloadedStrings #-}

module Common
    ( module Common
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (FilePath)

tshow :: Show a => a -> Text
tshow = T.pack . show
