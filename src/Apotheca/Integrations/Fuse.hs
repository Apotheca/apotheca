module Apotheca.Integrations.Fuse
( mountApotheca
) where

import           Control.Monad         (forM)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe            (fromJust)

import           System.Directory
import           System.Environment    (withArgs)
import           System.FilePath
import           System.IO

import           System.Fuse



mountApotheca = undefined
