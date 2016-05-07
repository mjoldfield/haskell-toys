-- |
-- Module      : Generic.hs
-- Copyright   : (c) 2016, M J Oldfield
-- Stability   : Experimental
--
-- == Rationale
--
-- I find rather too much boilerplate in little Haskell command
-- line toys I write.
--
-- This module is an attempt to abstract that common code, in the
-- hope that I'll both be more efficient and write less hacky
-- tools.
--
-- Often particular modules will be more helpful when writing
-- code which e.g. handles Images with Juicy Pixel.
--

module Toy.Generic
    ( processGeneric, processArgs
    ) where

import System.Environment
import System.FilePath.Posix

-- |'processGeneric' makes it easier to apply functions
-- which take input and output filenames to a list of
-- input names, by programmatically constructing the
-- output name. Typically we munge the basename, then
-- e.g. change the suffix:
--
-- @
-- processGeneric mungePNG (++ "-m") (\f -> replaceExtension f "png")
-- @
--
processGeneric :: (FilePath -> FilePath -> IO ()) -> (String -> String) -> String -> FilePath -> IO ()
processGeneric f g suffix inf = f inf out
   where out     = replaceExtension newStem suffix
         newStem = replaceBaseName inf newBase
         newBase = g $ takeBaseName inf

-- |'processArgs' just calls a function with every command
-- line argument.
--
-- @
-- main = processArgs putStrLn
-- @
processArgs :: (FilePath -> IO()) -> IO ()
processArgs f = getArgs >>= mapM_ f

