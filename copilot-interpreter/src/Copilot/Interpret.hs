-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | An interpreter for Copilot specifications.

{-# LANGUAGE Safe #-}

module Copilot.Interpret
  ( Format (..)
  , interpret
  , paint
  ) where

import Data.List
import Copilot.Core
import Copilot.Interpret.Eval
import Copilot.Interpret.Render

-- | Output format for the results of a Copilot spec interpretation.
data Format = Table | CSV

-- | Interpret a Copilot specification.
interpret :: Format  -- ^ Format to be used for the output.
          -> Int     -- ^ Number of steps to interpret.
          -> Spec    -- ^ Specification to interpret.
          -> String
interpret format k spec =
  case format of
    Table   -> renderAsTable e
    CSV     -> renderAsCSV e
  where
    e = eval Haskell k spec

paint :: Int     -- ^ Number of steps to interpret.
      -> Spec    -- ^ Specification to interpret.
      -> IO ()
paint k spec =
    paintEvaluation spec e
  where
    e = eval Haskell k spec

paintEvaluation :: Spec
                -> ExecTrace
                -> IO ()
paintEvaluation spec e = do
    putStrLn "\\documentclass{standalone}"
    putStrLn "\\usepackage{tikz}"
    putStrLn "\\usepackage{tikz-timing}"
    putStrLn "\\begin{document}"
    putStrLn "\\begin{tikztimingtable}"
    printObserverOutputs
    printTriggerOutputs
    putStrLn "\\end{tikztimingtable}"
    putStrLn "\\end{document}"
  where
    signal :: String -> [String] -> String
    signal name values = format name <> " & " <> concat values <> "\\\\"
      where
        format n = "$" <> n <> "$"

    printTriggerOutputs :: IO ()
    printTriggerOutputs = mapM_ putStrLn (map printTriggerOutput trigs)
      where
        printTriggerOutput :: (String, [Maybe [Output]], Int) -> String
        printTriggerOutput (name, ls, argsLength) =
          signal name trig
          <>
          concatMap (\(v, n) -> signal n (showValues v)) (zip (args argsLength) argNames)
          where
            trig :: [String]
            trig = concatMap printTriggerOutputListElem ls

            args :: Int -> [[Maybe Output]]
            args argsLength = transpose $ transMaybes ls argsLength

            argNames = [name <> "_{arg" <> show n <> "}" | n <- [0..]]

        -- Push Maybe's to inner level.
        transMaybes :: [Maybe [Output]] -> Int -> [[Maybe Output]]
        transMaybes []       _          = []
        transMaybes (xs:xss) argsLength = case xs of
          Just xs' -> map Just xs' : transMaybes xss argsLength
          Nothing  -> replicate argsLength Nothing : transMaybes xss argsLength

        -- Ignores the value, just interprets as a boolean
        printTriggerOutputListElem :: Maybe [Output] -> [String]
        printTriggerOutputListElem Nothing = ["H"]
        printTriggerOutputListElem (Just _) = ["L"]

    ---------------------------------------------------------------------------
    printObserverOutputs :: IO ()
    printObserverOutputs = mapM_ putStrLn (map observerOutput obsvs)
      where
        observerOutput (name, ls) = signal name (showValues $ map Just ls)

    ---------------------------------------------------------------------------
    showValues :: [Maybe String] -> [String]
    showValues = map showValue

    showValue :: Maybe String -> String
    showValue Nothing = "S"
    showValue (Just s) | isBoolean s = showValueBoolean s
                       | otherwise   = showValueNumber s

    showValueBoolean :: String -> String
    showValueBoolean "true"  = "D{T}"
    showValueBoolean "false" = "D{F}"

    showValueNumber :: String -> String
    showValueNumber n = "D{" <> n <> "}"
    ---------------------------------------------------------------------------

    trigs :: [(String, [Maybe [Output]], Int)]
    trigs = map addArgsLength (interpTriggers e)
      where
        addArgsLength :: (String, [Maybe [Output]]) -> (String, [Maybe [Output]], Int)
        addArgsLength (name, output) = (name, output, argsLength)
          where
            argsLength = case find (\t -> triggerName t == name) (specTriggers spec) of
              Nothing -> error "Couldn't find given trigger in spec, should never occur!"
              Just t -> length $ triggerArgs t

    obsvs :: [(String, [Output])]
    obsvs = interpObservers e

isBoolean "true" = True
isBoolean "false" = True
isBoolean _ = False
