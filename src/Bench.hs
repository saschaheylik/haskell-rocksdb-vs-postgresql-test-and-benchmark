module Bench
    ( benchTime
    , bench
    ) where

import Test.Benchmark.Function
import Text.Printf (printf, PrintfArg)
import Control.Monad (forM_)

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

benchTime :: Int -> (Int -> IO a) -> IO Float
benchTime n func = timeAction $ forM_ [0 .. n] func

bench :: Int -> [Char] -> (Int -> IO a) -> IO ()
bench nInt name func = do
    s <- benchTime nInt func
    let n = fromIntegral nInt
    let (opDur, opDurU) = if (s/n * 1000) < 1 then
                            ((s/n*1000000), "µs") else (s/n*1000, "ms")
    let (ops, maybeK) = if (n/s) >= 1000 then ((n/s)/1000, "k") else ((n/s), "")
    putStrLn $ "benchmark \"" ++ name ++ "\":\n (run " ++ show n ++ "x) OP/S: " ++
        roundToStr 1 ops ++ maybeK ++ ", total time: " ++ roundToStr 3 s ++ "s, " ++
        roundToStr 1 opDur ++ opDurU ++ " per OP"
