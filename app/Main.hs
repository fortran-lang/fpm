module Main where

import           Fpm                            ( getArguments
                                                , start
                                                )

main :: IO ()
main = getArguments >>= start
