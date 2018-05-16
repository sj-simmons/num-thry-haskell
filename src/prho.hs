--
-- You can compile this with                                             
--     ghc --make prho.hs
-- or, if you are using stack, with
--     stack exec -- ghc --make prho.hs
-- and then run it from the command line with
--     echo 12312312441 | ./prho
-- or, say,
--     echo "(2^31-1) * (2^19 -1)" | bc | ./prho
--
-- SSimmons April 2017

import NumThry (prhoFactor)
        
main = do 
    n <- getLine
    putStrLn $ n ++ ": " ++ show (prhoFactor (read n))
