
--
-- Main routine for meta assembler
--        reads in a file of statements as first argument
--        prints to standard out

main = do { s <- getArgs;
         f <- readFile ( head s);
           (putStr  ( unlines  (  map  (formatstatement) ( dostatements   ( concat ( lines f ) ))))) ; 
           (putStr ( unlines ( (map (fmtsym) (fst ( tosymtablefromraw f ) ))))); }
--         (print ( show( dostatements ( concat ( lines f )) ) )) ; }

