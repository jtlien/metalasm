--
--  main routine for disassembler
--       will read in a file for first argument
--         file has  lines with
--         @addr  hexdata
--         where addr is the address in hex and hexdata is the data in hex
--             at that address   (.hex) file format

main = do { s <- getArgs;
            f <- readFile (head s);
             (putStr ( fmtDisassemblyInput ( lines f))); }
