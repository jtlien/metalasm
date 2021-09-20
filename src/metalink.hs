import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List
import Data.Char
import Control.Monad
import Text.Printf


--readOne::String-> [String]
--readOne s = lines  =<< (readFile s)

rp::String->IO (String, [(Int,String)]) 
rp path = do 
           s <- readFile path
           let sl = map parseAnInput ( lines s )
           return ( (path ,sl))

setglpath::String->String
setglpath s = a ++ ".gbl"
            where (a,b) = break (=='.') s
--
-- read the global symbols
--
rpgl::String->IO (String, [(String,Int)]) 
rpgl path = do 
           let npath = setglpath path
           s <- readFile npath
           let sl = map parseAnInputG ( lines s )
           return ( (path ,sl))
          
--withAFile::String->(String->(Int,String))->(String,[(Int,String)])
--withAFile s fun = (s, map ( fun ) (readOne s))
 
--readall f = withAFile f (parseAnInput)

-- An input is @HexAdress [blanks] HexData
--
parseAnInput::String->(Int,String)
parseAnInput indata = formInput ( break (' '==) (deblank indata) )

-- Global Symbol is String [blanks] HexAdress 
--
parseAnInputG::String->(String,Int)
parseAnInputG indata = formInputG ( break (' '==) (deblank indata) )

--
-- For an .asm file input, = @HexAddr [blanks] HexData
formInput::(String,String)->(Int,String)
formInput (a,b)  = (formAddr a ,drop 1 b )
     
--
-- For an .glb file input, = String HexAddr  = Global symbol
--
formInputG::(String,String)->(String,Int)
formInputG (a,b)  = (a , fromHex ( drop 1 b ))
     

deblank (' ':xs) = ' ' : deblank (dropWhile (' '==) xs)
deblank (x:xs) = x : deblank xs
deblank [] = []

formAddr s | (length s < 2 ) = -1
formAddr s | ((length s > 1) && ( head s == '@')) = fromHex ( drop 1 s )

fromHex::String->Int
fromHex s = fromBase 16 ( fromAlphaDigits s )

fromDec::String->Int
fromDec s = fromBase 10 ( fromDigits s )
 
toBase :: Int -> Int -> [Int]
toBase b v = toBase' [] v where
  toBase' a 0 = a
  toBase' a v = toBase' (r:a) q where (q,r) = v `divMod` b
 
fromBase :: Int -> [Int] -> Int
fromBase b ds = foldl' (\n k -> n * b + k) 0 ds
 
toAlphaDigits :: [Int] -> String
toAlphaDigits = map convert where
  convert n | n < 10    = chr (n + ord '0')
            | otherwise = chr (n + ord 'a' - 10)
 
fromAlphaDigits :: String -> [Int]
fromAlphaDigits = map convert where
 convert c | isDigit c = ord c - ord '0'
           | isUpper c = ord c - ord 'A' + 10
           | isLower c = ord c - ord 'a' + 10

fromDigits :: String -> [Int]
fromDigits = map convert where
 convert c | isDigit c = ord c - ord '0'
           

data Flag
    = Blanks                -- -b
    | Help                  -- --help
    deriving (Eq,Ord,Enum,Show,Bounded)
 
flags =
   [Option ['b'] []       (NoArg Blanks)
        "Implies the -n option but doesn't count blank lines."
   ,Option []    ["help"] (NoArg Help)
        "Print this help message"
   ]
 
parse argv = case getOpt Permute flags argv of
    (args,fs,[]) -> do
        let files = if null fs then ["-"] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitWith ExitSuccess
            else return (nub (concatMap set args), files)
 
    (_,_,errs)      -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
 
    where header = "Usage: metalink [-b] [file ...]"
 
     
         
          set f      = [f]

showone::(String,[(Int,String)])->String
showone (a,b) = a ++ showdata b

showtwo::(Int,String)->String
showtwo (a,b) | (a > 0 ) || (a==0) = "@" ++ ( toAlphaDigits ( toBase 16 a))  ++ " " ++  b ++ "\n"
              | otherwise = ""

showdata::[(Int,String)]->String
showdata [] = []
showdata (x:xs) = showoneline x ++ showdata xs

showoneline::(Int,String)->String
showoneline (a,b) = show a ++ " " ++ b ++ " "

comparefst::(Int,String)->(Int,String)->Ordering
comparefst (a,b) (c,d) | (a < c ) = LT
                       | otherwise = GT

comparefst2::(Int,String,String)->(Int,String,String)->Ordering
comparefst2 (a,b,c) (d,e,f) | (a < d ) = LT
                       | otherwise = GT

comparethird::(Int,String,Int)->(Int,String,Int)->Ordering
comparethird (a,b,c) (d,e,f) | (c < f ) = LT
                       | otherwise = GT

expandindata::[(String,[(Int,String)])]->[(Int,String,String)]
expandindata [] =[]
expandindata (x:xs) = (expandone x ) ++ expandindata xs

expandone::(String,[(Int,String)])->[(Int,String,String)]
expandone (a,[]) = []
expandone (a,(x:xs)) = [expander a x] ++ (expandone (a,xs) )

expander::String->(Int,String)->(Int,String,String)
expander a (b,c) = (b,a,c)

triplecomp::(Int,String,String)->(Int,String,String)->Bool
triplecomp (a,b,c) (d,e,f) = ( a == d )

triplecomp3::(Int,String,Int)->(Int,String,Int)->Bool
triplecomp3 (a,b,c) (d,e,f) = ( c == f )

lengt1::[a]->Bool
lengt1 xs = (length xs) > 1

checkindata s = concat $ filter (lengt1) $ groupBy (triplecomp) $ sortBy (comparefst2)( expandindata s)

checklengths s = groupBy (triplecomp3) $ sortBy (comparethird) (map (length3)( expandindata s))

length3::(Int,String,String)->(Int,String,Int)
length3 (a,b,c) = (a,b,(length c))

formatderrors::[(Int,String,String)]->[String]
formatderrors [] = []
formatderrors (x:xs) = [formataderror x] ++ formatderrors xs

formatlerrors::[[(Int,String,Int)]]->[String]
formatlerrors [] = []
formatlerrors l | (length l == 1) = []
formatlerrors l@(x:xs) | (length l > 1 ) = ["Multple bitfield lengths" ++ "\n"]

formataderror::(Int,String,String)->String
formataderror (a,b,c) = "Duplicate address: " ++ show a ++ "From file: " ++ b ++ "\n"

main = do
    (args, files) <- getArgs >>= parse
    indata <- mapM rp files          -- read the data from each file in the list
    let sortedin = sortBy (comparefst)  (concat  (map (snd) indata))  -- sort the data
    let lengtherrors = concat (formatlerrors $ checklengths indata)
    let duperrors = concat ( formatderrors $ checkindata indata )
    putStr ( lengtherrors ++ duperrors )
    writeFile "link.out"  ( concat ( map (showtwo) sortedin));
