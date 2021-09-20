-- Start of routines

-- trim space from end and beginning of string
--
trim x = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace x

-- set the default bits
defaultbits::[(Int,Int)]
defaultbits = concat ( map ( assign ) defaults )

seceq1::(Int,Int)->Bool
seceq1 (a,b) = (b==1)

-- Make a list of the default to 1 bits
defaultsetbits::[(Int,Int)]
defaultsetbits = filter ( seceq1) defaultbits

-- groupby KEY SEQ returns [(keyval, [seq elements]) ...]
groupby :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupby _ [] = []
groupby k (x:xs) = (k x, a):groupby k b
  where (a,b) = group2 k (k x) (x:xs);

-- get a group of consecutive elements x with (k x) == k0
group2 :: Eq b => (a -> b) -> b -> [a] -> ([a],[a])
group2 k k0 seq = (a,b) 
  where
     a = takewhile ((== k0) . k) seq
     b = drop (length a) seq

-- take consecutive elements for which pred is true
takewhile :: (a -> Bool) -> [a] -> [a]
takewhile pred [] = []
takewhile pred (x:xs)
 | not (pred x) = []
 | otherwise = x : takewhile pred xs

sbitxerror:: (Int,Int,String,String,String)->String
sbitxerror (a,b,c,d,e) = "bit " ++ show(a) ++ " set by macro " ++ c ++ " field " ++ d ++ " to val = " ++ show( b) ++ "; "

abitxerror:: [(Int,Int,String,String,String)] -> String
abitxerror [] = []
abitxerror (x:xs) = sbitxerror x  ++ abitxerror xs

bitxerror::[[(Int,Int,String,String,String)]] -> String
bitxerror [] = []
bitxerror (x:xs) = (abitxerror x ) ++ "end"  ++ (bitxerror xs)

secund (a,b,c,d,e) =b


valCount::[(Int,Int,String,String,String)]-> Int
valCount x = length ( nub ( sort ( map secund x ) ) ) 
--valCount x = 1

--
-- filter out all the bit assignments and get the ones that
--   have different values to set the bit to
filterValsdiff:: [[(Int,Int,String,String,String)]]->[[(Int,Int,String,String,String)]] 
filterValsdiff [] = []
filterValsdiff (x:xs) | (( valCount x ) > 1) = x : filterValsdiff xs
                      | (( valCount x) == 1) = filterValsdiff xs


filterLen::Int->[[a]]->[[a]]
filterLen n [] = []
filterLen n (x:xs) | ((length x ) > n) = x : filterLen n  xs
                   | ((length x ) == n) = filterLen n xs
    

fsteq:: (Int,Int,String,String,String)->(Int,Int,String,String,String)->Bool
fsteq (x,y,z,r,t) (u,v,w,s,o) = x== u

rangetostr::(Int,Int,String)->(Int,Int,String,String)
rangetostr (a,b,c) = (a, b, c, show(a) )

firstfour (a,b,c,d,e)=(a,b,c,d)

--
--  Create a string that list errors,( if any) in a bit assignment
--
errorstrfrombitlist::[([(Int,Int)],String,String,String)]->String
errorstrfrombitlist s = bitxerror (  filterValsdiff (filterLen 1  ( groupBy (fsteq) (expandbitlist s )  ) ))

-- Convert integer to hex ( on hex digit)
hexfromint:: Int -> Char
hexfromint x = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']!! x

hexfromints::[Int] -> [Char]
hexfromints [] = []
hexfromints (x:xs) = [hexfromint x] ++ hexfromints xs 

-- compare where (x,0) wins over (x,1) or (x,2)
comparefst:: (Int,Int) -> (Int,Int) -> Ordering  
comparefst (a,c) (b,d) | ( ((3 * a) + c) <  ((3* b) + d ) ) = LT
                       | ( ((3 * a) + c) ==  ((3* b) + d ) ) = EQ
                       | otherwise = GT

frstdiv4eq0::(Int,Int) -> (Int,Int) ->Bool
frstdiv4eq0 (a,_) (b,_)= ( div a 4 ) == (div b 4)

setbit:: (Int,Int) -> Int
setbit (x,y) | y < 2 = (2 ^ ( 3 - ( mod x  4))) * y
setbit (x,y) | y == 2 = 0                         

setbits::[(Int,Int)] -> Int
setbits [] = 0
setbits (x:xs) = (setbit x ) + (setbits xs)

-- produce blank bits  all x with x % 4 == 0 in (x,2)
blankbits:: [(Int,Int)] ->[(Int,Int)]
blankbits x = zip (  [a | a <-[0..(biggest x)], (mod a 4) == 0 ] ) ( replicate (biggest x) (2) )
   where biggest n = ( maximum ( map ( fst) n ) )

--
testto x  =   groupBy ( frstdiv4eq0) x

-- combine list of setbits with list of default setbits=1
combine::[(Int,Int)]->[(Int,Int)]
combine x  = nub ( sortBy ( comparefst) ( x ++ defaultsetbits ) ) 

-- A set of (bitpos, val=0,1) specifying a one or zero set at a bit
--   plus the width of the word
intsfrombits::[(Int,Int)] ->Int -> [Int]
intsfrombits x y =  map ( setbits)  ( groupBy ( frstdiv4eq0  )  ( nub (  sortBy (comparefst) ( (combine x ) ++ ( blankbits ( [(y,2 )] )) )) ) )

-- symbol table version
evalmacParam::  [([Int],Val) ] -> Paramtable -> Int -> [(Int,Int)]
evalmacParam [] _ _  = []
evalmacParam (x:xs) y z = ( assignParam x y ) ++ ( evalmacParam xs  y z )
                 
--
-- non symbol table version
--
evalmac::  [([Int],Val) ] -> Int  -> [(Int,Int)]
evalmac [] _  = []
evalmac (x:xs) z  =  (assign  x ) ++   ( evalmac xs  z )


bitsfromints:: Int -> [Int]
bitsfromints 0 = []
bitsfromints n = (mod n 2 ) : bitsfromints ( div n 2 )

bitsfromintsrev::Int -> [Int]
bitsfromintsrev 0 = []
bitsfromintsrev n = ( bitsfromintsrev ( div n 2)) ++ [(mod n 2)]

-- assign a field of bits to , just does integers
assign:: ([Int],Val) -> [(Int,Int)]
assign (a,Ival b) = zip a (bitsfromintsrev b )
assign (a,_ ) = []

-- assign a field of bits to 
--  
assignParam:: ([Int],Val)-> Paramtable ->  [(Int,Int)]
assignParam (a, Ival b) _  =  zip a ( bitsfromintsrev b )
assignParam (a, Sval b) s  = case (lookup (trim b)  s  ) of 
                   Just y ->   zip a ( bitsfromintsrev y )
                   Nothing -> []


-- evaluate one statement
--    Takes a list of Assignment lists, applies symbol table and current loc
--     and produces a list of set bits
--evalstate:: [[ Assignment ]]-> Paramtable -> Int  -> [(Int,Int)]
--evalstate [] _ _ = []
--evalstate (x:xs) y z  = evalmac x y z ++ (evalstate xs y z )

evalstate:: [[([Int],Val)]]-> Int  -> [(Int,Int)]
evalstate [] _  = []
evalstate (x:xs) z  = evalmac x z  ++ (evalstate xs  z )

evalstateParam:: [[([Int],Val)]]->Paramtable-> Int  -> [(Int,Int)]
evalstateParam [] _  _  = []
evalstateParam (x:xs) s z  = evalmacParam x s  z  ++ (evalstateParam xs s  z )

linesBysemi:: String -> [String]
linesBysemi ""         =  []
linesBysemi s          =  let (l, s') = break (== ';') s
                      in  l : case s' of
                                []      -> []
                                (_:s'') -> linesBysemi s''
linesBycomma:: String -> [String]
linesBycomma ""         =  []
linesBycomma s          =  let (l, s') = break (== ',') s
                      in  l : case s' of
                                []      -> []
                                (_:s'') -> linesBycomma s''

-- list of labels followed list of Macros
buildStatement:: String -> ([String],[String])
buildStatement s =(  fst ( separateStatement s ), linesBycomma ( head ( snd ( separateStatement s) ) ) )

-- parmlist is comma separated list of expressions eg  CA+3,DEC_FP+3,BEGIN+1
--
getparmlist::String->[String]
getparmlist [] = []
getparmlist s = fst( break (','==) s ) : getparmlist ( drop 1 (snd ( break (','==) s )) ) 

expandoneparm:: String -> (String,[String])
expandoneparm x = ( fst( break (=='(') x) , getparmlist (fst ( break (==')' ) ( drop 1 (  snd( break (=='(') x )))  )))

expandparm::[String]->[(String,[String])]
expandparm [] = []
expandparm (x:xs) = expandoneparm x :  expandparm xs 

--
-- parameter form of build statement
--        string statement give [labels], [macros]
-- where macro = ( macrostring, [ parmeterstrings] ) 
--
buildStatementPar:: String -> ([String],[(String,[String])])
buildStatementPar s =(  fst ( separateStatement s ), expandparm( linesBycomma ( head ( snd ( separateStatement s)) ) )  )

-- skip over the labels and get the macros [(macrostr,[parmeterstrings])]
--
statementMacros:: String -> [(String,[String])]
statementMacros s = expandparm ( linesBycomma ( head (snd (separateStatement s))))

getlabels:: String -> [(String,Int)]
getlabels "" = []
getlabels s | length ( fst(break (== ':') s  ))  == (length s) = [ ( fst( break ( == ':') s), 0) ]
             |  otherwise = ( fst(break ( == ':') s) , 1 ) : getlabels ( drop 1( snd(break ( == ':' ) s ) ))
                      
sndeq1::(String,Int)->Bool
sndeq1 (x,y) = (y==1)

unint::([(String,Int)],[(String,Int)])-> ([String],[String])
unint (a,b) =( ( map fst a ), ( map fst b) )

--
-- form statement into [labels] and [macros]
--
separateStatement:: String->([String],[String])
separateStatement x = unint(  partition  sndeq1 ( getlabels x ) ) 

removeSemicol::String->String
removeSemicol x = reverse ( dropWhile (';'== ) (dropWhile (' '==) ( reverse x ) ))

removeSpace::String->String
removeSpace x = dropWhile (' '==) x

doMacrolu:: String -> Maybe [([Int],Val)]
doMacrolu x = lookup ( trim ( removeSemicol  x))  macrodefs 

doMac::String->[([Int],Val)]
doMac x = case (doMacrolu x) of
           Just y  -> y
           Nothing -> []   --  ([],"Unable to find macro show(x)")

addname::String->([Int],Val)->([Int],Val,String)
addname s (a,b) = (a,b,s)
--
-- Like doMac, except keeps track of which macro string set fields
--
doMacstr::String->[([Int],Val,String)]
doMacstr x = case (doMacrolu x) of
           Just y  -> map (addname x ) y
           Nothing -> []   --  ([],"Unable to find macro show(x)")

-- evaluate a macro instance
--        macroinstance = ( macroname, [(param_expressions) ])
ptablefrommacroinst:: Symboltable->(String,[(String)])->(String, Paramtable)
ptablefrommacroinst st (a,b)   = ((removeSpace a), parmtablefromlist b st 0 )

-- takes a valid statement to hex
--dotohex x = hexfromints ( intsfrombits ( evalstate ( map  ( doMac) x)) 96 ) 96   

-- Routines for doing macro (expression) expansion
--     
doop:: Int -> Char ->Int -> Int
doop x op _ | ( notElem  op ['+','-','*','%','/'] ) = x
doop x op y | op == '+'  = x + y
            | op == '-'  = x - y
            | op == '*'  = x * y
			| op == '/'  = div x y
			| op == '%'  = mod x y
      
reststr::String->String
reststr s = snd( breakl  ['+','-','*','/','%',':'] s)

nextstr::String->String
nextstr s = fst( breakl  ['+','-','*','/','%',':'] s)

nextval:: String->Int
nextval s | ( (length s) == 0 ) = 0
          | ((head s) == '#' )= intfromStr s
          | (elem ( head s) ['0','1','2','3','4','5','6','7','8','9'] ) = intfromStr s
          | otherwise = 0

doopcr::(Int,String)->Int
doopcr (x,s) | ( ( length s ) == 0 ) =  x
             | otherwise =  doopcr ( doopc (x,s) )

doopc::(Int,String) -> (Int, String)
doopc (x,s) | ((length s ) ==0 ) = (x,[])
             | ((head s)  == '+')  = ( x + nextval ( nextstr ( drop 1 s)) , reststr (drop 1 s) )
             | ((head s) == '-')  = ( x - nextval ( nextstr (drop 1 s)), reststr ( drop 1 s) )
	     | ((head s ) == '*')  = ( x * nextval ( nextstr (drop 1 s)), reststr ( drop 1 s) )
	     | ((head s) == '/')  = ( div x (nextval ( nextstr (drop 1 s))), reststr( drop 1 s))
	     | ((head s) == '%')  = ( mod x ( nextval ( nextstr (drop 1 s))), reststr( drop 1 s))
             | ((head s) == ':')  = ( x -  ( mod x ( nextval ( nextstr (drop 1 s)))), reststr( drop 1 s))
             | ( length s == 0 ) = (x,[])
             | otherwise = (x,[])


-- span for any of the chars in a set
--
breakl:: [Char]->String->(String,String)
breakl a b = break ( `elem` a ) b

topow::( Char,Int)-> Int
topow (a,b) = case (elemIndex a "0123456789" ) of 
                Just y -> y * b 
                Nothing -> 0

topowsixteen::( Char,Int)-> Int
topowsixteen (a,b) = case (elemIndex a "0123456789ABCDEF" ) of 
                Just y -> y * b 
                Nothing -> 0

topoweight::( Char,Int)-> Int
topoweight (a,b) = case (elemIndex a "01234567" ) of 
                Just y -> y * b 
                Nothing -> 0

topowtwo::( Char,Int)-> Int
topowtwo (a,b) = case (elemIndex a "01" ) of 
                Just y -> y * b 
                Nothing -> 0

lookupSym::String->Symboltable->Int
lookupSym s st = case ( lookup (trim s) st) of
                  Just x -> x
                  Nothing -> 0
--
-- String is either decimal digits or #hex digits
--          or B bindigits or Ooctdigits
intfromStr::String->Int
intfromStr [] = 0
intfromStr x | ( head x == '#') = foldr (+) 0 (map (topowsixteen)  ( zip (reverse ( drop 1 x)  ) ( map (16 ^ ) [0..] )) )
             | ( head x == 'B') = foldr (+) 0 (map (topowtwo)  ( zip (reverse (drop 1 x  )) ( map (2 ^ ) [0..] )) )
             | ( head x == 'O') = foldr (+) 0 (map (topoweight)  ( zip (reverse (drop 1 x  )) ( map (8 ^ ) [0..] )) )
             | otherwise = foldr (+) 0 (map (topow)  ( zip (reverse x  ) ( map (10 ^ ) [0..] )) )


parseExp::String->Symboltable->Int
parseExp s tab  | ( (length s) == 0 ) = 0
                | ((head s) == '#' )= intfromStr s
                | (elem ( head s) ['0','1','2','3','4','5','6','7','8','9'] ) = intfromStr s
               | ( length ( take 1 ( reststr  s ))==0) = (lookupSym ( nextstr  s )   tab )
               | otherwise  =  doopcr ( lookupSym ( nextstr s )  tab ,  reststr s ) 


--symTable=[("ca",300),("FA.HALT",400)]

getparens:: String->(String,String)
getparens s =( fst (breakl ['('] s ), fst( breakl [')'] ( drop 1 (  snd( breakl ['('] s ) ) ) ) )

--
-- make a symbol table from the set of statement strings
-- 
mksym::[String]->[(String,Int)]
mksym x =   concat(  map (symfromstate) ( setlocations ( parseallstate x)  0 ))

-- statement string -> [(Int=LOC,[Lables],String=Statement)]
locsfromraw::String->[(Int,[String],String)]
locsfromraw x = reverse ( drop 1 ( reverse ( setlocations ( parseallstate  (linesBysemi x ))  0 ) ) )
--
-- used to filter for second value of tuple > 0 
--
scndgtzero (a,b) = ( b > 0 )
-- 
-- Simple parse of statements to determine if the statement is org statement
--             org=xxx
--
parseState::String->(Int,[String],String )
parseState s | (trim  (fst ( break (=='=')  s ) ) == "ORG")= doorg s
             | otherwise = (-1, map (fst) ( filter (scndgtzero) (getlabels s)), s )

-- step thru the statements and
--  set the location ( first of 3-tuple) to max of last loc+1 or the value
--    of the org statement
--
scanfunc::(Int,[String],String)->(Int,[String],String)->(Int,[String],String)
scanfunc (a,b,c) (d,e,f) | ( d > 0 ) = (d, e, f)
                         | otherwise = ( (max ( a+1) d), e, f )

setloc::(Int,[String],String)->Int->(Int,[String],String)
setloc (a,b,c) d | a < 0 = (d,b,c)
                 | otherwise = ((max a d),b,c)
--
-- Take a list of (Loc,[Labels],Statement) and an initial int
--   and produce a new list of (Loc,[Labels],Statement)
--   with locations updated
setlocations::[(Int,[String],String)]->Int->[(Int,[String],String)]
setlocations [] _ = []
--setlocations x  m = scanl (scanfunc ) (m,[],[]) x
setlocations (x:xs) m | ((forst x ) > -1 )=  [(setloc x  m )] ++ setlocations xs m 
                      | otherwise =  [(setloc x  m )] ++ setlocations xs (max ( forst ( head xs) ) (m+1) )

forst (a,b,c)= a
--
-- Parse each statement  statement -> (Int=loc,[String=Label],String=Macros) 
--
parseallstate::[String]->[(Int,[String],String)]
parseallstate [] = []
parseallstate (x:xs) = parseState x : parseallstate xs 
--
-- given a parsed statement tuple (Loc,[Labels],Statement) make
--  the labels for the statement into symbol table entries
--
symfromstate::(Int,[String],String)->[(String,Int)]
symfromstate (a,b,c) =  map ( symassign a ) b 
                 where symassign x y = (y, x )
--
-- Add current 
newsymtable::Symboltable->Int->Symboltable
newsymtable a b = a ++ [("ca",b)]

--
-- org has no label  take string for an org statement and parse it
--   producing (Loc,[Labels],Statement)
--
doorg::String->(Int,[String],String)
doorg s = (intfromStr( drop 1 (snd( break (=='=') s))),[],s)

parmtablefromstring::String->Symboltable->Int->Parmtable
parmtablefromstring  s st n = [( "$"++show(n), parseExp s st) ]
--
-- Make a parm table from a list of parm strings using the current symbol table
--
parmtablefromlist:: [String]->Symboltable->Int->Paramtable
parmtablefromlist [] _ _ = []
parmtablefromlist (x:xs) st n = ( parmtablefromstring x st n ) ++ (parmtablefromlist xs st (n+1))

--
-- make a raw statement string into  list of [(String,Parmtable)]
--    or list of [(macronames, parmtable) ]

toparmevalfromraw::String->Symboltable->[(String,Parmtable)]
toparmevalfromraw s st =  map (ptablefrommacroinst st) (snd (buildStatementPar s ) )    

--
-- produces a list of assignments 
--    assignment is list of [(bitrange, Val)] macroname, parmtable,Errorstring
-- 
assignlistfromparmeval::[(String,Parmtable)]->[([([Int],Val)],String, Parmtable,String)]
assignlistfromparmeval [] = []
assignlistfromparmeval (x:xs) = case ( lookup (trim (fst x))  macrodefs ) of 
                                Just y  -> [( y, (fst x),  (snd x) , "")] ++ assignlistfromparmeval xs                           
                                Nothing -> [([],(fst x),  (snd x), ( (fst x) ++ " Not found in macros" ))] ++ assignlistfromparmeval xs

--pack4::([Int],Val)->String->String->([Int],Val,String,String)
pack4 (a,b) c d = (a,b,c,d)

--given Parmtable, set of bits to be assigned a value form ( [bit_assignment_tuples],macroname,
--             bitfields, errorstr )
abitfromassign::String->Parmtable->String->([Int],Val)-> ([(Int,Int)],String,String,String)
abitfromassign s pt d x  = (assignParam x pt, s, show ( fst ( x) ), d)

--
-- bitlist =[([ bit range, value ],  macrostring, string = [ range],error string) ]
--
--  result = [(setbit , tovalue)], macroname, rangestring, errorstring

tobitlistfromassign:: ([([Int],Val)],String,Parmtable,String)->[([(Int,Int)],String,String,String)]
tobitlistfromassign (a,b,c,d) | (length a > 0 ) = map ( abitfromassign b c d) a                         
                              | otherwise = [([],b,"",d)]




-- 
--     make list of expanded macros = [( list of bittoval assignments,
--                                         macrostring, bitfields )]
--
tobitlistfromassignlist:: [([([Int],Val)],String,Parmtable,String)]->[([(Int,Int)],String,String,String)]
tobitlistfromassignlist x = concat ( map ( tobitlistfromassign) x )

packfour::String->((Int,Int),String,String)->(Int,Int,String,String,String)
packfour s ((a,b),c,d) = (a,b,c,d,s)

bitsfromassign::([(Int,Int)],String,String,String)->[(Int,Int,String,String,String)]
bitsfromassign (a,b,c,d) = map ( packfour d ) ( zip3 a ( replicate (length a)  b ) ( replicate (length b ) c))

expandbitlist::[([(Int,Int)],String,String,String)]->[(Int,Int,String,String,String)]
expandbitlist [] = []
expandbitlist (x:xs) = bitsfromassign x ++ expandbitlist xs

first (a,b,c,d) = a

expandjustbitlist::[([(Int,Int)],String,String,String)]->[(Int,Int)]
expandjustbitlist [] = []
expandjustbitlist (x:xs) =  (first x ) ++ expandjustbitlist xs

fourth (a,b,c,d) = d


anerror::([(Int,Int)],String,String,String)->String
anerror x | (length (fourth x) > 0 ) =  "ERROR: " ++ fourth x ++ "\n" 
             | otherwise = []
             

erroraccum::[([(Int,Int)],String,String,String)]->String
erroraccum [] = []
erroraccum (x:xs) =  anerror x  ++ erroraccum xs
--
-- Take a list of assignments and make into ( errorstring, result string) for this 
--    statement
-- 
frombitslisttoanswer:: [([(Int,Int)],String,String,String)]->Int->(Int,String,String)
frombitslisttoanswer x n  | ( length ( errorstrfrombitlist x ) > 0 ) = (n, (errorstrfrombitlist x ) ++ (erroraccum x), "0")
                   | otherwise = (n, erroraccum x , hexfromints ( intsfrombits (expandjustbitlist x ) bitwidth ) )
 
--
-- Do one statement, taking from string and applying symbol table
--     gives an error string, followed by hex string of meta-assembled data
--
dostate::Symboltable->String->(Int,String,String)
dostate st s = frombitslisttoanswer ( tobitlistfromassignlist ( assignlistfromparmeval (
                       toparmevalfromraw s st ) ) ) 0
--
-- Like do state, but works on (loc,[Labels],Statement) tuple
--   symboltable entry ("ca",loc) added to symbol table to include current
--      address when expanding
dostateloc::Symboltable->(Int,[String],String)->(Int,String,String)
dostateloc st (a,b,s) | (fst ( break (=='=')  s )  == "ORG") = (a,[],[])
                      | otherwise  = frombitslisttoanswer ( tobitlistfromassignlist ( assignlistfromparmeval (
                       toparmevalfromraw s (newsymtable st a ) ) ) ) a

-- join a 3 tuple to 
--  a 2 tuple 
jointuple (a,b,c) (d,e) = (a,b,c,d,e)

-- Take a symboltable, a statement tuple ( loc, [Labels], Statement) 
--            and produce a (Loc, Error, answer, [Labels],Statement) tuple
--
dostateall::Symboltable->(Int,[String],String)->(Int,String,String,[String],String)
dostateall st (a, b,s ) = jointuple ( dostateloc st (a, b, s) ) (b,s)

-- take the input data
--  and get the symboltable and a list of statements
--
tosymtablefromraw::String->(Symboltable,[String])
tosymtablefromraw x  = (mksym ( linesBysemi x ), (linesBysemi x) )
--
-- Given input data in a string ( semicolon seperated statements), return
--   a list of (Loc,Error String, Hex String,[Labels],Source String) 
--
dostatements::String->[(Int,String,String,[String],String)]
dostatements x =  map (dostateall ( fst( tosymtablefromraw x ) ) ) (locsfromraw  x)

errorprint::String->String
errorprint [] = []
errorprint s = "ERROR:" ++ s

formatstate::String->String
formatstate [] = []
formatstate s = fst ( break (==',') s) ++ "\n" ++ formatstate ( snd( break (==',') s) )


itoh x 
  | x >= 0 && x <= 9      = chr (x + (ord '0'))
  | x >= 10 && x <= 15    = chr (x - 10 + (ord 'a'))
  | otherwise             = error ("number out of range (itoh) " ++ (show x))

frominttohex::Int->String
frominttohex 0 = "0"
frominttohex i = tail ( reverse (inttohex1 i) )

inttohex1 0 =  "0" 
inttohex1 i =    l
             where 
                 c = itoh (i `mod` 16) 
                 l1 = inttohex1 (i `div` 16)
                 l = c:l1 
                 
addlf::String->String
addlf x = x ++ ",\n"

tolines:: String->String
tolines x = concat ( map (addlf ) ( take ((length ( lbc x ))-1) (lbc x ))) ++ last ( lbc x)
             where lbc x = linesBycomma x

-- format answer as address++string of hex digits
--
ansformat::Int->String->String
ansformat a b | (length b < 1) = []
                   | otherwise =  (frominttohex a ) ++ "::" ++ b ++ "\n"

-- given ( address,error string, hex string answer, [labels], source string )
--   give formated output
formatstatement::(Int,String,String,[String],String)->String
formatstatement (a,b,c,d,e) | ( trim (fst ( break (=='=')  e ) ) == "ORG") = tolines(trim e) ++ ";"  ++ "\n\n" 
                  | otherwise =  (errorprint b ) ++ tolines(e) ++ ";"  ++ "\n\n"  ++ (ansformat a c )


selac (a,b,c) = (a,c)

mapc::(Int,String)->(Int,[(String,[String])])
mapc (a,c) = (a, statementMacros c)

filtermac::(Int,[(String,[String])])
filtermac (a,b) = ( a, filter (fsteq "NAD") b )

fsteq::String->(String,[String])->Bool
fsteq a (b,c) | (a == b) = True
              | otherwise = False

takehead::(Int,[(String,[String])])->(Int,String,String)
takehead (a,b) = (a,d) 
         where
             d = fst ( snd ) head b 


getexterns::String->[(Int,String)]
getexterns s =  map ( filtermac . mapc . selac ) locsfromraw ( concat (lines s ))
findexterns::SymbolTable->[(Int,String)]->[(Int,String)]
findexterns st b = filter (notinsymtab st) b

notinsymtab::SymbolTable->(Int,String)->Bool
notinsymtab st (a,b) | (lookup b st == -1) = True
                 | otherwise = False


fmtsym::(String,Int)->String
fmtsym (a,b) = "Symbol: " ++ trim ( a ) ++ " .." ++ show ( b) 

main = do { s <- getArgs;
         f <- readFile ( head s);
         (putStr  ( unlines  (  map  (formatstatement) ( dostatements   ( concat ( lines f ) ))))) ; 
           (putStr ( unlines ( (map (fmtsym) (fst ( tosymtablefromraw f ) )))));           (putStr ( unlines ( (map (fmtext) (findexterns (getexterns f ))))));
                
}
--         (print ( show( dostatements ( concat ( lines f )) ) )) ; }

