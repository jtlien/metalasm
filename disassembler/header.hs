
import System.Environment
import Debug.Trace
import GHC.Integer
import Data.List
import Data.Char

-- An assignment list, or macro

data Val = Ival Integer | Sval String

type Assignment =  ([Int], Int )
type Symboltable = [(String,Int)]
type Paramtable = [(String,Integer)]
type Parmtable=[(String,Integer)]
type Macro = (String,[(String,[Int],Val)])
type DisassembledField=(String,[Int],Integer)
type DisassembledFieldList=[DisassembledField]
type SplitOutLine=(String,String)
type SplitOutLineInt=(Int,Int)   

globals::Symboltable
globals = []
