import System.Environment
import GHC.Integer
import Data.List
import Data.Char

-- An assignment list, or macro

data Val = Ival Int | Sval String

type Assignment =  ([Int], Int )
type Symboltable = [(String,Int)]
type Paramtable = [(String,Int)]
type Parmtable=[(String,Int)]
type Macro = (String,[([Int],Val)])
type Fieldtable = [(String,[Int])]
globals::Symboltable
globals = []

--
-- microcode fields, must be lower case?
--
