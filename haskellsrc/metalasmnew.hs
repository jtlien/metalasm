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

-- 
-- microcode fields, must be lower case?
-- 

-- A word is 
-- ORG or
-- Labels and number followed by macros any number followed by ;
--   macro is  macro_string followed by optional paramlist
--   paramlist is ( followed by expressions any number followed by )
--   params are set to $1, $2 .. $n and substituted into 
--    field definitions for the macro

bitwidth = 96

ben0=[0..2]
afd=[3]
ret=[4]
fdn=[5]
fdv=[6]
frcfirst=[7]
ben1=[8..10]
nad=[11..20]
maluzero=[21]
malu=[21..23]
quo_bits=[21..23]
enable_div_misc=[24]
override_ralu_field=[25]
residual=[26..27]
flow_xtos=[28]
exr_field=[29]
eyr_field=[30]
shift=[31..32]
mark_bit=[33]
input_lat_field=[34]
mul_cnt_msb=[34]
enable_excp=[35]
msc_a=[36..39]
msc_b=[40..43]
emit=[44..59]
msc_c=[44..47]
msc_d=[48..51]
wire_or=[48..51]
div_clk=[44..47]
rfx_wr_addr=[44..47]
emit_right_half=[52..59]
emit_right_three=[48..59]
shift_l_r=[52]
shift_ge_64=[53]
shift_dist=[54..59]
rfx_rd_addr=[52..59]
interrupt_mask=[52..59]
latx=[60]
laty=[61]
exmx=[60..61]
exl_field=[62]
eyl_field=[63]
eymx=[62..63]
flow_sel=[64..65]
sign=[64,65,74]
rf_rd_sel=[66..67]
ealu=[66..67]
zero_eymx=[68]
spare3=[69]
parity=[70]
rf_wr_sel=[71]
flipr=[72]
flipw=[73]

allfields = [("ben0",ben0),
 ("afd",afd),
 ("ret",ret),
 ("fdn",fdn),
 ("fdv",fdv),
 ("frcfirst",frcfirst),
  ("ben1",ben1),
  ("nad",nad),
  ("maluzero",maluzero),
  ("malu",malu),
  ("quo_bits",quo_bits),
  ("enable_div_misc",enable_div_misc),
  ("override_ralu_field",override_ralu_field),
  ("residual",residual),
  ("flow_xtos",flow_xtos),
  ("exr_field",exr_field),
  ("eyr_field",eyr_field),
  ("shift",shift),
  ("mark_bit",mark_bit),
  ("input_lat_field",input_lat_field),
  ("mul_cnt_msb",mul_cnt_msb),
  ("enable_excp",enable_excp),
  ("msc_a",msc_a),
  ("msc_b",msc_b),
  ("emit",emit),
  ("msc_c",msc_c),
  ("msc_d",msc_d),
  ("wire_or",wire_or),
  ("div_clk",div_clk),
  ("rfx_wr_addr",rfx_wr_addr),
  ("emit_right_half",emit_right_half),
  ("emit_right_three",emit_right_three),
  ("shift_l_r",shift_l_r),
  ("shift_ge_64",shift_ge_64),
  ("shift_dist",shift_dist),
  ("rfx_rd_addr",rfx_rd_addr),
  ("interrupt_mask",interrupt_mask),
  ("latx",latx),
  ("laty",laty),
  ("exmx",exmx),
  ("exl_field",exl_field),
  ("eyl_field",eyl_field),
  ("eymx",eymx),
  ("flow_sel",flow_sel),
  ("sign",sign),
  ("rf_rd_sel",rf_rd_sel),
  ("ealu",ealu),
  ("zero_eymx",zero_eymx),
  ("spare3",spare3),
  ("parity",parity),
  ("rf_wr_sel",rf_wr_sel),
  ("flipr",flipr),
  ("flipw",flipw)]

-- fields can have default values
defaults::[([Int],Val)]
defaults=[(afd,Ival 1),(ben1,Ival 6),(enable_div_misc,Ival 1),(override_ralu_field,Ival 1),
  (flow_xtos,Ival 1), (exr_field,Ival 1),(shift,Ival 2),(mark_bit,Ival 1),(input_lat_field,Ival 1),
  (mul_cnt_msb,Ival 1),(enable_excp,Ival 1),(msc_a,Ival 8),(msc_b,Ival 8),
  (eymx,Ival 3),(latx,Ival 1),(laty,Ival 1),
  (zero_eymx,Ival 1),(exl_field,Ival 1),(eyl_field,Ival 1)]

constants::Symboltable
constants=[("SGL",0),("DBL",1),("EXT",3),("NEAREST",0),
           ("ZERO",1),("PLUS INF",2),("MINUS INF",3),
           ("INTEGER.OVFL",0),("INTEGER.DIV0",4),("FLOATING.INVL",5),
           ("FLOATING.INEX",6)]


macrodefs:: [Macro]
--
--
-- macrodefs are just list of [(macrostring,[(fieldname,field,val)] )
--   macroname is what macro will be invoke with
--  [(fieldname,field, val)] = field name,
--                    list of fields = set of ints, val = int or string
--       if val is int, field will get val of int
--       if val is string, field will get lookup val parmTable
--       where parmTable is made up of the parms that are at the end
--       of the macro invocation in parens  e.g.  NAD(CA+3), puts CA+3 in the
--          table as [("$0", CA+3)]

macrodefs = [ ("BEN 9 NOT INT DIV 0 TRAP",[ ( "ben0",ben0, Ival 0), ("ben1",ben1,Ival 2)] ),
 ("BEN 9 CNTR EQ 0",[( "ben0",ben0, Ival 0), ("ben1",ben1, Ival 0 )] ),
 ("BEN 9 32 X 32", [ ( "ben0",ben0, Ival 0), ("ben1",ben1, Ival 3)] ),
 ("BEN 9 NOP", [ ( "ben0",ben0, Ival 0), ("ben1",ben1, Ival 4)] ),
 ("BEN 9 NOT EXR 15", [ ( "ben0",ben0,Ival 0), ("ben1",ben1,Ival 5)] ),
 ("BEN 9 NOT FRACY 00", [ ( "ben0",ben0,Ival 0), ("ben1",ben1,Ival 6)] ),
 ("BEN 9 NOT FLAG 1", [ ( "ben0",ben0, Ival 0), ("ben1",ben1,Ival 7)] ),
 ("BEN 9 NOT OV TRAP", [ ( "ben0",ben0, Ival 1), ("ben1",ben1, Ival 0)] ),
 ("BEN 9 NOT UN TRAP", [ ( "ben0",ben0, Ival 1), ("ben1",ben1,Ival 1)] ),
 ("BEN 9 NOT DZ TRAP", [ ( "ben0",ben0, Ival 1), ("ben1",ben1,Ival 2)] ),
 ("BEN 9 NOT IV TRAP", [ ( "ben0",ben0, Ival 1), ("ben1",ben1,Ival 3)] ),
 ("BEN 9 NOT IX TRAP", [ ( "ben0",ben0, Ival 1), ("ben1",ben1, Ival 4)] ),
 ("BEN 9 NOT UNORDERED REL", [ ( "ben0",ben0, Ival 1), ("ben1",ben1, Ival 5)] ),
 ("BEN 9 MAG SUB", [ ( "ben0",ben0, Ival 1), ("ben1",ben1,Ival 6)] ),
 ("BEN 9 NOT IX TRAP COND AND IX TRAP", [ ( "ben0",ben0, Ival 1), ("ben1",ben1,Ival 7)] ),
 ("BEN 9 NOT LIVE IX COND", [ ( "ben0",ben0, Ival 2), ("ben1",ben1, Ival 0)] ),
 ("BEN 9 NOT PREC MSB", [ ( "ben0",ben0, Ival 2), ("ben1",ben1,Ival 1)] ),
 ("BEN 9 NOT FLUSH TO ZERO", [ ( "ben0",ben0, Ival  2), ("ben1",ben1,Ival 2)] ),
 ("BEN 9 NOT SIGN YR", [ ( "ben0",ben0, Ival 2), ("ben1",ben1, Ival 3)] ),
 ("BEN 9 NOT FRACX F BIT", [ ( "ben0",ben0, Ival 2), ("ben1",ben1,Ival 4)] ),
 ("BEN 9 NOT PACK I BIT", [ ( "ben0",ben0,Ival 2), ("ben1",ben1,Ival 5)] ),
 ("BEN 9 MALU EQ 0", [ ( "ben0",ben0, Ival 2), ("ben1",ben1,Ival 6)] ),
 ("BEN 9 NOT XNO", [ ( "ben0",ben0, Ival 2), ("ben1",ben1, Ival 7)] ),
 ("BEN 9 NOT EX0 18", [ ( "ben0",ben0, Ival 3), ("ben1",ben1,Ival 0)] ),
 ("BEN 9 NOT EX0 19", [ ( "ben0",ben0, Ival 3), ("ben1",ben1,Ival 1)] ),
 ("BEN 9 NOT POST NORM", [ ( "ben0",ben0, Ival 3), ("ben1",ben1,Ival 2)] ),
 ("BEN 9 FRAC OVFL", [ ( "ben0",ben0, Ival  3), ("ben1",ben1, Ival 3)] ),
 ("BEN 9 NOT MUL OR DIV 99", [ ( "ben0",ben0, Ival  3), ("ben1",ben1, Ival 4)] ),
 ("BEN 9 MALU 99-63 EQ 0", [ ( "ben0",ben0, Ival  3), ("ben1",ben1, Ival 5)] ),
 ("BEN 9 NOT SIGN EYL", [ ( "ben0",ben0, Ival  3), ("ben1",ben1, Ival 6)] ),
 ("BEN 9 MEM OP", [ ( "ben0",ben0, Ival  3), ("ben1",ben1, Ival 7)] ),
 ("BEN 8 EALU 00", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 0)] ),
 ("BEN 9 EALU EQ 0", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 0)] ),
 ("BEN 8 FLAG 1", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 1)] ),
 ("BEN 9 NOT FLAG 2", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 1)] ),
 ("BEN 8 RMODE MSB", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 2)] ),
 ("BEN 9 NOT RMODE LSB", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 2)] ),
 ("BEN 8 PREC MSB", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 3)] ),
 ("BEN 9 NOT EXP OVLF", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 3)] ),
 ("BEN 8 EXP UNFL", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 4)] ),
 ("BEN 9 NOT EXP OVFL", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 4)] ),
 ("BEN 8 SPARE 3", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 5)] ),
 ("BEN 9 NOT INT OVFL", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 5)] ),
 ("BEN 8 EX1 OR EY1 26", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 6)] ),
 ("BEN 9 NOT EX0 OR EY0", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 6)] ),
 ("BEN 8 XN0", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 7)] ),
 ("BEN 9 NOT YN0", [ ( "ben0",ben0, Ival  4), ("ben1",ben1, Ival 7)] ),
 ("BEN 8 XINF", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 0)] ),
 ("BEN 9 NOT YINF", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 0)] ),
 ("BEN 8 XNAN", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 1)] ),
 ("BEN 9 NOT YNAN", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 1)] ),
 ("BEN 8 EX1 OR EY1 2A", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 2)] ),
 ("BEN 9 NOT EX0 AND EY0", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 2)] ),
 ("BEN 8 EXT PREC", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 3)] ),
 ("BEN 9 ALU DATA NOT VALID", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 3)] ),
 ("BEN 8 NOT FADD TYPE", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 4)] ),
 ("BEN 9 NOT MUL", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 4)] ),
 ("BEN 8 EXR 00", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 5)] ),
 ("BEN 9 SPARE 4", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 5)] ),
 ("BEN 8 SPARE 5", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 6)] ),
 ("BEN 9 NOT DEC CARRY OUT", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 6)] ),
 ("BEN 8 SPARE 6", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 7)] ),
 ("BEN 9 NOT ROUNDED", [ ( "ben0",ben0, Ival  5), ("ben1",ben1, Ival 7)] ),
 ("BEN 7 ALUOP 2", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 0)] ),
 ("BEN 8 ALUOP 3", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 0)] ),
 ("BEN 9 NOT ALUOP 4", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 0)] ),
 ("BEN 7 XN0", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 1)] ),
 ("BEN 8 EY0 39", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 1)] ),
 ("BEN 9 NOT FY0 39", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 1)] ),
 ("BEN 7 EX1", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 2)] ),
 ("BEN 8 EX0", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 2)] ),
 ("BEN 9 NOT FX0", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 2)] ),
 ("BEN 7 EY1", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 3)] ),
 ("BEN 8 EY0 3C", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 3)] ),
 ("BEN 9 NOT FY0 3C", [ ( "ben0",ben0, Ival  7), ("ben1",ben1, Ival 3)] ),
 ("SIGN ADD", [ ( "sign",sign, Ival  0) ]),
 ("SIGN MUL OR DIV", [ ( "sign",sign, Ival  1) ]),
 ("SIGN X OP", [ ( "sign",sign, Ival  2) ]),
 ("SIGN Y OP", [ ( "sign",sign, Ival  3) ]),
 ("SIGN ZERO", [ ( "sign",sign, Ival  4) ]),
 ("SIGN ONE", [ ( "sign",sign, Ival  5) ]),
 ("SIGN XR", [ ( "sign",sign, Ival  6) ]),
 ("SIGN RMODE", [ ( "sign",sign, Ival  7) ]),
 ("EXMX EYL", [ ( "exmx",exmx, Ival  0) ]),
 ("EXMX ADD", [ ( "exmx",exmx, Ival  0) ]),
 ("EXMX EXL", [ ( "exmx",exmx, Ival  1) ]),
 ("EXMX ZERO", [ ( "exmx",exmx, Ival  2) ]),
 ("EXMX EXR", [ ("exmx",exmx, Ival  3) ]),
 ("EYMX EYR", [ ( "eymx",eymx, Ival  0) ]),
 ("EYMX PRIO ENCODE", [ ( "eymx",eymx, Ival  1) ]),
 ("EYMX EMIT", [ ( "eymx",eymx, Ival  2),("emit",emit, Sval "$0") ]), 
 ("EYMX EMIT SHORT", [ ( "eymx",eymx, Ival 2),("emit_right_three",emit_right_three,Sval "$0") ]),
 ("EYMX EYL", [ ("exmx",exmx, Ival 3) ]),
 ("EYMX ZERO", [("zero_eymx",zero_eymx, Ival 0 )]),
 ("EALU HALF X", [("ealu",ealu,Ival 0)]),
 ("EALU X+Y", [("ealu",ealu,Ival 1)]),
 ("EALU PASS X", [("ealu",ealu,Ival 1),("zero_eymx",zero_eymx, Ival 0)]),
 ("EALU PASS Y", [("ealu",ealu,Ival 1),("exmx",exmx,Ival 2)]),
 ("EALU X-Y", [("ealu",ealu,Ival 2)]),
 ("EALU HW", [("ealu",ealu,Ival 3),("zero_eymx",zero_eymx,Ival 0)]),
 ("EALU HW NOT EYMX ZERO", [("ealu",ealu,Ival 3)]),
 ("EALU ZERO", [("ealu",ealu, Ival 0),("exmx",exmx,Ival 2)]),
 ("RF RD SEL FD RAM", [("rf_rd_sel",rf_rd_sel, Ival 0)]),
 ("RF RD SEL HW", [("rf_rd_sel",rf_rd_sel,Ival 1)]),
 ("RF RD SEL EMIT", [("rf_rd_sel",rf_rd_sel,Ival 2),("flipr",flipr,Ival 1)]),
 ("RFX RD", [("rf_rd_sel",rf_rd_sel,Ival 2),("flipr",flipr,Ival 1),("rfx_rd_addr",rfx_rd_addr,Sval "$0")]),
 ("RF RD SEL CNTR", [("rf_rd_sel",rf_rd_sel,Ival 3)]),
 ("RF WR SEL EMIT", [("rf_wr_sel",rf_wr_sel, Ival 0),("flipw",flipw,Ival 13)]),
 ("RFX WR", [("rf_wr_sel",rf_wr_sel, Ival 0),("rfx_wr_addr",rfx_wr_addr,Sval "$0"),("flipw",flipw,Ival 1)]),
 ("RFX WR SEL CNTR", [("rf_wr_sel",rf_wr_sel,Ival 1)]),
 ("FLOW SEL XY", [("flow_sel",flow_sel, Ival 0)]),
 ("FLOW SEL XC", [("flow_sel",flow_sel,Ival 1)]),
 ("FLOW SEL CY", [("flow_sel",flow_sel,Ival 2)]),
 ("FLOW SEL CX", [("flow_sel",flow_sel,Ival 3)]),
 ("LD MUL CNTR 0", [("msc_a",msc_a,Ival 1),("mul_cnt_msb",mul_cnt_msb, Ival 0),("residual",residual, Ival 0),("override_ralu_field",override_ralu_field, Ival 0)]),
 ("LD MUL CNTR 1", [("msc_a",msc_a,Ival 1),("mul_cnt_msb",mul_cnt_msb, Ival 0),("residual",residual,Ival 1),("override_ralu_field",override_ralu_field, Ival 0)]),
 ("LD MUL CNTR 2", [("msc_a",msc_a,Ival 1),("mul_cnt_msb",mul_cnt_msb, Ival 0),("residual",residual,Ival 2),("override_ralu_field",override_ralu_field, Ival 0)]),
 ("LD MUL CNTR 3", [("msc_a",msc_a,Ival 1),("mul_cnt_msb",mul_cnt_msb, Ival 0),("residual",residual,Ival 3),("override_ralu_field",override_ralu_field, Ival 0)]),
 ("RESET MUL", [("msc_a",msc_a,Ival 1)]),
 ("SET FRC FLAG LO LAT A", [("msc_a",msc_a,Ival 2)]),
 ("MSC A SPARE 1", [("msc_a",msc_a,Ival 3)]),
 ("ENABLE RF WR", [("msc_a",msc_a,Ival 4)]),
 ("CALL", [("msc_a",msc_a,Ival 6)]),
 ("READ", [("msc_a",msc_a,Ival 11)]),
 ("ALLOW HW SET IX", [("msc_a",msc_a,Ival 12)]),
 ("PACK EXT FRAC", [("msc_a",msc_a,Ival 13)]),
 ("HW RMXS", [("msc_b",msc_b, Ival 0)]),
 ("SET SQR LAT", [("msc_b",msc_b,Ival 1)]),
 ("SET FRC FLAG LO LAT B", [("msc_b",msc_b,Ival 2)]),
 ("SET FRC FLAG LO LAT", [("msc_b",msc_b,Ival 2)]),
 ("FRC FLAG HI", [("msc_b",msc_b,Ival 3)]),
 ("FRC V BIT HI", [("msc_b",msc_b,Ival 4)]),
 ("FRC FIRST DECODE", [("frcfirst",frcfirst,Ival 1)]),
 ("LD BOTH PREC REG 0", [("msc_b",msc_b,Ival 6),("residual",residual,Sval "$0"),("rf_wr_sel",rf_wr_sel, Ival 0)]),
 ("LD BOTH PREC REG 1", [("msc_b",msc_b,Ival 6),("residual",residual,Sval "$0"),("rf_wr_sel",rf_wr_sel,Ival 1)]),
 ("LD BOTH PREC REG 2", [("msc_b",msc_b,Ival 7),("residual",residual,Sval "$0"),("rf_wr_sel",rf_wr_sel, Ival 0)]),
 ("LD BOTH PREC REG 3", [("msc_b",msc_b,Ival 7),("residual",residual,Sval "$0"),("rf_wr_sel",rf_wr_sel,Ival 1)]),
 ("FRC IR 02", [("msc_b",msc_b,Ival 11)]),
 ("LD SEQ CNTR", [("msc_b",msc_b,Ival 12),("emit_right_half",emit_right_half,Sval "$0"),("eymx",eymx,Ival 2),("ealu",ealu,Ival 1),("exmx",exmx,Ival 2)]),
 ("LD SEQ CNTR NOT EMIT", [("msc_b",msc_b,Ival 12)]),
 ("LD RF CNTRS", [("msc_b",msc_b,Ival 13)]),
 ("INC RD CNTRS", [("msc_b",msc_b,Ival 14)]),
 ("LD AND INCR RF CNTRS", [("msc_b",msc_b,Ival 15)]),
 ("SET OV", [("msc_a",msc_a,Ival 10),("msc_c",msc_c, Ival 0)]),
 ("SET UN", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 1)]),
 ("SET IV", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 2)]),
 ("SET DZ", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 3)]),
 ("SET IX", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 4)]),
 ("CLK DENORM STICK BIT", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 5)]),
 ("SET INTEGER OV", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 6)]),
 ("SET INTEGER DZ", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 7)]),
 ("SET FSUB LAT", [("flow_xtos",flow_xtos, Ival 0)]),
 ("SET FSUBR LAT", [("enable_div_misc",enable_div_misc, Ival 0)]),
 ("WR STATUS", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 10)]),
 ("SET SIGN MSB LAT", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 11)]),
 ("ASCII LAT", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 13),("maluzero",maluzero,Ival 1)]),
 ("ASCII ASIGN C BIT", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 14)]),
 ("CLR DENORM STICKY BIT", [("msc_a",msc_a,Ival 10),("msc_c",msc_c,Ival 15)]),
 ("RD STATUS", [("msc_b",msc_b,Ival 9),("msc_d",msc_d, Ival 0)]),
 ("MICRO RESET", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 1)]),
 ("MSC D SPARE 0", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 2)]),
 ("TRAP ALU", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 2)]),
 ("ZERO RALU CNTRLS", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 3)]),
 ("FRC PACK 00 HI", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 4)]),
 ("FRC HIDDEN BITS", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 5)]),
 ("SET MUL LAT", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 6)]),
 ("FRC QUO BITS", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 7),("quo_bits",quo_bits,Sval"$0")]),
 ("CLA INIT REM", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 7)]),
 ("LD RMODE REG", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 9),("residual",residual,Sval "$0")]),
 ("LD MICROFLAGS", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 10),("residual",residual,Sval "$0")]),
 ("LD RALU CNTL ZERO", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 11),("residual",residual, Ival 0)]),
 ("LD RALU CNTL HALF X", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 11),("residual",residual,Ival 1)]),
 ("LD RALU CNTL X", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 11),("residual",residual,Ival 2)]),
 ("LD RALU CNTL MINUS X", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 11),("residual",residual,Ival 3)]),
 ("ASCII CLR C BIT", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 12)]),
 ("RESET MUL LAT", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 14)]),
 ("SET XI ZERO LAT", [("msc_b",msc_b,Ival 9),("msc_d",msc_d,Ival 15)]),
 ("DIV CLK QB", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 14)]),
 ("DIV CLK Q", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 13)]),
 ("DIV CLK RS", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 11)]),
 ("DIV CLK D", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 7)]),
 ("DIV CLK QB Q RS", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 8)]),
 ("DIV CLK QB RS", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 10)]),
 ("DIV CLK RS D", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 3)]),
 ("DIV CLK QB D", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 6)]),
 ("DIV CLK QB Q", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 12)]),
 ("DIV CLK Q QB", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 12)]),
 ("DIV CLK Q RS", [("msc_a",msc_a,Ival 9),("div_clk",div_clk,Ival 9)]),
 ("DIV LD R CLR S MZ", [("enable_div_misc",enable_div_misc, Ival 0),("residual",residual,Ival 2),("malu",malu, Ival 0)]),
 ("DIV LD D CLR Q", [("enable_div_misc",enable_div_misc, Ival 0),("residual",residual,Ival 3),("maluzero",maluzero,Ival 1)]),
 ("DIV LD R CLR S", [("enable_div_misc",enable_div_misc, Ival 0),("residual",residual,Ival 2),("maluzero",maluzero,Ival 1)]),
 ("DIV LD S CLR R", [("enable_div_misc",enable_div_misc, Ival 0),("residual",residual,Ival 1),("maluzero",maluzero,Ival 1)]),
 ("DIV Q OUT CLR D", [("enable_div_misc",enable_div_misc, Ival 0),("residual",residual, Ival 0)]),
 ("CLEAR Q", [("enable_div_misc",enable_div_misc, Ival 0),("residual",residual,Ival 3)]),
 ("CLEAR S", [("enable_div_misc",enable_div_misc, Ival 0),("residual",residual,Ival 2)]),
 ("CLEAR R", [("enable_div_misc",enable_div_misc, Ival 0),("residual",residual,Ival 1)]),
 ("CLEAR D", [("enable_div_misc",enable_div_misc, Ival 0),("residual",residual, Ival 0)]),
 ("MALU ZERO", [("malu",malu, Ival 0),("shift_l_r",shift_l_r, Ival 0),("shift_ge_64",shift_ge_64,Ival 1)]),
 ("ASCII TO INTEGER", [("malu",malu,Ival 1)]),
 ("ASCII X+Y", [("malu",malu,Ival 2)]),
 ("ASCII X-Y", [("malu",malu,Ival 3)]),
 ("MALU PASS Y", [("malu",malu,Ival 5)]),
 ("MALU Y-X", [("malu",malu,Ival 6)]),
 ("MALU Y+X", [("malu",malu,Ival 7)]),
 ("ALLOW FIRST DECODE", [("afd",afd, Ival 0)]),
 ("RETURN", [("ret",ret,Ival 1)]),
 ("FPA DONE NEXT", [("fdn",fdn,Ival 1)]),
 ("FPA DATA VALID", [("fdv",fdv,Ival 1)]),
 ("LAT X", [("latx",latx, Ival 0),("msc_a",msc_a,Ival 11)]),
 ("LAT Y", [("laty",laty, Ival 0),("msc_a",msc_a,Ival 11)]),
 ("EXL", [("exl_field",exl_field, Ival 0),("msc_a",msc_a,Ival 11)]),
 ("EYL", [("eyl_field",eyl_field, Ival 0),("msc_a",msc_a,Ival 11)]),
 ("EXR", [("exr_field",exr_field, Ival 0)]),
 ("EYR", [("eyr_field",eyr_field, Ival 0)]),
 ("SHIFT R EXP DIFF", [("shift",shift, Ival 0)]),
 ("SHIFT L EALU", [("shift",shift,Ival 1),("override_ralu_field",override_ralu_field, Ival 0)]),
 ("SHIFT R EALU", [("shift",shift,Ival 1),("override_ralu_field",override_ralu_field,Ival 1)]),
 ("SHIFT L EMIT", [("shift",shift,Ival 2),("shift_l_r",shift_l_r, Ival 0),("shift_dist",shift_dist,Sval "$0")]),
 ("SHIFT R EMIT", [("shift",shift,Ival 2),("shift_l_r",shift_l_r,Ival 1),("shift_dist",shift_dist,Sval "$0")]),
 ("SHIFT PASS", [("shift",shift,Ival 2),("shift_l_r",shift_l_r, Ival 0),("shift_dist",shift_dist, Ival 0)]),
 ("SHIFT L PRIO ENCODE", [("shift",shift,Ival 3)]),
 ("SHIFT ZERO", [("shift_l_r",shift_l_r, Ival 0),("shift_ge_64",shift_ge_64,Ival 1)]),
 ("IA DE", [("wire_or",wire_or,Ival 1),("msc_b",msc_b,Ival 10)]),
 ("PROD OE", [("wire_or",wire_or,Ival 2),("msc_b",msc_b,Ival 10)]),
 ("MUL OE", [("wire_or",wire_or,Ival 4),("msc_b",msc_b,Ival 10)]),
 ("DIV OE", [("wire_or",wire_or,Ival 8),("msc_b",msc_b,Ival 10)]),
 ("WIRE OR ZERO", [("wire_or",wire_or, Ival 0),("msc_b",msc_b,Ival 10)]),
 ("INPUT LAT", [("input_lat_field",input_lat_field, Ival 0)]),
 ("FLOW X TO S", [("flow_xtos",flow_xtos,Ival 1)]),
 ("FLOW X TO T", [("flow_xtos",flow_xtos, Ival 0)]),
 ("FLOW Y TO S", [("flow_xtos",flow_xtos, Ival 0)]),
 ("FLOW Y TO T", [("flow_xtos",flow_xtos,Ival 1)]),
 ("ENABLE EXCEPTIONS", [("enable_excp",enable_excp, Ival 0)]),
 ("OVERRIDE RALU", [("override_ralu_field",override_ralu_field, Ival 0)]),
 ("RALU ZERO", [("override_ralu_field",override_ralu_field, Ival 0),("malu",malu, Ival 0)]),
 ("MARK", [("mark_bit",mark_bit, Ival 0)]),
 ("HALT", [("mark_bit",mark_bit, Ival 0)]),
 ("TWO WORDS TO ALU AFTER TRAP", [("fdn",fdn, Ival 0),("fdv",fdv,Ival 1)]),
 ("EXCP MSG TO ALU AFTER TRAP", [("fdn",fdn,Ival 1),("fdv",fdv, Ival 0)]),
 ("ONE WORD TO ALU AFTER TRAP", [("fdn",fdn,Ival 1),("fdv",fdv,Ival 1)]),
 ("FREM NOP TERMINATION", [("fdn",fdn, Ival 0),("fdv",fdv,Ival 1)]),
 ("FREM QUOTIENT DATA VALID", [("fdn",fdn,Ival 1),("fdv",fdv,Ival 1)]),
 ("FREM REMAINDER DATA VALID", [("fdv",fdv,Ival 1)]),
 ("EMIT RIGHT HALF", [("emit_right_half",emit_right_half,Sval "$0")]),
 ("NAD",[("nad",nad,Sval "$0")]) ]

-- Start of routines

-- trim space from end and beginning of string
--
trim x = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace x

-- set the default bits
defaultbits::[(Int,Int)]
defaultbits = concatMap assign  defaults 

seceq1::(Int,Int)->Bool
seceq1 (a,b) = (b==1)

-- Make a list of the default to 1 bits
defaultsetbits::[(Int,Int)]
defaultsetbits = filter seceq1 defaultbits

-- groupby KEY SEQ returns [(keyval, [seq elements]) ...]
--groupby :: Eq b => (a -> b) -> [a] -> [(b, [a])]
--groupby _ [] = []
--groupby k (x:xs) = (k x, a):groupby k b
--where (a,b) = group2 k (k x) (x:xs);

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
--
--
--
sbitxerror:: (Int,Int,String,String,String)->String
sbitxerror (a,b,c,d,e) = "bit " ++ show a ++ " set by macro " ++ c ++ " field " ++ d ++ " to val = " ++ show( b) ++ "; "
--
--
--
abitxerror:: [(Int,Int,String,String,String)] -> String
abitxerror xs = foldr ( (++) . sbitxerror) [] xs
--
--
--
bitxerror::[[(Int,Int,String,String,String)]] -> String
bitxerror [] = []
bitxerror (x:xs) = abitxerror x  ++ "end"  ++ bitxerror xs

secund (a,b,c,d,e) =b


valCount::[(Int,Int,String,String,String)]-> Int
valCount x = length ( nub ( sort ( map secund x ) ) ) 
--valCount x = 1

--
-- filter out all the bit assignments and get the ones that
--   have different values to set the bit to
--
filterValsdiff:: [[(Int,Int,String,String,String)]]->[[(Int,Int,String,String,String)]] 
filterValsdiff [] = []
filterValsdiff (x:xs) | valCount x  > 1 = x : filterValsdiff xs
                      | valCount x == 1 = filterValsdiff xs


filterLen::Int->[[a]]->[[a]]
filterLen n [] = []
filterLen n (x:xs) | length x  > n = x : filterLen n  xs
                   | length x  == n = filterLen n xs
--
--  dis assemble an Integer to its bitfields
--
disassembleFields::Integer->[(String,[Int],Integer)]
disassembleFields x = map ( evalFields x ) allfields
--
--  disassemble a hex string 
--
disassembleHex::String->[(String,[Int],Integer)]
disassembleHex str = disassembleFields ( read ("0x" ++ str))
--
--        produce disassembly output for a given hex string
--
fmtDisassembly::String->String
fmtDisassembly str = "\n" ++ fmtDisassemblyFields (disassembleHex str) ++ "\n\n" ++ concatMap (fmtDisassemblyMacros inval) (reverse ( tail (reverse (filterMacroDefs inval)))) ++ fmtDisassemblyMacrosNC inval (last (filterMacroDefs inval)) ++ ";" ++ "\n"
            where   inval = read ("0x" ++ str)
--
--
--
fmtDisassemblyInlines::(String,String)->String
fmtDisassemblyInlines (addr,hexval) = "\n" ++ addr ++ " " ++ hexval ++ "\n" ++ fmtDisassembly (reverse (map (reverseOneHex) hexval))
--
--
fmtDisassemblyAlllines::[(String,String)]->String
fmtDisassemblyAlllines xs = concatMap fmtDisassemblyInlines xs
--
--
fmtDisassemblyInput::[String]->String
fmtDisassemblyInput xs = fmtDisassemblyAlllines ( map splitLineIn xs)
--
--
splitLineIn::String->(String,String)
splitLineIn str = (dropWhile ( == '@') a, trim b )
                 where (a,b) = (head (words str), head (tail  (words str)))
--
--
--
fmtDisassemblyFields::[(String,[Int],Integer)]->String
fmtDisassemblyFields xs | length xs == 1 = fmtDisassemblyFieldNC (head xs) ++ ";"
fmtDisassemblyFields xs = foldr ( (++) . fmtDisassemblyField) " " (reverse ( tail (reverse xs))) ++ fmtDisassemblyFieldNC (last xs) ++ ";"
--
--
--
fmtDisassemblyField::(String,[Int],Integer)->String
fmtDisassemblyField (name,bits,value) = name ++ "=" ++ show value ++ ", "
--
--
--
fmtDisassemblyFieldNC::(String,[Int],Integer)->String
fmtDisassemblyFieldNC (name,bits,value) = name ++ "=" ++ show value 

--
--
--
evalFields::Integer->(String,[Int])->(String,[Int],Integer)
evalFields x (a, b) = ( a, b, evalField x b)
--
--
--
getAField::Integer->(String,[Int],Val)->(String,[Int],Integer)
getAField  b (fname,flst,_) = (fname,flst, evalField b flst)
--
--
--
fsteq:: (Int,Int,String,String,String)->(Int,Int,String,String,String)->Bool
fsteq (x,y,z,r,t) (u,v,w,s,o) = x== u
--
--
--
rangetostr::(Int,Int,String)->(Int,Int,String,String)
rangetostr (a,b,c) = (a, b, c, show a )

firstfour (a,b,c,d,e)=(a,b,c,d)

--
--  Create a string that list errors,( if any) in a bit assignment
--
errorstrfrombitlist::[([(Int,Int)],String,String,String)]->String
errorstrfrombitlist s = bitxerror (  filterValsdiff (filterLen 1  ( groupBy fsteq  (expandbitlist s )  ) ))
--
-- Convert integer to hex ( on hex digit)
--
hexfromint:: Int -> Char
hexfromint x = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']!! x
--
--
--
hexfromints::[Int] -> String
hexfromints xs = map hexfromint xs 

-- compare where (x,0) wins over (x,1) or (x,2)
--
comparefst:: (Int,Int) -> (Int,Int) -> Ordering  
comparefst (a,c) (b,d) | ((3 * a) + c) <  ((3* b) + d ) = LT
                       | ((3 * a) + c) ==  ((3* b) + d )  = EQ
                       | otherwise = GT

frstdiv4eq0::(Int,Int) -> (Int,Int) ->Bool
frstdiv4eq0 (a,_) (b,_)= div a 4  ==  div b 4
--
--
--
setbit:: (Int,Int) -> Int
setbit (x,y) | y < 2 = (2 ^ ( 3 - ( mod x  4))) * y
setbit (x,y) | y == 2 = 0                         
--
--
--
setbits::[(Int,Int)] -> Int
setbits xs = foldr ((+) . setbit) 0 xs

-- produce blank bits  all x with x % 4 == 0 in (x,2)
blankbits:: [(Int,Int)] ->[(Int,Int)]
blankbits x = zip (  [a | a <-[0..(biggest x)], mod a 4 == 0 ] ) ( replicate (biggest x) 2 )
   where biggest n = maximum ( map ( fst) n ) 

--
--
fstSame::(Int,Int)->(Int,Int)->Bool
fstSame  (a,b) (c,d) = (a==c)
--
--
--
testto  =   groupBy frstdiv4eq0 

-- combine list of setbits with list of default setbits=1
--
--    assumes sortBy will put all the bits from the field bit assignments
--           before the default bit assigns
--           the nubBy will pick the field bit assignment if there is no
--              default or not, and will pick up the default if there is no
--              field assignment
--
combine::[(Int,Int)]->[(Int,Int)]
combine x  = nubBy (fstSame)  ( sortBy ( comparefst) ( x ++ defaultsetbits ) ) 
--
-- A set of (bitpos, val=0,1,2) specifying a one or zero set at a bit
--   plus the width of the word          val=2 specifies blank bit
--
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
evalmac (x:xs) z  =  assign  x  ++   ( evalmac xs  z )

-- given an integer, get the list of bits, lowest to highest

bitsfromints:: Int -> [Int]
bitsfromints 0 = []
bitsfromints n = mod n 2  : bitsfromints ( div n 2 )
--
-- given an integer, get  the list of bits, highest to lowest
--
bitsfromintsrev::Integer -> [Int]
bitsfromintsrev 0 = []
bitsfromintsrev n = ( bitsfromintsrev ( div n 2)) ++ [fromInteger (mod n 2)]
-- 
-- given a list of [0 or 1] give the integer corresponding to that list of bits
--
intfrombits::[Int]->Integer
intfrombits x =  sum (map (multPair) (zip x ( map (2^) [0..])) )
--
--
multPair::(Int,Integer)->Integer
multPair (x,y) = (toInteger x)*y
--
--    get the nth bit of an Integer, return as 0 or 1
--
getABit::Integer->Int->Int
getABit val bitpos | (bitpos < 0) = error "bad bitpos"
getABit val bitpos | ( andInteger val (2^bitpos) == 2^bitpos) = 1
getABit val bitpos | otherwise = 0
--
--given an Integer,and a list of bit positions, get a list of the
--   corresponding bits
--
getBitList::Integer->[Int]->[Int]
getBitList  val xs = map (getABit val ) xs 
--
-- given an Integer and a list of bit positions, get the Integer value of thos
--     corresponding to those bits
--
getBitListVal::Integer->[Int]->Integer
getBitListVal val ls = intfrombits ( reverse (getBitList val ls))
--
-- Determine if a list of booleans are all true
--
allTrue::[Bool]->Bool
allTrue = foldr  (&&) True 
--
--  evaluate a macro based on the the given bits (inval integer)
--     to see if all the field values match 
--
evalMacro::Integer->Macro->Bool
evalMacro inval (mname,mlst)  = allTrue ( map ( fieldTrue  inval) mlst )
--
--   determine if the field values corresponds to the bits in inval
--        specified the by the field bits
--
fieldTrue::Integer->(String,[Int], Val)->Bool
fieldTrue inval (str, a, Sval b) = True
fieldTrue  inval (str, a,Ival b) = (evalField inval  a) == b
--
--   extract the bitfield of a field and convert it to integer
--
evalField::Integer->[Int]->Integer
evalField inval fls =  getBitListVal inval fls
--
--
-- find Macros that work
--
filterMacro inval  = filter (evalMacro inval) 
--
--
filterMacroDefs::Integer->[Macro]
filterMacroDefs inval = filterMacro inval macrodefs
--
--
--
fmtDisassemblyMacros::Integer->(String,[(String,[Int],Val)])->String
fmtDisassemblyMacros b (macname,flst) = "  " ++ macname  ++  (fmtMDisField b flst) ++ "," ++ "\n"
--
--
fmtDisassemblyMacrosNC::Integer->(String,[(String,[Int],Val)])->String
fmtDisassemblyMacrosNC b (macname,flst) = "  " ++ macname  ++  (fmtMDisField b flst) 
--
--
--
fmtMDisField::Integer->[(String,[Int],Val)]->String
fmtMDisField b flst | length valflst > 0 =  "(" ++ (showSvalFields  b valflst)  ++ ")"
                      where
                        valflst = filter (hasSvalField) flst 
fmtMDisField b flst  = " "
                 
--
--
--
hasSvalField::(String,[Int],Val)->Bool
hasSvalField (fname,fblst,Sval v) = True
hasSvalField (fname,fblst,Ival v) = False
--
--
--
showSvalFields::Integer->[(String,[Int],Val)]->String
showSvalFields b [] = ""
showSvalFields b xs = foldr ((++) . showSvalField b) "" (reverse ( tail (reverse xs))) ++ showSvalFieldNC  b (last xs)
--
--
--
showSvalField::Integer->(String,[Int],Val)->String
showSvalField b (fname,fblst,Sval v) = fname ++ "=" ++ show (evalField b fblst) ++ ","
showSvalField b (fname,fblst,Ival v) = ""
--
--
--
showSvalFieldNC::Integer->(String,[Int],Val)->String
showSvalFieldNC b (fname,fblst,Sval v) = fname ++ "=" ++ show (evalField b fblst) 
showSvalFieldNC b (fname,fblst,Ival v) = ""

--
--
--
bitzeropad::[Int]->Integer->[Int]
bitzeropad xs v | length xs > length ( bitsfromintsrev v) = (replicate ( length xs - length ( bitsfromintsrev v)) 0 ) ++ (bitsfromintsrev v)
bitzeropad xs v | length xs == length (bitsfromintsrev v) = bitsfromintsrev v
bitzeropad xs v | otherwise = error ("Integer does not fit in field " ++ " list = " ++ show xs ++ " integer = " ++ show v )
--
-- assign a field of bits to , just does integers
--
assign:: ([Int],Val) -> [(Int,Int)]
assign (a,Ival b) = zip a ( bitzeropad a b)
assign (a,_ ) = []

-- assign a field of bits to 
--  
assignParam:: ([Int],Val)-> Paramtable -> [(Int,Int)]
assignParam ( a, Ival b) _  =   zip a ( bitzeropad a b )
assignParam ( a, Sval b) s  = case (lookup (trim b)  s  ) of 
                   Just y ->   zip a ( bitzeropad a y )
                   Nothing -> []


-- evaluate one statement
--    Takes a list of Assignment lists, applies symbol table and current loc
--     and produces a list of set bits
--evalstate:: [[ Assignment ]]-> Paramtable -> Int  -> [(Int,Int)]
--evalstate [] _ _ = []
--evalstate (x:xs) y z  = evalmac x y z ++ (evalstate xs y z )
--
--
--
evalstate:: [[([Int],Val)]]-> Int  -> [(Int,Int)]
evalstate [] _  = []
evalstate (x:xs) z  = evalmac x z  ++ (evalstate xs  z )
--
--
--
evalstateParam:: [[([Int],Val)]]->Paramtable-> Int  -> [(Int,Int)]
evalstateParam [] _  _  = []
evalstateParam (x:xs) s z  = evalmacParam x s  z  ++ (evalstateParam xs s  z )
--
--
--
linesBysemi:: String -> [String]
linesBysemi ""         =  []
linesBysemi s          =  let (l, s') = break (== ';') s
                      in  l : case s' of
                                []      -> []
                                (_:s'') -> linesBysemi s''
--
--
--
linesBycomma:: String -> [String]
linesBycomma ""         =  []
linesBycomma s          =  let (l, s') = break (== ',') s
                      in  l : case s' of
                                []      -> []
                                (_:s'') -> linesBycomma s''
--
--
--
-- list of labels followed list of Macros
buildStatement:: String -> ([String],[String])
buildStatement s =(  fst ( separateStatement s ), linesBycomma ( head ( snd ( separateStatement s) ) ) )
--
-- parmlist is comma separated list of expressions eg  CA+3,DEC_FP+3,BEGIN+1
--
getparmlist::String->[String]
getparmlist [] = []
getparmlist s = fst( break (','==) s ) : getparmlist ( drop 1 (snd ( break (','==) s )) ) 
--
--
--
expandoneparm:: String -> (String,[String])
expandoneparm x = ( fst( break (=='(') x) , getparmlist (fst ( break (==')' ) ( drop 1 (  snd( break (=='(') x )))  )))
--
--
--
expandparm  = map expandoneparm 

--
-- parameter form of build statement
--        string statement give [labels], [macros]
-- where macro = ( macrostring, [ parmeterstrings] ) 
--
buildStatementPar:: String -> ([String],[(String,[String])])
buildStatementPar s =(  fst ( separateStatement s ), expandparm( linesBycomma ( head ( snd ( separateStatement s)) ) )  )

getlabels:: String -> [(String,Int)]
getlabels "" = []
getlabels s | length ( fst( break  (== ':') s  ))  == (length s) = [ ( fst( break ( == ':') s), 0) ]
             |  otherwise = ( fst( break  ( == ':') s) , 1 ) : getlabels ( drop 1( snd( break  ( == ':' ) s ) ))
                      
sndeq1::(String,Int)->Bool
sndeq1 (x,y) = y==1

unint::([(String,Int)],[(String,Int)])-> ([String],[String])
unint (a,b) =( ( map fst a ), ( map fst b) )

--
-- form statement into [labels] and [macros]
--
separateStatement:: String->([String],[String])
separateStatement x = unint(  partition  sndeq1 ( getlabels x ) ) 
--
--
removeSemicol::String->String
removeSemicol x = reverse ( dropWhile (';'== ) (dropWhile (' '==) ( reverse x ) ))
--
--
--
removeSpace  = dropWhile (' '==) 
--
--   lookup macros
--
doMacrolu:: String -> Maybe [(String,[Int],Val)]
doMacrolu x = lookup ( trim ( removeSemicol  x))  macrodefs 
--
--
doMac::String->[(String,[Int],Val)]
doMac x = case doMacrolu x of
           Just y  -> y
           Nothing -> []   --  ([],"Unable to find macro show(x)")
--
--
--
addname::String->(String,[Int],Val)->(String,[Int],Val,String)
addname s (str,a,b) = (str,a,b,s)
--
-- Like doMac, except keeps track of which macro string set fields
--
doMacstr::String->[(String,[Int],Val,String)]
doMacstr x = case (doMacrolu x) of
           Just y  -> map (addname x ) y
           Nothing -> []   --  ([],"Unable to find macro show(x)")
--
-- evaluate a macro instance
--        macroinstance = ( macroname, [(param_expressions) ])
--
ptablefrommacroinst:: Symboltable->(String,[(String)])->(String, Paramtable)
ptablefrommacroinst st (a,b)   = ( removeSpace a, parmtablefromlist b st 0 )

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
--
--
--
reststr::String->String
reststr s = snd( breakl  ['+','-','*','/','%',':'] s)
--
--
--
nextstr::String->String
nextstr s = fst( breakl  ['+','-','*','/','%',':'] s)
--
--
--
nextval:: String->Integer
nextval s | null s  = 0
          | head s == '#' = integerfromStr s
          | (elem ( head s) ['0','1','2','3','4','5','6','7','8','9'] ) = integerfromStr s
          | otherwise = 0

doopcr::(Integer,String)->Integer
doopcr (x,s) | null s =  x
             | otherwise =  doopcr ( doopc (x,s) )

doopc::(Integer,String) -> (Integer, String)
doopc (x,s) |  null s = (x,[])
             | head s  == '+'  = ( x + nextval ( nextstr ( drop 1 s)) , reststr (drop 1 s) )
             | head s == '-' = ( x - nextval ( nextstr (drop 1 s)), reststr ( drop 1 s) )
             | head s == '*'  = ( x * nextval ( nextstr (drop 1 s)), reststr ( drop 1 s) )
             | head s == '/'  = ( div x (nextval ( nextstr (drop 1 s))), reststr( drop 1 s))
             | head s == '%'  = ( mod x ( nextval ( nextstr (drop 1 s))), reststr( drop 1 s))
             | head s == ':'  = ( x -  ( mod x ( nextval ( nextstr (drop 1 s)))), reststr( drop 1 s))
             | null s = (x,[])
             | otherwise = (x,[])


-- span for any of the chars in a set
--
breakl::String->String->(String,String)
breakl a b = break ( `elem` a ) b
--
--
--
topow::( Char,Int)-> Int
topow (a,b) = case (elemIndex a "0123456789" ) of 
                Just y -> y * b 
                Nothing -> 0
--
--
--
topowI::( Char,Integer)-> Integer
topowI (a,b) = case elemIndex a "0123456789" of 
                Just y -> (toInteger y) * b 
                Nothing -> 0
--
--
--
topowsixteen::( Char,Int)-> Int
topowsixteen (a,b) = case elemIndex a "0123456789ABCDEF"  of 
                Just y -> y * b 
                Nothing -> 0
--
--
--
topowsixteenI::( Char,Integer)-> Integer
topowsixteenI (a,b) = case elemIndex a "0123456789ABCDEF"  of 
                Just y -> (toInteger y) * b 
                Nothing -> 0
--
--
--
topoweight::( Char,Int)-> Int
topoweight (a,b) = case elemIndex a "01234567"  of 
                Just y -> y * b 
                Nothing -> 0
--
--
--
topoweightI::( Char,Integer)-> Integer
topoweightI (a,b) = case elemIndex a "01234567"  of 
                Just y -> (toInteger y) * b 
                Nothing -> 0
--
--
--
topowtwo::( Char,Int)-> Int
topowtwo (a,b) = case (elemIndex a "01" ) of 
                Just y -> y * b 
                Nothing -> 0
--
--
--
topowtwoI::( Char,Integer)-> Integer
topowtwoI (a,b) = case (elemIndex a "01" ) of 
                Just y -> (toInteger y) * b 
                Nothing -> 0

lookupSym::String->Symboltable->Integer
lookupSym s st = case ( lookup (trim s) st) of
                  Just x -> toInteger x
                  Nothing -> 0
--
-- String is either decimal digits or #hex digits
--          or B bindigits or Ooctdigits
intfromStr::String->Int
intfromStr [] = 0
intfromStr x | ( head x == '#') = sum (map (topowsixteen)  ( zip (reverse ( drop 1 x)  ) ( map (16 ^ ) [0..] )) )
             | ( head x == 'B') = sum (map (topowtwo)  ( zip (reverse (drop 1 x  )) ( map (2 ^ ) [0..] )) )
             | ( head x == 'O') = sum (map (topoweight)  ( zip (reverse (drop 1 x  )) ( map (8 ^ ) [0..] )) )
             | otherwise = sum  (map (topow)  ( zip (reverse x  ) ( map (10 ^ ) [0..] )) )
--
--
--
integerfromStr::String->Integer
integerfromStr x | ( head x == '#') = sum (map (topowsixteenI)  ( zip (reverse ( drop 1 x)  ) ( map (16 ^ ) [0..] )) )
             | ( head x == 'B') = sum (map (topowtwoI) ( zip (reverse (drop 1 x  )) ( map (2 ^ ) [0..] )) )
             | ( head x == 'O') = sum (map (topoweightI)  ( zip (reverse (drop 1 x  )) ( map (8 ^ ) [0..] )) )
             | otherwise = sum (map (topowI)  ( zip (reverse x  ) ( map (10 ^ ) [0..] )) )
--
--
--
parseExp::String->Symboltable->Integer
parseExp s tab  | null s = 0
                | head s == '#' = integerfromStr s
                | elem ( head s) ['0','1','2','3','4','5','6','7','8','9']  = integerfromStr s
               | length ( take 1 ( reststr  s ))==0  = lookupSym ( nextstr  s )   tab 
               | otherwise  = toInteger ( doopcr ( lookupSym ( nextstr s )  tab ,  reststr s ) )


--symTable=[("ca",300),("FA.HALT",400)]

getparens:: String->(String,String)
getparens s =( fst (breakl ['('] s ), fst( breakl [')'] ( drop 1 (  snd( breakl ['('] s ) ) ) ) )

--
-- make a symbol table from the set of statement strings
-- 
mksym::[String]->[(String,Int)]
mksym x =   concatMap symfromstate ( setlocations ( parseallstate x)  0 )
--
--
--
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
--
--
--
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
setlocations (x:xs) m | ((forst x ) > -1 )=  (setloc x  m ) :  setlocations xs m 
                      | otherwise =  (setloc x m) :  setlocations xs (max ( forst ( head xs) ) (m+1) )
--
--
--
forst (a,b,c)= a

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

stripFieldNames::[(String,[Int],Val)]->[([Int],Val)]
stripFieldNames (x:xs) = stripFieldName x : stripFieldNames xs
--
--
--
stripFieldName::(String,[Int],Val)->([Int],Val)
stripFieldName (fname,lst,v) = (lst,v)
--
-- produces a list of assignments 
--    assignment is list of [(bitrange, Val)] macroname, parmtable,Errorstring
-- 
assignlistfromparmeval::[(String,Parmtable)]->[([([Int],Val)],String, Parmtable,String)]
assignlistfromparmeval [] = []
assignlistfromparmeval (x:xs) = case  lookup (trim (fst x))  macrodefs of 
                                Just y  -> ( map (stripFieldName) y, (fst x),  (snd x) , "") : assignlistfromparmeval xs                           
                                Nothing -> [([],(fst x),  (snd x), ( (fst x) ++ " Not found in macros" ))] ++ assignlistfromparmeval xs

--pack4::([Int],Val)->String->String->([Int],Val,String,String)
pack4 (a,b) c d = (a,b,c,d)

--given Parmtable, set of bits to be assigned a value from
--           ( [bit_assignment_tuples],macroname,
--             bitfields, errorstr )
--              macroname->parmtable->errorstr->assignment
--
abitfromassign::String->Parmtable->String->([Int],Val)-> ([(Int,Int)],String,String,String)
--abitfromassign s pt d x  = trace ("abit from assign = " ++ s ) (assignParam x pt, s, show ( fst ( x) ), d)
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
tobitlistfromassignlist x = concatMap  tobitlistfromassign x 
--
--
--
packfour::String->((Int,Int),String,String)->(Int,Int,String,String,String)
packfour s ((a,b),c,d) = (a,b,c,d,s)
--
--
--
bitsfromassign::([(Int,Int)],String,String,String)->[(Int,Int,String,String,String)]
bitsfromassign (a,b,c,d) = map ( packfour d ) ( zip3 a ( replicate (length a)  b ) ( replicate (length b ) c))
--
--
--
expandbitlist = foldr ((++) . bitsfromassign) [] 
--
--
--
first (a,b,c,d) = a
--
--
--
expandjustbitlist::[([(Int,Int)],String,String,String)]->[(Int,Int)]
expandjustbitlist xs = foldr ((++) . first ) [] xs
--
--
--
fourth (a,b,c,d) = d
--
--
--
anerror::([(Int,Int)],String,String,String)->String
anerror x | (length (fourth x) > 0 ) =  "ERROR: " ++ fourth x ++ "\n" 
             | otherwise = []
--
--
--
erroraccum::[([(Int,Int)],String,String,String)]->String
erroraccum xs =  foldr ((++) . anerror ) [] xs
--
-- Take a list of assignments and make into ( errorstring, result string) for this 
--    statement
-- 
frombitslisttoanswer:: [([(Int,Int)],String,String,String)]->Int->(Int,String,String)
frombitslisttoanswer x n  | not ( null ( errorstrfrombitlist x ) )  = (n, (errorstrfrombitlist x ) ++ (erroraccum x), "0")
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
--
--
--
errorprint::String->String
errorprint [] = []
errorprint s = "ERROR:" ++ s

formatstate::String->String
formatstate [] = []
formatstate s = fst ( break (==',') s) ++ "\n" ++ formatstate ( snd( break (==',') s) )


itoh x 
  | x >= 0 && x <= 9      = chr (x + ord '0')
  | x >= 10 && x <= 15    = chr (x - 10 + ord 'a' )
  | otherwise             = error ("number out of range (itoh) " ++ (show x))
--
--  take an integer and make into a hex string
--
frominttohex::Int->String
frominttohex 0 = "0"
frominttohex i = tail ( reverse (inttohex1 i) )
--
--
--
inttohex1 0 =  "0" 
inttohex1 i =    l
             where 
                 c = itoh (i `mod` 16) 
                 l1 = inttohex1 (i `div` 16)
                 l = c:l1 
--
--  Add , newline to a string
--
addlf::String->String
addlf x = x ++ ",\n"
--
--
--
tolines:: String->String
tolines x = concatMap addlf  ( take ((length ( lbc x ))-1) (lbc x )) ++ last ( lbc x)
             where lbc x = linesBycomma x

-- format answer as address++string of hex digits
--
ansformat::Int->String->String
ansformat a b | length b < 1 = []
                   | otherwise =  "@" ++ (frominttohex a ) ++ " " ++ b ++ "\n"

-- given ( address,error string, hex string answer, [labels], source string )
--   give formated output
formatstatement::(Int,String,String,[String],String)->String
formatstatement (a,b,c,d,e) | ( trim (fst ( break (=='=')  e ) ) == "ORG") = tolines(trim e) ++ ";"  ++ "\n\n" 
                  | otherwise =  (errorprint b ) ++ tolines(e) ++ ";"  ++ "\n\n"  ++ (ansformat a c )
--
--
--
fmtsym::(String,Int)->String
fmtsym (a,b) = "Symbol: " ++ trim a ++ " .." ++ show  b
--
--
--
reverseOneHex::Char->Char
reverseOneHex c = ['0','8','4','C','2','A','6','E','1','9','5','D','3','B','7','F']!!(read ("0x" ++ [c] ))

--
-- Main routine for meta assembler
--        reads in a file of statements as first argument
--        prints to standard out

main = do { s <- getArgs;
         f <- readFile ( head s);
           (putStr  ( unlines  (  map  (formatstatement) ( dostatements   ( concat ( lines f ) ))))) ; 
           (putStr ( unlines ( (map (fmtsym) (fst ( tosymtablefromraw f ) ))))); }
--         (print ( show( dostatements ( concat ( lines f )) ) )) ; }

