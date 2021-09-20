import System.Environment
import Data.List
import Data.Char

-- An assignment list, or macro

data Val = Ival Int | Sval String

type Assignment =  ([Int], Int )
type Symboltable = [(String,Int)]
type Paramtable = [(String,Int)]
type Parmtable=[(String,Int)]
type Macro = (String,[([Int],Val)])

globals::Symboltable
globals = []

--
-- microcode fields, must be lower case?
--


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
sign=[64..65]
rf_rd_sel=[66..67]
ealu=[66..67]
zero_eymx=[68]
spare3=[69]
parity=[70]
rf_wr_sel=[71]
flipr=[72]
flipw=[73]

-- fields can have default values
defaults::[([Int],Val)]
defaults=[(afd,Ival 1),(ben1,Ival 4),(enable_div_misc,Ival 1),(override_ralu_field,Ival 1),
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
-- macrodefs are just list of [(macrostring,[(field,val)] )
--   macroname is what macro will be invoke with
--  [(field, val)] = list of fields = set of ints, val = int or string
--       if val is int, field will get val of int
--       if val is string, field will get lookup val parmTable
--       where parmTable is made up of the parms that are at the end
--       of the macro invocation in parens  e.g.  NAD(CA+3), puts CA+3 in the
--          table as [("$0", CA+3)]

macrodefs = [ ("BEN 9 NOT INT DIV 0 TRAP",[ ( ben0, Ival 0), (ben1,Ival 2)] ),
 ("BEN 9 CNTR EQ 0",[( ben0, Ival 0), (ben1, Ival 0 )] ),
 ("BEN 9 32 X 32", [ ( ben0, Ival 0), (ben1, Ival 3)] ),
 ("BEN 9 NOP", [ ( ben0, Ival 0), (ben1, Ival 4)] ),
 ("BEN 9 NOT EXR 15", [ ( ben0,Ival 0), (ben1,Ival 5)] ),
 ("BEN 9 NOT FRACY 00", [ ( ben0,Ival 0), (ben1,Ival 6)] ),
 ("BEN 9 NOT FLAG 1", [ ( ben0, Ival 0), (ben1,Ival 7)] ),
 ("BEN 9 NOT OV TRAP", [ ( ben0, Ival 1), (ben1, Ival 0)] ),
 ("BEN 9 NOT UN TRAP", [ ( ben0, Ival 1), (ben1,Ival 1)] ),
 ("BEN 9 NOT DZ TRAP", [ ( ben0, Ival 1), (ben1,Ival 2)] ),
 ("BEN 9 NOT IV TRAP", [ ( ben0, Ival 1), (ben1,Ival 3)] ),
 ("BEN 9 NOT IX TRAP", [ ( ben0, Ival 1), (ben1, Ival 4)] ),
 ("BEN 9 NOT UNORDERED REL", [ ( ben0, Ival 1), (ben1, Ival 5)] ),
 ("BEN 9 MAG SUB", [ ( ben0, Ival 1), (ben1,Ival 6)] ),
 ("BEN 9 NOT IX TRAP COND AND IX TRAP", [ ( ben0, Ival 1), (ben1,Ival 7)] ),
 ("BEN 9 NOT LIVE IX COND", [ ( ben0, Ival 2), (ben1, Ival 0)] ),
 ("BEN 9 NOT PREC MSB", [ ( ben0, Ival 2), (ben1,Ival 1)] ),
 ("BEN 9 NOT FLUSH TO ZERO", [ ( ben0, Ival  2), (ben1,Ival 2)] ),
 ("BEN 9 NOT SIGN YR", [ ( ben0, Ival 2), (ben1, Ival 3)] ),
 ("BEN 9 NOT FRACX F BIT", [ ( ben0, Ival 2), (ben1,Ival 4)] ),
 ("BEN 9 NOT PACK I BIT", [ ( ben0,Ival 2), (ben1,Ival 5)] ),
 ("BEN 9 MALU EQ 0", [ ( ben0, Ival 2), (ben1,Ival 6)] ),
 ("BEN 9 NOT XNO", [ ( ben0, Ival 2), (ben1, Ival 7)] ),
 ("BEN 9 NOT EX0 18", [ ( ben0, Ival 3), (ben1,Ival 0)] ),
 ("BEN 9 NOT EX0 19", [ ( ben0, Ival 3), (ben1,Ival 1)] ),
 ("BEN 9 NOT POST NORM", [ ( ben0, Ival 3), (ben1,Ival 2)] ),
 ("BEN 9 FRAC OVFL", [ ( ben0, Ival  3), (ben1, Ival 3)] ),
 ("BEN 9 NOT MUL OR DIV 99", [ ( ben0, Ival  3), (ben1, Ival 4)] ),
 ("BEN 9 MALU 99-63 EQ 0", [ ( ben0, Ival  3), (ben1, Ival 5)] ),
 ("BEN 9 NOT SIGN EYL", [ ( ben0, Ival  3), (ben1, Ival 6)] ),
 ("BEN 9 MEM OP", [ ( ben0, Ival  3), (ben1, Ival 7)] ),
 ("BEN 8 EALU 00", [ ( ben0, Ival  4), (ben1, Ival 0)] ),
 ("BEN 9 EALU EQ 0", [ ( ben0, Ival  4), (ben1, Ival 0)] ),
 ("BEN 8 FLAG 1", [ ( ben0, Ival  4), (ben1, Ival 1)] ),
 ("BEN 9 NOT FLAG 2", [ ( ben0, Ival  4), (ben1, Ival 1)] ),
 ("BEN 8 RMODE MSB", [ ( ben0, Ival  4), (ben1, Ival 2)] ),
 ("BEN 9 NOT RMODE LSB", [ ( ben0, Ival  4), (ben1, Ival 2)] ),
 ("BEN 8 PREC MSB", [ ( ben0, Ival  4), (ben1, Ival 3)] ),
 ("BEN 9 NOT EXP OVLF", [ ( ben0, Ival  4), (ben1, Ival 3)] ),
 ("BEN 8 EXP UNFL", [ ( ben0, Ival  4), (ben1, Ival 4)] ),
 ("BEN 9 NOT EXP OVFL", [ ( ben0, Ival  4), (ben1, Ival 4)] ),
 ("BEN 8 SPARE 3", [ ( ben0, Ival  4), (ben1, Ival 5)] ),
 ("BEN 9 NOT INT OVFL", [ ( ben0, Ival  4), (ben1, Ival 5)] ),
 ("BEN 8 EX1 OR EY1 26", [ ( ben0, Ival  4), (ben1, Ival 6)] ),
 ("BEN 9 NOT EX0 OR EY0", [ ( ben0, Ival  4), (ben1, Ival 6)] ),
 ("BEN 8 XN0", [ ( ben0, Ival  4), (ben1, Ival 7)] ),
 ("BEN 9 NOT YN0", [ ( ben0, Ival  4), (ben1, Ival 7)] ),
 ("BEN 8 XINF", [ ( ben0, Ival  5), (ben1, Ival 0)] ),
 ("BEN 9 NOT YINF", [ ( ben0, Ival  5), (ben1, Ival 0)] ),
 ("BEN 8 XNAN", [ ( ben0, Ival  5), (ben1, Ival 1)] ),
 ("BEN 9 NOT YNAN", [ ( ben0, Ival  5), (ben1, Ival 1)] ),
 ("BEN 8 EX1 OR EY1 2A", [ ( ben0, Ival  5), (ben1, Ival 2)] ),
 ("BEN 9 NOT EX0 AND EY0", [ ( ben0, Ival  5), (ben1, Ival 2)] ),
 ("BEN 8 EXT PREC", [ ( ben0, Ival  5), (ben1, Ival 3)] ),
 ("BEN 9 ALU DATA NOT VALID", [ ( ben0, Ival  5), (ben1, Ival 3)] ),
 ("BEN 8 NOT FADD TYPE", [ ( ben0, Ival  5), (ben1, Ival 4)] ),
 ("BEN 9 NOT MUL", [ ( ben0, Ival  5), (ben1, Ival 4)] ),
 ("BEN 8 EXR 00", [ ( ben0, Ival  5), (ben1, Ival 5)] ),
 ("BEN 9 SPARE 4", [ ( ben0, Ival  5), (ben1, Ival 5)] ),
 ("BEN 8 SPARE 5", [ ( ben0, Ival  5), (ben1, Ival 6)] ),
 ("BEN 9 NOT DEC CARRY OUT", [ ( ben0, Ival  5), (ben1, Ival 6)] ),
 ("BEN 8 SPARE 6", [ ( ben0, Ival  5), (ben1, Ival 7)] ),
 ("BEN 9 NOT ROUNDED", [ ( ben0, Ival  5), (ben1, Ival 7)] ),
 ("BEN 7 ALUOP 2", [ ( ben0, Ival  7), (ben1, Ival 0)] ),
 ("BEN 8 ALUOP 3", [ ( ben0, Ival  7), (ben1, Ival 0)] ),
 ("BEN 9 NOT ALUOP 4", [ ( ben0, Ival  7), (ben1, Ival 0)] ),
 ("BEN 7 XN0", [ ( ben0, Ival  7), (ben1, Ival 1)] ),
 ("BEN 8 EY0 39", [ ( ben0, Ival  7), (ben1, Ival 1)] ),
 ("BEN 9 NOT FY0 39", [ ( ben0, Ival  7), (ben1, Ival 1)] ),
 ("BEN 7 EX1", [ ( ben0, Ival  7), (ben1, Ival 2)] ),
 ("BEN 8 EX0", [ ( ben0, Ival  7), (ben1, Ival 2)] ),
 ("BEN 9 NOT FX0", [ ( ben0, Ival  7), (ben1, Ival 2)] ),
 ("BEN 7 EY1", [ ( ben0, Ival  7), (ben1, Ival 3)] ),
 ("BEN 8 EY0 3C", [ ( ben0, Ival  7), (ben1, Ival 3)] ),
 ("BEN 9 NOT FY0 3C", [ ( ben0, Ival  7), (ben1, Ival 3)] ),
 ("SIGN ADD", [ ( sign, Ival  0) ]),
 ("SIGN MUL OR DIV", [ ( sign, Ival  1) ]),
 ("SIGN X OP", [ ( sign, Ival  2) ]),
 ("SIGN Y OP", [ ( sign, Ival  3) ]),
 ("SIGN ZERO", [ ( sign, Ival  4) ]),
 ("SIGN ONE", [ ( sign, Ival  5) ]),
 ("SIGN XR", [ ( sign, Ival  6) ]),
 ("SIGN RMODE", [ ( sign, Ival  7) ]),
 ("EXMX EYL", [ ( exmx, Ival  0) ]),
 ("EXMX ADD", [ ( exmx, Ival  0) ]),
 ("EXMX EXL", [ ( exmx, Ival  1) ]),
 ("EXMX ZERO", [ ( exmx, Ival  2) ]),
 ("EXMX EXR", [ (exmx, Ival  3) ]),
 ("EYMX EYR", [ ( eymx, Ival  0) ]),
 ("EYMX PRIO ENCODE", [ ( eymx, Ival  1) ]),
 ("EYMX EMIT", [ ( eymx, Ival  2),(emit, Sval "$0") ]), 
 ("EYMX EMIT SHORT", [ ( eymx, Ival 2),(emit_right_three,Sval "$0") ]),
 ("EYMX EYL", [ (exmx, Ival 3) ]),
 ("EYMX ZERO", [(zero_eymx, Ival 0 )]),
 ("EALU HALF X", [(ealu,Ival 0)]),
 ("EALU X+Y", [(ealu,Ival 1)]),
 ("EALU PASS X", [(ealu,Ival 1),(zero_eymx, Ival 0)]),
 ("EALU PASS Y", [(ealu,Ival 1),(exmx,Ival 2)]),
 ("EALU X-Y", [(ealu,Ival 2)]),
 ("EALU HW", [(ealu,Ival 3),(zero_eymx, Ival 0)]),
 ("EALU HW NOT EYMX ZERO", [(ealu,Ival 3)]),
 ("EALU ZERO", [(ealu, Ival 0),(exmx,Ival 2)]),
 ("RF RD SEL FD RAM", [(rf_rd_sel, Ival 0)]),
 ("RF RD SEL HW", [(rf_rd_sel,Ival 1)]),
 ("RF RD SEL EMIT", [(rf_rd_sel,Ival 2),(flipr,Ival 1)]),
 ("RFX RD", [(rf_rd_sel,Ival 2),(flipr,Ival 1),(rfx_rd_addr,Sval "$0")]),
 ("RF RD SEL CNTR", [(rf_rd_sel,Ival 3)]),
 ("RF WR SEL EMIT", [(rf_wr_sel, Ival 0),(flipw,Ival 13)]),
 ("RFX WR", [(rf_wr_sel, Ival 0),(rfx_wr_addr,Sval "$0"),(flipw,Ival 1)]),
 ("RFX WR SEL CNTR", [(rf_wr_sel,Ival 1)]),
 ("FLOW SEL XY", [(flow_sel, Ival 0)]),
 ("FLOW SEL XC", [(flow_sel,Ival 1)]),
 ("FLOW SEL CY", [(flow_sel,Ival 2)]),
 ("FLOW SEL CX", [(flow_sel,Ival 3)]),
 ("LD MUL CNTR 0", [(msc_a,Ival 1),(mul_cnt_msb, Ival 0),(residual, Ival 0),(override_ralu_field, Ival 0)]),
 ("LD MUL CNTR 1", [(msc_a,Ival 1),(mul_cnt_msb, Ival 0),(residual,Ival 1),(override_ralu_field, Ival 0)]),
 ("LD MUL CNTR 2", [(msc_a,Ival 1),(mul_cnt_msb, Ival 0),(residual,Ival 2),(override_ralu_field, Ival 0)]),
 ("LD MUL CNTR 3", [(msc_a,Ival 1),(mul_cnt_msb, Ival 0),(residual,Ival 3),(override_ralu_field, Ival 0)]),
 ("RESET MUL", [(msc_a,Ival 1)]),
 ("SET FRC FLAG LO LAT A", [(msc_a,Ival 2)]),
 ("MSC A SPARE 1", [(msc_a,Ival 3)]),
 ("ENABLE RF WR", [(msc_a,Ival 4)]),
 ("CALL", [(msc_a,Ival 6)]),
 ("READ", [(msc_a,Ival 11)]),
 ("ALLOW HW SET IX", [(msc_a,Ival 12)]),
 ("PACK EXT FRAC", [(msc_a,Ival 13)]),
 ("HW RMXS", [(msc_b, Ival 0)]),
 ("SET SQR LAT", [(msc_b,Ival 1)]),
 ("SET FRC FLAG LO LAT B", [(msc_b,Ival 2)]),
 ("SET FRC FLAG LO LAT", [(msc_b,Ival 2)]),
 ("FRC FLAG HI", [(msc_b,Ival 3)]),
 ("FRC V BIT HI", [(msc_b,Ival 4)]),
 ("FRC FIRST DECODE", [(frcfirst,Ival 1)]),
 ("LD BOTH PREC REG 0", [(msc_b,Ival 6),(residual,Sval "$0"),(rf_wr_sel, Ival 0)]),
 ("LD BOTH PREC REG 1", [(msc_b,Ival 6),(residual,Sval "$0"),(rf_wr_sel,Ival 1)]),
 ("LD BOTH PREC REG 2", [(msc_b,Ival 7),(residual,Sval "$0"),(rf_wr_sel, Ival 0)]),
 ("LD BOTH PREC REG 3", [(msc_b,Ival 7),(residual,Sval "$0"),(rf_wr_sel,Ival 1)]),
 ("FRC IR 02", [(msc_b,Ival 11)]),
 ("LD SEQ CNTR", [(msc_b,Ival 12),(emit_right_half,Sval "$0"),(eymx,Ival 2),(ealu,Ival 1),(exmx,Ival 2)]),
 ("LD SEQ CNTR NOT EMIT", [(msc_b,Ival 12)]),
 ("LD RF CNTRS", [(msc_b,Ival 13)]),
 ("INC RD CNTRS", [(msc_b,Ival 14)]),
 ("LD AND INCR RF CNTRS", [(msc_b,Ival 15)]),
 ("SET OV", [(msc_a,Ival 10),(msc_c, Ival 0)]),
 ("SET UN", [(msc_a,Ival 10),(msc_c,Ival 1)]),
 ("SET IV", [(msc_a,Ival 10),(msc_c,Ival 2)]),
 ("SET DZ", [(msc_a,Ival 10),(msc_c,Ival 3)]),
 ("SET IX", [(msc_a,Ival 10),(msc_c,Ival 4)]),
 ("CLK DENORM STICK BIT", [(msc_a,Ival 10),(msc_c,Ival 5)]),
 ("SET INTEGER OV", [(msc_a,Ival 10),(msc_c,Ival 6)]),
 ("SET INTEGER DZ", [(msc_a,Ival 10),(msc_c,Ival 7)]),
 ("SET FSUB LAT", [(flow_xtos, Ival 0)]),
 ("SET FSUBR LAT", [(enable_div_misc, Ival 0)]),
 ("WR STATUS", [(msc_a,Ival 10),(msc_c,Ival 10)]),
 ("SET SIGN MSB LAT", [(msc_a,Ival 10),(msc_c,Ival 11)]),
 ("LD INTERRUPT MASK", [(msc_a,Ival 10),(msc_c,Ival 12),(interrupt_mask,Sval "$0")]),
 ("ASCII LAT", [(msc_a,Ival 10),(msc_c,Ival 13),(maluzero,Ival 1)]),
 ("ASCII ASIGN C BIT", [(msc_a,Ival 10),(msc_c,Ival 14)]),
 ("CLR DENORM STICKY BIT", [(msc_a,Ival 10),(msc_c,Ival 15)]),
 ("RD STATUS", [(msc_b,Ival 9),(msc_d, Ival 0)]),
 ("MICRO RESET", [(msc_b,Ival 9),(msc_d,Ival 1)]),
 ("MSC D SPARE 0", [(msc_b,Ival 9),(msc_d,Ival 2)]),
 ("TRAP ALU", [(msc_b,Ival 9),(msc_d,Ival 2)]),
 ("ZERO RALU CNTRLS", [(msc_b,Ival 9),(msc_d,Ival 3)]),
 ("FRC PACK 00 HI", [(msc_b,Ival 9),(msc_d,Ival 4)]),
 ("FRC HIDDEN BITS", [(msc_b,Ival 9),(msc_d,Ival 5)]),
 ("SET MUL LAT", [(msc_b,Ival 9),(msc_d,Ival 6)]),
 ("FRC QUO BITS", [(msc_b,Ival 9),(msc_d,Ival 7),(quo_bits,Sval"$0")]),
 ("CLA INIT REM", [(msc_b,Ival 9),(msc_d,Ival 7)]),
 ("LD RMODE REG", [(msc_b,Ival 9),(msc_d,Ival 9),(residual,Sval "$0")]),
 ("LD MICROFLAGS", [(msc_b,Ival 9),(msc_d,Ival 10),(residual,Sval "$0")]),
 ("LD RALU CNTL ZERO", [(msc_b,Ival 9),(msc_d,Ival 11),(residual, Ival 0)]),
 ("LD RALU CNTL HALF X", [(msc_b,Ival 9),(msc_d,Ival 11),(residual,Ival 1)]),
 ("LD RALU CNTL X", [(msc_b,Ival 9),(msc_d,Ival 11),(residual,Ival 2)]),
 ("LD RALU CNTL MINUS X", [(msc_b,Ival 9),(msc_d,Ival 11),(residual,Ival 3)]),
 ("ASCII CLR C BIT", [(msc_b,Ival 9),(msc_d,Ival 12)]),
 ("RESET MUL LAT", [(msc_b,Ival 9),(msc_d,Ival 14)]),
 ("SET XI ZERO LAT", [(msc_b,Ival 9),(msc_d,Ival 15)]),
 ("DIV CLK QB", [(msc_a,Ival 9),(div_clk,Ival 14)]),
 ("DIV CLK Q", [(msc_a,Ival 9),(div_clk,Ival 13)]),
 ("DIV CLK RS", [(msc_a,Ival 9),(div_clk,Ival 11)]),
 ("DIV CLK D", [(msc_a,Ival 9),(div_clk,Ival 7)]),
 ("DIV CLK QB Q RS", [(msc_a,Ival 9),(div_clk,Ival 8)]),
 ("DIV CLK QB RS", [(msc_a,Ival 9),(div_clk,Ival 10)]),
 ("DIV CLK RS D", [(msc_a,Ival 9),(div_clk,Ival 3)]),
 ("DIV CLK QB D", [(msc_a,Ival 9),(div_clk,Ival 6)]),
 ("DIV CLK QB Q", [(msc_a,Ival 9),(div_clk,Ival 12)]),
 ("DIV CLK Q QB", [(msc_a,Ival 9),(div_clk,Ival 12)]),
 ("DIV CLK Q RS", [(msc_a,Ival 9),(div_clk,Ival 9)]),
 ("DIV LD R CLR S MZ", [(enable_div_misc, Ival 0),(residual,Ival 2),(malu, Ival 0)]),
 ("DIV LD D CLR Q", [(enable_div_misc, Ival 0),(residual,Ival 3),(maluzero,Ival 1)]),
 ("DIV LD R CLR S", [(enable_div_misc, Ival 0),(residual,Ival 2),(maluzero,Ival 1)]),
 ("DIV LD S CLR R", [(enable_div_misc, Ival 0),(residual,Ival 1),(maluzero,Ival 1)]),
 ("DIV Q OUT CLR D", [(enable_div_misc, Ival 0),(residual, Ival 0)]),
 ("CLEAR Q", [(enable_div_misc, Ival 0),(residual,Ival 3)]),
 ("CLEAR S", [(enable_div_misc, Ival 0),(residual,Ival 2)]),
 ("CLEAR R", [(enable_div_misc, Ival 0),(residual,Ival 1)]),
 ("CLEAR D", [(enable_div_misc, Ival 0),(residual, Ival 0)]),
 ("MALU ZERO", [(malu, Ival 0),(shift_l_r, Ival 0),(shift_ge_64,Ival 1)]),
 ("ASCII TO INTEGER", [(malu,Ival 1)]),
 ("ASCII X+Y", [(malu,Ival 2)]),
 ("ASCII X-Y", [(malu,Ival 3)]),
 ("MALU PASS Y", [(malu,Ival 5)]),
 ("MALU Y-X", [(malu,Ival 6)]),
 ("MALU Y+X", [(malu,Ival 7)]),
 ("ALLOW FIRST DECODE", [(afd, Ival 0)]),
 ("RETURN", [(ret,Ival 1)]),
 ("FPA DONE NEXT", [(fdn,Ival 1)]),
 ("FPA DATA VALID", [(fdv,Ival 1)]),
 ("LAT X", [(latx, Ival 0),(msc_a,Ival 11)]),
 ("LAT Y", [(laty, Ival 0),(msc_a,Ival 11)]),
 ("EXL", [(exl_field, Ival 0),(msc_a,Ival 11)]),
 ("EYL", [(eyl_field, Ival 0),(msc_a,Ival 11)]),
 ("EXR", [(exr_field, Ival 0)]),
 ("EYR", [(eyr_field, Ival 0)]),
 ("SHIFT R EXP DIFF", [(shift, Ival 0)]),
 ("SHIFT L EALU", [(shift,Ival 1),(override_ralu_field, Ival 0)]),
 ("SHIFT R EALU", [(shift,Ival 1),(override_ralu_field,Ival 1)]),
 ("SHIFT L EMIT", [(shift,Ival 2),(shift_l_r, Ival 0),(shift_dist,Sval "$0")]),
 ("SHIFT R EMIT", [(shift,Ival 2),(shift_l_r,Ival 1),(shift_dist,Sval "$0")]),
 ("SHIFT PASS", [(shift,Ival 2),(shift_l_r, Ival 0),(shift_dist, Ival 0)]),
 ("SHIFT L PRIO ENCODE", [(shift,Ival 3)]),
 ("SHIFT ZERO", [(shift_l_r, Ival 0),(shift_ge_64,Ival 1)]),
 ("IA DE", [(wire_or,Ival 1),(msc_b,Ival 10)]),
 ("PROD OE", [(wire_or,Ival 2),(msc_b,Ival 10)]),
 ("MUL OE", [(wire_or,Ival 4),(msc_b,Ival 10)]),
 ("DIV OE", [(wire_or,Ival 8),(msc_b,Ival 10)]),
 ("WIRE OR ZERO", [(wire_or, Ival 0),(msc_b,Ival 10)]),
 ("INPUT LAT", [(input_lat_field, Ival 0)]),
 ("FLOW X TO S", [(flow_xtos,Ival 1)]),
 ("FLOW X TO T", [(flow_xtos, Ival 0)]),
 ("FLOW Y TO S", [(flow_xtos, Ival 0)]),
 ("FLOW Y TO T", [(flow_xtos,Ival 1)]),
 ("ENABLE EXCEPTIONS", [(enable_excp, Ival 0)]),
 ("OVERRIDE RALU", [(override_ralu_field, Ival 0)]),
 ("RALU ZERO", [(override_ralu_field, Ival 0),(malu, Ival 0)]),
 ("MARK", [(mark_bit, Ival 0)]),
 ("HALT", [(mark_bit, Ival 0)]),
 ("TWO WORDS TO ALU AFTER TRAP", [(fdn, Ival 0),(fdv,Ival 1)]),
 ("EXCP MSG TO ALU AFTER TRAP", [(fdn,Ival 1),(fdv, Ival 0)]),
 ("ONE WORD TO ALU AFTER TRAP", [(fdn,Ival 1),(fdv,Ival 1)]),
 ("FREM NOP TERMINATION", [(fdn, Ival 0),(fdv,Ival 1)]),
 ("FREM QUOTIENT DATA VALID", [(fdn,Ival 1),(fdv,Ival 1)]),
 ("FREM REMAINDER DATA VALID", [(fdv,Ival 1)]),
 ("EMIT RIGHT HALF", [(emit_right_half,Sval "$0")]),
 ("NAD",[(nad,Sval "$0")]) ]
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
--
--
--
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

intfromhex::String->Integer
intfromhex str =      sum  (zipWith (*) (map (hexCharToInt) (reverse str)) (map (16^ ) [0..]))
--
--
hexCharToInt::Char->Integer
hexCharToInt c | c == '0' = 0
hexCharToInt c | c == '1' = 1
hexCharToInt c | c == '2' = 2
hexCharToInt c | c == '3' = 3
hexCharToInt c | c == '4' = 4
hexCharToInt c | c == '5' = 5
hexCharToInt c | c == '6' = 6
hexCharToInt c | c == '7' = 7
hexCharToInt c | c == '8' = 8
hexCharToInt c | c == '9' = 9
hexCharToInt c | c == 'A' = 10
hexCharToInt c | c == 'B' = 11
hexCharToInt c | c == 'C' = 12
hexCharToInt c | c == 'D' = 13
hexCharToInt c | c == 'E' = 14
hexCharToInt c | c == 'F' = 15



-- Convert integer to hex ( on hex digit)
hexfromint:: Int -> Char
hexfromint x = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']!! x
--
--
--
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

-- A set of (bitpos, val=0,1,2) specifying a one or zero set at a bit
--   plus the width of the word          val=2 specifies blank bit
intsfrombits::[(Int,Int)] ->Int -> [Int]i
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
--
--  get a list of 0,1 values corresponding to an int
--
bitsfromints:: Int -> [Int]
bitsfromints 0 = []
bitsfromints n = (mod n 2 ) : bitsfromints ( div n 2 )
--
--   reverse of bitsfromints
--
bitsfromintsrev::Int -> [Int]
bitsfromintsrev 0 = []
bitsfromintsrev n = ( bitsfromintsrev ( div n 2)) ++ [(mod n 2)]
--
-- assign a field of bits to , just does integers
--
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
--
--
--
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
--
-- Determine if a list of booleans are all true
--
allTrue::[Bool]->Bool
allTrue lst = foldr  (&&) True lst
--
--  evaluate a macro based on the the given bits (inval integer)
--     to see if all the field values match 
--
evalMacro::(String,[ ( [Integer], Integer)] )-> Integer->Bool
evalMacro (mname,mlst)  inval= allTrue ( map ( fieldTrue  inval) mlst )
--
--   determine if the field values corresponds to the bits in inval
--        specified the by the field bits
--
fieldTrue::Integer->([Integer], Integer)->Bool
fieldTrue  inval (a,b) = (evalField inval  a) == b
--
--   extract the bitfield of a field and convert it to integer
--
evalField::Integer->[Integer]->Int
evalField inval fls = bitsToInt ( getBits inval fls)
--
--
-- find Macros that work
--
filterMacro::Integer->[(String,[([Integer],Integer)])]->[(String,[([Integer],Integer)])]
filterMacro inval mls = filter (evalMacro inval)
      
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
                   | otherwise =  "@" ++ (frominttohex a ) ++ " " ++ b ++ "\n"

-- given ( address,error string, hex string answer, [labels], source string )
--   give formated output
formatstatement::(Int,String,String,[String],String)->String
formatstatement (a,b,c,d,e) | ( trim (fst ( break (=='=')  e ) ) == "ORG") = tolines(trim e) ++ ";"  ++ "\n\n" 
                  | otherwise =  (errorprint b ) ++ tolines(e) ++ ";"  ++ "\n\n"  ++ (ansformat a c )


fmtsym::(String,Int)->String
fmtsym (a,b) = "Symbol: " ++ trim ( a ) ++ " .." ++ "0x" ++ frominttohex b

main = do { s <- getArgs;
         f <- readFile ( head s);
         (putStr  ( unlines  (  map  (formatstatement) ( dostatements   ( concat ( lines f ) ))))) ; 
           (writeFile "out.sym" ( unlines ( (map (fmtsym) (fst ( tosymtablefromraw f ) ))))); }
--         (print ( show( dostatements ( concat ( lines f )) ) )) ; }

