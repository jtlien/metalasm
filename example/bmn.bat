makeallfields bitfields
cat header.hs bitfields defaults allfields constants macros tailn.hs main_assemble.hs  >metalasm.hs
ghc --make metalasm.hs
metapre %1.src %1.pre
metalasm  %1.pre >%1.lst
cp out.sym %1.sym
grep "^\@" %1.lst >%1.hex
