OPT MODULE

MODULE  'mathieeedoubbas'

EXPORT PROC noterate(rate,note,fine,base) ->fine 0 <- 100 -> 200
DEF a,b,c,d,no,bo

  MOVE.L  note,D0
  CMP.L   base,D0
  BNE.S   nogoodno
  CMP.W   #100,fine        ->IF note=base
  BEQ     nocalcatall
nogoodno:
MOVEM.L  D3,-(A7)
  ADDQ.L  #3,D0
  MOVE.L  D0,D2
  MOVEQ   #12,D1
  DIVU    D1,D0
  EXT.L   D0
  MOVE.L  D0,no   ->no=(note+3)/12
  MULU    D1,D0
  SUB.L   D0,D2
                   ->n=(note+3)-(((note+3)/12)*12)
  EXT.L   D2
  CMP.W   #12,D2
  BCS.S   evr12okno
  MOVEQ   #0,D2
evr12okno:
  MULU    #800,D2   -> ba*(100*8)
  LEA notearray(PC),A0
  MOVE.L  fine,D0
  AND.L   #$FF,D0
  LSL.L   #3,D0
  ADD.L   D0,D2
  MOVE.L  0(A0,D2.L),c
  MOVE.L  4(A0,D2.L),d


  MOVE.L  base,D0
  ADDQ.L  #3,D0
  MOVE.L  D0,D2
  MOVEQ   #12,D1
  DIVU    D1,D0
  EXT.L   D0
  MOVE.L  D0,bo   ->bo=(base+3)/12
  MULU    D1,D0
  SUB.L   D0,D2
                  ->ba=(base+3)-(((base+3)/12)*12)
  EXT.L   D2
  CMP.W   #12,D2
  BCS.S   evr12ok
  MOVEQ   #0,D2
evr12ok:
  MULU    #800,D2   -> ba*(100*8)
  ADD.L   #800,D2
  MOVE.L  0(A0,D2.L),a
  MOVE.L  4(A0,D2.L),b


  MOVE.L  mathieeedoubbasbase,A6
  MOVEQ   #1,D0
  MOVE.L  bo,D2
  SUB.L   no,D2
  BEQ.S   oszczedzno
  BPL.S   wiekszyno
  NEG.L   D2                ->IF bo<no
  ROL.W   D2,D0
  JSR     IeeeDPFlt(A6)     -> 2^(no-bo)
  MOVE.L  c,D2
  MOVE.L  d,D3
  JSR     IeeeDPMul(A6)     -> (n+fine)*2^(no-bo)
  BRA.S   zrobno
wiekszyno:
  ROL.W   D2,D0             ->IF bo>no
  JSR     IeeeDPFlt(A6)     ->2^(bo-no)
  MOVE.L  D0,D2
  MOVE.L  D1,D3
  MOVE.L  c,D0
  MOVE.L  d,D1
  JSR     IeeeDPDiv(A6)     -> (n+fine)/2^(bo-no)
  BRA.S   zrobno
oszczedzno:
  MOVE.L  c,D0
  MOVE.L  d,D1
zrobno:
  MOVE.L  rate,A0
  MOVEM.L  (A0)+,D2/D3
  JSR     IeeeDPMul(A6)     -> ((n+fine)/2^(bo-no))*rate
  MOVE.L  a,D2
  MOVE.L  b,D3
  JSR     IeeeDPDiv(A6)     -> (((n+fine)/2^(bo-no))*rate)/ba
  MOVE.L  D0,a
  MOVE.L  D1,b
MOVEM.L  (A7)+,D3
  RETURN  a,b
nocalcatall:            ->IF note=base AND fine=0
  MOVE.L  rate,A0
  MOVE.L  (A0)+,a
  MOVE.L  (A0)+,b

ENDPROC a,b

notearray:
INCBIN 'tonetable.bin'
