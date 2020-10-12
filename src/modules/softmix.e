OPT ASM

ENUM MXCMD_SETUP=1001,
     MXCMD_END,
     MXCMD_PLAYCHANNEL,
     MXCMD_STOPCHANNEL,
     MXCMD_STOPCHANNELMASK,
     MXCMD_FREECHANNELS,
     MXCMD_SETFREQUENCY,
     MXCMD_SETVOLUME,
     MXCMD_LAST
/*
OBJECT data
  outsampledata:PTR TO INT -> 16bit, stereo
  channeldata:PTR TO channel
  channelmask:LONG
  bufwork:PTR TO LONG -> 32bit, stereo
  workframes:LONG
ENDOBJECT

OBJECT channel
  sampledata:LONG
  sampledatalength:LONG
  skip:LONG        -> = xxxxx frequency / remix frequency (xxxxx.yyyyy)
  addmodulo:LONG   -> = yyyyy
  loopframes:LONG
  volumeleft:INT   -> /256 * 100% (less eq than 32767)
  volumeright:INT  -> /256 * 100% volume left and right
  flags:LONG
  current:LONG
  pointer:LONG
  modulo:LONG -> current modulo
ENDOBJECT
*/

CONST DT_OUTSAMPLEDATA=0,
      DT_CHANNELDATA=4,
      DT_CHANNELMASK=8,
      DT_BUFWORK=12,
      DT_WORKFRAMES=16,
      DT_SIZEOF=20

SET CHF_LOOPED, CHF_BACKWARD, CHF_STEREO
ENUM CHB_LOOPED=0, CHB_STEREO, CHB_BACKWARD
      
CONST CH_SAMPLEDATA=0,
      CH_SAMPLEDATALENGTH=4,
      CH_SKIP=8,
      CH_ADDMODULO=12,
      CH_LOOPFRAMES=16,
      CH_VOLUMELEFT=20,
      CH_VOLUMERIGHT=22,
      CH_FLAGS=24,
      CH_CURRENT=28,
      CH_POINTER=32,
      CH_MODULO=36,
      CH_SIZEOF=40

begin:
        MOVEQ #0,D0
        RTS
        LONG "MX00"
        LONG "NAME"
        CHAR 'software mixing',0
        LONG "AUTH"
        CHAR 'Rafal Michalski (c) 1999',0
        LONG "CSUM", $FFFFFFFF
        BRA  md_cmd
        CHAR '$VER:midiIn mixmodule 0.1 (25.03.99)',0
md_cmd:    ->(A6 - execbase) (A0-sampleinfo) (A1-ptr to data) (D0-command) (D1-data)
        CMPI.L #MXCMD_LAST,D0
        BCS.S  md_ok
        RTS
md_ok:  SUBI.L #MXCMD_SETUP,D0
        BPL.S  md_ok2
        RTS
md_ok2: LSL.L  #2,D0
        JMP    md_jump1(PC,D0.L)
md_jump1:
        BRA    mcmd_setup
        BRA    mcmd_end
        BRA    mcmd_playchannel
        BRA    mcmd_stopchannel
        BRA    mcmd_stopchannelmask
        BRA    mcmd_freechannels
        BRA    mcmd_setfrequency
        BRA    mcmd_setvolume


->  (A1) = data
mixptr: MOVE.L  DT_CHANNELDATA(A1),A2
        MOVE.L  DT_CHANNELMASK(A1),D0
        BNE.S   something
        MOVE.L  DT_OUTSAMPLEDATA(A1),A0
        MOVE.L  DT_WORKFRAMES(A1),D0
zerolp: CLR.L   (A0)+
        DBRA    D0,zerolp
        RTS
something:
        SUBA.W  #CH_SIZEOF,A2
        MOVEQ   #-1,D1
firstlp:LEA     CH_SIZEOF(A2),A2
        ADDQ.L  #1,D1
        LSR.L   #1,D0
        BCC.S   firstlp
        EXG    D1,A0
        MOVE.L  DT_BUFWORK(A1),A5
        MOVE.L  DT_WORKFRAMES(A1),D3
        MOVE.L  CH_MODULO(A2),D4
        MOVE.L  CH_ADDMODULO(A2),A6
        MOVE.L  CH_SKIP(A2),D2
        MOVEM.W CH_VOLUMELEFT(A2),D5/D6
        MOVE.L  CH_POINTER(A2),A3
        MOVE.L  CH_CURRENT(A2),D1
        MOVE.L  CH_FLAGS(A2),D7
        ANDI.W  #CHF_BACKWARD OR CHF_STEREO,D7
        ADD.W   D7,D7
        JMP    mix_jumpf(PC,D7.W)
mix_jumpf:
        BRA     firstloopm
        BRA     firstloop
        BRA     firstloopmbk
        BRA     firstloopbk
nextloopend1:
        EXG    A0,D1
        MOVE.L  DT_CHANNELMASK(A1),D0
        BCLR    D1,D0
        MOVE.L  D0,DT_CHANNELMASK(A1)
        ADDQ.L  #1,D1
        LSR.L   D1,D0
        BNE.S   middle
firstz:
        MOVE.L  DT_OUTSAMPLEDATA(A1),A4
        MOVE.L  DT_BUFWORK(A1),A5
        MOVE.L  DT_WORKFRAMES(A1),D3
        ADDQ.W #1,D3
        ADD.W  D3,D3
        SUBQ.W #1,D3
        MOVE.W #$7FFF,D1
firstzl:
        MOVE.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            -> convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  fznothigh
        ROL.L  #8,D0
        EXT.W  D0
        EOR.W  D1,D0        ->FFFF to 8000,  0000 to 7FFF
fznothigh:
        MOVE.W D0,(A4)+
        DBRA   D3,firstzl
        RTS
nextloop1:
        EXG    A0,D1
        MOVE.L  DT_CHANNELMASK(A1),D0
        ADDQ.L  #1,D1
        LSR.L   D1,D0
        BEQ.S   firstz
middle: SUBQ.L  #1,D1
middlel:LEA     CH_SIZEOF(A2),A2
        ADDQ.L  #1,D1
        LSR.L   #1,D0
        BCC.S   middlel
        EXG     D1,A0
        MOVE.L  DT_BUFWORK(A1),A5
        MOVE.L  DT_WORKFRAMES(A1),D3
        MOVE.L  CH_MODULO(A2),D4
        MOVE.L  CH_ADDMODULO(A2),A6
        MOVE.L  CH_SKIP(A2),D2
        MOVEM.W CH_VOLUMELEFT(A2),D5/D6
        MOVE.L  CH_POINTER(A2),A3
        MOVE.L  CH_CURRENT(A2),D1
        MOVE.L  CH_FLAGS(A2),D7
        ANDI.W  #CHF_BACKWARD OR CHF_STEREO,D7
        ADD.W   D7,D7
        TST.L   D0
        BEQ.S   last
        JMP    mix_jumps(PC,D7.W)
mix_jumps:
        BRA     secloopm
        BRA     secloop
        BRA     secloopmbk
        BRA     secloopbk
nextloopend2:
        EXG     A0,D1
        MOVE.L  DT_CHANNELMASK(A1),D0
        BCLR    D1,D0
        MOVE.L  D0,DT_CHANNELMASK(A1)
        ADDQ.L  #1,D1
        LSR.L   D1,D0
        BRA.S   middle
nextloop2:
        EXG     A0,D1
        MOVE.L  DT_CHANNELMASK(A1),D0
        ADDQ.L  #1,D1
        LSR.L   D1,D0
        BRA.S   middle

last:   MOVE.L  DT_OUTSAMPLEDATA(A1),A4
        MOVE.L  A0,-(A7)
        JMP     mix_jumpl(PC,D7.W)
mix_jumpl:
        BRA     lastloopm
        BRA     lastloop
        BRA     lastloopmbk
        BRA     lastloopbk
nextloopend3:
        MOVE.L  (A7)+,D1
        MOVE.L  DT_CHANNELMASK(A1),D0
        BCLR    D1,D0
        MOVE.L  D0,DT_CHANNELMASK(A1)
        RTS
nextloop3:
        ADDQ.L  #4,A7
        RTS

        

-> A2 = channel data, A5 = buffer32, D3=buflen-1 (in frames), D4 = modulo, A6 = addmodulo, D2 = skip, D5,D6 -> voll,volr
-> first & second
-> A3 = end sample, D1 = -offset
-> A3 = start loop, D1 = +offset
-> last
-> A4 = buffer16, A3 = end sample, D1 = -offset
-> A4 = buffer16, A3 = start loop, D1 = +offset


firstloop:
INT $4CB3,$0081,$1C00 ->  MOVEM.W 0(A3,D1.L*4),D0/D7   -> D1 < 0 and increases *2 / *4 stereo
        MULS    D5,D0             -> D5 = leftvolume
        MOVE.L  D0,(A5)+          -> the xdata 24 bit RAW LEFT
        MULS    D6,D7             -> D6 = rightvolume
        MOVE.L  D7,(A5)+          -> the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            -> D4=modulo, A6=addmodulo
        ADDX.L D2,D1            -> D2=skip
        BPL.S  itsendofsample1
notendofsample1:
        DBRA   D3,firstloop
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop1
itsendofsample1:
        SUB.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample1
        BRA.S  endofsample1
clearloop1:
        CLR.L  (A5)+
        CLR.L  (A5)+
endofsample1:
        DBRA   D3,clearloop1
        BRA    nextloopend1

firstloopm:
LONG $30331A00 -> MOVE.W 0(A3,D1.L*2),D0   -> D1 < 0 and increases *2 / *4 stereo
        MOVE.W  D0,D7
        MULS    D5,D0             -> D5 = leftvolume
        MOVE.L  D0,(A5)+          -> the xdata 24 bit RAW LEFT
        MULS    D6,D7             -> D6 = rightvolume
        MOVE.L  D7,(A5)+          -> the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            -> D4=modulo, A6=addmodulo
        ADDX.L D2,D1            -> D2=skip
        BPL.S  itsendofsample1m
notendofsample1m:
        DBRA   D3,firstloopm
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop1
itsendofsample1m:
        SUB.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample1m
        BRA.S  endofsample1

secloop:
INT $4CB3,$0081,$1C00 ->  MOVEM.W 0(A3,D1.L*4),D0/D7   -> D1 < 0 and increases *2 / *4 stereo
        MULS   D5,D0             -> D5 = leftvolume
        ADD.L  D0,(A5)+         -> the xdata 24 bit RAW LEFT
        MULS   D6,D7             -> D6 = rightvolume
        ADD.L  D7,(A5)+         -> the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            -> D4=modulo, A6=addmodulo
        ADDX.L D2,D1            -> D2=skip
        BPL.S  itsendofsample2
notendofsample2:
        DBRA   D3,secloop
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop2
itsendofsample2:
        SUB.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample2
        BRA    nextloopend2

secloopm:
LONG $30331A00 -> MOVE.W 0(A3,D1.L*2),D0   -> D1 < 0 and increases *2 / *4 stereo
        MOVE.W D0,D7
        MULS   D5,D0             -> D5 = leftvolume
        ADD.L  D0,(A5)+         -> the xdata 24 bit RAW LEFT
        MULS   D6,D7             -> D6 = rightvolume
        ADD.L  D7,(A5)+         -> the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            -> D4=modulo, A6=addmodulo
        ADDX.L D2,D1            -> D2=skip
        BPL.S  itsendofsample2m
notendofsample2m:
        DBRA   D3,secloopm
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop2
itsendofsample2m:
        SUB.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample2m
        BRA    nextloopend2

lastloop:
INT $4CB3,$0081,$1C00 ->  MOVEM.W 0(A3,D1.L*4),D0/D7   -> D1 < 0 and increases *2 / *4 stereo
        MOVE.L D7,A0
        MULS   D5,D0             -> D5 = leftvolume
        ADD.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            -> convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh1
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W  #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh1:
        MOVE.W D0,(A4)+
        MOVE.L A0,D0
        MULS   D6,D0             -> D6 = rightvolume
        ADD.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            -> convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh2
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W  #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh2:
        MOVE.W D0,(A4)+
        ADD.L  A6,D4            -> D4=modulo, D3=addmodulo
        ADDX.L D2,D1            -> D2=skip
        BPL.S  itsendofsample3
notendofsample3:
        DBRA   D3,lastloop
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop3
itsendofsample3:
        SUB.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample3
        ADD.W  D3,D3
        BEQ.S  endofsample3
        SUBQ.W #1,D3
restuniloop:
        MOVE.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            -> convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh3
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh3:
        MOVE.W D0,(A4)+
        DBRA   D3,restuniloop
endofsample3:
        BRA    nextloopend3

lastloopm:
LONG $30331A00  -> MOVE.W 0(A3,D1.L*2),D0   -> D1 < 0 and increases *2 / *4 stereo
        MOVE.L D0,A0
        MULS   D5,D0             -> D5 = leftvolume
        ADD.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            -> convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh1m
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh1m:
        MOVE.W D0,(A4)+
        MOVE.L A0,D0
        MULS   D6,D0             -> D6 = rightvolume
        ADD.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            -> convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh2m
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh2m:
        MOVE.W D0,(A4)+
        ADD.L  A6,D4            -> D4=modulo, D3=addmodulo
        ADDX.L D2,D1            -> D2=skip
        BPL.S  itsendofsample3m
notendofsample3m:
        DBRA   D3,lastloopm
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop3
itsendofsample3m:
        SUB.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample3m
        ADD.W  D3,D3
        BEQ    nextloopend3
        SUBQ.W #1,D3
        BRA    restuniloop



/* bckwrd */

-> A3 = start loop, A5 = buffer, D3=buflen-1 (in frames), D1 = +offset, D4 = modulo, A6 = addmodulo, D2 = skip, D5,D6 -> voll,volr
firstloopbk:
INT $4CB3,$0081,$1C00 ->  MOVEM.W 0(A3,D1.L*4),D0/D7   -> D1 < 0 and increases *2 / *4 stereo
        MULS   D5,D0             -> D5 = leftvolume
        MOVE.L D0,(A5)+          -> the xdata 24 bit RAW LEFT
        MULS   D6,D7             -> D6 = rightvolume
        MOVE.L D7,(A5)+          -> the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            -> D4=modulo, A6=addmodulo
        SUBX.L D2,D1            -> D2=skip
        BMI.S  itsendofsample1bk
notendofsample1bk:
        DBRA   D3,firstloopbk
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop1
itsendofsample1bk:
        ADD.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample1bk
        BRA    endofsample1

firstloopmbk:
LONG $30331A00 -> MOVE.W 0(A3,D1.L*2),D0   -> D1 < 0 and increases *2 / *4 stereo
        MOVE.W  D0,D7
        MULS    D5,D0             -> D5 = leftvolume
        MOVE.L  D0,(A5)+          -> the xdata 24 bit RAW LEFT
        MULS    D6,D7             -> D6 = rightvolume
        MOVE.L  D7,(A5)+          -> the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            -> D4=modulo, A6=addmodulo
        SUBX.L D2,D1            -> D2=skip
        BMI.S  itsendofsample1mbk
notendofsample1mbk:
        DBRA   D3,firstloopmbk
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop1
itsendofsample1mbk:
        ADD.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample1mbk
        BRA    endofsample1

secloopbk:
INT $4CB3,$0081,$1C00 ->  MOVEM.W 0(A3,D1.L*4),D0/D7   -> D1 < 0 and increases *2 / *4 stereo
        MULS   D5,D0             -> D5 = leftvolume
        ADD.L  D0,(A5)+         -> the xdata 24 bit RAW LEFT
        MULS   D6,D7             -> D6 = rightvolume
        ADD.L  D7,(A5)+         -> the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            -> D4=modulo, A6=addmodulo
        SUBX.L D2,D1            -> D2=skip
        BMI.S  itsendofsample2bk
notendofsample2bk:
        DBRA   D3,secloopbk
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop2
itsendofsample2bk:
        ADD.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample2bk
        BRA    nextloopend2

secloopmbk:
LONG $30331A00 -> MOVE.W 0(A3,D1.L*2),D0   -> D1 < 0 and increases *2 / *4 stereo
        MOVE.W D0,D7
        MULS   D5,D0             -> D5 = leftvolume
        ADD.L  D0,(A5)+         -> the xdata 24 bit RAW LEFT
        MULS   D6,D7             -> D6 = rightvolume
        ADD.L  D7,(A5)+         -> the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            -> D4=modulo, A6=addmodulo
        SUBX.L D2,D1            -> D2=skip
        BMI.S  itsendofsample2mbk
notendofsample2mbk:
        DBRA   D3,secloopmbk
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop2
itsendofsample2mbk:
        ADD.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample2mbk
        BRA    nextloopend2

lastloopbk:
INT $4CB3,$0081,$1C00 ->  MOVEM.W 0(A3,D1.L*4),D0/D7   -> D1 < 0 and increases *2 / *4 stereo
        MOVE.L D7,A0
        MULS   D5,D0             -> D5 = leftvolume
        ADD.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            -> convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh1bk
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W  #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh1bk:
        MOVE.W D0,(A4)+
        MOVE.L A0,D0
        MULS   D6,D0             -> D6 = rightvolume
        ADD.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            -> convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh2bk
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W  #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh2bk:
        MOVE.W D0,(A4)+
        ADD.L  A6,D4            -> D4=modulo, D3=addmodulo
        SUBX.L D2,D1            -> D2=skip
        BMI.S  itsendofsample3bk
notendofsample3bk:
        DBRA   D3,lastloopbk
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop3
itsendofsample3bk:
        ADD.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample3bk
        ADD.W  D3,D3
        BEQ    nextloopend3
        SUBQ.W #1,D3
        BRA    restuniloop

lastloopmbk:
LONG $30331A00  -> MOVE.W 0(A3,D1.L*2),D0   -> D1 < 0 and increases *2 / *4 stereo
        MOVE.L D0,A0
        MULS   D5,D0             -> D5 = leftvolume
        ADD.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            -> convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh1mbk
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh1mbk:
        MOVE.W D0,(A4)+
        MOVE.L A0,D0
        MULS   D6,D0             -> D6 = rightvolume
        ADD.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            -> convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh2mbk
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh2mbk:
        MOVE.W D0,(A4)+
        ADD.L  A6,D4            -> D4=modulo, D3=addmodulo
        SUBX.L D2,D1            -> D2=skip
        BMI.S  itsendofsample3mbk
notendofsample3mbk:
        DBRA   D3,lastloopmbk
        MOVE.L D4,CH_MODULO(A2)
        MOVE.L D1,CH_CURRENT(A2)
        BRA    nextloop3
itsendofsample3mbk:
        ADD.L  CH_LOOPFRAMES(A2),D1
        BTST   #CHB_LOOPED,CH_FLAGS(A2)
        BNE.S  notendofsample3mbk
        ADD.W  D3,D3
        BEQ    nextloopend3
        SUBQ.W #1,D3
        BRA    restuniloop




