      bopt w2-
      MC68020

LVOAllocVec   = -684  ;(D0,D1)
LVOFreeVec    = -690  ;(A1)
execbase  = -40
dosbase   = -44 ;A6=anybase(A4)
gfxbase   = -52
MEMF_PUBLIC   = 1
MEMF_FAST     = 4
MEMF_CLEAR    = $10000

;MXCMD_SETUP        ;<(mixfreq,channels,maxsamples,buffer,bufferframes); >userdata, mixcode
;MXCMD_END          ;<(userdata)
;MXCMD_LOAD         ;<(address,frames,stereo,userdata)
;MXCMD_UNLOAD       ;<(samplenum,userdata)
;MXCMD_SETMIXPERIOD ;(newmixfrequency,userdata)
;MXCMD_PLAYCHANNEL  ;(offset,freq,volume,pan,loop,channel,samplenum,userdata)
;MXCMD_STOPCHANNEL  ;(chann,userdata)
;MXCMD_STOPCHANNELMASK (chann {1=stop LSB-MSB}, userdata)
;MXCMD_FREECHANNELS ;(userdata) > channmask (1=free)
;MXCMD_SETFREQUENCY ;(newfreq,chann,userdata)
;MXCMD_SETVOLUME    ;(newvolume,newpan,chann,userdata)

  XDEF mxcmd_setup__iiiii,mxcmd_end__i,mxcmd_load__iiii,mxcmd_unload__ii
  XDEF mxcmd_setmixperiod__ii,mxcmd_playchannel__iiiiiiii
  XDEF mxcmd_stopchannel__ii,mxcmd_stopchannelmask__ii,mxcmd_freechannels__ii
  XDEF mxcmd_setfrequency__iii,mxcmd_setvolume__iiii


SMPINFO_ADDRESS = 0
SMPINFO_FRAMES = 4
SMPINFO_STEREO = 8
SMPINFO_SIZEOF = 12

;OBJECT data
;  outsampledata:PTR TO INT ; 16bit, stereo
;  channeldata:PTR TO channel
;  channelmask:LONG
;  bufwork:PTR TO LONG ; 32bit, stereo
;  workframes:LONG ;numbuffer frames-1
;  mixfreq:LONG
;  sampledata:LONG
;  samplenum:LONG
;ENDOBJECT

;OBJECT channel
;  sampleinfo:LONG
;  actfreq:LONG
;  skip:LONG        ; = xxxxx frequency / remix frequency (xxxxx.yyyyy)
;  addmodulo:LONG   ; = yyyyy
;  loopframes:LONG
;  volumeleft:INT   ; /256 * 100% (less eq than 32767)
;  volumeright:INT  ; /256 * 100% volume left and right
;  current:LONG
;  pointer:LONG
;  modulo:LONG ; current modulo
;  flags:LONG
;ENDOBJECT


DT_OUTSAMPLEDATA=0
DT_CHANNELDATA=4
DT_CHANNELMASK=8
DT_BUFWORK=12
DT_WORKFRAMES=16
DT_MIXFREQ=20
DT_SAMPLEDATA=24
DT_SAMPLENUM=28
DT_SIZEOF=32

CHF_LOOPED=1
CHF_STEREO=2
CHF_BACKWARD=4
CHB_LOOPED=0
CHB_STEREO=1
CHB_BACKWARD=2
      
CH_SAMPLEDATA=0
CH_ACTFREQ=4
CH_SKIP=8
CH_ADDMODULO=12
CH_LOOPFRAMES=16
CH_VOLUMELEFT=20
CH_VOLUMERIGHT=22
CH_CURRENT=24
CH_POINTER=28
CH_MODULO=32
CH_FLAGS=36
CH_SIZEOF=40

;        $VER:midiIn mixmodule 0.1 (25.03.99) Rafal Michalski (c) 1999

  CArgs bufferframes.l,buffer.l,maxsamples.l,channels.l,mixfreq.l
mxcmd_setup__iiiii:
        MOVE.L  bufferframes(A7),D0
        LSL.L   #3,D0 ->*8
        MOVE.L  maxsamples(A7),D1
        MULU    #SMPINFO_SIZEOF,D1  -> size of sampleinfo
        ADD.L   D1,D0
        ADD.L   #CH_SIZEOF*32+DT_SIZEOF,D0
        MOVE.L  #MEMF_PUBLIC|MEMF_FAST|MEMF_CLEAR,D1
        MOVE.L  execbase(A4),A6
        JSR     LVOAllocVec(A6)
        TST.L   D0
        BEQ.S   mxcmd_setup_error1
        MOVE.L  D0,A1
        MOVE.L  mixfreq(A7),DT_MIXFREQ(A1)
        MOVE.L  buffer(A7),DT_OUTSAMPLEDATA(A1)
        MOVE.L  bufferframes(A7),D1
        SUBQ.L  #1,D1
        MOVE.L  D1,DT_WORKFRAMES(A1)
        LEA     DT_SIZEOF(A1),A2
        MOVE.L  A2,DT_CHANNELDATA(A1)
        LEA     CH_SIZEOF*32(A2),A2
        MOVE.L  A2,DT_BUFWORK(A1)
        ADDQ.L  #1,D1
        LSL.L   #3,D1 ->*8
        ADD.L   D1,A2
        MOVE.L  A2,DT_SAMPLEDATA(A1)
        MOVE.L  maxsamples(A7),DT_SAMPLENUM(A1)
        LEA     mixptr(PC),A0
        MOVE.L  A0,D1
mxcmd_setup_error1:
        RTS

  CArgs userdata.l
mxcmd_end__i:
        MOVE.L  userdata(A7),D0
        BEQ.S   mxcmd_setup_error1
        MOVE.L  D0,A1
        MOVE.L  execbase(A4),A6
        JSR     LVOFreeVec(A6)
        RTS

  CArgs #8,stereo.l,frames.l,address.l
mxcmd_load__iiii:
        MOVE.L  userdata(A7),A1
        MOVE.L  DT_SAMPLENUM(A1),D1
        SUBQ.W  #1,D1
        MOVE.L  DT_SAMPLEDATA(A1),A0
        MOVEQ   #SMPINFO_SIZEOF,D0
.loop:  TST.L   (A0)
        BEQ.S   .found
        ADDA.L  D0,A0
        DBRA    D1,.loop
        MOVEQ   #0,D0
        RTS
.found: MOVE.L  address(A7),(A0)+
        MOVE.L  frames(A7),(A0)+
        MOVE.L  stereo(A7),(A0)+
        MOVE.L  DT_SAMPLENUM(A1),D0
        SUB.L   D1,D0
        RTS
  CArgs #8,samplenum.l
mxcmd_unload__ii:
        MOVE.L  samplenum(A7),D0
        SUBQ.L  #1,D0
        MOVE.L  userdata(A7),A1
        MULU    #SMPINFO_SIZEOF,D0
        MOVE.L  DT_SAMPLEDATA(A1),A3
        ADD.L   D0,A3
        MOVE.L  DT_CHANNELMASK(A1),D1
        MOVE.L  DT_CHANNELDATA(A1),A2
        MOVEQ   #31,D0
.loop:  CMPA.L  CH_SAMPLEDATA(A2),A3
        BNE.S   .skip
        BCLR    D0,D1
.skip:  LEA     CH_SIZEOF(A2),A2
        DBRA    D0,.loop
        MOVE.L  D1,DT_CHANNELMASK(A1)
        CLR.L   (A3)
        RTS

  CArgs #8,newmixfrequency.l
mxcmd_setmixperiod__ii:
        MOVE.L  userdata(A7),A1
        MOVE.L  newmixfrequency(A7),D0
        MOVEM.L D2-D7,-(A7)
        MOVE.L  D0,A0
        BEQ.S   .quit
        MOVE.L  D0,DT_MIXFREQ(A1)
        MOVE.L  DT_CHANNELDATA(A1),A2
        MOVEQ   #31,D7
.loop:  MOVE.L  CH_ACTFREQ(A2),D0
        MOVE.L  A0,D1
        BSR     divr                ;samplefreq/mixfrequency
        MOVEM.L D5/D6,CH_SKIP(A2)
        LEA     CH_SIZEOF(A2),A2
        DBRA    D7,.loop
.quit:  MOVEM.L (A7)+,D2-D7
        RTS

  CArgs #12,channel.l,loop.l,pan.l,volume.l,freq.l,offset.l
mxcmd_playchannel__iiiiiiii:
        MOVEA.L userdata(A7),A1
        MOVE.L  samplenum(A7),D1
        SUBQ.L  #1,D1
        MOVE.L  channel(A7),D0
        MOVEA.L A7,A0
        MOVEM.L D2-D7/A4,-(A7)
        BFCLR   DT_CHANNELMASK(A1){D0:1}
        MOVE.L  D0,D7
        MOVE.L  DT_CHANNELDATA(A1),A2
        MULU    #CH_SIZEOF,D0
        ADD.L   D0,A2
        MOVE.L  DT_SAMPLEDATA(A1),A3
        MULU    #SMPINFO_SIZEOF,D1
        ADD.L   D1,A3
        MOVE.L  A3,CH_SAMPLEDATA(A2)
        BEQ     psquit
        TST.L   SMPINFO_STEREO(A3)
        SNE     D2
        ANDI.W  #CHF_STEREO,D2
        MOVE.L  freq(A0),D0
        BPL.S   .forw
        BSET    #CHB_BACKWARD,D2
        NEG.L   D0
.forw:  MOVE.L  DT_MIXFREQ(A1),D1
        MOVE.L  D0,CH_ACTFREQ(A2)
        MOVE.B  D2,CH_FLAGS(A2)
        BSR     divr                ;samplefreq/mixfrequency
        MOVE.L  D5,CH_SKIP(A2)
        MOVE.L  D6,CH_ADDMODULO(A2)
        CLR.L   CH_MODULO(A2)
        MOVE.L  volume(A0),D0
        MOVE.L  pan(A0),D1
        BSR     cvolume
        MOVE.W  D1,CH_VOLUMELEFT(A2)
        MOVE.W  D2,CH_VOLUMERIGHT(A2)
        MOVE.L  SMPINFO_ADDRESS(A3),A4
        BTST    #CHB_BACKWARD,CH_FLAGS(A2)
        BNE.S   lpbck
        MOVE.L  SMPINFO_FRAMES(A3),D1
        TST.L   SMPINFO_STEREO(A3)
        BEQ.S   .nlopm
        LEA     0(A4,D1.L*2),A4
.nlopm: LEA     0(A4,D1.L*2),A4
        MOVE.L  loop(A0),D2
        SPL     D0
        ANDI.W  #CHF_LOOPED,D0
        OR.B    D0,CH_FLAGS(A2)
        MOVE.L  D1,D0
        SUB.L   D2,D0
        NEG.L   D1
nlpbk:  ADD.L   offset(A0),D1
lpbken: MOVE.L  D0,CH_LOOPFRAMES(A2)
        MOVE.L  D1,CH_CURRENT(A2)
        MOVE.L  A4,CH_POINTER(A2)
        BFSET   DT_CHANNELMASK(A1){D7:1}
psquit: MOVEM.L (A7)+,D2-D7/A4
        RTS
lpbck:  MOVEQ   #0,D1
        MOVE.L  loop(A0),D0
        BMI.S   nlpbk
        BSET    #CHB_LOOPED,CH_FLAGS(A2)
        TST.L   SMPINFO_STEREO(A3)
        BEQ.S   .nlopm
        LEA     0(A4,D0.L*2),A4
.nlopm: LEA     0(A4,D0.L*2),A4
        MOVE.L  offset(A0),D1
        SUB.L   D0,D1
        BPL.S   .nover
        MOVEQ   #0,D1
.nover: NEG.L   D0
        ADD.L   SMPINFO_FRAMES(A3),D0
        BRA.S   lpbken

  CArgs #8,chann.l
mxcmd_stopchannel__ii:
        MOVE.L  userdata(A7),A1
        MOVE.L  chann(A7),D0
        BFCLR   DT_CHANNELMASK(A1){D0:1}
        RTS
mxcmd_stopchannelmask__ii:
        MOVE.L  userdata(A7),A1
        MOVE.L  chann(A7),D0
        MOVE.L  D2,A0
        MOVEQ   #31,D2
.loop:  LSR.L   #1,D0
        ROXL.L  #1,D1
        DBRA    D2,.loop
        NOT.L   D1
        AND.L   D1,DT_CHANNELMASK(A1)
        MOVE.L  A0,D2
        RTS
mxcmd_freechannels__ii:
        MOVE.L  userdata(A7),A1
        MOVE.L  D2,A0
        MOVE.L  DT_CHANNELMASK(A1),D1
        MOVEQ   #31,D2
.loop:  LSR.L   #1,D1
        ROXL.L  #1,D0
        DBRA    D2,.loop
        MOVE.L  A0,D2
        NOT.L   D0
        AND.L   chann(A7),D0
        RTS

  CArgs #12,newfreq.l
mxcmd_setfrequency__iii:
        MOVE.L  userdata(A7),A1
        MOVE.L  newfreq(A7),D1
        MOVE.L  chann(A7),D0
        MOVEM.L D2-D7,-(A7)
        MOVE.L  D0,D7
        MOVE.L  DT_CHANNELDATA(A1),A2
        MULU    #CH_SIZEOF,D0
        ADD.L   D0,A2
        MOVE.L  D1,D0
        SMI     D1
        BTST    #CHB_BACKWARD,CH_FLAGS(A2)
        SNE     D2
        EOR.B   D2,D1
        BNE.S   .chnge
        TST.L   D0
        BPL.S   .mixit
        NEG.L   D0
.mixit: MOVE.L  D0,CH_ACTFREQ(A2)
        MOVE.L  DT_MIXFREQ(A1),D1
        BSR     divr                ;samplefreq/mixfrequency
        MOVEM.L D5/D6,CH_SKIP(A2)
.quit:  MOVEM.L (A7)+,D2-D7
        RTS
.chnge: BFCLR   DT_CHANNELMASK(A1){D7:1}
        BEQ.S   .quit
        MOVE.L  CH_SAMPLEDATA(A2),A3
        MOVE.L  SMPINFO_ADDRESS(A3),D3
        MOVE.L  SMPINFO_FRAMES(A3),D1
        TST.L   D0
        BMI.S   .backward
        MOVE.L  CH_POINTER(A2),D2
        SUB.L   D3,D2
        TST.L   SMPINFO_STEREO(A3)
        BEQ.S   .mono
        ADD.L   D1,D3
        ADD.L   D1,D3
        ASR.L   #1,D2
.mono:  ADD.L   D1,D3
        ADD.L   D1,D3
        ASR.L   #1,D2
        SUB.L   D1,D2
        ADD.L   CH_CURRENT(A2),D2
        BCLR    #CHB_BACKWARD,CH_FLAGS(A2)
        MOVEM.L D2/D3,CH_CURRENT(A2)
.mixit2:MOVE.L  D0,CH_ACTFREQ(A2)
        MOVE.L  DT_MIXFREQ(A1),D1
        BSR     divr                ;samplefreq/mixfrequency
        MOVEM.L D5/D6,CH_SKIP(A2)
        BFSET   DT_CHANNELMASK(A1){D7:1}
        MOVEM.L (A7)+,D2-D7
        RTS
.backward:
        BTST    #CHB_LOOPED,CH_FLAGS(A2)
        BEQ.S   .nolop
        MOVE.L  D1,D2
        SUB.L   CH_LOOPFRAMES(A2),D2
        SUB.L   D2,D1
        TST.L   SMPINFO_STEREO(A3)
        BEQ.S   .mono2
        ADD.L   D2,D2
.mono2: ADD.L   D2,D2
        ADD.L   D2,D3
.nolop: ADD.L   CH_CURRENT(A2),D1
        BPL.S   .nlok
        MOVEQ   #0,D1
.nlok:  BSET    #CHB_BACKWARD,CH_FLAGS(A2)
        MOVEM.L D1/D3,CH_CURRENT(A2)
        NEG.L   D0
        BRA.S   .mixit2

  CArgs #12,newpan.l,newvolume.l
mxcmd_setvolume__iiii:
        MOVE.L  newvolume(A7),D0
        MOVE.L  newpan(A7),D1
        MOVE.L  D2,A2
        MOVE.L  D3,A3
        BSR.S   cvolume
        MOVE.L  userdata(A7),A1
        MOVE.L  DT_CHANNELDATA(A1),A0
        MOVE.L  chann(A7),D0
        MULU    #CH_SIZEOF,D0
        ADD.L   D0,A0
        MOVEM.W D1/D2,CH_VOLUMELEFT(A0)
        MOVE.L  A2,D2
        MOVE.L  A3,D3
        RTS
;#############################################################################33
;                        calculate left and right volume
;#############################################################################33
cvolume:   ;<D0=volume(65536=100%), D1=panorama(0:65536) >D1,D2 l,r(256=100%)
        LSR.L   #6,D1
        MOVE.W  #1024,D2
        SUB.W   D1,D2
        MOVE.L  #$100000,D3
        MULU    D1,D1
        MULU    D2,D2
        NEG.L   D1
        ADD.L   D3,D1
        NEG.L   D2
        ADD.L   D3,D2
        LSR.L   #8,D0
        MULU.L  D0,D1
        DIVU.L  D3,D1
        MULU.L  D0,D2
        DIVU.L  D3,D2
        RTS
;#############################################################################33
;                        divide xxxx/yyyy=aaaa.bbbb
;#############################################################################33
divr:   MOVEQ   #32,D3      ;D5.D6=D0/D1
        BFFFO   D0{0:32},D4
        SUB.W   D4,D3
        LSL.L   D4,D0
        DIVUL.L D1,D2:D0
        MOVE.L  D0,D5
        LSR.L   D4,D5       ;xxx.
        MOVE.L  D0,D6       ;xxx.yyy
        MOVE.L  D2,D0
        TST.W   D3
        BLE.S   dkoniec
dloop:  BFFFO   D0{0:32},D4
        SUB.W   D4,D3
        BPL.S   dok
        ADD.W   D3,D4
dok:    LSL.L   D4,D0
        DIVUL.L D1,D2:D0
        LSL.L   D4,D6
        ADD.L   D0,D6
        MOVE.L  D2,D0
        TST.W   D3
        BGT.S   dloop
dkoniec: RTS


;###############################################################################
;                                main engine
;###############################################################################

;  (A1) = data
mixptr: MOVE.L  DT_CHANNELDATA(A1),A2
        MOVE.L  DT_CHANNELMASK(A1),D0
        BNE.S   something
        MOVE.L  DT_OUTSAMPLEDATA(A1),A0
        MOVE.L  DT_WORKFRAMES(A1),D0
zerolp: CLR.L   (A0)+
        DBRA    D0,zerolp
        RTS
something:
        MOVEQ   #0,D1
        BFFFO   D0{0:32},D1
        MOVE.L  D0,A4
        MOVEQ   #CH_SIZEOF,D0
        MULU    D1,D0
        ADD.L   D0,A2
        NOT.W   D1          ;MSB-LSB <=> LSB-MSB
        MOVE.L  DT_BUFWORK(A1),A5
        MOVE.L  DT_WORKFRAMES(A1),D3
        MOVE.L  CH_MODULO(A2),D4
        MOVE.L  CH_ADDMODULO(A2),A6
        MOVE.L  CH_SKIP(A2),D2
        MOVEM.W CH_VOLUMELEFT(A2),D5/D6
        MOVE.L  CH_POINTER(A2),A3
        MOVE.B  CH_FLAGS(A2),D7
        MOVE.L  CH_CURRENT(A2),A0
        ANDI.W  #CHF_BACKWARD|CHF_STEREO,D7
        ADD.W   D7,D7
        EXG     D1,A0
        JMP    mix_jumpf(PC,D7.W)
mix_jumpf:
        BRA     firstloopm
        BRA     firstloop
        BRA     firstloopmbk
        BRA     firstloopbk
nextloopend1:
        MOVE.L  A0,D1
        MOVE.L  A4,D0
        BCLR    D1,D0
        BRA.S   nextloopskip1
nextloop1:
        MOVE.L  A0,D1
        MOVE.L  A4,D0
nextloopskip1:
        NEG.W   D1      ;LSB-MSB <=> MSB-LSB +1
        MOVEQ   #32,D2
        SUB.W   D1,D2
        BEQ.S   firstz
        BFFFO   D0{D1:D2},D1
        BNE.S   middle
firstz:
        MOVE.L  D0,DT_CHANNELMASK(A1)
        MOVE.L  DT_OUTSAMPLEDATA(A1),A4
        MOVE.L  DT_BUFWORK(A1),A5
        MOVE.L  DT_WORKFRAMES(A1),D3
        ADDQ.W #1,D3
        ADD.W  D3,D3
        SUBQ.W #1,D3
        MOVE.W #$7FFF,D1
firstzl:
        MOVE.L  (A5)+,D0         ; the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            ; convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  fznothigh
        ROL.L  #8,D0
        EXT.W  D0
        EOR.W  D1,D0        ;FFFF to 8000,  0000 to 7FFF
fznothigh:
        MOVE.W D0,(A4)+
        DBRA   D3,firstzl
        RTS

nextloopend2:
        MOVE.L  A0,D1
        MOVE.L  A4,D0
        NOT.W   D1        ;MSB-LSB <=> LSB-MSB
        BCLR    D1,D0
        BRA.S   nextloopskip2
nextloop2:
        MOVE.L  A0,D1
        MOVE.L  A4,D0
nextloopskip2:
        SWAP    D1
middle: MOVEQ   #CH_SIZEOF,D7
        MULU    D1,D7
        MOVE.L  DT_CHANNELDATA(A1),A2
        ADD.L   D7,A2

        MOVE.W  D1,D7
        ADDQ.W  #1,D1
        MOVEQ   #32,D2
        SUB.W   D1,D2
        BEQ.S   midskip
        BFFFO   D0{D1:D2},D1
        MOVEA.L D0,A4
midskip:SNE     D0
        SWAP    D1
        MOVE.W  D7,D1

        MOVE.L  DT_BUFWORK(A1),A5
        MOVE.L  DT_WORKFRAMES(A1),D3
        MOVE.L  CH_MODULO(A2),D4
        MOVE.L  CH_ADDMODULO(A2),A6
        MOVE.L  CH_SKIP(A2),D2
        MOVEM.W CH_VOLUMELEFT(A2),D5/D6
        MOVE.L  CH_POINTER(A2),A3
        MOVE.B  CH_FLAGS(A2),D7
        MOVE.L  CH_CURRENT(A2),A0
        EXG     D1,A0
        ANDI.W  #CHF_BACKWARD|CHF_STEREO,D7
        ADD.W   D7,D7
        TST.B   D0
        BEQ.S   last
        JMP     mix_jumps(PC,D7.W)
mix_jumps:
        BRA     secloopm
        BRA     secloop
        BRA     secloopmbk
        BRA     secloopbk

last:   MOVEM.L A0/A4,-(A7)
        MOVE.L  DT_OUTSAMPLEDATA(A1),A4
        JMP     mix_jumpl(PC,D7.W)
mix_jumpl:
        BRA     lastloopm
        BRA     lastloop
        BRA     lastloopmbk
        BRA     lastloopbk
nextloopend3:
        MOVEM.L (A7)+,D0/D1
        NOT.W   D0
        BCLR    D0,D1
        MOVE.L  D1,DT_CHANNELMASK(A1)
        RTS
nextloop3:
        MOVEM.L (A7)+,A0/A4
        MOVE.L  A4,DT_CHANNELMASK(A1)
        RTS

        

; A2 = channel data, A5 = buffer32, D3=buflen-1 (in frames), D4 = modulo, A6 = addmodulo, D2 = skip, D5,D6 = voll,volr
; first & second
; A3 = end sample, D1 = -offset
; A3 = start loop, D1 = +offset
; last
; A4 = buffer16, A3 = end sample, D1 = -offset
; A4 = buffer16, A3 = start loop, D1 = +offset


firstloop:
        MOVEM.W 0(A3,D1.L*4),D0/D7   ; D1 < 0 and increases *2 / *4 stereo
        MULS    D6,D0             ; D6 = leftvolume
        MOVE.L  D0,(A5)+          ; the xdata 24 bit RAW LEFT
        MULS    D5,D7             ; D5 = rightvolume
        MOVE.L  D7,(A5)+          ; the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            ; D4=modulo, A6=addmodulo
        ADDX.L D2,D1            ; D2=skip
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
        MOVE.W 0(A3,D1.L*2),D0   ; D1 < 0 and increases *2 / *4 stereo
        MOVE.W  D0,D7
        MULS    D6,D0             ; D6 = leftvolume
        MOVE.L  D0,(A5)+          ; the xdata 24 bit RAW LEFT
        MULS    D5,D7             ; D5 = rightvolume
        MOVE.L  D7,(A5)+          ; the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            ; D4=modulo, A6=addmodulo
        ADDX.L D2,D1            ; D2=skip
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
        MOVEM.W 0(A3,D1.L*4),D0/D7   ; D1 < 0 and increases *2 / *4 stereo
        MULS   D6,D0             ; D6 = leftvolume
        ADD.L  D0,(A5)+         ; the xdata 24 bit RAW LEFT
        MULS   D5,D7             ; D5 = rightvolume
        ADD.L  D7,(A5)+         ; the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            ; D4=modulo, A6=addmodulo
        ADDX.L D2,D1            ; D2=skip
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
        MOVE.W 0(A3,D1.L*2),D0   ; D1 < 0 and increases *2 / *4 stereo
        MOVE.W D0,D7
        MULS   D6,D0             ; D6 = leftvolume
        ADD.L  D0,(A5)+         ; the xdata 24 bit RAW LEFT
        MULS   D5,D7             ; D5 = rightvolume
        ADD.L  D7,(A5)+         ; the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            ; D4=modulo, A6=addmodulo
        ADDX.L D2,D1            ; D2=skip
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
        MOVEM.W 0(A3,D1.L*4),D0/D7   ; D1 < 0 and increases *2 / *4 stereo
        MOVE.L D7,A0
        MULS   D6,D0             ; D6 = leftvolume
        ADD.L  (A5)+,D0         ; the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            ; convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh1
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W  #$7FFF,D0        ;FFFF to 8000,  0000 to 7FFF
nothigh1:
        MOVE.W D0,(A4)+
        MOVE.L A0,D0
        MULS   D5,D0             ; D5 = rightvolume
        ADD.L  (A5)+,D0         ; the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            ; convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh2
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W  #$7FFF,D0        ;FFFF to 8000,  0000 to 7FFF
nothigh2:
        MOVE.W D0,(A4)+
        ADD.L  A6,D4            ; D4=modulo, D3=addmodulo
        ADDX.L D2,D1            ; D2=skip
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
restuniskip:
        SUBQ.W #1,D3
        MOVE.W #$7FFF,D1
restuniloop:
        MOVE.L  (A5)+,D0         ; the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            ; convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh3
        ROL.L  #8,D0
        EXT.W  D0
        EOR.W  D1,D0        ;FFFF to 8000,  0000 to 7FFF
nothigh3:
        MOVE.W D0,(A4)+
        DBRA   D3,restuniloop
endofsample3:
        BRA    nextloopend3

lastloopm:
        MOVE.W 0(A3,D1.L*2),D0   ; D1 < 0 and increases *2 / *4 stereo
        MOVE.L D0,A0
        MULS   D6,D0             ; D6 = leftvolume
        ADD.L  (A5)+,D0         ; the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            ; convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh1m
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W #$7FFF,D0        ;FFFF to 8000,  0000 to 7FFF
nothigh1m:
        MOVE.W D0,(A4)+
        MOVE.L A0,D0
        MULS   D5,D0             ; D5 = rightvolume
        ADD.L  (A5)+,D0         ; the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            ; convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh2m
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W #$7FFF,D0        ;FFFF to 8000,  0000 to 7FFF
nothigh2m:
        MOVE.W D0,(A4)+
        ADD.L  A6,D4            ; D4=modulo, D3=addmodulo
        ADDX.L D2,D1            ; D2=skip
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
        BRA    restuniskip



;/* bckwrd */

; A3 = start loop, A5 = buffer, D3=buflen-1 (in frames), D1 = +offset, D4 = modulo, A6 = addmodulo, D2 = skip, D6,D5 ; voll,volr
firstloopbk:
        MOVEM.W 0(A3,D1.L*4),D0/D7   ; D1 < 0 and increases *2 / *4 stereo
        MULS   D6,D0             ; D6 = leftvolume
        MOVE.L D0,(A5)+          ; the xdata 24 bit RAW LEFT
        MULS   D5,D7             ; D5 = rightvolume
        MOVE.L D7,(A5)+          ; the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            ; D4=modulo, A6=addmodulo
        SUBX.L D2,D1            ; D2=skip
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
        MOVE.W 0(A3,D1.L*2),D0   ; D1 < 0 and increases *2 / *4 stereo
        MOVE.W  D0,D7
        MULS    D6,D0             ; D6 = leftvolume
        MOVE.L  D0,(A5)+          ; the xdata 24 bit RAW LEFT
        MULS    D5,D7             ; D5 = rightvolume
        MOVE.L  D7,(A5)+          ; the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            ; D4=modulo, A6=addmodulo
        SUBX.L D2,D1            ; D2=skip
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
        MOVEM.W 0(A3,D1.L*4),D0/D7   ; D1 < 0 and increases *2 / *4 stereo
        MULS   D6,D0             ; D6 = leftvolume
        ADD.L  D0,(A5)+         ; the xdata 24 bit RAW LEFT
        MULS   D5,D7             ; D5 = rightvolume
        ADD.L  D7,(A5)+         ; the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            ; D4=modulo, A6=addmodulo
        SUBX.L D2,D1            ; D2=skip
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
        MOVE.W 0(A3,D1.L*2),D0   ; D1 < 0 and increases *2 / *4 stereo
        MOVE.W D0,D7
        MULS   D6,D0             ; D6 = leftvolume
        ADD.L  D0,(A5)+         ; the xdata 24 bit RAW LEFT
        MULS   D5,D7             ; D5 = rightvolume
        ADD.L  D7,(A5)+         ; the xdata 24 bit RAW RIGHT
        ADD.L  A6,D4            ; D4=modulo, A6=addmodulo
        SUBX.L D2,D1            ; D2=skip
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
        MOVEM.W 0(A3,D1.L*4),D0/D7   ; D1 < 0 and increases *2 / *4 stereo
        MOVE.L D7,A0
        MULS   D6,D0             ; D6 = leftvolume
        ADD.L  (A5)+,D0         ; the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            ; convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh1bk
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W  #$7FFF,D0        ;FFFF to 8000,  0000 to 7FFF
nothigh1bk:
        MOVE.W D0,(A4)+
        MOVE.L A0,D0
        MULS   D5,D0             ; D5 = rightvolume
        ADD.L  (A5)+,D0         ; the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            ; convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh2bk
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W  #$7FFF,D0        ;FFFF to 8000,  0000 to 7FFF
nothigh2bk:
        MOVE.W D0,(A4)+
        ADD.L  A6,D4            ; D4=modulo, D3=addmodulo
        SUBX.L D2,D1            ; D2=skip
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
        BRA    restuniskip

lastloopmbk:
        MOVE.W 0(A3,D1.L*2),D0   ; D1 < 0 and increases *2 / *4 stereo
        MOVE.L D0,A0
        MULS   D6,D0             ; D6 = leftvolume
        ADD.L  (A5)+,D0         ; the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            ; convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh1mbk
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W #$7FFF,D0        ;FFFF to 8000,  0000 to 7FFF
nothigh1mbk:
        MOVE.W D0,(A4)+
        MOVE.L A0,D0
        MULS   D5,D0             ; D5 = rightvolume
        ADD.L  (A5)+,D0         ; the xdata 24 bit RAW LEFT
        ASR.L  #8,D0            ; convert to 16 bit
        MOVE.L D0,D7
        EXT.L  D7
        CMP.L  D7,D0
        BEQ.S  nothigh2mbk
        ROL.L  #8,D0
        EXT.W  D0
        EORI.W #$7FFF,D0        ;FFFF to 8000,  0000 to 7FFF
nothigh2mbk:
        MOVE.W D0,(A4)+
        ADD.L  A6,D4            ; D4=modulo, D3=addmodulo
        SUBX.L D2,D1            ; D2=skip
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
        BRA    restuniskip




