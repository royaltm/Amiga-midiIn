OPT MODULE

MODULE  'exec/nodes','exec/interrupts','exec/memory',
				'hardware/custom','hardware/intbits','hardware/dmabits','hardware/cia'

EXPORT CONST BUFFLEN=256, FIXEDMAX=$1000000

OBJECT datamain
	channels:PTR TO channel
	channelmask:LONG
	busy:LONG
	softint:PTR TO is
	bufferwork:PTR TO LONG
	buffercopy:PTR TO INT
	bufferswap:PTR TO INT
	calibration:PTR TO CHAR -> middle of calibration table
	chip1:PTR TO CHAR  ->address of chip buffer+4
	chip2:PTR TO CHAR
	chip3:PTR TO CHAR
	chip4:PTR TO CHAR
ENDOBJECT

CONST DM_CHAN=0, DM_CHANNELMASK=4, DM_BUSY=8, DM_SOFTINT=12,
			DM_BUFFERWORK=16, DM_BUFFERCOPY=20, DM_BUFFERSWAP=24,
			DM_CALIBRATION=28, DM_CHIPADR=32

EXPORT OBJECT envelope -> realtime update volume for each channell
	alt:LONG      -> fixpoint current ratio 0 - 1 (nor: 1=0x0100.0000)
	climb:LONG    -> fixpoint step volume; attack; decay; 0x0; release
	hit:LONG      -> fixpoint hit volume; atthit; susthit; ; 0x0
	decay:LONG    -> fixpoint decay climb volume ( < 0); 0x0
	susthit:LONG  -> fixpoint sustain hit level
ENDOBJECT

->      ********** don't change the size of any object here!!!

CONST     EVALT=0,      EVCLIMB=4,      EVHIT=8,      EVDECAY=12,      EVSUSTHIT=16

OBJECT channel
	current:LONG
	skip:LONG -> 0,1,2,3,4 ...    { based on sample frequency and remix freq
	addmodulo:LONG -> [0-1]/2^32  }
	modulo:LONG -> current modulo
	volumel:LONG -> /256 * 100% (less eq than 32767)
	volumer:LONG -> /256 * 100% volume left and right
	smpenddata:LONG
	looplength:LONG -> looplen sample frames (minus offset to end)
	envel:envelope
	loop:INT -> loop boolean
	stereo:INT -> boolean
ENDOBJECT

CONST CH_CURRENT=0, CH_SKIP=4, CH_ADDMODULO=8, CH_MODULO=12,
			CH_VOLUMEL=16,  CH_VOLUMER=20,
			CH_SMPENDDATA=24,  CH_LOOPLENGTH=28, CH_ENVEL=32,
			CH_LOOP=52, CH_STEREO=54


DEF mainint:PTR TO is, softint:PTR TO is

EXPORT PROC setvolume(chan,voll,volr)
DEF chanptr:PTR TO channel, ch:REG
	IF mainint
		MOVE.L  chan,ch
		AND.L   #$1F,ch
		chanptr:=mainint.data::datamain.channels[ch]
		MOVE.L  chanptr,A0; MOVE.L  voll,D0; MOVE.L volr,D1
		MOVEM.L D0/D1,CH_VOLUMEL(A0)
/*  chanptr.volumel:=voll
		chanptr.volumer:=volr */
	ENDIF
ENDPROC

EXPORT PROC modifyfreq(chan,skip,modulo)
DEF chanptr:PTR TO channel, ch:REG
	IF mainint
		MOVE.L  chan,ch
		AND.L   #$1F,ch
		chanptr:=mainint.data::datamain.channels[ch]
		MOVE.L  chanptr,A0; MOVE.L  skip,D0; MOVE.L modulo,D1
		MOVEM.L D0/D1,CH_SKIP(A0)
/*  chanptr.skip:=skip
		chanptr.addmodulo:=modulo */
	ENDIF
ENDPROC

EXPORT PROC playchannel(chan,smpframes,smpendptr,looplenframes,skip,modulo,stereo,voll,volr,env:PTR TO envelope)
DEF dataptr:PTR TO datamain, chanptr:PTR TO channel, x:REG, ch:REG, e:PTR TO envelope
	IF mainint
		MOVE.L  chan,ch
		AND.L   #$1F,ch
		dataptr:=mainint.data
		chanptr:=dataptr.channels[ch]
		e:=chanptr.envel
		x:=dataptr.channelmask
		BCLR    ch,x
		MOVE.L  dataptr,A0
		MOVE.L  x,DM_CHANNELMASK(A0)            -> stop channel !!!!!!
		MOVE.L  env,A1; MOVEM.L  (A1),D0-D2/A2/A3  -> SIZE OF envelope
		MOVE.L  e,A1;   MOVEM.L  D0-D2/A2/A3,(A1)
		MOVE.L  chanptr,A1
		MOVE.L  smpendptr,CH_SMPENDDATA(A1)
		MOVE.L  skip,CH_SKIP(A1)
		MOVE.L  modulo,CH_ADDMODULO(A1)
		CLR.L   CH_MODULO(A1)
		MOVE.L  stereo,D0
		MOVE.W  D0,CH_STEREO(A1)
		MOVE.L  voll,CH_VOLUMEL(A1)
		MOVE.L  volr,CH_VOLUMER(A1)
		MOVE.L  smpframes,D0
		NEG.L   D0
		MOVE.L  D0,CH_CURRENT(A1)
		MOVEQ   #-1,D2
		MOVE.L  looplenframes,D1
		BNE.S   willoop
		NEG.L   D0
		MOVE.L  D0,D1
		MOVEQ   #0,D2
willoop:
		MOVE.L  D1,CH_LOOPLENGTH(A1)
		MOVE.W  D2,CH_LOOP(A1)
		BSET    ch,x
		MOVE.L  x,DM_CHANNELMASK(A0)
->TextF(10,10,'alt=\h climb=\h hit=\h',e.alt, e.climb, e.hit)
->TextF(10,20,'decay=\h shit=\h',e.decay, e.susthit)

->WriteF('\nmask:\h\n',dataptr.channelmask)
->WriteF('\h \h\n',dataptr.channels,chanptr)
/*WriteF('\d \d \h \h \d \d \h \d \h \d \d\n',
	chanptr.current,chanptr.skip,chanptr.modulo,chanptr.addmodulo,chanptr.volumel,chanptr.volumer,chanptr.smpenddata,chanptr.looplength,chanptr.envdata,chanptr.loop,chanptr.stereo)
*/
	ENDIF
ENDPROC
/*
EXPORT PROC getalt(ch)
DEF e:PTR TO envelope
	e:=globenvdata[ch]
	WriteF('alt=\h climb=\h hit=\h ',e.alt, e.climb, e.hit)
	WriteF('decay=\h shit=\h\n',e.decay, e.susthit)
ENDPROC
*/
EXPORT PROC freechannels(chanmask)
DEF dataptr:PTR TO datamain
	IF mainint
		dataptr:=mainint.data
		MOVE.L  dataptr,A0
		MOVE.L  DM_CHANNELMASK(A0),D0
		NOT.L   D0
		AND.L   D0,chanmask
	ELSE
		RETURN -1
	ENDIF
ENDPROC chanmask

EXPORT PROC setrelease(ch,par) -> par:= dmafreq * t / (BUFFLEN*10) 
DEF envel:PTR TO envelope      ->      !!!!! MUST BE > 0 !!!!
	IF mainint
		ANDI.L  #$1F,ch
		envel:=mainint.data::datamain.channels[ch].envel
		MOVE.L  par,D1
		MOVE.L  envel,A0
		CMP.W   #256,D1
		BHI.S   normaldiv

		MOVE.L  (A0),D0
		LSR.L   #8,D0
		MOVE.L  D0,D2
		DIVU    D1,D2
		MOVE.L  D2,D1; SUB.W D1,D1; SWAP    D1
		SUB.L   D1,D0
		MOVE.W  D2,D1; NEG.L D1
		ASL.L   #8,D0
		ASL.L   #8,D1
		MOVEQ   #0,D2
		MOVEM.L D0/D1/D2,(A0)
		RETURN
normaldiv:
		MOVE.L  (A0),D0
		MOVE.L  D0,D2
		DIVU    D1,D2
		MOVE.L  D2,D1; SUB.W D1,D1; SWAP    D1
		SUB.L   D1,D0
		MOVE.W  D2,D1; NEG.L D1
		MOVEQ   #0,D2
		MOVEM.L D0/D1/D2,(A0)
	ENDIF
ENDPROC

EXPORT PROC stopchannels(chanmask)
DEF dataptr:PTR TO datamain
	IF mainint
		dataptr:=mainint.data
		MOVE.L  dataptr,A0
		MOVE.L  chanmask,D0
		NOT.L   D0
		AND.L   D0,DM_CHANNELMASK(A0)
	ENDIF
ENDPROC

EXPORT PROC stopselectedchan(smpendptr)
DEF dataptr:PTR TO datamain,smpend:REG
	IF mainint
		dataptr:=mainint.data
		MOVE.L  dataptr,A0
		MOVE.L  DM_CHAN(A0),A0
		MOVEQ   #SIZEOF channel,D0
		MOVE.L  D0,A1
		MOVEQ   #31,D2
		MOVE.L  smpendptr,smpend
sslchloop:
		CMP.L   CH_SMPENDDATA(A0),smpend
		ADDA.L  A1,A0
		SNE     D0
		ADD.B   D0,D0
		ROXR.L  #1,D1
		DBRA    D2,sslchloop
		MOVE.L  dataptr,A0
		AND.L   D1,DM_CHANNELMASK(A0)
	ENDIF
ENDPROC


EXPORT PROC allocandenable(calibrationptr,mixper) HANDLE
DEF dataptr=0:PTR TO datamain, chanptr=0:PTR TO channel,
		chip1=0, chip2=0, chip3=0, chip4=0, bufferwork=0,
		mint=0:PTR TO is


/*
WriteF('mixper:\d',mixper)
WriteF('mem is:\d*2+data:\d+chan:\d\n',SIZEOF is,SIZEOF datamain,(32*SIZEOF channel))
WriteF('mem in:\d\n',SIZEOF is+SIZEOF is+SIZEOF datamain+(32*SIZEOF channel))
WriteF('mem work:\d\n',16*BUFFLEN)
WriteF('sumfast:\d chip:\d\n',SIZEOF is+SIZEOF is+SIZEOF datamain+(32*SIZEOF channel)+(16*BUFFLEN),BUFFLEN*4)
*/
	IF mainint THEN RETURN TRUE
	NEW softint,mint,dataptr,chanptr[32]
	chip1:=NewM(BUFFLEN,MEMF_CHIP OR MEMF_CLEAR)
	chip2:=NewM(BUFFLEN,MEMF_CHIP OR MEMF_CLEAR)
	chip3:=NewM(BUFFLEN,MEMF_CHIP OR MEMF_CLEAR)
	chip4:=NewM(BUFFLEN,MEMF_CHIP OR MEMF_CLEAR)
	bufferwork:=NewR(16*BUFFLEN)
->WriteF('bf:\h\n',bufferwork)
	mint.ln.type:=NT_INTERRUPT
	mint.ln.name:='Play14_interrupt'
	mint.data:=dataptr
	mint.code:={intplay}
	softint.ln.type:=NT_INTERRUPT
	softint.ln.name:='Play14_slave_int'
	softint.ln.pri:=32
	softint.data:=dataptr
	softint.code:={mixchannels}
	dataptr.channels:=chanptr
	dataptr.bufferwork:=bufferwork
	dataptr.buffercopy:=8*BUFFLEN+bufferwork
	dataptr.bufferswap:=12*BUFFLEN+bufferwork ->12*...
	dataptr.calibration:=calibrationptr+128
	dataptr.chip1:=chip1+4 ->3
	dataptr.chip2:=chip2+4 ->0
	dataptr.chip3:=chip3+4 ->2
	dataptr.chip4:=chip4+4 ->1
	dataptr.softint:=softint

	LEA CUSTOMADDR,A0
	MOVE.W  #INTF_AUD0+INTF_AUD1+INTF_AUD2+INTF_AUD3,INTENA(A0)
	MOVE.W  #DMAF_AUD0+DMAF_AUD1+DMAF_AUD2+DMAF_AUD3,DMACON(A0)
	MOVE.W  #01,AUD2+AC_VOL(A0)
	MOVE.W  #64,AUD1+AC_VOL(A0)
	MOVE.W  #01,AUD3+AC_VOL(A0)
	MOVE.W  #64,AUD0+AC_VOL(A0)
	MOVE.L  mixper,D0
	MOVE.W  D0,AUD1+AC_PER(A0)
	MOVE.W  D0,AUD2+AC_PER(A0)
	MOVE.W  D0,AUD0+AC_PER(A0)
	MOVE.W  D0,AUD3+AC_PER(A0)
	MOVE.W  #BUFFLEN/2,D0
	MOVE.W  D0,AUD1+AC_LEN(A0)
	MOVE.W  D0,AUD2+AC_LEN(A0)
	MOVE.W  D0,AUD0+AC_LEN(A0)
	MOVE.W  D0,AUD3+AC_LEN(A0)
	MOVE.L  chip1,AUD3+AC_PTR(A0)
	MOVE.L  chip2,AUD0+AC_PTR(A0)
	MOVE.L  chip3,AUD2+AC_PTR(A0)
	MOVE.L  chip4,AUD1+AC_PTR(A0)
	SetIntVector(INTB_AUD1,mint)

	waitsome(29)

	LEA CUSTOMADDR,A0
	MOVE.W  #DMAF_AUD0+DMAF_AUD1+DMAF_AUD2+DMAF_AUD3+DMAF_SETCLR, DMACON(A0)
	MOVE.W  #INTF_AUD1, INTREQ(A0)
	MOVE.W  #INTF_AUD1+INTF_SETCLR, INTENA(A0)

	mainint:=mint

EXCEPT
	END dataptr,chanptr[32]
	IF bufferwork THEN Dispose(bufferwork)
	IF chip1 THEN Dispose(chip1)
	IF chip2 THEN Dispose(chip2)
	IF chip3 THEN Dispose(chip3)
	IF chip4 THEN Dispose(chip4)
	END mint,softint
	RETURN FALSE
ENDPROC TRUE

PROC waitsome(n)

	MOVE.L  n,D2
waitmain:
	MOVE.W  VHPOSR+CUSTOMADDR,D1
	AND.W   #$FF00,D1
waitline:
	MOVE.W  VHPOSR+CUSTOMADDR,D0
	AND.W   #$FF00,D0
	CMP.W   D0,D1
	BEQ.S   waitline
	DBRA    D2,waitmain

ENDPROC

EXPORT PROC disposeanddisable()
DEF dataptr:PTR TO datamain, mint=0:PTR TO is
	IF mint:=mainint
		mainint:=0
		LEA CUSTOMADDR,A0
		MOVE.W  #DMAF_AUD0+DMAF_AUD1+DMAF_AUD2+DMAF_AUD3,DMACON(A0)
		MOVE.W  #INTF_AUD0+INTF_AUD1+INTF_AUD2+INTF_AUD3,INTENA(A0)
		SetIntVector(INTB_AUD1,0)
		IF dataptr:=mint.data
			Dispose(dataptr.bufferwork)
			Dispose(dataptr.chip1-4)
			Dispose(dataptr.chip2-4)
			Dispose(dataptr.chip3-4)
			Dispose(dataptr.chip4-4)
			END dataptr.channels[32]
			END dataptr
		ENDIF
		END mint,softint
	ENDIF
ENDPROC

EXPORT PROC ledstatus(leds)
	MOVE.L  leds,D0
	LEA   CIAA_ADDR+CIAPRA,A0
	NOT.W   D0
	AND.W   #CIAF_LED,D0
	MOVE.B  D0,(A0)
ENDPROC

EXPORT PROC getsounddata(ptra,ptrb)
	IF mainint
		^ptra:=mainint.data+DM_BUFFERSWAP -> address of ptr to buffer
	ELSE
		^ptra:=0
	ENDIF
	^ptrb:=BUFFLEN
ENDPROC

intplay:
				MOVE.W  #INTF_AUD1,INTREQ(A0)
				MOVEM.L D2-D7/A2-A4/A6,-(A7)
				MOVE.L  DM_BUFFERSWAP(A1),A0
				MOVEM.L DM_CALIBRATION(A1),A2-A6
				MOVE.W  #BUFFLEN/4-2,D7
				MOVEQ   #$40,D5
				MOVEQ   #1,D0
				MOVEQ   #0,D4
ipcalibrloop:
				MOVE.B  D0,D4
ipcloop1:
				LSL.L   #8,D1
				MOVE.L  (A0)+,D0
				MOVE.B  D0,D1
				LSR.B   #2,D1
				ASR.W   #8,D0
				LSL.L   #8,D2
				SUB.B   D5,D1
				ADD.B   0(A2,D0.W),D1
				MOVE.B  D0,D2
				SWAP    D0
				LSL.L   #8,D3
				MOVE.B  D0,D3
				LSR.B   #2,D3
				ASR.W   #8,D0
				SUB.B   D5,D3
				ADD.B   0(A2,D0.W),D3
				LSL.L   #8,D4
				BCC.S   ipcalibrloop
				MOVE.L  D1,(A5)+  ->LO RIGHT
				MOVE.L  D2,(A6)+  ->HI RIGHT
				MOVE.B  D0,D4
				MOVE.L  D4,(A4)+  ->HI LEFT
				MOVEQ   #1,D4
				MOVE.L  D3,(A3)+  ->LO LEFT
				DBRA    D7,ipcloop1
				MOVEM.L (A7)+,D2-D7/A2-A4/A6
				TST.L   DM_BUSY(A1)
				BEQ.S   ipsoftdone
				CLR.L   DM_CHANNELMASK(A1)
				RTS
ipsoftdone:
				SUBQ.L  #1,DM_BUSY(A1)
				MOVE.L  DM_SOFTINT(A1),A1
				JMP     Cause(A6)

mixchannels:                -> executed by softint
				MOVEM.L D2-D7/A2-A4/A6,-(A7)
				MOVE.L  DM_CHAN(A1),A2           -> first channel
				SUBA.L  A4,A4
				MOVE.L  DM_CHANNELMASK(A1),D7
				BNE.S   mfirst
				MOVE.L  DM_BUFFERCOPY(A1),A4
				MOVE.W  #BUFFLEN-1,D1
				MOVEQ   #0,D0
mclearstuff:
				MOVE.L  D0,(A4)+
				DBRA    D1,mclearstuff
				BRA     ipendofint
mfirstloop:
				ADDQ.L  #1,A4
				LEA     SIZEOF channel(A2),A2
mfirst: LSR.L   #1,D7
				BCC.S   mfirstloop
				MOVE.L  DM_BUFFERWORK(A1),A5
				LEA     BUFFLEN*8(A5),A6
				LEA     CH_ENVEL(A2),A3
				MOVEM.L CH_VOLUMEL(A2),D5/D6
				MOVEM.L (A3),D1/D2
				TST.L   D2
				BEQ.S   mfev_nochange
				ADD.L   D2,(A3)
				CMP.L   EVHIT(A3),D1
				BNE.S   mfev_nochange
				TST.L   EVHIT(A3)
				BEQ.S   mfirstenvend
				MOVEM.L EVDECAY(A3), D2/D3
				MOVEM.L D2/D3,EVCLIMB(A3)
				CLR.L   EVDECAY(A3)
mfev_nochange:
				SWAP    D1
				MULU    D1,D5
				MULU    D1,D6
				LSR.L   #8,D5
				LSR.L   #8,D6
				MOVEM.L (A2),D1-D4  ->D1-curr D2-skip D3-addm D4-mod D5/D6-vol A3-end
				MOVE.L  CH_SMPENDDATA(A2),A3
				TST.W   CH_STEREO(A2)
				BNE     firstloop
				BRA     firstloopm
mnextloop:
				BNE.S   mnochange1    -> not end of sample
mfirstenvend:
				MOVE.L  A4,D0
				MOVE.L  DM_CHANNELMASK(A1),D1
				BCLR    D0,D1
				MOVE.L  D1,DM_CHANNELMASK(A1)
mnochange1:
				MOVE.L  DM_CHANNELMASK(A1),D7
				ADDQ.L  #1,A4
				MOVE.L  A4,D0
				LSR.L   D0,D7
				BNE.S   mnochange2
mnextlastenvzero:
				MOVE.L  DM_BUFFERWORK(A1),A5
				MOVE.L  DM_BUFFERCOPY(A1),A4
				LEA     BUFFLEN*8(A5),A6
mrestloop:
				MOVE.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
				ASR.L  #8,D0            -> convert to 16 bit
				MOVE.L D0,D7
				EXT.L  D7
				CMP.L  D7,D0
				BEQ.S  mnothigh
				ROL.L  #8,D0
				EXT.W  D0
				EORI.W  #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
mnothigh:
				MOVE.W D0,(A4)+
				CMPA.L A6,A5            -> A6 end of resample area
				BCS.S  mrestloop
				BRA    ipendofint
mnextdo:
				BNE.S   mnochangedo
mnextdo2:
				MOVE.L  A4,D0
				MOVE.L  DM_CHANNELMASK(A1),D1
				BCLR    D0,D1
				MOVE.L  D1,DM_CHANNELMASK(A1)
mnochangedo:
				MOVE.L  DM_CHANNELMASK(A1),D7
				MOVE.L  A4,D0
				ADDQ.L  #1,D0
				LSR.L   D0,D7
mnochloop2:
				ADDQ.L  #1,A4
mnochange2:
				LEA     SIZEOF channel(A2),A2
				LSR.L   #1,D7
				BCC.S   mnochloop2
				MOVE.L  DM_BUFFERWORK(A1),A5
				LEA     BUFFLEN*8(A5),A6
				LEA     CH_ENVEL(A2),A3
				MOVEM.L CH_VOLUMEL(A2),D5/D6
				MOVEM.L (A3),D1/D2
				TST.L   D2
				BEQ.S   mnev_nochange
				ADD.L   D2,(A3)
				CMP.L   EVHIT(A3),D1
				BNE.S   mnev_nochange
				TST.L   EVHIT(A3)
				BEQ.S   mnextenvend
				MOVEM.L EVDECAY(A3), D2/D3
				MOVEM.L D2/D3,EVCLIMB(A3)
				CLR.L   EVDECAY(A3)
mnev_nochange:
				SWAP    D1
				MULU    D1,D5
				MULU    D1,D6
				LSR.L   #8,D5
				LSR.L   #8,D6
				MOVEM.L (A2),D1-D4  ->D1-curr D2-skip D3-addm D4-mod D5/D6-vol A3-end
				MOVE.L  CH_SMPENDDATA(A2),A3
				TST.L   D7
				BEQ.S   mcloseend
				TST.W   CH_STEREO(A2)
				BNE     secloop
				BRA     secloopm
mcloseend:
				MOVE.L  A4,-(A7)
				MOVE.L  DM_BUFFERCOPY(A1),A4
				TST.W   CH_STEREO(A2)
				BNE     lastloop
				BRA     lastloopm
mnextenvend:
				TST.L   D7
				BNE     mnextdo2
				MOVE.L  A4,D0
				MOVE.L  DM_CHANNELMASK(A1),D1
				BCLR    D0,D1
				MOVE.L  D1,DM_CHANNELMASK(A1)
				BRA     mnextlastenvzero
mthisend:
				MOVE.L  (A7)+,A4
				BNE.S   ipendofint
				MOVE.L  A4,D0
				MOVE.L  DM_CHANNELMASK(A1),D1
				BCLR    D0,D1
				MOVE.L  D1,DM_CHANNELMASK(A1)

ipendofint:
				MOVE.L  DM_BUFFERSWAP(A1),A0
				MOVE.L  DM_BUFFERCOPY(A1),DM_BUFFERSWAP(A1)
				MOVE.L  A0,DM_BUFFERCOPY(A1)
				LEA     BUFFLEN*4-16(A0),A0
				MOVEM.L  DM_CALIBRATION(A1),A2-A6
				MOVEQ   #$40,D5
				MOVEQ   #1,D0
				MOVEQ   #0,D4
ipcalibrloop2:
				MOVE.B  D0,D4
				LSL.L   #8,D1
				MOVE.L  (A0)+,D0
				MOVE.B  D0,D1
				LSR.B   #2,D1
				ASR.W   #8,D0
				LSL.L   #8,D2
				SUB.B   D5,D1
				ADD.B   0(A2,D0.W),D1
				MOVE.B  D0,D2
				SWAP    D0
				LSL.L   #8,D3
				MOVE.B  D0,D3
				LSR.B   #2,D3
				ASR.W   #8,D0
				SUB.B   D5,D3
				ADD.B   0(A2,D0.W),D3
				LSL.L   #8,D4
				BCC.S   ipcalibrloop2
				MOVE.L  D1,-(A5)  ->LO RIGHT
				MOVE.L  D2,-(A6)  ->HI RIGHT
				MOVE.B  D0,D4
				MOVE.L  D4,-(A4)  ->HI LEFT
				MOVE.L  D3,-(A3)  ->LO LEFT
				MOVEM.L (A7)+,D2-D7/A2-A4/A6
				MOVEQ   #0,D0
				MOVE.L  D0,DM_BUSY(A1)
				RTS


firstloop:
INT $4CB3,$0081,$1C00 ->  MOVEM.W 0(A3,D1.L*4),D0/D7   -> D1 < 0 and increases *2 / *4 stereo
				MULS    D5,D0             -> D5 = leftvolume
				MULS    D6,D7             -> D6 = rightvolume
				MOVEM.L D0/D7,(A5)      -> the xdata 24 bit RAW LEFT
				ADDQ.L  #8,A5           -> the xdata 24 bit RAW RIGHT
				ADD.L  D3,D4            -> D4=modulo, D3=addmodulo
				ADDX.L D2,D1            -> D2=skip
				BPL.S  itsendofsample1
notendofsample1:
				CMPA.L  A6,A5            -> A6 end of resample area
				BCS.S  firstloop
				MOVE.L D4,CH_MODULO(A2)
				MOVE.L D1,CH_CURRENT(A2)
				BRA    mnextloop
itsendofsample1:
				SUB.L  CH_LOOPLENGTH(A2),D1
				TST.W  CH_LOOP(A2)
				BNE.S  notendofsample1
				MOVEQ  #0,D1
				CMPA.L  A6,A5            -> A6 end of resample area
				BCC.S  endofsample1
clearloop1:
				MOVE.L D1,(A5)+
				MOVE.L D1,(A5)+
				CMPA.L  A6,A5            -> A6 end of resample area
				BCS.S  clearloop1
endofsample1:
				MOVEQ  #0,D0             ->signal fZ end of sample
				BRA    mnextloop

secloop:
INT $4CB3,$0081,$1C00 ->  MOVEM.W 0(A3,D1.L*4),D0/D7   -> D1 < 0 and increases *2 / *4 stereo
				MULS   D5,D0             -> D5 = leftvolume
				ADD.L  D0,(A5)+         -> the xdata 24 bit RAW LEFT
				MULS   D6,D7             -> D6 = rightvolume
				ADD.L  D7,(A5)+         -> the xdata 24 bit RAW RIGHT
				ADD.L  D3,D4            -> D4=modulo, D3=addmodulo
				ADDX.L D2,D1            -> D2=skip
				BPL.S  itsendofsample2
notendofsample2:
				CMPA.L  A6,A5            -> A6 end of resample area
				BCS.S  secloop
				MOVE.L D4,CH_MODULO(A2)
				MOVE.L D1,CH_CURRENT(A2)
				BRA    mnextdo
itsendofsample2:
				SUB.L  CH_LOOPLENGTH(A2),D1
				TST.W  CH_LOOP(A2)
				BNE.S  notendofsample2
				MOVEQ  #0,D0
				BRA    mnextdo

lastloop:
LONG $30331C00 -> MOVE.W 0(A3,D1.L*4),D0   -> D1 < 0 and increases *2 / *4 stereo
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
LONG $30331C02 -> MOVE.W 2(A3,D1.L*4),D0   -> D1 < 0 and increases *2 / *4 stereo
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
				ADD.L  D3,D4            -> D4=modulo, D3=addmodulo
				ADDX.L D2,D1            -> D2=skip
				BPL.S  itsendofsample3
notendofsample3:
				CMPA.L  A6,A5            -> A6 end of resample area
				BCS.S  lastloop
				MOVE.L D4,CH_MODULO(A2)
				MOVE.L D1,CH_CURRENT(A2)
				BRA    mthisend
itsendofsample3:
				SUB.L  CH_LOOPLENGTH(A2),D1
				TST.W  CH_LOOP(A2)
				BNE.S  notendofsample3
				CMPA.L  A6,A5            -> A6 end of resample area
				BCC.S  endofsample3
restuniloop:
				MOVE.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
				ASR.L  #8,D0            -> convert to 16 bit
				MOVE.L D0,D7
				EXT.L  D7
				CMP.L  D7,D0
				BEQ.S  nothigh3
				ROL.L  #8,D0
				EXT.W  D0
				EORI.W  #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh3:
				MOVE.W D0,(A4)+
				CMPA.L  A6,A5            -> A6 end of resample area
				BCS.S  restuniloop
endofsample3:
				MOVEQ  #0,D0
				BRA    mthisend


firstloopm:
LONG $30331A00 -> MOVE.W 0(A3,D1.L*2),D0   -> D1 < 0 and increases *2 / *4 stereo
				MOVE.W  D0,D7
				MULS    D5,D0             -> D5 = leftvolume
				MULS    D6,D7             -> D6 = rightvolume
				MOVEM.L D0/D7,(A5)      -> the xdata 24 bit RAW LEFT
				ADDQ.L  #8,A5           -> the xdata 24 bit RAW RIGHT
				ADD.L  D3,D4            -> D4=modulo, D3=addmodulo
				ADDX.L D2,D1            -> D2=skip
				BPL.S  itsendofsample1m
notendofsample1m:
				CMPA.L  A6,A5            -> A6 end of resample area
				BCS.S  firstloopm
				MOVE.L D4,CH_MODULO(A2)
				MOVE.L D1,CH_CURRENT(A2)
				BRA    mnextloop
itsendofsample1m:
				SUB.L  CH_LOOPLENGTH(A2),D1
				TST.W  CH_LOOP(A2)
				BNE.S  notendofsample1m
				MOVEQ  #0,D1
				CMPA.L  A6,A5            -> A6 end of resample area
				BCC.S  endofsample1m
clearloop1m:
				MOVE.L D1,(A5)+
				MOVE.L D1,(A5)+
				CMPA.L  A6,A5            -> A6 end of resample area
				BCS.S  clearloop1m
endofsample1m:
				MOVEQ  #0,D0             ->signal fZ end of sample
				BRA    mnextloop

secloopm:
LONG $30331A00 -> MOVE.W 0(A3,D1.L*2),D0   -> D1 < 0 and increases *2 / *4 stereo
				MOVE.W  D0,D7
				MULS    D5,D0             -> D5 = leftvolume
				ADD.L   D0,(A5)+         -> the xdata 24 bit RAW LEFT
				MULS    D6,D7             -> D6 = rightvolume
				ADD.L   D7,(A5)+         -> the xdata 24 bit RAW RIGHT
				ADD.L   D3,D4            -> D4=modulo, D3=addmodulo
				ADDX.L  D2,D1            -> D2=skip
				BPL.S  itsendofsample2m
notendofsample2m:
				CMPA.L  A6,A5            -> A6 end of resample area
				BCS.S  secloopm
				MOVE.L D4,CH_MODULO(A2)
				MOVE.L D1,CH_CURRENT(A2)
				BRA    mnextdo
itsendofsample2m:
				SUB.L  CH_LOOPLENGTH(A2),D1
				TST.W  CH_LOOP(A2)
				BNE.S  notendofsample2m
				MOVEQ  #0,D0
				BRA    mnextdo

lastloopm:
LONG $30331A00 -> MOVE.W 0(A3,D1.L*2),D0   -> D1 < 0 and increases *2 / *4 stereo
				MULS   D5,D0             -> D5 = leftvolume
				ADD.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
				ASR.L  #8,D0            -> convert to 16 bit
				MOVE.L D0,D7
				EXT.L  D7
				CMP.L  D7,D0
				BEQ.S  nothigh1m
				ROL.L  #8,D0
				EXT.W  D0
				EORI.W  #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh1m:
				MOVE.W D0,(A4)+
LONG $30331A00 -> MOVE.W 0(A3,D1.L*2),D0   -> D1 < 0 and increases *2 / *4 stereo
				MULS   D6,D0             -> D6 = rightvolume
				ADD.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
				ASR.L  #8,D0            -> convert to 16 bit
				MOVE.L D0,D7
				EXT.L  D7
				CMP.L  D7,D0
				BEQ.S  nothigh2m
				ROL.L  #8,D0
				EXT.W  D0
				EORI.W  #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh2m:
				MOVE.W D0,(A4)+
				ADD.L  D3,D4            -> D4=modulo, D3=addmodulo
				ADDX.L D2,D1            -> D2=skip
				BPL.S  itsendofsample3m
notendofsample3m:
				CMPA.L  A6,A5            -> A6 end of resample area
				BCS.S  lastloopm
				MOVE.L D4,CH_MODULO(A2)
				MOVE.L D1,CH_CURRENT(A2)
				BRA    mthisend
itsendofsample3m:
				SUB.L  CH_LOOPLENGTH(A2),D1
				TST.W  CH_LOOP(A2)
				BNE.S  notendofsample3m
				CMPA.L  A6,A5            -> A6 end of resample area
				BCC.S  endofsample3m
restuniloopm:
				MOVE.L  (A5)+,D0         -> the xdata 24 bit RAW LEFT
				ASR.L  #8,D0            -> convert to 16 bit
				MOVE.L D0,D7
				EXT.L  D7
				CMP.L  D7,D0
				BEQ.S  nothigh3m
				ROL.L  #8,D0
				EXT.W  D0
				EORI.W  #$7FFF,D0        ->FFFF to 8000,  0000 to 7FFF
nothigh3m:
				MOVE.W D0,(A4)+
				CMPA.L  A6,A5            -> A6 end of resample area
				BCS.S  restuniloopm
endofsample3m:
				MOVEQ  #0,D0
				BRA    mthisend

