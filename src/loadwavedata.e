OPT MODULE
MODULE 'exec/memory'

EXPORT PROC loadWAVEdata(fh,frames,channels,bytesize) HANDLE
DEF k,l,buff=0,t,p=0,x,buffsize, skip


	k:=Shl(frames,1) ->always 16 bit!!!
	IF channels > 1 THEN k:=Shl(k,1) -> stereo!!!

	buffsize:=Mul(8192*bytesize,channels)

	l:=Mul(Mul(frames,channels),bytesize) ->readlen

 	p:=NewM(k,MEMF_FAST)
	buff:=NewR(buffsize)
 	t:=p

  IF channels > 2
    skip:=(channels-2)*bytesize ; channels:=2
  ELSE
    skip:=0 
  ENDIF

	WHILE l>0
		x:=IF l < buffsize THEN l ELSE buffsize
		IF Read(fh,buff,x)<>x THEN Raise("READ")
		l:=l-x

    MOVEM.L D3-D5,-(A7)
    MOVE.L buff,A0; MOVE.L t,A1; MOVE.L A0,A2; ADDA.L x, A2
    MOVE.L skip,D5
    MOVE.L channels,D4; SUBQ.L #1,D4
    MOVE.L bytesize,D2; SUBQ.L #1,D2; BNE.S lwd_mainloop
    MOVE.W #-128,D1
lwd8_mainloop:
    MOVE.W D4,D3
lwd8_chanloop:
    MOVE.B  (A0)+,D0; ADD.B D1,D0; LSL.W #8,D0; MOVE.W D0,(A1)+
    DBRA  D3,lwd8_chanloop
    ADDA.L D5,A0
    CMPA.L A2,A0
    BCS.S lwd8_mainloop
    BRA.S lwd_endloop
lwd_mainloop:
    MOVE.W D4,D3
lwd_chanloop:
    MOVE.W D2,D1
lwd_byteloop:
    MOVE.B (A0)+,D0; ROR.W #8,D0; DBRA D1,lwd_byteloop
    MOVE.W  D0,(A1)+
    DBRA D3,lwd_chanloop
    ADDA.L D5,A0    -> skip
    CMPA.L A2,A0
    BCS.S lwd_mainloop
lwd_endloop:
    MOVE.L  A1,t
    MOVEM.L (A7)+,D3-D5
	ENDWHILE


EXCEPT DO

IF buff THEN Dispose(buff)

IF exception
	IF p THEN Dispose(p)
	ReThrow()
ENDIF

ENDPROC p,k



