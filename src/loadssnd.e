OPT MODULE
MODULE 'exec/memory'


EXPORT PROC loadSSND(fh,frames,smpsize,channels) HANDLE
DEF k,l,buff=0,z,t,p=0,x,buffsize

	k:=Shl(frames,1) ->always 16 bit!!!
	IF channels>1 THEN	k:=Shl(k,1) -> stereo!!!

	IF smpsize <= 8
		z:=1
	ELSEIF smpsize <= 16
		z:=2
	ELSEIF smpsize <= 24
		z:=3
	ELSE
		z:=4
	ENDIF
	buffsize:=Mul(8192*z,channels)

	l:=Mul(Mul(frames,channels),z) ->readlen

 	p:=NewM(k,MEMF_FAST)
	buff:=NewR(buffsize)
 	t:=p

	WHILE l>0
		x:=min(l,buffsize)
		IF Read(fh,buff,x)<>x THEN Raise("READ")
		l:=l-x

		MOVE.L	t,A1 ;				MOVE.L	buff,A0 ;				MOVE.L	x,A2	->endbuf
		ADD.L		A0,A2 ;				MOVE.L	channels,D1 ;		SUBQ.L	#1,D1

		MOVE.L	z,D0	;				SUBQ.L	#1,D0	;					BEQ		loop8
		SUBQ.L	#1,D0	;				BEQ		loop16
		SUBQ.L	#1,D0	;				BEQ		loop24
		BRA		loop32
return:
		MOVE.L	A1,t
	ENDWHILE


EXCEPT DO

IF buff THEN Dispose(buff)

IF exception
	IF p THEN Dispose(p)
	ReThrow()
ENDIF

ENDPROC p,k

PROC min(a,b) IS IF a<b THEN a ELSE b

loop8:
				MOVE.B	(A0)+,D0
				LSL.W		#8,D0
				MOVE.W	D0,(A1)+
				MOVE.L	D1,D2
				BEQ.S		mono8
				SUBQ.L	#1,D2
stereo8:
				MOVE.B	(A0)+,D0
				DBRA		D2,stereo8
				LSL.W		#8,D0
				MOVE.W	D0,(A1)+
mono8:	CMPA.L	A2,A0
				BCS.S		loop8
				BRA			return

loop16:
				MOVE.W	(A0)+,(A1)+
				MOVE.L	D1,D2
				BEQ.S		mono16
				SUBQ.L	#1,D2
stereo16:
				MOVE.W	(A0)+,D0
				DBRA		D2,stereo16
				MOVE.W	D0,(A1)+
mono16:	CMPA.L	A2,A0
				BCS.S		loop16
				BRA			return

loop24:
				MOVE.B	(A0)+,D0
				LSL.W		#8,D0
				MOVE.B	(A0)+,D0
				ADDQ.L	#1,A0
				MOVE.W	D0,(A1)+
				MOVE.L	D1,D2
				BEQ.S		mono24
				SUBQ.L	#1,D2
stereo24:
				MOVE.B	(A0)+,D0
				LSL.W		#8,D0
				MOVE.B	(A0)+,D0
				ADDQ.L	#1,A0
				DBRA		D2,stereo24
				MOVE.W	D0,(A1)+
mono24:	CMPA.L	A2,A0
				BCS.S		loop24
				BRA			return

loop32:
				MOVE.L	(A0)+,D0
				SWAP		D0
				MOVE.W	D0,(A1)+
				MOVE.L	D1,D2
				BEQ.S		mono32
				SUBQ.L	#1,D2
stereo32:
				MOVE.L	(A0)+,D0
				DBRA		D2,stereo32
				SWAP		D0
				MOVE.W	D0,(A1)+
mono32:	CMPA.L	A2,A0
				BCS.S		loop32
				BRA			return

