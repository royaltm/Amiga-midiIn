OPT MODULE

MODULE  'dos/dos', 'exec/memory'

EXPORT PROC loadsvxBODY(fh,frames,smpsize,chanoffset) HANDLE
DEF t,k,z,p=0,buffsize,buff=0,l,x,skip=FALSE,f

	k:=Shl(frames,1) ->always 16 bit!!!
	IF chanoffset THEN	skip:=1
  k:=Shl(k,skip) -> stereo!!!

	IF smpsize <= 8 THEN z:=1 ELSE z:=2

	buffsize:=16384*z

 	p:=NewM(k,MEMF_FAST)
	buff:=NewR(buffsize)

  FOR f:=0 TO skip

   	t:=p+(f*2)
  	l:=Mul(frames,z) ->readlen

  	WHILE l > 0
  		x:=min(l,buffsize)
  		IF Read(fh,buff,x)<>x THEN Raise("READ")
  		l:=l-x
      chanoffset:=chanoffset-x

  		MOVE.L	t,A1 ;				MOVE.L	buff,A0 ;				MOVE.L	x,A2	->endbuf
  		ADD.L		A0,A2;        MOVE.L  skip,D1   ->flag to skip
  		MOVE.L	z,D0	;				SUBQ.L	#1,D0	;					BEQ		svx_loop8
  		BRA		svx_loop16
svx_powrot:
  		MOVE.L	A1,t
  	ENDWHILE
    IF (skip <> 0) AND (f=0)
      IF Seek(fh,chanoffset,OFFSET_CURRENT) = -1 THEN Raise("READ")
    ENDIF

  ENDFOR

EXCEPT DO

IF buff THEN Dispose(buff)

IF exception
	IF p THEN Dispose(p)
	ReThrow()
ENDIF

ENDPROC p,k

PROC min(a,b) IS IF a<b THEN a ELSE b


svx_loop8:
				MOVE.B	(A0)+,D0
				LSL.W		#8,D0
				MOVE.W	D0,(A1)+
				TST.L	  D1
				BEQ.S		svx_mono8
				ADDQ.L	#2,A1
svx_mono8:
      	CMPA.L	A2,A0
				BCS.S		svx_loop8
				BRA			svx_powrot

svx_loop16:
				MOVE.W	(A0)+,(A1)+
				TST.L   D1
				BEQ.S		svx_mono16
				ADDQ.L	#2,A1
svx_mono16:
      	CMPA.L	A2,A0
				BCS.S		svx_loop16
				BRA			svx_powrot
