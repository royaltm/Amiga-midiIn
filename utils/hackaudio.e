MODULE 'exec/ports','exec/nodes','dos/dos','devices/audio','exec/io','exec/tasks'

CONST ILEMSG=10,DEV_OPENDEVICE=-6, DEV_CLOSEDEVICE=-12

DEF mp:PTR TO mp,functab[6]:ARRAY OF LONG,
    audioport,arequest:PTR TO ioaudio,commandnames:PTR TO LONG

OBJECT haumsg
  mn:mn
  vec:LONG
  ioa:LONG
  task:LONG
  io:ioaudio
ENDOBJECT

PROC main() HANDLE
DEF ioa:PTR TO ioaudio,sig,sigw,msg:PTR TO haumsg,vec,pri,
    s[512]:STRING,t,i,es[5]:STRING,ts
  IF (mp:=CreateMsgPort())=0 THEN Raise("MEM")
  sigw:=Shl(1,mp.sigbit) OR SIGBREAKF_CTRL_C
  installAll('Bars&PipesPro')
  REPEAT
    sig:=Wait(sigw)
    WHILE msg:=GetMsg(mp)
      vec:=msg.vec
      ioa:=msg.io
      SELECT vec
        CASE -6
          t:='OpenDevice'
        CASE -12
          t:='CloseDevice'
        CASE -18
          t:='Expunge'
        CASE -30
          t:='BeginIO'
        CASE -36
          t:='AbortIO'
        DEFAULT
          t:='??'
      ENDSELECT
      pri:=ioa.io.mn.ln.pri; MOVE.L pri,D0; EXT.W D0; EXT.L D0; MOVE.L D0,pri
      IF (ts:=msg.task)=0 THEN ts:='<none>'
      StringF(s,'\s(\h) \s flags:\h[2] ch:\h[1] pri:\d key:\d len:\d tc:\q\s\q',t,msg.ioa,commandnames[ioa.io.command], ioa.io.flags, ioa.io.unit, pri, ioa.allockey, ioa.length, ts)
      IF (ioa.io.command=ADCMD_ALLOCATE) AND (ioa.length > 0)
        StringF(es,'\n'); StrAdd(s,es)
        FOR i:=ioa.data TO ioa.data+ioa.length-1
          StringF(es,' \z\h[2]',Char(i)); StrAdd(s,es)
        ENDFOR
      ENDIF
      msg.mn.ln.type:=NT_FREEMSG
      PrintF('\s\n',s)
    ENDWHILE
  UNTIL sig AND SIGBREAKF_CTRL_C
  PrintF('Quit!\n',s)

EXCEPT DO
  freeAll()
  IF mp THEN DeleteMsgPort(mp)
ENDPROC

PROC installAll(taskname) HANDLE
DEF i,v=0,l,p,msg:PTR TO haumsg
  FOR i:=0 TO 5 DO functab[i]:=NIL
  commandnames:=['CMD_INVALID',
          'CMD_RESET',
          'CMD_READ',
          'CMD_WRITE',
          'CMD_UPDATE',
          'CMD_CLEAR',
          'CMD_STOP',
          'CMD_START',
          'CMD_FLUSH',
          'ADCMD_FREE',
          'ADCMD_SETPREC',
          'ADCMD_FINISH',
          'ADCMD_PERVOL',
          'ADCMD_LOCK',
          'ADCMD_WAITCYCLE','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31',
          'ADCMD_ALLOCATE']
  v:=0
  FOR i:=0 TO 5
    v:=v-6
    l:={mymsg}-{myfunc}
    functab[i]:=p:=NewR(l+(ILEMSG*SIZEOF haumsg))
    PutLong({msgport},mp)
    PutLong({badtaskname},taskname)
    PutLong({myexecbase},execbase)
    PutLong({myvec},v)
    CopyMem({myfunc},p,l)
    msg:=p+l; FOR p:=1 TO ILEMSG; msg.mn.ln.type:=NT_FREEMSG; msg++; ENDFOR
  ENDFOR
  IF (audioport:=CreateMsgPort())=NIL THEN Raise("MEM")
  IF (arequest:=CreateIORequest(audioport,SIZEOF ioaudio))=NIL THEN Raise("MEM")
  arequest.data:=0
  arequest.length:=0
  arequest.allockey:=0
  arequest.io.command:=CMD_INVALID
  arequest.io.mn.ln.pri:=-128
  IF OpenDevice('audio.device', 0, arequest, 0)<>0 THEN Raise("OPAU")
  v:=0
  FOR i:=0 TO 5
    v:=v-6
    PutLong(functab[i]+{mykey}-{myfunc},arequest.allockey)
    PutLong(functab[i]+{oldfunc}-{myfunc},SetFunction(arequest.io.device, v, functab[i]))
  ENDFOR
EXCEPT
  FOR i:=0 TO 5
    IF functab[i]<>NIL; Dispose(functab[i]); functab[i]:=NIL; ENDIF
  ENDFOR
  IF arequest THEN DeleteIORequest(arequest) ; arequest:=0
  IF audioport THEN DeleteMsgPort(audioport) ; audioport:=0
  ReThrow()
ENDPROC

PROC freeAll()
DEF v=0,oldf,i
  FOR i:=0 TO 5  
    v:=v-6
    IF functab[i]
      oldf:=Long(functab[i]+{oldfunc}-{myfunc})
      SetFunction(arequest.io.device, v, oldf)
    ENDIF
  ENDFOR
  IF arequest; CloseDevice(arequest); DeleteIORequest(arequest) ; arequest:=0; ENDIF
  IF audioport THEN DeleteMsgPort(audioport) ; audioport:=0
  FOR i:=0 TO 5 DO IF functab[i]<>NIL THEN Dispose(functab[i])
ENDPROC


myfunc:
        MOVEM.L D0/A0-A3/A6,-(A7)
        LEA     mymsg(PC),A2
        MOVEQ   #ILEMSG-1,D0
myloop: CMPI.B  #NT_FREEMSG,8(A2)
        BEQ.S   myskip1
        LEA     SIZEOF haumsg(A2),A2
        DBRA    D0,myloop
        BRA.S   myskipit
myskip1:
        MOVE.L  A1,-(A7)
        MOVE.L  myvec(PC),SIZEOF mn(A2)
        MOVE.L  A1,SIZEOF mn+4(A2)
        MOVEQ   #SIZEOF ioaudio-1,D0
        LEA     SIZEOF mn+12(A2),A0
mycplp: MOVE.B  (A1)+,(A0)+
        DBRA    D0,mycplp

        MOVE.L  myexecbase(PC),A6
        MOVE.L  276(A6),A3 ;->this task
        CMPI.B  #NT_PROCESS,8(A3)
        BNE.S   mynamefound
        MOVE.L  172(A3),D0 ;->pr_CLI
        BEQ.S   mynamefound
        LSL.L   #2,D0
        MOVE.L  D0,A3
        MOVE.L  16(A3),D0 ;-> command Name
        LSL.L   #2,D0
        MOVE.L  D0,A3
mynametrim:
        CMPI.B  #33,(A3)+
        BCS.S   mynametrim
        SUBQ.L  #1,A3
        BRA.S   mynamefound2
mynamefound:
        MOVE.L  10(A3),A3
mynamefound2:
        MOVE.L  A3,SIZEOF mn+8(A2) -> task/process cli name
        MOVE.L  A2,A1
        MOVE.L  msgport(PC),A0
        JSR     PutMsg(A6)
        MOVE.L  (A7)+,A1
myskipit:
        MOVEQ   #DEV_BEGINIO,D0
        CMP.L   myvec(PC),D0      -> czy BeginIO()
        BEQ.S   mybeginio
        MOVEQ   #DEV_OPENDEVICE,D0
        CMP.L   myvec(PC),D0      -> czy OpenDevice()
        BEQ.S   myopendevice
        MOVEQ   #DEV_CLOSEDEVICE,D0
        CMP.L   myvec(PC),D0      -> czy CloseDevice()
        BEQ     myclosedevice
mynotbeginio:
        MOVEM.L (A7)+,D0/A0-A3/A6
        MOVE.L  A2,-(A7)
        MOVE.L  oldfunc(PC),A2
        JSR     (A2)
        MOVE.L (A7)+,A2
        RTS
mybeginio:
        MOVE.L  mykey(PC),D0
        CMP.W   SIZEOF io(A1),D0 -> allockey
        BNE.S   mynotbeginio
        CMP.W   #CMD_WRITE,SIZEOF mn+8(A1) -> czy WRITE
        BEQ.S   myadcmdwrite
        BRA.S   myadcmdflush
myadcmdwrite:
        BCLR    #IOB_QUICK,SIZEOF mn+10(A1) ->Flags
myadcmdflush:
        CLR.B   SIZEOF mn+11(A1) -> Error
        BTST    #IOB_QUICK,SIZEOF mn+10(A1) ->Flags
        BNE.S   myend
        MOVE.L  myexecbase(PC),A6
        JSR     ReplyMsg(A6)
myend:  MOVEM.L (A7)+,D0/A0-A3/A6
        RTS
myopendevice:
        MOVE.L  badtaskname(PC),A0
        MOVE.L  A3,D0 -> taskname
        BEQ     mynotbeginio
mynamecmplp:
        MOVE.B  (A0)+,D0
        CMP.B   (A3)+,D0
        BNE     mynotbeginio
        TST.B   D0
        BNE.S   mynamecmplp
        MOVE.L  SIZEOF io+2(A1),A0 ->Data
        MOVE.B  (A0),D0
        MOVE.L  D0,SIZEOF mn+4(A1) -> Unit
        CLR.B   SIZEOF mn+11(A1) -> Error
        MOVE.L  mykey(PC),D0
        MOVE.W  D0,SIZEOF io(A1) -> allockey
        MOVEM.L (A7)+,D0/A0-A3/A6
        MOVE.L  A6,SIZEOF mn(A1) -> Device
        MOVEQ   #0,D0
        RTS
myclosedevice:
        MOVE.L  mykey(PC),D0
        CMP.W   SIZEOF io(A1),D0 -> allockey
        BNE     mynotbeginio
        MOVEQ   #-1,D0
        MOVE.L  D0,SIZEOF mn(A1) -> Device
        CLR.L   SIZEOF mn+4(A1)  -> Unit
        MOVEM.L (A7)+,D0/A0-A3/A6
        MOVEQ   #0,D0
        RTS


badtaskname:LONG 0
myexecbase:LONG 0
msgport:LONG 0
oldfunc:LONG 0
mykey:  LONG 0
myvec:  LONG 0
mymsg:  LONG 0
