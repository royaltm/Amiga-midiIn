OPT MODULE
MODULE '*play14unlim','*notecalc','*extloader',
    'exec/ports','exec/memory','exec/nodes','devices/audio','exec/io',
    'mathieeedoubbas'


EXPORT OBJECT sfx
  ln:ln
  myself:PTR TO sfx
PRIVATE
  start:LONG
  loop:LONG
  length:LONG
  type:INT  -> 1 if stereo
  loadcnest:INT   -> every time sample is loaded this goes up, unload decs it
  rate[2]:ARRAY OF LONG -> sample rate (dblIEEE)
  maxvolume:INT
  name[256]:ARRAY OF CHAR
ENDOBJECT

EXPORT DEF ledbool, scopedata:PTR TO INT, scopelen
           -> ^^^ ledcontrol, ptr 4 realtime scopes

DEF arequest:PTR TO ioaudio,audioport

/* ---------------------------------------------------------------
   ==================== Object sfx methods =======================
   --------------------------------------------------------------- */

EXPORT PROC pathname() OF sfx
  IF self.name[]<>0 THEN RETURN self.name
ENDPROC ''

EXPORT PROC filetype() OF sfx
DEF t:sampleinfo
  IF self.name[]=0 THEN RETURN 0
  IF loader_recon(self.name,t)=FALSE THEN Raise("UNRE")
ENDPROC t.type,t.descr

EXPORT PROC init(name) OF sfx
DEF t:sampleinfo

  IF self.start THEN self.end() -> empty object for any case!
  self.loadcnest:=0
  self.maxvolume:=0
  self.myself:=self

  IF loader_recon(name,t)=FALSE THEN Raise("UNRE")

  AstrCopy(self.name,name,256);
  self.ln.name:=FilePart(self.name)

ENDPROC t.type,t.descr -> four letter type and string describing type


EXPORT PROC load() OF sfx HANDLE
DEF t:sampleinfo, cnst=0, cf, name

  IF self.start
    self.loadcnest:=self.loadcnest+1
    RETURN
  ENDIF
  self.myself:=self

  name:=self.name

  IF loader_recon(name,t)=FALSE THEN Raise("UNRE")

  IF loader_get(name,t)=FALSE THEN Raise("UNRE")

  IF t.channels > 1 THEN self.type:=1 ELSE self.type:=0
  cnst:=1  -> first time loaded!!!

  self.length:=t.bytelength
  self.ln.name:=FilePart(self.name)
  self.rate[0]:=0; self.rate[1]:=0
  self.setloop(t.loop)
  IF t.rate<3000 THEN t.rate:=3000
  self.setrate(t.rate)
  self.start:=t.start
  self.maxvolume:=getmaxvolume(self.start,self.length)

  IF Long({loaded})=0
    IF cf:=Open('ENVARC:CyberSound/SoundDrivers/14Bit_Calibration',OLDFILE)
      Read(cf,{calibration},256)
      PutLong({loaded},-1)
      Close(cf)
    ENDIF
  ENDIF

EXCEPT DO
  self.loadcnest:=cnst
  IF exception THEN ReThrow()
ENDPROC

EXPORT PROC unload() OF sfx
DEF c:REG
  IF self.start
    c:=self.loadcnest-1
    IF c <= 0 THEN self.end() ELSE self.loadcnest:=c
  ELSE
    self.loadcnest:=0
  ENDIF
ENDPROC

EXPORT PROC end() OF sfx
DEF a
  self.loadcnest:=0
  self.maxvolume:=0
  IF a:=self.start
    self.start:=0
    IF arequest THEN stopselectedchan(a+self.length)
    Dispose(a)
  ENDIF
ENDPROC

mixperptr:
LONG  125,$40DBB5CA,$3D70A3D7 ->28375=40DBB5CA 3D70A3D7

EXPORT PROC changepitch(chan,pitch,range,note,fine,base) OF sfx ->pitch 0 ; 16383 , 8192=nochange
DEF a:REG,b:REG,c:REG,d:REG,e,f,mpr:PTR TO LONG,mixper
  IF self.start
    IF arequest
      IF 0=mathieeedoubbasbase THEN RETURN FALSE
      pitch:=pitch-8192
      IF pitch >= 0
        a,b:=IeeeDPFlt(pitch)
        e,f:=IeeeDPDiv(a,b,$40C00000,0) ->0.0 <-> 1.0         $40C00000 $0 = 8192
        a,b:=noterate(self.rate,note+range,fine,base)
      ELSE
        a,b:=IeeeDPFlt(-pitch)
        e,f:=IeeeDPDiv(a,b,$40c00000,0) ->0.0 <-> 1.0
        a,b:=noterate(self.rate,note-range,fine,base)
      ENDIF
      c,d:=noterate(self.rate,note,fine,base)
      a,b:=IeeeDPSub(a,b,c,d) ->(freq2-freq)
      a,b:=IeeeDPMul(a,b,e,f)
      c,d:=IeeeDPAdd(a,b,c,d) ->(freq2-freq)*pitch+freq

      mpr:={mixperptr}
      mixper:=mpr[]
      a:=mpr[1]; b:=mpr[2]
      a,b:=IeeeDPDiv(c,d,a,b) ->a.x rate/mixrate
      c,d:=IeeeDPFloor(a,b) ->a.0
      a,b:=IeeeDPSub(a,b,c,d) ->.x
      c:=IeeeDPFix(c,d) ->a
      a,b:=IeeeDPMul($41E00000,$0,a,b) ->2^32*.x
      b:=Shl(IeeeDPFix(a,b),1)

      IF arequest THEN modifyfreq(chan,c,b)
    ENDIF
  ENDIF
ENDPROC TRUE

EXPORT PROC playstereo(chan,repeat,note=60,fine=0,base=60,voll=256,volr=256,pitch=8192,prange=0,t1=0,t2=0,stn=255,tskip=0) OF sfx
DEF a:REG,b:REG,c:REG,d:REG,e,f,mpr:PTR TO LONG,dmafreq,
    ev:envelope

  IF self.start
    mpr:={mixperptr}
    dmafreq:=period(mpr[])
    MOVE.L  dmafreq,a; MOVE.L t1,D0; MULU D0,a; DIVU #BUFFLEN*10,a; EXT.L a
->TextF(10,100,'dma=\d t1=\d t2=\d a=\d',dmafreq,t1,t2,a)
    IF a
      b:=Div(FIXEDMAX,a)
      ev.alt:=FIXEDMAX-Mul(b,a)
      ev.climb:=b
      ev.hit:=FIXEDMAX-b
    ELSE
      ev.alt:=FIXEDMAX-1
      ev.climb:=1
      ev.hit:=FIXEDMAX-1
    ENDIF
    MOVE.L  dmafreq,a; MOVE.L t2,D0; MULU D0,a; DIVU #BUFFLEN*10,a; EXT.L a
    MOVE.L  stn,D0; ADDQ.L #1,D0; SWAP D0; MOVE.L D0,stn
    IF a=0 THEN a:=1
    b:=Div(stn-FIXEDMAX, a)
    ev.decay:=b
    ev.susthit:=FIXEDMAX+Mul(b,a)-b

->TextF(10,10,'alt=\h climb=\h hit=\h',ev.alt, ev.climb, ev.hit)
->TextF(10,20,'decay=\h shit=\h',ev.decay, ev.susthit)

    IF 0=mathieeedoubbasbase THEN RETURN FALSE
      pitch:=pitch-8192
      IF pitch >= 0
        a,b:=IeeeDPFlt(pitch)
        e,f:=IeeeDPDiv(a,b,$40C00000,0) ->0.0 <-> 1.0         $40C00000 $0 = 8192
        a,b:=noterate(self.rate,note+prange,fine,base)
      ELSE
        a,b:=IeeeDPFlt(-pitch)
        e,f:=IeeeDPDiv(a,b,$40c00000,0) ->0.0 <-> 1.0
        a,b:=noterate(self.rate,note-prange,fine,base)
      ENDIF
      c,d:=noterate(self.rate,note,fine,base)
      a,b:=IeeeDPSub(a,b,c,d) ->(freq2-freq)
      a,b:=IeeeDPMul(a,b,e,f)
      c,d:=IeeeDPAdd(a,b,c,d) ->(freq2-freq)*pitch+freq

    a:=mpr[1]; b:=mpr[2]
    a,b:=IeeeDPDiv(c,d,a,b) ->a.x rate/mixrate
    c,d:=IeeeDPFloor(a,b) ->a.0
    e:=IeeeDPFix(c,d) ->a
    a,b:=IeeeDPSub(a,b,c,d) ->.x
    a,b:=IeeeDPMul($41E00000,$0,a,b) ->2^32*.x
    b:=Shl(IeeeDPFix(a,b),1)
->    a:=self.start+self.length
    c:=Shr(self.length,IF self.type=0 THEN 1 ELSE 2)
    IF repeat THEN d:=c-self.loop ELSE d:=0

->WriteF('chan=\d,smpframes=\d,smpendptr=\h\nlooplenframes=\d,skip=\d,modulo=\h,stereo=\d,voll=\d,volr=\d',
->chan,c,self.start+self.length,d,e,b,self.type,voll,volr)
    c:=c-Div(Mul(self.setrate(),tskip),1000)
    IF c < 0 THEN c:=0
    IF arequest
      playchannel(chan,c,self.start+self.length,d,e,b,self.type,voll,volr,ev)
    ENDIF
  ENDIF

ENDPROC TRUE


PROC setrate(newrate=0) OF sfx
DEF ret,a,b

  ret:=0
  IF mathieeedoubbasbase
    a:=self.rate[0]; b:=self.rate[1]
    IF a OR b THEN ret:=IeeeDPFix(a,b)
    IF newrate>0
      a,b:=IeeeDPFlt(newrate)
      self.rate[0]:=a; self.rate[1]:=b
    ENDIF
  ELSE
    Raise("MATH")
  ENDIF

ENDPROC ret

EXPORT PROC basefreq() OF sfx IS self.setrate()
EXPORT PROC frames() OF sfx
  IF self.start THEN RETURN Shr(self.length,IF self.type AND 1 THEN 2 ELSE 1)
ENDPROC -1
EXPORT PROC length() OF sfx
  IF self.start THEN RETURN self.length
ENDPROC -1
EXPORT PROC stereo() OF sfx
  IF self.start THEN RETURN (IF self.type AND 1 THEN TRUE ELSE FALSE)
ENDPROC FALSE
EXPORT PROC maxvolume() OF sfx
  IF self.maxvolume < 256 THEN RETURN 256
ENDPROC self.maxvolume
EXPORT PROC setloop(loopframe) OF sfx
  IF loopframe < 0 THEN RETURN self.loop
  IF Shl(loopframe,IF self.type THEN 2 ELSE 1) >= self.length THEN loopframe:=0
  self.loop:=loopframe
ENDPROC -1

/* ---------------------------------------------------------------
   ========================= other PROCs =========================
   --------------------------------------------------------------- */
EXPORT PROC changevolume(chan,volumeleft,volumeright) IS setvolume(chan,volumeleft,volumeright)

EXPORT PROC setDMAperiod(mixper)
DEF a,b,c,d,mptr:PTR TO LONG

  mptr:={mixperptr}
->  IF 0=(mathieeedoubbasbase:=OpenLibrary('mathieeedoubbas.library',0)) THEN Raise("MATH")
  IF 0=mathieeedoubbasbase THEN Raise("MATH")
  c,d:=IeeeDPFlt(mixper)
  a,b:=IeeeDPDiv($414B0F87,$80000000,c,d) ->  3546895/mixper
  mptr[]:=mixper
  mptr[1]:=a
  mptr[2]:=b
->  CloseLibrary(mathieeedoubbasbase)
  IF arequest
    freeaudiodevice()
    RETURN allocaudiodevice()
  ENDIF
ENDPROC

EXPORT PROC releasechan(chan,tenths) -> 1/10 sekundy
DEF mptr:PTR TO LONG,a:REG
  IF arequest
    IF tenths
      mptr:={mixperptr}
      a:=period(mptr[])
      MOVE.L tenths,D0; MULU D0,a; DIVU #BUFFLEN*10,a; EXT.L a
      setrelease(chan,a)
    ELSE
      MOVE.L chan,D0; MOVEQ #1,a; LSL.L D0,a
      stopchannels(a)
    ENDIF
  ENDIF
ENDPROC

EXPORT PROC soundsoff(chanmask)
  IF arequest THEN stopchannels(chanmask)
ENDPROC

EXPORT PROC soundoff(chan)
  IF arequest
    MOVEQ #1,D0; MOVE.L chan,D2; LSL.L D2,D0; MOVE.L D0,chan
    stopchannels(chan)
  ENDIF
ENDPROC

EXPORT PROC period(mixrate)
  IF mixrate THEN RETURN Div(3546895,mixrate)
ENDPROC -1

EXPORT PROC chanfree(chan)  -> 0 - 31 for one channel
  IF arequest=0 THEN RETURN TRUE
  MOVEQ #1,D0; MOVE.L chan,D1;  LSL.L D1,D0;  MOVE.L D0,chan
  IF freechannels(chan) THEN RETURN TRUE
ENDPROC FALSE

EXPORT PROC channelsfree(chanmask) IS freechannels(chanmask)

EXPORT PROC ledchange()
  IF arequest THEN ledstatus(ledbool)
ENDPROC

EXPORT PROC is_audio_on() IS IF arequest THEN TRUE ELSE FALSE

EXPORT PROC freeaudiodevice()
DEF areq=0:PTR TO ioaudio
  scopedata:=0
  IF areq:=arequest
    arequest:=0
    disposeanddisable()
    CloseDevice(areq)
    DeleteIORequest(areq)
  ENDIF
  IF audioport
    DeleteMsgPort(audioport)
    audioport:=0
  ENDIF
ENDPROC

EXPORT PROC allocaudiodevice()
DEF mpr:PTR TO LONG,areq=0:PTR TO ioaudio
  IF arequest THEN RETURN TRUE
  IF audioport:=CreateMsgPort()
    IF areq:=CreateIORequest(audioport,SIZEOF ioaudio)
      areq.data:=[15]:CHAR  -> 15(%00001111) - albo wszystko albo nic
      areq.length:=1
      areq.allockey:=0
      areq.io.command:=ADCMD_ALLOCATE
      areq.io.mn.ln.pri:=127
      mpr:={mixperptr}
      IF OpenDevice('audio.device',0,areq,0)=0
        IF allocandenable({calibration},mpr[])
          ledstatus(ledbool)
          getsounddata({scopedata}, {scopelen})
          arequest:=areq ; RETURN TRUE
        ENDIF
        CloseDevice(areq)
        DeleteIORequest(areq)
        DeleteMsgPort(audioport) ; audioport:=0
        Raise("MEM")
      ENDIF
      DeleteIORequest(areq)
    ELSE
      DeleteMsgPort(audioport) ; audioport:=0
      Raise("MEM")
    ENDIF
    DeleteMsgPort(audioport) ; audioport:=0
  ELSE
    Raise("MEM")
  ENDIF

ENDPROC FALSE


/* ---------------------------------------------------------------
   ======================== slave PROCs ==========================
   --------------------------------------------------------------- */
PROC getmaxvolume(start,length)
DEF max
  MOVE.L  start,A0;  MOVE.L  A0,A1;  ADD.L  length,A1;  MOVEQ #0,D1
mv_loop:
  MOVE.W  (A0)+,D0
  BPL.S   mv_notneg
  NEG.W   D0
mv_notneg:
  CMP.W   D0,D1
  BCC.S   mv_less
  MOVE.W  D0,D1
mv_less:
  CMPA.L   A1,A0
  BCS.S   mv_loop
  MOVE.L  D1,max
  IF max < 16384 THEN RETURN 512
  max:=32767*256/max
ENDPROC max

calibration:
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
LONG  $40404040,$40404040,$40404040,$40404040
loaded:
LONG  0


/* 
pathname=sfx.pathname()
type,descr=sfx.init(name)
sfx.load()
sfx.unload()
sfx.end()
sfx.changepitch(chan,pitch,range,note,fine,base); pitch: 0 ; 16383 , 8192=nochange
sfx.playstereo(chan,repeat,note=60,fine=0,base=60,voll=256,volr=256,pitch=8192,prange=0,t1=0,t2=0,stn=255)
sfx.setrate(newrate)
numframes=sfx.frames()
bytelength=sfx.length()
boolean=sfx.stereo()
sfx.setloop(loopframe)

changevolume(chan,volumeleft,volumeright)
setDMAperiod(mixper)
releasechan(chan,tenths) -> 1/10 sekundy
soundoff(chanmask)
mixper=period(mixrate)
boolean=chanfree(chan)
freemask=channelsfree(maskchan)
ledchange()
freeaudiodevice()
boolean=allocaudiodevice()

*/

