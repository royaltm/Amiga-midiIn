OPT MODULE
OPT OSVERSION=37

MODULE '*playAHI_custom','*notecalc','*extloader','*mbfxdefs',
    'exec/ports','exec/memory','exec/nodes','devices/audio','exec/io',
    'mathieeedoubbas','utility','utility/tagitem'

EXPORT ENUM SFX_SET_AUDIO_STATUS=TAG_USER,
            SFX_SET_AUDIO_ID,
            SFX_SET_CHANNELS,
            SFX_SET_MIXFREQ,
            SFX_SET_APPLYAUDIO,
            SFX_GET_AUDIO_STATUS

EXPORT OBJECT sfx
  ln:ln
  myself:PTR TO sfx
PRIVATE
  start:LONG
  loop:LONG
  length:LONG
  type:INT  -> 1 if stereo
  loadcnest:INT   -> every time sample is loaded this goes up, unload decs it
  id:LONG
  rate:LONG -> sample rate
  maxvolume:INT
  name[256]:ARRAY OF CHAR
ENDOBJECT


/* ---------------------------------------------------------------
   ========================= sfx init ============================
   --------------------------------------------------------------- */

EXPORT PROC initsoundfx(numsamples)
  snd_init(numsamples)
ENDPROC

EXPORT PROC freesoundfx()
  snd_end()
ENDPROC

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
  self.id:=0
  self.myself:=self

  IF loader_recon(name,t)=FALSE THEN Raise("UNRE")

  AstrCopy(self.name,name,256);
  self.ln.name:=FilePart(self.name)

ENDPROC t.type,t.descr -> four letter type and string describing type


EXPORT PROC load() OF sfx HANDLE
DEF t:sampleinfo, cnst=0, name

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
  self.setloop(t.loop)
  IF t.rate<3000 THEN t.rate:=3000
  self.setrate(t.rate)
  self.start:=t.start
  self.maxvolume:=getmaxvolume(self.start,self.length)

  IF cnst=1
    self.id:=snd_setsample(self.start, self.frames(), self.type)
    IF self.id=0
      self.end(); cnst:=0
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
    snd_delsample(self.id)
    Dispose(a)
  ENDIF
ENDPROC

EXPORT PROC changepitch(chan,pitch,range,note,fine,base,uptime=0,tyl=FALSE) OF sfx ->pitch 0 ; 16383 , 8192=nochange
DEF a:REG,b:REG,c:REG,d:REG,e,f, rate[2]:ARRAY OF LONG
  IF self.start
    IF 0=mathieeedoubbasbase THEN RETURN FALSE
    pitch:=pitch-8192
    IF pitch >= 0
      a,b:=IeeeDPFlt(pitch)
      e,f:=IeeeDPDiv(a,b,$40C00000,0) ->0.0 <-> 1.0         $40C00000 $0 = 8192
      c,d:=IeeeDPFlt(self.rate); rate[0]:=c; rate[1]:=d
      a,b:=noterate(rate,note+range,fine,base)
    ELSE
      a,b:=IeeeDPFlt(-pitch)
      e,f:=IeeeDPDiv(a,b,$40c00000,0) ->0.0 <-> 1.0
      c,d:=IeeeDPFlt(self.rate); rate[0]:=c; rate[1]:=d
      a,b:=noterate(rate,note-range,fine,base)
    ENDIF
    c,d:=noterate(rate,note,fine,base)
    a,b:=IeeeDPSub(a,b,c,d) ->(freq2-freq)
    a,b:=IeeeDPMul(a,b,e,f)
    c,d:=IeeeDPAdd(a,b,c,d) ->(freq2-freq)*pitch+freq

    a:=IeeeDPFix(c,d); IF tyl THEN a:=-a
    snd_setfreq(chan,uptime,a)
  ENDIF
ENDPROC TRUE

EXPORT PROC play(chan,repeat,note=60,fine=0,base=60,volume=65536,pan=$8000,pitch=8192,prange=0,t1=0,t2=0,stn=255,tskip=0) OF sfx
DEF a:REG,b:REG,c:REG,d:REG,e,f, rate[2]:ARRAY OF LONG,
    ev:envelope

  IF self.start
    IF ((t1 OR t2)=0) AND (stn=255)
      ev:=0
    ELSE
      ev.attack:=t1
      ev.decay:=t2
      ev.sustain:=stn
    ENDIF
    IF 0=mathieeedoubbasbase THEN RETURN FALSE
    pitch:=pitch-8192
    IF pitch >= 0
      a,b:=IeeeDPFlt(pitch)
      e,f:=IeeeDPDiv(a,b,$40C00000,0) ->0.0 <-> 1.0         $40C00000 $0 = 8192
      c,d:=IeeeDPFlt(self.rate); rate[0]:=c; rate[1]:=d
      a,b:=noterate(rate,note+prange,fine,base)
    ELSE
      a,b:=IeeeDPFlt(-pitch)
      e,f:=IeeeDPDiv(a,b,$40c00000,0) ->0.0 <-> 1.0
      c,d:=IeeeDPFlt(self.rate); rate[0]:=c; rate[1]:=d
      a,b:=noterate(rate,note-prange,fine,base)
    ENDIF
    c,d:=noterate(rate,note,fine,base)
    a,b:=IeeeDPSub(a,b,c,d) ->(freq2-freq)
    a,b:=IeeeDPMul(a,b,e,f)
    c,d:=IeeeDPAdd(a,b,c,d) ->(freq2-freq)*pitch+freq

    a:=IeeeDPFix(c,d)

->WriteF('chan=\d,smpframes=\d,smpendptr=\h\nlooplenframes=\d,skip=\d,modulo=\h,stereo=\d,voll=\d,volr=\d',
->chan,c,self.start+self.length,d,e,b,self.type,voll,volr)
    snd_playsample(chan,self.id,tskip,a,volume,pan,IF repeat THEN self.loop ELSE -1,ev)
  ENDIF

ENDPROC TRUE


PROC setrate(newrate=0) OF sfx
DEF ret

  ret:=self.rate
  IF newrate>0 THEN self.rate:=newrate

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
EXPORT PROC audio_attrs(tags:PTR TO tagitem) -> dokoïcz!!!!!!!!!!!!!
DEF tg,tstate,d,audioon=FALSE
  tstate:=tags
  WHILE tags:=NextTagItem({tstate})
    tg:=tags.tag; d:=tags.data
    SELECT tg
      CASE SFX_SET_AUDIO_STATUS
        snd_stopchannelmask($FFFFFFFF)
        IF d THEN audioon:=snd_audioon() ELSE snd_audiooff()
      CASE SFX_SET_AUDIO_ID
        snd_setaudioid(d)
      CASE SFX_SET_CHANNELS
        snd_setnumchannels(d)
      CASE SFX_SET_MIXFREQ
        snd_setmixfreq(d)
      CASE SFX_SET_APPLYAUDIO
        IF d THEN IF is_ahi_on() THEN audioon:=snd_audioon()
      CASE SFX_GET_AUDIO_STATUS
        audioon:=is_ahi_on(); IF d THEN ^d:=audioon
    ENDSELECT
  ENDWHILE
ENDPROC audioon

EXPORT PROC changevolume(chan,volume,pan) IS snd_setvolume(chan,volume,pan)

EXPORT PROC releasechan(chan,tenths) IS snd_release(chan,tenths)

EXPORT PROC soundsoff(chanmask)
  IF chanmask THEN snd_stopchannelmask(chanmask)
ENDPROC

EXPORT PROC soundoff(chan)
  snd_stopchannel(chan)
ENDPROC

EXPORT PROC chanfree(chan)  -> 0 - 31 for one channel
  MOVE.L chan,D0; MOVEQ #1,D1; LSL.L D0,D1; MOVE.L D1,chan
ENDPROC IF get_freechannels(chan) THEN TRUE ELSE FALSE

EXPORT PROC channelsfree(chanmask) IS get_freechannels(chanmask)



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

