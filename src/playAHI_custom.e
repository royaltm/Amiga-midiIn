OPT MODULE
OPT PREPROCESS
OPT OSVERSION=37

MODULE 'exec/io','exec/ports','exec/nodes','ahi','devices/ahi','utility/hooks','utility/tagitem',
       'tools/installhook','*modules/softmix','*mbfxdefs'



/*
 success:=snd_init(maxsamples)
          snd_end()
 success:=snd_audioon(channels,mixfreq)
          snd_audiooff()
 success:=snd_setsample(number,address,length,stereo)
          snd_delsample(number)
 channel:=snd_playsample(channel,number,offset,freq,volume,pan,stereo,loop,envelope,am,fm)
          snd_stopchannel(channel)
          snd_stopsample(number)
          snd_setfreq(channel,newfreq)
          snd_setglide(channel,uptime,destfreq)
          snd_setvolume(channel,newvolume,newpan)
          snd_release(channel,releasetime)
*          set_AMmodulation(channel,am)
*          set_FMmodulation(channel,fm)
*chanmask:=get_freechannels(chanmask)
*          get_sounddataoutput(ptra,ptrb)
*/
/*
PROC snd_init(maxsamples)
PROC snd_audiooff()
PROC snd_playsample(channel,number,offset,freq,volume,pan,loop=-1,env=0)
PROC snd_delsample(number)
PROC snd_setsample(address,length,stereo)
PROC get_freechannels(chanmask)
PROC snd_setnumchannels(channels)
PROC is_ahi_on()
PROC snd_stopchannel(channel)
PROC snd_setaudioid(audioid)
PROC ledstatus(leds)
PROC snd_stopchannelmask(channel)
PROC snd_audioon()
PROC snd_end()
PROC snd_setvolume(channel,newvolume,newpan)
PROC snd_release(channel,release)
PROC snd_setfreq(channel,uptime,destfreq)
PROC snd_setmixfreq(newmixfreq)

DEF scopedata,ahibase,scopelen
/* 15 private global variable(s) in this module */


  code from module "modules/softmix.m" used:
    mxcmd_setvolume()/4 (2x)
    mxcmd_setfrequency()/3 (2x)
    mxcmd_freechannels()/2 (1x)
    mxcmd_stopchannelmask()/2 (2x)
    mxcmd_stopchannel()/2 (3x)
    mxcmd_playchannel()/8 (1x)
    mxcmd_setmixperiod()/2 (1x)
    mxcmd_unload()/2 (1x)
    mxcmd_load()/4 (1x)
    mxcmd_end()/1 (1x)
    mxcmd_setup()/5 (1x)
  code from module "emodules:tools/installhook.m" used:
    installhook()/2 (2x)
*/

/*
OBJECT udata
  outsampledata:PTR TO INT
  channeldata:PTR TO channel
  channelmask:LONG
  bufwork:PTR TO LONG
  workframes:LONG
  mixfreq:LONG
  sampledata:LONG
ENDOBJECT

OBJECT mchannel
  sampleinfo:LONG
  actfreq:LONG
  skip:LONG      
  addmodulo:LONG 
  loopframes:LONG
  volumeleft:INT 
  volumeright:INT
	current:LONG
	pointer:LONG
	modulo:LONG 
  flags:LONG
ENDOBJECT
*/

OBJECT channel
  volume:LONG  -> 0 = off
  freq:LONG
  voladd:LONG  -> 0 = noaddenvelope
  volenv:LONG  -> 0 = noenvelope
  voltarget:LONG
  voldecay:LONG
  pan:LONG
  freqtarget:LONG
  freqadd:LONG -> 0 = nochange
  uptime:LONG
/*
  fmdata:PTR TO INT
  amdata:PTR TO INT
  fmampfp:INT
  fmampexp:INT -> max 15
  fmspeed:LONG -> fixed
  fmcurrent:LONG -> fixed
  fmlength:LONG -> fixed
  amamp:LONG -> 0-65535 =100%
  amspeed:LONG -> fixed
  amcurrent:LONG -> fixed
  amlength:LONG -> fixed
*/
ENDOBJECT  

DEF ahimp:PTR TO mp, ahiio:PTR TO ahirequest, ahidevice, ahictrl:PTR TO ahiaudioctrl,
    phook:PTR TO hook,shook:PTR TO hook,chans:PTR TO channel,
    userdata:PTR TO LONG,mixcode,outbuffer:PTR TO LONG,outbufswap,
    mixfreq,channelsnum,ahiaudioid,cntmix

EXPORT DEF scopedata:PTR TO INT, scopelen
           -> ^^^ ptr 4 realtime scopes

EXPORT PROC snd_init(maxsamples) HANDLE
  NEW chans[32],phook,shook,outbuffer[512]
  installhook(phook, {realtimeplay})
  installhook(shook, {soundmix})
  mixfreq:=28000; channelsnum:=32
  userdata,mixcode:=mxcmd_setup(mixfreq,channelsnum,maxsamples,outbuffer,256)
  IF userdata=NIL THEN Raise("MEM")
  IF (ahimp:=CreateMsgPort())=NIL THEN Raise("MEM")
  IF (ahiio:=CreateIORequest(ahimp, SIZEOF ahirequest))=NIL THEN Raise("MEM")
  ahiio.version:=4  /* Open at least version 4. */
  IF OpenDevice('ahi.device', AHI_NO_UNIT, ahiio, NIL) THEN Raise("NAHI")
  ahidevice:=TRUE
  ahibase:=ahiio.std::io.device
EXCEPT
  IF ahiio THEN DeleteIORequest(ahiio); ahiio:=NIL
  IF ahimp THEN DeleteMsgPort(ahimp); ahimp:=NIL
  ReThrow()
ENDPROC

EXPORT PROC snd_end()
  IF ahidevice
    IF ahictrl THEN snd_audiooff()
    ahidevice:=FALSE
    CloseDevice(ahiio)
    IF ahiio THEN DeleteIORequest(ahiio); ahiio:=NIL
    IF ahimp THEN DeleteMsgPort(ahimp); ahimp:=NIL
    mxcmd_end(userdata)
  ENDIF
  END chans[32],phook,shook,outbuffer[512]
ENDPROC

EXPORT PROC snd_audioon() HANDLE
DEF pfreq,ahismpinfo:ahisampleinfo,i
  IF ahidevice=FALSE THEN RETURN FALSE
  IF ahictrl THEN snd_audiooff()
  scopelen:=256
  pfreq:=Shl(50,16)
  IF NIL=(ahictrl:=AhI_AllocAudioA([AHIA_AUDIOID,ahiaudioid,
                             AHIA_MIXFREQ,mixfreq,
                             AHIA_CHANNELS,1,
                             AHIA_SOUNDS,1,
                             AHIA_SOUNDFUNC,shook,
                             AHIA_PLAYERFUNC, phook,
                             AHIA_PLAYERFREQ, pfreq,
                             AHIA_MINPLAYERFREQ, pfreq-65536,
                             AHIA_MAXPLAYERFREQ, pfreq+65536,
                             AHIA_USERDATA,0,TAG_DONE])) THEN RETURN FALSE
  IF AHIE_OK<>AhI_ControlAudioA(ahictrl,[AHIC_PLAY,TRUE,
                                         AHIC_MIXFREQ_QUERY,{mixfreq},
                                         TAG_DONE]) THEN RETURN FALSE
  mxcmd_setmixperiod(mixfreq,userdata)
  ahismpinfo.type:=AHIST_S16S
  ahismpinfo.address:=outbuffer
  ahismpinfo.length:=256
  FOR i:=0 TO 511 DO outbuffer[i]:=NIL
  IF AHIE_OK <> AhI_LoadSound(0, AHIST_DYNAMICSAMPLE, ahismpinfo, ahictrl) THEN RETURN FALSE
  AhI_PlayA( ahictrl, [
   AHIP_BEGINCHANNEL,0,
   AHIP_FREQ,AHI_MIXFREQ,
   AHIP_VOL,$10000,
   AHIP_PAN,$8000,
   AHIP_SOUND,0,
   AHIP_OFFSET,outbufswap,
   AHIP_LENGTH,256,
   AHIP_ENDCHANNEL,0,TAG_DONE,0])

EXCEPT
  snd_audiooff()
  ReThrow()
ENDPROC TRUE

EXPORT PROC snd_setaudioid(audioid)
  IF audioid <> AHI_INVALID_ID THEN ahiaudioid:=audioid
ENDPROC
EXPORT PROC snd_setnumchannels(channels)
  IF channels THEN channelsnum:=channels
  IF channels < 32
    MOVE.L channels,D1; MOVEQ #1,D0; LSL.L D1,D0; NEG.L D0; MOVE.L D0,channels
    mxcmd_stopchannelmask(channels,userdata)
  ENDIF
ENDPROC
EXPORT PROC snd_setmixfreq(newmixfreq)
  IF newmixfreq THEN mixfreq:=newmixfreq
ENDPROC

EXPORT PROC snd_audiooff()
DEF ahc
  IF ahc:=ahictrl
    cntmix:=0; WHILE cntmix < 2 DO NOP
    ahictrl:=NIL; AhI_FreeAudio(ahc)
  ENDIF
  scopedata:=0
ENDPROC

EXPORT PROC snd_setsample(address,length,stereo)
  IF ahidevice=FALSE THEN RETURN FALSE
ENDPROC mxcmd_load(address,length,stereo,userdata)

EXPORT PROC snd_delsample(number)
  IF ahidevice=FALSE THEN RETURN
  IF number THEN mxcmd_unload(number,userdata)
ENDPROC

EXPORT PROC snd_playsample(channel,number,offset,freq,volume,pan,loop=-1,env=0:PTR TO envelope)
DEF chan:PTR TO channel,a:REG,b:REG,c:REG
->DEF udata:PTR TO udata, mch:PTR TO mchannel,smp:PTR TO LONG
  IF ahictrl
    chan:=chans[channel]
    mxcmd_stopchannel(channel,userdata)
/*
    IF fm
      chan.fmdata:=fm.data
      chan.fmlength:=Shl(fm.length,16); chan.fmcurrent:=fm.length; chan.fmspeed:=fm.speed
      a:=chan.amplitude; MOVEQ #0,D1; MOVE.L a,D0;/*BFFFO D0{0,15},D1*/ LONG $EDC0100F
      MOVEQ #16,D0; SUB.L D1,D0; SUBI.L #$10000,a; LSR.L D0,a; MOVE.L D1,b
      chan.fmampfp:=a; chan.fmampexp:=b
    ENDIF
*/
    IF env
      IF a:=env.attack*5
        MOVE.L #65535,D0; DIVU a,D0; MOVEQ #0,a; MOVE.W D0,a
      ELSE
        MOVE.L #65535,a
      ENDIF
      IF c:=env.sustain
        MOVE.L c,D0; LSL.L #8,c; MOVE.B D0,c
      ELSE
        MOVEQ #1,c
      ENDIF
      IF b:=env.decay*5
        MOVE.L #65535,D0; SUB.L c,D0; DIVU b,D0; ; MOVEQ #0,b; MOVE.W D0,b
      ELSE
        MOVE.L #65535,b
      ENDIF
    ENDIF
    chan.volume:=volume; chan.freqadd:=0; chan.uptime:=0; chan.pan:=pan; chan.freq:=freq
    IF env
      chan.voladd:=a; chan.volenv:=1; chan.voldecay:=-b; chan.voltarget:=c
      volume:=0
    ELSE
      chan.voladd:=0; chan.volenv:=0; chan.voltarget:=1
    ENDIF
    IF number THEN mxcmd_playchannel(offset,freq,volume,pan,loop,channel,number,userdata)

/*
    udata:=userdata
    mch:=udata.channeldata
    WriteF('cmask=\h \n',udata.channelmask)
    WriteF('ch:\d \h\n\h\n\h\n\h\n\h\n\h\n\h\n\h\n\h\n\h\n\h\n',mch[channel],mch[channel].sampleinfo,
  mch[channel].actfreq,
  mch[channel].skip,
  mch[channel].addmodulo,
  mch[channel].loopframes,
  mch[channel].volumeleft,
  mch[channel].volumeright,
	mch[channel].current,
	mch[channel].pointer,
	mch[channel].modulo,
  mch[channel].flags)
    smp:=mch[channel].sampleinfo
    WriteF('\h, \h, \h\n',smp[0],smp[1],smp[2])
*/
  ENDIF
ENDPROC

EXPORT PROC snd_stopchannel(channel)
  IF ahidevice THEN mxcmd_stopchannel(channel,userdata)
ENDPROC

EXPORT PROC snd_stopchannelmask(channel)
  IF ahidevice THEN mxcmd_stopchannelmask(channel,userdata)
ENDPROC

EXPORT PROC snd_setvolume(channel,newvolume,newpan)
DEF chan:PTR TO channel
  IF ahictrl
    chan:=chans[channel]
    chan.volume:=newvolume
    chan.pan:=newpan
    IF chan.volenv=0 THEN mxcmd_setvolume(newvolume,newpan,channel,userdata)
  ENDIF
ENDPROC

EXPORT PROC snd_release(channel,release)
DEF chan:PTR TO channel,a:REG,b:REG, v:REG
  IF ahictrl
    chan:=chans[channel]
    IF chan.voltarget=0 THEN RETURN -> already released!
    b:=IF (v:=chan.volenv)=0 THEN 65535 ELSE v
    IF a:=release*5
      MOVE.L b,D0; DIVU a,D0; MOVEQ #0,a; MOVE.W D0,a
      chan.voladd:=0; chan.voltarget:=0; IF v=0 THEN chan.volenv:=b
      chan.voladd:=IF a THEN -a ELSE -1
    ELSE
      snd_stopchannel(channel)
    ENDIF
  ENDIF
ENDPROC

EXPORT PROC snd_setfreq(channel,uptime,destfreq)
DEF chan:PTR TO channel,a
  IF ahictrl
    chan:=chans[channel]
    IF destfreq<0 
      IF chan.freq>0 THEN chan.freq:=-chan.freq
      destfreq:=-destfreq
    ELSE
      IF chan.freq<0 THEN chan.freq:=-chan.freq
    ENDIF
    chan.freqtarget:=destfreq
    IF a:=uptime
      chan.uptime:=a
      chan.freqadd:=Div(destfreq-Abs(chan.freq),a)
    ELSE
      IF a:=chan.uptime
        chan.freqadd:=Div(destfreq-Abs(chan.freq),a)
      ELSE
        mxcmd_setfrequency(destfreq,channel,userdata)
      ENDIF
    ENDIF
  ENDIF
ENDPROC

EXPORT PROC get_freechannels(chanmask)
  IF ahictrl THEN RETURN mxcmd_freechannels(chanmask,userdata)
ENDPROC chanmask

EXPORT PROC ledstatus(leds)
/*	MOVE.L  leds,D0
	LEA   CIAA_ADDR+CIAPRA,A0
	NOT.W   D0
	AND.W   #CIAF_LED,D0
	MOVE.B  D0,(A0)
*/
ENDPROC

EXPORT PROC is_ahi_on() IS IF ahictrl THEN TRUE ELSE FALSE

PROC soundmix()
  MOVEM.L D2-D7/A2-A6,-(A7)
  ADDQ.L #1,cntmix
  MOVE.L outbufswap,D0
  EORI.W #256,D0
  MOVE.L D0,outbufswap
  AhI_SetSound( 0, 0, outbufswap, 256, ahictrl, 0 )
  MOVEA.L userdata,A1
  MOVE.L  outbufswap,D0;  LSL.L   #2,D0;  ADD.L   outbuffer,D0
  MOVE.L  D0,(A1)
  MOVE.L  A1,scopedata
  MOVEA.L mixcode,A5
  JSR (A5)
  MOVEM.L (A7)+,D2-D7/A2-A6
ENDPROC

PROC realtimeplay()
DEF i:REG,x:REG,v:REG,b:REG,chan:PTR TO channel,chmask:REG ->,p:PTR TO INT
  MOVEM.L D2/A2-A6,-(A7)
  chan:=chans
  chmask:=Not(get_freechannels($FFFFFFFF))
  FOR i:=0 TO 31
    IF chmask AND 1
      IF x:=chan.volenv
        IF (b:=chan.voladd) < 0
          ADD.L b,x
          IF x <= chan.voltarget
            x:=chan.voltarget
            chan.voladd:=0
          ENDIF
          chan.volenv:=x
        ELSEIF b > 0
          ADD.L b,x
          BTST #16,x; BEQ.S rtp_cont1; MOVE.L #65535,x; chan.voladd:=chan.voldecay
rtp_cont1:
          chan.volenv:=x
        ENDIF
        IF x=0 THEN mxcmd_stopchannel(i,userdata)
        v:=chan.volume
        MOVE.L v,D0; MOVE.L x,D1; LSR.L #1,D0
     /* MULU.L D0,D1 ; DIVU.L #65535,D1 */
LONG    $4c001001,$4C7C1001,$0000FFFF
        ADD.L D1,D1; MOVE.L D1,v
        mxcmd_setvolume(v,chan.pan,i,userdata)
      ENDIF
      IF b:=chan.freqadd
        v:=Abs(chan.freq)+b
        IF (b:=chan.uptime) <= 0
          v:=chan.freqtarget; chan.freqadd:=0; chan.uptime:=0
        ELSE
          chan.uptime:=b-1
        ENDIF
        chan.freq:=IF chan.freq < 0 THEN -v ELSE v
        mxcmd_setfrequency(chan.freq,i,userdata)
      ENDIF
/*
      IF p:=chan.fmdata
        b:=chan.fmcurrent-chan.fmspeed; IF b < 0 THEN b:=b+chan.fmlength
        chan.fmcurrent:=b; SWAP b; EXT.L b
        x:=chan.fmampfp; v:=chan.fmampexp
        MULU b,x; DIVU #32767,x; EXT.L x; MOVEQ  #1,D0; LSL.L v,D0; ADD.L D0,x
        IF (b:=p[b]) > 0
          b:=chan.freq
          MOVEQ #0,D1; MOVE.L b,D0; /*BFFFO D0{0,15},D1*/ LONG $EDC0100F ; BEQ.S fm_skip1
          MOVEQ #16,D2; SUB.L D1,D2; SUB.L D2,v; LSR.L D2,D0
fm_skip1: MULU D0,x; LSR.L v,x
          mxcmd_setfrequency(x,i,userdata)
        ELSEIF b < 0
          b:=chan.freq
          MOVEQ #0,D2; MOVEQ #0,D1; MOVE.L b,D0;/*BFFFO D0{0,15},D1*/ LONG $EDC0100F ; BEQ.S fm_skip2
          MOVEQ #16,D2; SUB.L D1,D2; SUB.L D2,v
fm_skip2: LSL.L v,D0; MOVE.L x,D1;/*DIVU.L D1,D0*/; LONG $4C410000; LSL.L D2,D0; MOVE.L D0,x
          mxcmd_setfrequency(x,i,userdata)
        ELSE
          mxcmd_setfrequency(chan.freq,i,userdata)
        ENDIF
      ENDIF
*/
    ENDIF
    chan++
    LSR.L #1,chmask
  ENDFOR
  MOVEM.L (A7)+,D2/A2-A6
ENDPROC

