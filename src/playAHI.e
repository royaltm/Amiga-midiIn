OPT MODULE
OPT PREPROCESS
OPT OSVERSION=37

MODULE 'exec/io','exec/ports','exec/nodes','ahi','devices/ahi','utility/hooks','utility/tagitem',
       'tools/installhook'

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
*          set_FMmodulation(channel,am)
*chanmask:=get_freechannels(chanmask)
*          get_sounddataoutput(ptra,ptrb)
*/

OBJECT sampleinfo
  address:LONG
  length:LONG
  stereo:LONG
ENDOBJECT

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
ENDOBJECT  

EXPORT OBJECT envelope -> realtime update volume for each channell
  attack:CHAR
  decay:CHAR
  sustain:CHAR
ENDOBJECT

DEF ahimp:PTR TO mp, ahiio:PTR TO ahirequest, ahidevice, ahictrl:PTR TO ahiaudioctrl,
    phook:PTR TO hook,ihook:PTR TO hook,smpinfo:PTR TO sampleinfo,chans:PTR TO channel,
    ahieffinfo:PTR TO ahieffchannelinfo,maxsamplenum,
    playchanmask

EXPORT PROC snd_init(maxsamples) HANDLE
  maxsamplenum:=maxsamples
  NEW smpinfo[maxsamplenum], chans[32], phook,ihook
  ahieffinfo:=NewR(32*4+SIZEOF ahieffchannelinfo)
  installhook(phook, {realtimeplay})
  installhook(ihook, {ahieffciproc})
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
  ENDIF
  END smpinfo[maxsamplenum],chans[32],phook,ihook
  IF ahieffinfo THEN Dispose(ahieffinfo)
ENDPROC

EXPORT PROC snd_audioon(channels,mixfreq) HANDLE
DEF pfreq,i,ahismpinfo:ahisampleinfo
  IF ahictrl THEN snd_audiooff()
  playchanmask:=0
  pfreq:=Shl(50,16)
  ahieffinfo.effect:=AHIET_CHANNELINFO
  ahieffinfo.func:=ihook
  ahieffinfo.channels:=channels
  IF NIL=(ahictrl:=AhI_AllocAudioA([AHIA_AUDIOID,AHI_DEFAULT_ID,
                             AHIA_MIXFREQ,mixfreq,
                             AHIA_CHANNELS,channels,
                             AHIA_SOUNDS,maxsamplenum,
                             AHIA_PLAYERFUNC, phook,
                             AHIA_PLAYERFREQ, pfreq,
                             AHIA_MINPLAYERFREQ, pfreq-65536,
                             AHIA_MAXPLAYERFREQ, pfreq+65536,
                             AHIA_USERDATA,0,TAG_DONE])) THEN RETURN FALSE
  IF AHIE_OK<>AhI_SetEffect(ahieffinfo,ahictrl) THEN Raise("MEM")
  FOR i:=0 TO maxsamplenum-1
    IF smpinfo[i].address<>0
      ahismpinfo.type:=IF smpinfo[i].stereo THEN AHIST_S16S ELSE AHIST_M16S
      ahismpinfo.address:=smpinfo[i].address
      ahismpinfo.length:=smpinfo[i].length
      IF AHIE_OK <> AhI_LoadSound(i, AHIST_SAMPLE, ahismpinfo, ahictrl) THEN Raise("MEM")
    ENDIF
  ENDFOR
  IF AHIE_OK<>AhI_ControlAudioA(ahictrl,[AHIC_PLAY,TRUE,TAG_DONE]) THEN Raise("MEM")
EXCEPT
  snd_audiooff()
  ReThrow()
ENDPROC TRUE

EXPORT PROC snd_audiooff()
  IF ahictrl
    ahieffinfo.effect:=AHIET_CHANNELINFO OR AHIET_CANCEL
    AhI_SetEffect(ahieffinfo,ahictrl)
    AhI_FreeAudio(ahictrl); ahictrl:=NIL
    playchanmask:=0
  ENDIF
ENDPROC

EXPORT PROC snd_setsample(number,address,length,stereo)
DEF ahismpinfo:ahisampleinfo
  IF smpinfo[number].address<>0 THEN snd_delsample(number)
  IF ahictrl
    ahismpinfo.type:=IF stereo THEN AHIST_S16S ELSE AHIST_M16S
    ahismpinfo.address:=address
    ahismpinfo.length:=length
    IF AHIE_OK <> AhI_LoadSound(number, AHIST_SAMPLE, ahismpinfo, ahictrl) THEN RETURN FALSE
  ENDIF
  smpinfo[number].address:=address
  smpinfo[number].length:=length
  smpinfo[number].stereo:=stereo
ENDPROC TRUE

EXPORT PROC snd_delsample(number)
  IF ahictrl
    IF smpinfo[number].address THEN AhI_UnloadSound(number, ahictrl )
  ENDIF
  smpinfo[number].address:=0
ENDPROC

EXPORT PROC snd_playsample(channel,number,offset,freq,volume,pan,loop=-1,env=0:PTR TO envelope,am=0,fm=0)
DEF chan:PTR TO channel,a:REG,b:REG,c:REG
  IF ahictrl
    IF smpinfo[number].address
      chan:=chans[channel]
      IF env
        a:=env.attack*5; MOVE.L #65535,D0; DIVU a,D0; MOVEQ #0,a; MOVE.W D0,a
        c:=env.sustain; MOVE.L c,D0; LSL.L #8,c; MOVE.B D0,c
        b:=env.decay*5; MOVE.L #65535,D0; SUB.L c,D0; DIVU b,D0; ; MOVEQ #0,b; MOVE.W D0,b
      ENDIF
      chan.volume:=volume; chan.freqadd:=0; chan.pan:=pan; chan.freq:=freq
      IF env
        chan.voladd:=a; chan.volenv:=1; chan.voldecay:=-b; chan.voltarget:=c
        volume:=0
      ELSE
        chan.voladd:=0; chan.volenv:=0; chan.voltarget:=0
      ENDIF
      AhI_PlayA( ahictrl, [
       AHIP_BEGINCHANNEL,channel,
       AHIP_FREQ,freq,
       AHIP_VOL,volume,
       AHIP_PAN,pan,
       AHIP_SOUND,number,
       AHIP_OFFSET,offset,
       AHIP_LENGTH,smpinfo[number].length-offset,
       AHIP_LOOPFREQ,freq,
       AHIP_LOOPVOL,volume,
       AHIP_LOOPPAN,pan,
       AHIP_LOOPSOUND,IF loop=-1 THEN AHI_NOSOUND ELSE number,
       AHIP_LOOPOFFSET,IF loop=-1 THEN 0 ELSE loop,
       AHIP_LOOPLENGTH,IF loop=-1 THEN 0 ELSE smpinfo[number].length-loop,
       AHIP_ENDCHANNEL,0,TAG_DONE])
      MOVE.L channel,D0; MOVEQ  #1,D1; LSL.L D0,D1; OR.L D1,playchanmask
    ENDIF
  ENDIF
ENDPROC

EXPORT PROC snd_stopchannel(channel)
DEF chan:PTR TO channel
  IF ahictrl
    chan:=chans[channel]
    IF chan.volume
      chan.volume:=0; chan.voladd:=0; chan.volenv:=0; chan.freqadd:=0; chan.voltarget:=0
      AhI_SetSound(channel, AHI_NOSOUND, 0, 0, ahictrl, AHISF_IMM )
      MOVE.L channel,D0; MOVEQ  #-2,D1; ROL.L D0,D1; AND.L D1,playchanmask
    ENDIF
  ENDIF
ENDPROC

EXPORT PROC snd_setfreq(channel,newfreq)
DEF chan:PTR TO channel
  IF ahictrl
    chan:=chans[channel]
    IF chan.volume
      chan.freqadd:=0; chan.freq:=newfreq
      AhI_SetFreq(channel, newfreq, ahictrl, AHISF_IMM)
    ENDIF
  ENDIF
ENDPROC

EXPORT PROC snd_setvolume(channel,newvolume,newpan)
DEF chan:PTR TO channel
  IF ahictrl
    chan:=chans[channel]
    IF chan.volume
      chan.volume:=newvolume
      IF chan.volenv=0 THEN AhI_SetVol(channel, newvolume, newpan, ahictrl, AHISF_IMM)
    ENDIF
  ENDIF
ENDPROC

EXPORT PROC snd_release(channel,release)
DEF chan:PTR TO channel,a:REG,b:REG, v:REG
  IF ahictrl
    chan:=chans[channel]
    IF chan.volume
      IF (v:=chan.volenv)=0 THEN b:=65535 ELSE b:=v
      IF a:=release*5
        MOVE.L b,D0; DIVU a,D0; MOVEQ #0,a; MOVE.W D0,a
        chan.voladd:=-a; chan.voltarget:=0; IF v=0 THEN chan.volenv:=65535
      ELSE
        snd_stopchannel(channel)
      ENDIF
    ENDIF
  ENDIF
ENDPROC

EXPORT PROC snd_setglide(channel,uptime,destfreq)
DEF chan:PTR TO channel
  IF ahictrl
    chan:=chans[channel]
    IF chan.volume
      chan.freqtarget:=destfreq
      chan.freqadd:=Div(destfreq-chan.freq,uptime*5)
    ENDIF
  ENDIF
ENDPROC

EXPORT PROC get_freechannels(chanmask)
  IF ahictrl
    MOVE.L playchanmask,D0; NOT.L D0; AND.L D0,chanmask
  ELSE
    chanmask:=0
  ENDIF
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

PROC realtimeplay()
DEF i:REG,volenv:REG,vol:REG,b:REG,chan:PTR TO channel
  chan:=chans
  FOR i:=0 TO 31
    IF vol:=chan.volume
      IF volenv:=chan.volenv
        IF (b:=chan.voladd) < 0
          volenv:=chan.volenv; ADD.L b,volenv
          IF volenv <= chan.voltarget
            volenv:=chan.voltarget
            chan.voladd:=0
          ENDIF
          chan.volenv:=volenv
        ELSEIF b > 0
          volenv:=chan.volenv; ADD.L b,volenv
          BTST #16,volenv; BEQ.S rtp_cont1; MOVE.L #65535,volenv; chan.voladd:=chan.voldecay
rtp_cont1:
          chan.volenv:=volenv
        ENDIF
        IF volenv=0
          chan.freqadd:=0; chan.volume:=0
          AhI_SetSound(i, AHI_NOSOUND, 0, 0, ahictrl, AHISF_IMM )
          MOVE.L i,D0; MOVEQ  #-2,D1; ROL.L D0,D1; AND.L D1,playchanmask
        ENDIF
        SUBQ.L #1,vol; MULU volenv,vol; DIVU #65535,vol; AND.L #$0000FFFF,vol
        AhI_SetVol(i, vol, chan.pan, ahictrl, AHISF_IMM)
      ENDIF
      IF (b:=chan.freqadd) < 0
        vol:=chan.freq+b
        IF vol <= chan.freqtarget; vol:=chan.freqtarget; chan.freqadd:=0; ENDIF
      ELSEIF (b:=chan.freqadd) > 0
        vol:=chan.freq+b
        IF vol >= chan.freqtarget; vol:=chan.freqtarget; chan.freqadd:=0; ENDIF
      ENDIF
      IF b
        chan.freq:=vol; AhI_SetFreq(i, vol, ahictrl, AHISF_IMM)
      ENDIF
    ENDIF
    chan++
  ENDFOR
ENDPROC

ahieffciproc: MOVE.W  8(A1),D0  ->channels
              LEA SIZEOF ahieffchannelinfo(A1), A1
              MOVE.W  D0,D1
              SUBQ.W  #1,D1
              LSL.W   #2,D0
              ADDA.W  D0,A1
              LEA aeci_backup(PC),A0
              MOVE.L  D2,-(A7)
              MOVEQ   #0,D2
aeci_loop:    MOVE.L  -(A1),D0
              CMP.L   (A0),D0
              BEQ.S   aeci_skip
              BSET    D1,D2
              MOVE.L  D0,(A0)+
              DBRA    D1,aeci_loop
              BRA.S   aeci_end
aeci_skip:    BCLR    D1,D2
              ADDQ.L  #4,A0
              DBRA    D1,aeci_loop
aeci_end:     MOVE.L  D2,playchanmask
              MOVE.L  (A7)+,D2
              MOVEQ   #0,D0
              RTS
aeci_backup:
LONG 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

