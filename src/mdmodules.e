OPT MODULE

ENUM MXCMD_SETUP=0,   -><A0=[mixfreq,channels,maxsamples,buffer,bufferframes]:LONG; >D0=userdata, A0=mixcode
       MXCMD_END,       -><A1=userdata
       MXCMD_LOAD,      -><A0=[address,frames,stereo]:LONG, D0=samplenum, A1=userdata
       MXCMD_UNLOAD,    -><D0=samplenum, A1=userdata
       MXCMD_SETMIXPERIOD, -><D0=newmixfrequency, A1=userdata
       MXCMD_PLAYCHANNEL, -><A0=[offset,freq,volume,pan,loop]:LONG, D0=channel, D1=samplenum, A1=userdata
       MXCMD_STOPCHANNEL,  -><D0=channel, A1=userdata
       MXCMD_STOPCHANNELMASK, -><D0=channelmask (1=stop) LSB-MSB, A1=userdata
       MXCMD_FREECHANNELS, ->>D0=channelmask LSB-MSB, A1=userdata
       MXCMD_SETFREQUENCY, ->>D0=channel, D1=freq, A1=userdata
       MXCMD_SETVOLUME,    ->>D0=channel, D1=volume, D2=pan, A1=userdata
       MXCMD_LAST

EXPORT OBJECT mixing_module PRIVATE
 segment:LONG
 name:LONG
 author:LONG
 proc:LONG
 mixproc:LONG
 data:LONG
ENDOBJECT

EXPORT PROC name() OF mixing_module IS IF self.name THEN self.name ELSE ''
EXPORT PROC author() OF mixing_module IS IF self.author THEN self.author ELSE ''

EXPORT PROC init(name) OF mixing_module HANDLE
DEF cmd:PTR TO LONG,i,x,seg
  IF (seg:=LoadSeg(name))=0 THEN Raise("OPEN")
  cmd:=seg*4+8
  IF cmd[]++<>"MX00"THEN Raise("BADM")
  FOR i:=1 TO 4
    x:=cmd[]++
    SELECT x
      CASE "NAME"
        self.name:=cmd[]++
      CASE "AUTH"
        self.author:=cmd[]++
      CASE "FNLS"
        self.proc:=cmd[]++
      DEFAULT
        cmd[]++
    ENDSELECT
  ENDFOR
  IF (self.name=0) OR (self.author=0) OR (self.proc=0) THEN Raise("BADM")
  self.segment:=seg
EXCEPT
  UnLoadSeg(seg)
  RETURN FALSE
ENDPROC TRUE

EXPORT PROC end() OF mixing_module
DEF r,cm,seg
  IF self.data
    r:=self.data;self.data:=0
    IF cm:=self.proc
      MOVE.L  r,A1; MOVE.L cm,A2; LEA MXCMD_END*4(A2),A2; MOVE.L  execbase,A6
      MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
    ENDIF
    self.mixproc:=0
  ENDIF
  IF seg:=self.segment
    self.proc:=0; self.segment:=0; UnLoadSeg(seg)
  ENDIF
ENDPROC

EXPORT PROC setup(mixfreq,channels,maxsamples,buffer,bufferframes) OF mixing_module
DEF ar:PTR TO LONG,a,b,cm
  IF (self.segment=0) THEN RETURN FALSE
  IF self.data THEN self.end()
  ar:=[mixfreq,channels,maxsamples,buffer,bufferframes]:LONG
  IF cm:=self.proc
    MOVE.L  ar,A0; MOVE.L cm,A2; LEA MXCMD_SETUP*4(A2),A2; MOVE.L  execbase,A6
    MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
    MOVE.L D0,a; MOVE.L A0,b
    self.data:=a; IF a THEN self.mixproc:=b
  ENDIF
ENDPROC IF self.data THEN TRUE ELSE FALSE


EXPORT PROC loadsample(samplenum,address,frames,stereo) OF mixing_module
DEF ar:PTR TO LONG,b,cm
  IF (b:=self.data)=0 THEN RETURN FALSE
  ar:=[address,frames,stereo]:LONG; b:=self.data
  IF cm:=self.proc
    MOVE.L  ar,A0; MOVE.L samplenum,D0
    MOVE.L b,A1; MOVE.L cm,A2; LEA MXCMD_LOAD*4(A2),A2; MOVE.L  execbase,A6
    MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
  ENDIF
ENDPROC

EXPORT PROC unloadsample(samplenum) OF mixing_module
DEF b,cm
  IF (b:=self.data)=0 THEN RETURN FALSE
  IF cm:=self.proc
    MOVE.L samplenum,D0
    MOVE.L b,A1; MOVE.L cm,A2; LEA MXCMD_UNLOAD*4(A2),A2; MOVE.L  execbase,A6
    MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
  ENDIF
ENDPROC

EXPORT PROC setmixperiod(newmixfrequency) OF mixing_module
DEF b,cm
  IF (b:=self.data)=0 THEN RETURN FALSE
  IF cm:=self.proc
    MOVE.L newmixfrequency,D0
    MOVE.L b,A1; MOVE.L cm,A2; LEA MXCMD_SETMIXPERIOD*4(A2),A2; MOVE.L  execbase,A6
    MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
  ENDIF
ENDPROC

EXPORT PROC playchannel(channel,samplenum,offset,freq,volume,pan,loop=-1) OF mixing_module
DEF ar:PTR TO LONG, b,cm
  IF (b:=self.data)=0 THEN RETURN FALSE
  IF cm:=self.proc
    ar:=[offset,freq,volume,pan,loop]:LONG
    MOVE.L ar,A0; MOVE.L channel,D0; MOVE.L samplenum,D1
    MOVE.L b,A1; MOVE.L cm,A2; LEA MXCMD_PLAYCHANNEL*4(A2),A2; MOVE.L  execbase,A6
    MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
  ENDIF
ENDPROC

EXPORT PROC stopchannel(channel) OF mixing_module
DEF b,cm
  IF (b:=self.data)=0 THEN RETURN FALSE
  IF cm:=self.proc
    MOVE.L channel,D0
    MOVE.L b,A1; MOVE.L cm,A2; LEA MXCMD_STOPCHANNEL*4(A2),A2; MOVE.L  execbase,A6
    MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
  ENDIF
ENDPROC

EXPORT PROC stopchannelmask(channelmask) OF mixing_module
DEF b,cm
  IF (b:=self.data)=0 THEN RETURN FALSE
  IF cm:=self.proc
    MOVE.L channelmask,D0
    MOVE.L b,A1; MOVE.L cm,A2; LEA MXCMD_STOPCHANNELMASK*4(A2),A2; MOVE.L  execbase,A6
    MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
  ENDIF
ENDPROC

EXPORT PROC freechannels() OF mixing_module
DEF b,cm
  IF (b:=self.data)=0 THEN RETURN FALSE
  IF cm:=self.proc
    MOVE.L b,A1; MOVE.L cm,A2; LEA MXCMD_FREECHANNELS*4(A2),A2; MOVE.L  execbase,A6
    MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
    MOVE.L D0,b
  ENDIF
ENDPROC b

EXPORT PROC setfrequency(channel,freq) OF mixing_module
DEF b,cm
  IF (b:=self.data)=0 THEN RETURN FALSE
  IF cm:=self.proc
    MOVE.L channel,D0; MOVE.L freq,D1
    MOVE.L b,A1; MOVE.L cm,A2; LEA MXCMD_SETFREQUENCY*4(A2),A2; MOVE.L  execbase,A6
    MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
  ENDIF
ENDPROC

EXPORT PROC setvolume(channel,volume,pan) OF mixing_module
DEF b,cm
  IF (b:=self.data)=0 THEN RETURN FALSE
  IF cm:=self.proc
    MOVE.L channel,D0; MOVE.L volume,D1; MOVE.L pan,D2
    MOVE.L b,A1; MOVE.L cm,A2; LEA MXCMD_SETVOLUME*4(A2),A2; MOVE.L  execbase,A6
    MOVEM.L D2-D7/A4-A5,-(A7); JSR (A2); MOVEM.L (A7)+,D2-D7/A4-A5
  ENDIF
ENDPROC

