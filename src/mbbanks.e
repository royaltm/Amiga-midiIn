OPT OSVERSION=37
OPT MODULE

MODULE '*soundfx'

EXPORT ENUM  LEFTFIX=0,RIGHTFIX,ANYFIX -> it's obsolete
EXPORT CONST TYPE_VERSION_32=$80
EXPORT SET   B_LOOP, B_DUR_ON, B_DRUM, B_MONO, B_ADDAFTERT
EXPORT CONST FINE_CENTR=116
EXPORT CONST NUMBANKS=60

EXPORT OBJECT bank
  instr:PTR TO sfx  -> pointer to initialised instrument object
  midi:CHAR         -> 1-16 midi channel
  pri:CHAR          -> 1-60 priority
  type:CHAR         -> =TYPE_VERSION_32
  base:CHAR         -> base note (60)=C
  fine:CHAR         -> fine-FINE_CENTR= finetune (-100,100)
  set:CHAR          -> LOOP, DURATION_ON, DRUM, MONOPHONIC
  lobound:CHAR      -> lo keyboard range
  hibound:CHAR      -> hi keyboard range
  volume:INT        -> 0-512 (512=200%)
  velsens:CHAR      -> 0-100 velocity sens
  release:CHAR      -> ENVELOPE release time in sec./10
  panorama:INT      -> 0-256 128=centered panorama
  panwide:CHAR      -> 0-30 autopan width
  pitchsens:CHAR    -> 0-12 note range pitchsensitivity
  attack:CHAR       -> ENVELOPE attack time in sec./10
  decay:CHAR        -> ENVELOPE decay time in sec./10
  sustainlev:CHAR   -> ENVELOPE 0-255 percent of volume for sustain
  aftersens:CHAR    -> 0-100 aftertouch sensitivity
  firstskip:INT     -> 0-3000 first n miliseconds sample skip
  mctrlvol:CHAR     -> 0 OR 100; 100 is for MIDI_VOLUME enabled
  mctrlpan:CHAR     -> 0 OR 100; 100 is for MIDI_PAN enabled
  group:CHAR        -> 0-? exclusive group number 0=NONE
  pad0:CHAR
  pad1:INT
ENDOBJECT

EXPORT DEF bd:PTR TO bank, prilist:PTR TO LONG


EXPORT PROC deletebank(bn:PTR TO bank)
DEF snd:PTR TO sfx
  IF snd:=bn.instr
    bn.instr:=NIL
    snd.unload()
  ENDIF
  bn.midi:=1
  bn.pri:=1
  bn.type:=TYPE_VERSION_32
  bn.base:=60
  bn.fine:=FINE_CENTR
  bn.set:=0
  bn.lobound:=0
  bn.hibound:=127
  bn.volume:=256
  bn.velsens:=0
  bn.release:=0
  bn.panorama:=128
  bn.panwide:=0
  bn.pitchsens:=0
  bn.attack:=0
  bn.decay:=0
  bn.sustainlev:=255
  bn.aftersens:=0
  bn.firstskip:=0
  bn.mctrlvol:=0
  bn.mctrlpan:=0
  bn.group:=0
ENDPROC

EXPORT PROC initbanks(cn=FALSE:PTR TO bank)
DEF f:REG,bn:PTR TO bank
  FOR f:=0 TO NUMBANKS-1
    bn:=bd[f]
    bn.instr:=0
    bn.midi:=1
    bn.pri:=1
    bn.type:=TYPE_VERSION_32
    bn.base:=60
    bn.fine:=FINE_CENTR
    bn.set:=0
    bn.lobound:=128/NUMBANKS*f
    bn.hibound:=128/NUMBANKS*f+127-(128/NUMBANKS*(NUMBANKS-1))
    bn.volume:=256
    bn.velsens:=0
    bn.release:=0
    bn.panorama:=128
    bn.panwide:=0
    bn.pitchsens:=0
    bn.attack:=0
    bn.decay:=0
    bn.sustainlev:=255
    bn.aftersens:=0
    bn.firstskip:=0
    bn.mctrlvol:=0
    bn.mctrlpan:=0
    bn.group:=0
    prilist[f]:=bn
  ENDFOR
  IF cn
    cn.instr:=0
    cn.midi:=1
    cn.pri:=1
    cn.type:=TYPE_VERSION_32
    cn.base:=60
    cn.fine:=FINE_CENTR
    cn.set:=0
    cn.lobound:=0
    cn.hibound:=127
    cn.volume:=256
    cn.velsens:=0
    cn.release:=0
    cn.panorama:=128
    cn.panwide:=0
    cn.pitchsens:=0
    cn.attack:=0
    cn.decay:=0
    cn.sustainlev:=255
    cn.aftersens:=0
    cn.firstskip:=0
    cn.mctrlvol:=0
    cn.mctrlpan:=0
    cn.group:=0
  ENDIF
ENDPROC

EXPORT PROC sortbank()
DEF f:REG,bn:PTR TO bank,bankpri:REG,i:REG,cb:PTR TO bank
  FOR i:=1 TO NUMBANKS-1
    cb:=prilist[i]
    bankpri:=cb.pri
    f:=i
    bn:=prilist[f-1]
    WHILE bn.pri > bankpri
      prilist[f]:=bn;  DEC f;  prilist[f]:=cb
      EXIT f=0
      bn:=prilist[f-1]
    ENDWHILE
  ENDFOR
ENDPROC

EXPORT PROC checkbank(bn:PTR TO bank)
DEF l,h
  IF (bn.midi-1) >= 16 THEN RETURN FALSE
  IF (bn.pri-1) >= NUMBANKS THEN RETURN FALSE
  IF (bn.type AND TYPE_VERSION_32)=0
    IF bn.type=LEFTFIX ; bn.panorama:=0
    ELSEIF bn.type=RIGHTFIX ; bn.panorama:=256
    ELSEIF bn.type=ANYFIX ; bn.panorama:=128
    ELSE ; RETURN FALSE
    ENDIF
    bn.type:=bn.type OR TYPE_VERSION_32
  ENDIF
  IF bn.base > 127 THEN RETURN FALSE
  IF bn.fine < (FINE_CENTR-100) THEN bn.fine:=bn.fine*200/16+FINE_CENTR-100 -> convert old finetune
  IF bn.fine > (FINE_CENTR+100) THEN RETURN FALSE
  IF (l:=bn.lobound) > 127 THEN RETURN FALSE
  IF (h:=bn.hibound) > 127 THEN RETURN FALSE
  IF l > h THEN RETURN FALSE
  IF (bn.volume > 512) OR (bn.volume < 0) THEN RETURN FALSE
  IF bn.velsens > 100 THEN RETURN FALSE
  IF (bn.panorama > 256) OR (bn.panorama < 0) THEN RETURN FALSE
  IF bn.panwide > 30 THEN RETURN FALSE
  IF bn.pitchsens > 12 THEN RETURN FALSE
  IF bn.aftersens > 100 THEN RETURN FALSE
  IF (bn.firstskip > 3000) OR (bn.firstskip < 0) THEN RETURN FALSE
  IF bn.mctrlvol > 100 THEN RETURN FALSE
  IF bn.mctrlpan > 100 THEN RETURN FALSE
  IF bn.group > 16 THEN RETURN FALSE
ENDPROC TRUE
