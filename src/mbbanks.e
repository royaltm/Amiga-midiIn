OPT OSVERSION=37
OPT MODULE

MODULE '*mblocale','*soundfx_ahi','*mbreport','*midibplists',
       'exec/lists','exec/nodes','exec/semaphores'

EXPORT ENUM   LEFTFIX=0,RIGHTFIX,ANYFIX -> it's obsolete
EXPORT CONST  TYPE_VERSION_32=$80
EXPORT SET    B_LOOP, B_DUR_ON, B_DRUM, B_MONO, B_ADDAFTERT
EXPORT CONST  FINE_CENTR=116
EXPORT CONST  NUMBANKS=60
EXPORT ENUM   SORT_PRI, SORT_MIDI, SORT_NAME, SORT_RANGE

EXPORT OBJECT bank
  instr:PTR TO sfx  -> pointer to initialised instrument object
  midi:CHAR         -> 1-16 midi channel
  pri:CHAR          -> 1-60 priority
  type:CHAR         -> =TYPE_VERSION_32
  base:CHAR         -> base note (60)=C
  fine:CHAR         -> fine-FINE_CENTR= finetune (-100,100)
  set:CHAR          -> LOOP, DURATION_ON, DRUM, MONOPHONIC, ADDITIVEAFTERTOUCH
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
  monovsens:CHAR    -> monoslide velocity sens (0-100)
  monoslide:INT     -> uptime for sliding in modo mone (0=off) (0-1500)
ENDOBJECT

EXPORT DEF prilist:PTR TO LONG
DEF lastexcp,          -> for displaying errors on unrecognized sample type
    banksem:PTR TO ss
/*
  ============================================================================
                              Banks basic commands
  ============================================================================
*/
EXPORT PROC lockbanksaccess() IS ObtainSemaphore(banksem)
EXPORT PROC releasebanksaccess()
  IF banksem.nestcount THEN ReleaseSemaphore(banksem)
ENDPROC

EXPORT PROC cmpbanks(bn1:PTR TO bank, bn2:PTR TO bank, type)
  IF bn1.instr = NIL THEN IF bn2.instr <> NIL THEN RETURN -1
  IF bn2.instr = NIL THEN IF bn1.instr <> NIL THEN RETURN 1
  SELECT type
    CASE SORT_PRI
      IF bn2.pri > bn1.pri THEN RETURN 1
      IF bn2.pri < bn1.pri THEN RETURN -1
    CASE SORT_MIDI
      IF bn2.midi > bn1.midi THEN RETURN 1
      IF bn2.midi < bn1.midi THEN RETURN -1
    CASE SORT_NAME
      IF (bn1.instr<>NIL) AND (bn2.instr<>NIL) THEN RETURN OstrCmp(bn1.instr.ln.name, bn2.instr.ln.name)
    CASE SORT_RANGE
      IF bn2.lobound > bn1.lobound THEN RETURN 1
      IF bn2.lobound < bn1.lobound THEN RETURN -1
      IF bn2.hibound > bn1.hibound THEN RETURN 1
      IF bn2.hibound < bn1.hibound THEN RETURN -1
  ENDSELECT      
ENDPROC 0

EXPORT PROC clearinstr(bn:PTR TO bank)
DEF snd:PTR TO sfx
  IF snd:=bn.instr
    bn.instr:=NIL; snd.unload()
  ENDIF
ENDPROC snd

EXPORT PROC setinstr(bn:PTR TO bank, snd:PTR TO sfx) HANDLE
  IF snd=bn.instr THEN RETURN TRUE
  clearinstr(bn)
  IF snd=NIL THEN RETURN TRUE
  printstatus(getLocStr(STRID_LOADINGSAMPLE),snd.ln.name,0)
  snd.load()
  bn.instr:=snd
EXCEPT
  report_exception()
  RETURN FALSE
ENDPROC TRUE

EXPORT PROC setinstrname(bn:PTR TO bank, slist:PTR TO lh, instrname)
DEF snd:PTR TO sfx, x=FALSE
    IF instrname=0
      clearinstr(bn); x:=TRUE
    ELSEIF StrLen(FilePart(instrname))=0
      clearinstr(bn); x:=TRUE
    ELSEIF snd:=addsnd(instrname,slist)
      x:=setinstr(bn,snd)
    ENDIF
ENDPROC x

EXPORT PROC xchgbanks(bn1:PTR TO bank,bn2:PTR TO bank)
DEF bn:bank
  CopyMem(bn1,bn,SIZEOF bank)
  bn1.instr:=bn2.instr; CopyMem(bn2,bn1,SIZEOF bank)
  bn2.instr:=bn.instr; CopyMem(bn,bn2,SIZEOF bank)
ENDPROC

EXPORT PROC setbank(bn:PTR TO bank, fbn:PTR TO bank)
DEF x
  x:=setinstr(bn, fbn.instr); fbn.instr:=bn.instr
  CopyMem(fbn, bn, SIZEOF bank)
ENDPROC x

EXPORT PROC deletebank(bn:PTR TO bank)
  clearinstr(bn)
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
  bn.monovsens:=0
  bn.monoslide:=0
ENDPROC

EXPORT PROC initbanks(bd:PTR TO bank)
DEF f,bn:PTR TO bank
  FOR f:=0 TO NUMBANKS-1
    bn:=bd[f]
    bn.instr:=0
    bn.midi:=1
    bn.pri:=1
    bn.type:=TYPE_VERSION_32
    bn.base:=60
    bn.fine:=FINE_CENTR
    bn.set:=0
    bn.lobound:=0   -> 128/NUMBANKS*f
    bn.hibound:=127 ->128/NUMBANKS*f+127-(128/NUMBANKS*(NUMBANKS-1))
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
    bn.monovsens:=0
    bn.monoslide:=0
    prilist[f]:=bn
  ENDFOR
  IF banksem=NIL
    NEW banksem; InitSemaphore(banksem)
  ENDIF
ENDPROC

EXPORT PROC sortbanks()
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
  IF bn.monovsens > 100 THEN bn.monovsens:=0
  IF (bn.monoslide > 1500) OR (bn.monoslide < 0) THEN bn.monoslide:=0
ENDPROC TRUE

/*
  ============================================================================
                          Sample list basic commands
  ============================================================================
*/

EXPORT PROC addsnd(name, slist:PTR TO lh, quiet=FALSE) HANDLE
DEF snd=0:PTR TO sfx,a,b, ln:PTR TO lln
 IF ln:=FindName(slist,FilePart(name)) THEN RETURN ln.pointer,FALSE
 NEW snd
 a,b:=snd.init(name)
 printstatus(0,snd.ln.name,b)
 addsorted(slist,snd.ln)
EXCEPT
  IF snd THEN END snd
  printstatus(0,FilePart(name),'????')
  IF (exception<>"UNRE") OR (lastexcp=FALSE)
    lastexcp:=IF exception="UNRE" THEN TRUE ELSE FALSE
    IF quiet=FALSE THEN report_exception()
  ELSE
    Delay(1)
  ENDIF
ENDPROC snd,IF snd THEN TRUE ELSE FALSE

EXPORT PROC addingsamplesover()
  closestatus();  lastexcp:=FALSE
ENDPROC

EXPORT PROC delsnd(snd:PTR TO sfx, banks:PTR TO bank, notused=FALSE)
DEF f=0,removed=FALSE
  WHILE f < NUMBANKS
    IF banks[f].instr=snd
      IF notused=FALSE
        banks[f].instr:=NIL
      ELSE
        f:=NUMBANKS+1
      ENDIF
    ENDIF
    INC f
  ENDWHILE
  IF f=NUMBANKS; Remove(snd.ln);  END snd;  removed:=TRUE; ENDIF
ENDPROC removed

EXPORT PROC clearsmplist(bn:PTR TO bank,slist:PTR TO lh,notused=FALSE)
DEF ln:PTR TO lln,ln2, cleared=FALSE
  ln:=slist.head
  WHILE ln2:=ln.ln.succ
    IF delsnd(ln.pointer, bn, notused) THEN cleared:=TRUE
    ln:=ln2
  ENDWHILE
ENDPROC cleared
