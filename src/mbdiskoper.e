OPT MODULE
OPT OSVERSION=37

MODULE 'fabio/IFFParser_oo','exec/lists','exec/nodes',
  'intuition/intuition','reqtools','libraries/reqtools','libraries/midi',
  'workbench/workbench','icon',
  '*midibplists','*mbbanks','*mblocale','*soundfx','*mbreport'


CONST MAGIC_VERSION_0_PREFS="mIn0"

EXPORT OBJECT mbprefs
  magic:LONG
  mainwinx:INT
  mainwiny:INT
  mainwinw:INT
  mainwinh:INT
  volumewinx:INT
  volumewiny:INT
  volumewinw:INT
  volumewinh:INT
  volumehide:INT
  envelwinx:INT
  envelwiny:INT
  envelwinw:INT
  envelwinh:INT
  envelhide:INT
  scopewinx:INT
  scopewiny:INT
  scopewinw:INT
  scopewinh:INT
  scopehide:INT
  midimonwinx:INT
  midimonwiny:INT
  midimonwinw:INT
  midimonwinh:INT
  midimonhide:INT
  dmaper:INT
PRIVATE
  routeoffs:INT
  sndoffs:INT
  prjoffs:INT
PUBLIC
  currentmcm:INT
  msgflags:INT
  chanflags:INT
  maxchannels:CHAR
  led:CHAR
  midictrl:CHAR
  activeb:CHAR
ENDOBJECT

EXPORT DEF prjpath, sndpath, mysrclist:PTR TO lh

DEF projecticon:PTR TO diskobject


EXPORT PROC save_project(name,slist:PTR TO lh,bnk:PTR TO bank,prefs:PTR TO mbprefs, saveicons) HANDLE
  DEF iff:PTR TO iffparser,snd:PTR TO sfx, nb:bank
  DEF f,i,ln:PTR TO ln

  NEW iff.iffparser()

  iff.save(name)
  iff.createchunk("MBFF","FORM")

  iff.createchunk("MBFF","BANK")
  f:=SIZEOF bank
  iff.writechunk({f},4)
  FOR f:=0 TO NUMBANKS-1
    CopyMem(bnk[f], nb, SIZEOF bank)
    IF snd:=nb.instr
      ln:=slist.head; i:=1
      WHILE ln.succ
        IF snd=getvarafterln(ln) THEN nb.instr:=i
        ln:=ln.succ; INC i
      ENDWHILE
    ENDIF
    iff.writechunk(nb,SIZEOF bank)
  ENDFOR
  iff.closechunk()

  iff.createchunk("MBFF","PREF")
    prefs.magic:=MAGIC_VERSION_0_PREFS
    prefs.sndoffs:=SIZEOF mbprefs
    prefs.prjoffs:=StrLen(sndpath) + 1 + SIZEOF mbprefs
    prefs.routeoffs:=prefs.prjoffs + StrLen(prjpath) + 1
    iff.writechunk(prefs,SIZEOF mbprefs)
    iff.writechunk(sndpath,StrLen(sndpath)+1)
    iff.writechunk(prjpath,StrLen(prjpath)+1)
    ln:=mysrclist.head
    WHILE ln.succ
      IF ln.name[] <> " " THEN iff.writechunk(ln.name+2,StrLen(ln.name+2)+1)
      ln:=ln.succ
    ENDWHILE
    iff.writechunk('',1)
  iff.closechunk()

  IF slist.head.succ
    iff.createchunk("MBFF","LSMP")
        ln:=slist.head
        WHILE ln.succ
          snd:=getvarafterln(ln)
          IF i:=snd.pathname() THEN iff.writechunk(i,StrLen(i)+1)
          ln:=ln.succ
        ENDWHILE
    iff.closechunk()
  ENDIF

  iff.closechunk()               -> Here we close FORM chunk!
  iff.close()                    -> AND here we close IFF save file session.

  IF saveicons THEN savemidiin_icon(name)

EXCEPT DO
  END iff
  IF exception THEN ReThrow()
ENDPROC

EXPORT PROC load_project(name,slist:PTR TO lh,bnk:PTR TO bank,prefs:PTR TO mbprefs) HANDLE
DEF iff:PTR TO iffparser,snd:PTR TO sfx,
    ln:PTR TO ln,f:PTR TO CHAR,v,z,l,h,s,
    str[256]:STRING, nb:PTR TO bank, bufern[256]:ARRAY OF CHAR,
    instr[NUMBANKS]:ARRAY OF LONG

  NEW iff.iffparser()

  iff.load(name)
  iff.setscan("MBFF","PREF")
  iff.setscan("MBFF","BANK")
  iff.setscan("MBFF","LSMP")
  iff.exit("MBFF","FORM")     -> We will stop when FORM ends!

  iff.scan()

  FOR f:=0 TO NUMBANKS-1
    instr[f]:=0
  ENDFOR
  initbanks()

  ln:=slist.head
  WHILE ln.succ
    snd:=getvarafterln(ln)
    ln:=ln.succ
    Remove(ln.pred);  END snd
  ENDWHILE

  IF prefs
    IF s:=iff.first("MBFF","PREF")
      z:=iff.size()
      IF z >= SIZEOF mbprefs
        CopyMem(s,prefs,SIZEOF mbprefs)
        IF prefs.magic = MAGIC_VERSION_0_PREFS
          IF z > prefs.sndoffs
            v:=s+prefs.sndoffs
            IF StrLen(v) > 0 THEN StrCopy(sndpath,v,ALL)
          ENDIF  
          IF z > prefs.prjoffs
            v:=s+prefs.prjoffs
            IF StrLen(v) > 0 THEN StrCopy(prjpath,v,ALL)
          ENDIF  
          IF z > prefs.routeoffs
            v:=s+prefs.routeoffs
            WHILE ((f:=StrLen(v)) > 0) AND ((v+f) < (s+z))
              ln:=mysrclist.head
              WHILE ln.succ
                IF StrCmp(ln.name+2,v,ALL) THEN ln.name[]:="+"
                ln:=ln.succ
              ENDWHILE
              v:=v+f+1
            ENDWHILE
          ENDIF  
        ELSE
          prefs:=0
        ENDIF
      ELSE
        prefs:=0
      ENDIF
    ELSE
      prefs:=0
    ENDIF
  ENDIF

  IF s:=iff.first("MBFF","BANK")
    IF (v:=Long(s)) <= SIZEOF bank
      IF (z:=(iff.size()-4)/v) >= 1
        IF z > NUMBANKS THEN z:=NUMBANKS
        FOR f:=0 TO (z-1)
          nb:=f*v+s+4
          instr[f]:=nb.instr ;  nb.instr:=0 -> no accident play!
          CopyMem(nb, bnk[f], v)
          IF checkbank(bnk[f])=FALSE THEN Raise("bprj")
        ENDFOR
      ENDIF
    ELSE
      Raise("bprj")
    ENDIF
  ELSE
    Raise("bprj")
  ENDIF

  IF s:=iff.first("MBFF","LSMP")
    z:=iff.size()
    v:=s;  h:=1 -> number of sample
    WHILE z>0
      IF (z:=z-StrLen(v)-1) >= 0
        printstatus(getLocStr(STRID_LOOKINGFORSAMPLE),FilePart(v),'')
        IF ln:=FindName(slist,FilePart(v))
          snd:=getvarafterln(ln)
        ELSEIF (snd:=getsound(v))=FALSE                  -> not here!
          StrCopy(str,FilePart(v))    -> only name
          AstrCopy(bufern,sndpath,256)  -> sample path!
          AddPart(bufern,str,256)       ->  'path/name'
          LowerStr(bufern)             -> nice this one
          IF ln:=FindName(slist,FilePart(bufern))
            snd:=getvarafterln(ln)
          ELSEIF (snd:=getsound(bufern))=FALSE  -> neither here!
            IF 1=(l:=asksfx(str))
              AstrCopy(bufern,sndpath,256)  -> (new) sample path!
              AddPart(bufern,str,256)   ->  (new) 'path/name'
              LowerStr(bufern)             -> nice this one
              IF ln:=FindName(slist,FilePart(bufern))
                snd:=getvarafterln(ln)
              ELSE
                snd:=getsound(bufern)
              ENDIF
            ELSEIF l=2
              z:=0
            ENDIF
          ENDIF
        ENDIF
        IF snd
          IF ln=0 THEN addsorted(slist,snd.ln)
          FOR f:=0 TO NUMBANKS-1
            IF instr[f]=h
              IF loadsmp(snd) THEN bnk[f].instr:=snd -> wow can play now!
            ENDIF
          ENDFOR
        ENDIF
      ENDIF
      v:=v+StrLen(v)+1
      INC h
    ENDWHILE
  ENDIF
  
  iff.close()

  getmidiin_icon(name)

EXCEPT DO
  closestatus()
  END iff                            -> ALWAYS END the OBJECT before exiting!!!!
  IF exception="bprj" THEN initbanks()
  IF exception THEN ReThrow()
ENDPROC prefs

PROC loadsmp(snd:PTR TO sfx) HANDLE
  printstatus(getLocStr(STRID_LOADINGSAMPLE),0,0)
  snd.load() -> may cause the exception
EXCEPT
  report_exception()
  RETURN FALSE
ENDPROC TRUE

PROC getsound(name) HANDLE
DEF snd=0:PTR TO sfx,a,b
  NEW snd
  a,b:=snd.init(name)
  printstatus(0,0,b)
EXCEPT
    IF snd THEN END snd
    RETURN 0
ENDPROC snd

EXPORT PROC mergeproject(name,slist:PTR TO lh,bnk:PTR TO bank,numbnk) HANDLE
DEF iff:PTR TO iffparser,snd:PTR TO sfx,
    ln:PTR TO ln,f:PTR TO CHAR,v,z,l,h,s,n,skip=FALSE,
    str[256]:STRING, nb:PTR TO bank, bufern[256]:ARRAY OF CHAR,
    instr[NUMBANKS]:ARRAY OF LONG

  NEW iff.iffparser()

  iff.load(name)
  iff.setscan("MBFF","BANK")
  iff.setscan("MBFF","LSMP")
  iff.exit("MBFF","FORM")     -> We will stop when FORM ends!

  iff.scan()

  FOR f:=0 TO NUMBANKS-1 DO instr[f]:=FALSE

  IF s:=iff.first("MBFF","BANK")
    IF (v:=Long(s)) <= SIZEOF bank
      IF (z:=(iff.size()-4)/v) >= 1
        n:=0;
        FOR f:=0 TO (z-1)
          nb:=f*v+s+4
          IF nb.instr <> NIL; INC n; IF checkbank(nb)=FALSE THEN Raise("bprj"); ENDIF
        ENDFOR -> n=how many active banks
        IF n > 0
          l:=Min(numbnk+n-1,NUMBANKS-1)
          FOR f:=numbnk TO l DO EXIT bnk[f].instr <> 0 -> overwriting???
          IF f <> (l+1)  THEN l:=reqskipover() ELSE l:=TRUE
          IF l=0 THEN Raise("cncl")
          IF l=2 THEN skip:=TRUE
          f:=numbnk; l:=n
          REPEAT
            IF (bnk[f].instr = NIL) OR (skip = FALSE) THEN DEC l
            INC f
          UNTIL (l = 0) OR (f = NUMBANKS)
          IF l > 0 THEN IF 0=reqdontfit(l) THEN Raise("cncl")  -> is it gonna fit?
          n:=numbnk; f:=0
          REPEAT /* main merging function */
            IF skip AND (bnk[n].instr <> NIL) -> skip over?
              INC n
            ELSE
              nb:=f*v+s+4
              IF nb.instr <> NIL
                instr[n]:=nb.instr ;  nb.instr:=0 -> no accident play!
                deletebank(bnk[n]); CopyMem(nb, bnk[n], v)
                INC n
              ENDIF
              INC f
            ENDIF
          UNTIL (n = NUMBANKS) OR (f=z)
        ENDIF
      ENDIF
    ELSE
      Raise("bprj")
    ENDIF
  ELSE
    Raise("bprj")
  ENDIF

  IF s:=iff.first("MBFF","LSMP")
    z:=iff.size()
    v:=s;  h:=1 -> number of sample
    WHILE z>0
      IF (z:=z-StrLen(v)-1) >= 0
        printstatus(getLocStr(STRID_LOOKINGFORSAMPLE),FilePart(v),'')
        IF ln:=FindName(slist,FilePart(v))
          snd:=getvarafterln(ln)
        ELSEIF (snd:=getsound(v))=FALSE                  -> not here!
          StrCopy(str,FilePart(v))    -> only name
          AstrCopy(bufern,sndpath,256)  -> sample path!
          AddPart(bufern,str,256)       ->  'path/name'
          LowerStr(bufern)             -> nice this one
          IF ln:=FindName(slist,FilePart(bufern))
            snd:=getvarafterln(ln)
          ELSEIF (snd:=getsound(bufern))=FALSE  -> neither here!
            IF 1=(l:=asksfx(str))
              AstrCopy(bufern,sndpath,256)  -> (new) sample path!
              AddPart(bufern,str,256)   ->  (new) 'path/name'
              LowerStr(bufern)             -> nice this one
              IF ln:=FindName(slist,FilePart(bufern))
                snd:=getvarafterln(ln)
              ELSE
                snd:=getsound(bufern)
              ENDIF
            ELSEIF l=2
              z:=0
            ENDIF
          ENDIF
        ENDIF
        IF snd
          IF ln=0 THEN addsorted(slist,snd.ln)
          FOR f:=0 TO NUMBANKS-1
            IF instr[f]=h
              IF loadsmp(snd) THEN bnk[f].instr:=snd -> wow can play now!
            ENDIF
          ENDFOR
        ENDIF
      ENDIF
      v:=v+StrLen(v)+1
      INC h
    ENDWHILE
  ENDIF

  iff.close()

EXCEPT DO
  closestatus()
  END iff                            -> ALWAYS END the OBJECT before exiting!!!!
  IF exception THEN ReThrow()
ENDPROC


PROC asksfx(name)
DEF req=0:PTR TO rtfilerequester,file[109]:ARRAY OF CHAR
DEF ask
  IF (ask:=reqsample(name))=1
    IF req:=RtAllocRequestA(0,0)
      AstrCopy(file,name,109)
      RtChangeReqAttrA(req,[RTFI_DIR,sndpath,NIL])
      IF RtFileRequestA(req,file,getLocStr(STRID_CHOOSEANOTHERSAMPLE),[
                RTFI_FLAGS,FREQF_PATGAD,0])
        StrCopy(sndpath,req.dir,ALL)
        StrCopy(name,file,ALL)
      ELSE
        ask:=0
      ENDIF
    ELSE
      ask:=2
    ENDIF
  ENDIF
  IF req THEN RtFreeRequest(req)
ENDPROC ask

/*
  ========================================================================
*/
EXPORT PROC savesettings(slist:PTR TO lh,prefs:PTR TO mbprefs) HANDLE
  DEF iff:PTR TO iffparser,snd:PTR TO sfx,
      i,ln:PTR TO ln

  NEW iff.iffparser()

  iff.save({preferencesname})
  iff.createchunk("MBFF","FORM")

  iff.createchunk("MBFF","PREF")
    prefs.magic:=MAGIC_VERSION_0_PREFS
    prefs.sndoffs:=SIZEOF mbprefs
    prefs.prjoffs:=StrLen(sndpath) + 1 + SIZEOF mbprefs
    prefs.routeoffs:=prefs.prjoffs + StrLen(prjpath) + 1
    iff.writechunk(prefs,SIZEOF mbprefs)
    iff.writechunk(sndpath,StrLen(sndpath)+1)
    iff.writechunk(prjpath,StrLen(prjpath)+1)
    ln:=mysrclist.head
    WHILE ln.succ
      IF ln.name[] <> " " THEN iff.writechunk(ln.name+2,StrLen(ln.name+2)+1)
      ln:=ln.succ
    ENDWHILE
    iff.writechunk('',1)
  iff.closechunk()

  IF slist.head.succ
    iff.createchunk("MBFF","LSMP")
        ln:=slist.head
        WHILE ln.succ
          snd:=getvarafterln(ln)
          IF i:=snd.pathname() THEN iff.writechunk(i,StrLen(i)+1)
          ln:=ln.succ
        ENDWHILE
    iff.closechunk()
  ENDIF

  iff.closechunk()               -> Here we close FORM chunk!
  iff.close()                    -> AND here we close IFF save file session.

EXCEPT DO
  END iff
  report_exception()
ENDPROC

/* 
  ================== this function loads prefs only once!!! on the beginning
*/
PROC load_project_bis(name,slist:PTR TO lh,bnk:PTR TO bank,prefs:PTR TO mbprefs) HANDLE
DEF ret=0
  ret:=load_project(name,slist,bnk,prefs)
EXCEPT
  IF exception THEN report_exception()
ENDPROC ret

EXPORT PROC loadsettings(slist:PTR TO lh,prefs:PTR TO mbprefs,bnk,projectname) HANDLE
DEF iff=0:PTR TO iffparser,snd:PTR TO sfx,s,v,z,
    name[300]:ARRAY OF CHAR

  IF StrLen(projectname) > 0
    AstrCopy(name,prjpath,300); AddPart(name,projectname,300)
    IF load_project_bis(name,slist,bnk,prefs) THEN RETURN
  ENDIF
  defaultsettings(prefs)
  AstrCopy(name,{preferencesname},300)
  IF FileLength(name) = -1 THEN RETURN -1

  printstatus('',0,0)

  NEW iff.iffparser()
  iff.load(name)
  iff.setscan("MBFF","PREF")
  iff.setscan("MBFF","LSMP")
  iff.exit("MBFF","FORM")     -> We will stop when FORM ends!

  iff.scan()

  IF s:=iff.first("MBFF","PREF")
    z:=iff.size()
    IF s[z-1] <> 0 THEN Raise("PREF")
    IF convertprefs(s,z,prefs)=FALSE THEN Raise("PREF")
  ENDIF

  IF s:=iff.first("MBFF","LSMP")
    z:=iff.size()
    v:=s
    printstatus(getLocStr(STRID_INITINSTRUMENTS),0,0)
    WHILE z>0
      IF (z:=z-StrLen(v)-1) >= 0
        printstatus(0,FilePart(v),0)
        IF snd:=getsound(v)
          addsorted(slist,snd.ln)
        ENDIF
      ENDIF
      v:=v+StrLen(v)+1
    ENDWHILE
  ENDIF

  iff.close()

EXCEPT DO
  closestatus()
  END iff                            -> ALWAYS END the OBJECT before exiting!!!!
  IF exception
    defaultsettings(prefs)
    report_exception()
  ENDIF
ENDPROC

OBJECT mbprefs_old
  dmaper:INT
  routeoffs:INT
  sndoffs:INT
  prjoffs:INT
  buffsize:CHAR
  led:CHAR
  midictrl:CHAR
  activeb:CHAR
ENDOBJECT

PROC convertprefs(s:PTR TO mbprefs,l,prefs:PTR TO mbprefs)
DEF ln:PTR TO ln,oprefs:PTR TO mbprefs_old,v,f
  
  IF s.magic <> MAGIC_VERSION_0_PREFS
    oprefs:=s;
    IF oprefs.routeoffs <> SIZEOF mbprefs_old THEN RETURN FALSE
    prefs.dmaper:=oprefs.dmaper
    prefs.routeoffs:=NIL
    ln:=mysrclist.head
    WHILE ln.succ
      IF StrCmp(ln.name+2,s+oprefs.routeoffs,ALL) THEN ln.name[]:="+"
      ln:=ln.succ
    ENDWHILE
    prefs.sndoffs:=oprefs.sndoffs
    prefs.prjoffs:=oprefs.prjoffs
    prefs.led:=oprefs.led
    prefs.midictrl:=oprefs.midictrl
    prefs.activeb:=oprefs.activeb
  ELSE
    IF l < SIZEOF mbprefs THEN RETURN FALSE
    CopyMem(s,prefs,SIZEOF mbprefs)
  ENDIF
  IF prefs.sndoffs
    v:=s+prefs.sndoffs
    IF StrLen(v) > 0 THEN StrCopy(sndpath,v,ALL)
  ENDIF  
  IF prefs.prjoffs
    v:=s+prefs.prjoffs
    IF StrLen(v) > 0 THEN StrCopy(prjpath,v,ALL)
  ENDIF  
  IF prefs.routeoffs
    v:=s+prefs.routeoffs
    WHILE (f:=StrLen(v)) > 0
      ln:=mysrclist.head
      WHILE ln.succ
        IF StrCmp(ln.name+2,v,ALL) THEN ln.name[]:="+"
        ln:=ln.succ
      ENDWHILE
      v:=v+f+1
      IF v >= (s+l) THEN Raise("PREF")
    ENDWHILE
  ENDIF  
ENDPROC TRUE

PROC defaultsettings(prefs:PTR TO mbprefs)
  prefs.mainwinx:=-1
  prefs.mainwiny:=-1
  prefs.mainwinh:=-1
  prefs.mainwinw:=-1
  prefs.volumewinx:=-1
  prefs.volumewiny:=-1
  prefs.volumewinh:=-1
  prefs.volumewinw:=-1
  prefs.volumehide:=TRUE
  prefs.envelwinx:=-1
  prefs.envelwiny:=-1
  prefs.envelwinh:=-1
  prefs.envelwinw:=-1
  prefs.envelhide:=TRUE
  prefs.scopewinx:=-1
  prefs.scopewiny:=-1
  prefs.scopewinh:=-1
  prefs.scopewinw:=-1
  prefs.scopehide:=TRUE
  prefs.midimonwinx:=-1
  prefs.midimonwiny:=-1
  prefs.midimonwinh:=-1
  prefs.midimonwinw:=-1
  prefs.midimonhide:=TRUE
  prefs.dmaper:=125
  prefs.routeoffs:=NIL
  prefs.sndoffs:=NIL
  prefs.prjoffs:=NIL
  prefs.currentmcm:=32
  prefs.led:=TRUE
  prefs.midictrl:=TRUE
  prefs.activeb:=0
  prefs.maxchannels:=4
  prefs.msgflags:=MMF_NOTEOFF OR MMF_NOTEON OR MMF_PITCHBEND OR MMF_POLYPRESS OR MMF_CTRL OR MMF_CHANPRESS
  prefs.chanflags:=$FFFF
  StrCopy(prjpath,'PROGDIR:Projects')
  SetStr(sndpath,0)
ENDPROC


preferencesname: CHAR 'PROGDIR:midiIn.pref',0


/* ========================================================================
                                icons functions
   ======================================================================== */

EXPORT PROC freemidiin_icon()
  IF iconbase
    IF projecticon THEN FreeDiskObject(projecticon)
    projecticon:=0
  ENDIF
ENDPROC

PROC getmidiin_icon(name)
DEF s[300]:ARRAY OF CHAR
  IF iconbase
    freemidiin_icon()
    IF name
      projecticon:=GetDiskObject(name)
    ELSE
      AstrCopy(s,prjpath,300)
      AddPart(s,'def_midiIn_project',300)
      IF 0=(projecticon:=GetDiskObject(s))
        AstrCopy(s,'ENV:Sys/def_midiIn_project',STRLEN+1)
        IF 0=(projecticon:=GetDiskObject(s))
          AstrCopy(s,'PROGDIR:Icons/def_midiIn_project',STRLEN+1)
          IF 0=(projecticon:=GetDiskObject(s))
            projecticon:=GetDefDiskObject(WBPROJECT)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDPROC

PROC savemidiin_icon(name)
DEF olddeftool
  IF iconbase
    IF projecticon=0 THEN getmidiin_icon(0)
    IF projecticon
      olddeftool:=projecticon.defaulttool
      projecticon.defaulttool:='midiIn:midiIn'
      PutDiskObject(name,projecticon)
      projecticon.defaulttool:=olddeftool
    ENDIF
  ENDIF
ENDPROC
