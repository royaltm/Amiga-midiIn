OPT MODULE
OPT OSVERSION=37

MODULE 'fabio/IFFParser_oo','exec/lists','exec/nodes',
  'intuition/intuition','reqtools','libraries/reqtools','libraries/midi',
  'workbench/workbench','icon',
  '*midibplists','*mbbanks','*mblocale','*mbreport','*soundfx_ahi','*mbundo'


EXPORT CONST MAGIC_VERSION_0_PREFS="mIn0",
             MAGIC_VERSION_1_PREFS="mIn1"

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
  ahiaudioid:LONG -> new in version 1
  mixfreq:LONG
ENDOBJECT

OBJECT undosaveheader
  undoversion:INT
  undotypemax:INT
  banksize:INT
  bankscnt:INT
ENDOBJECT

CONST VERSION_0_UNDO=1000

EXPORT DEF prjpath, sndpath, mysrclist:PTR TO lh

DEF projecticon:PTR TO diskobject


EXPORT PROC save_project(name,slist:PTR TO lh,bnk:PTR TO bank,prefs:PTR TO mbprefs, saveicons, saveundo) HANDLE
DEF iff:PTR TO iffparser,snd:PTR TO sfx, nb:bank,
    f,i,ln:PTR TO lln,
    undosavehd:undosaveheader,c

  NEW iff.iffparser()

  iff.save(name)
  iff.createchunk("MBFF","FORM")

  iff.createchunk("MBFF","BANK")
  f:=SIZEOF bank
  iff.writechunk({f},4)
  FOR f:=0 TO NUMBANKS-1
    CopyMem(bnk[f], nb, SIZEOF bank)
    IF snd:=nb.instr THEN nb.instr:=convlnptrtonum(snd,slist)
    iff.writechunk(nb,SIZEOF bank)
  ENDFOR
  iff.closechunk()

  iff.createchunk("MBFF","PREF")
    prefs.magic:=MAGIC_VERSION_1_PREFS
    prefs.sndoffs:=SIZEOF mbprefs
    prefs.prjoffs:=StrLen(sndpath) + 1 + SIZEOF mbprefs
    prefs.routeoffs:=prefs.prjoffs + StrLen(prjpath) + 1
    iff.writechunk(prefs,SIZEOF mbprefs)
    iff.writechunk(sndpath,StrLen(sndpath)+1)
    iff.writechunk(prjpath,StrLen(prjpath)+1)
    ln:=mysrclist.head
    WHILE ln.ln.succ
      IF ln.ln.name[] <> " " THEN iff.writechunk(ln.ln.name+2,StrLen(ln.ln.name+2)+1)
      ln:=ln.ln.succ
    ENDWHILE
    iff.writechunk('',1)
  iff.closechunk()

  IF slist.head.succ
    iff.createchunk("MBFF","LSMP")
        ln:=slist.head
        WHILE ln.ln.succ
          snd:=ln.pointer
          IF i:=snd.pathname() THEN iff.writechunk(i,StrLen(i)+1)
          ln:=ln.ln.succ
        ENDWHILE
    iff.closechunk()
  ENDIF

  IF saveundo
    iff.createchunk("MBFF","UNDO")
    undosavehd.undoversion:=VERSION_0_UNDO
    undosavehd.undotypemax:=UNDO_SET_MAX
    undosavehd.banksize:=SIZEOF bank
    undosavehd.bankscnt:=NUMBANKS
    iff.writechunk(undosavehd,SIZEOF undosaveheader)
    f:=NIL; f,i,c:=exportundo(f)
    WHILE f
      iff.writechunk({c},4); iff.writechunk(f,i)
      f,i,c:=exportundo(f)
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

EXPORT PROC loadinstrumentsfrombank(name,slist:PTR TO lh) HANDLE
  load_project(name,slist,NIL,NIL)
EXCEPT
  RETURN FALSE
ENDPROC TRUE

EXPORT PROC load_project(name,slist:PTR TO lh,bnk:PTR TO bank,prefs:PTR TO mbprefs) HANDLE
DEF iff:PTR TO iffparser, f,z,s,v,
    nb:PTR TO bank,instr[NUMBANKS]:ARRAY OF LONG,
    undosavehd:undosaveheader,b,x

  NEW iff.iffparser()

  iff.load(name)
  iff.setscan("MBFF","PREF")
  iff.setscan("MBFF","BANK")
  iff.setscan("MBFF","UNDO")
  iff.setscan("MBFF","LSMP")
  iff.exit("MBFF","FORM")     -> We will stop when FORM ends!

  iff.scan()
  FOR f:=0 TO NUMBANKS-1 DO instr[f]:=0

  IF bnk

    clearsmplist(bnk,slist)
  
    IF prefs
      IF s:=iff.first("MBFF","PREF")
        z:=iff.size()
        IF convertprefs(s,z,prefs)=FALSE THEN prefs:=0
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
            deletebank(bnk[f]); CopyMem(nb, bnk[f], v)
            IF checkbank(bnk[f])=FALSE THEN Raise("bprj")
          ENDFOR
        ENDIF
      ELSE
        Raise("bprj")
      ENDIF
    ELSE
      Raise("bprj")
    ENDIF

    IF s:=iff.first("MBFF","UNDO")
      IF (v:=Long(s)) > SIZEOF undosaveheader
        CopyMem(s,undosavehd,SIZEOF undosaveheader)
        IF (undosavehd.undoversion=VERSION_0_UNDO) AND (undosavehd.undotypemax<=UNDO_SET_MAX)
          IF (z:=undosavehd.banksize) <= SIZEOF bank
            IF (b:=undosavehd.bankscnt) <= NUMBANKS
              f:=s+SIZEOF undosaveheader
              WHILE f<(s+v)
                CopyMem(f,{x},4); f:=f+4
                EXIT (f:=importundo(f,x,b,z))=NIL
              ENDWHILE
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF

  ELSE
    IF iff.first("MBFF","BANK")=NIL THEN Raise("brpj")
  ENDIF

  IF s:=iff.first("MBFF","LSMP")
    z:=iff.size()
    load_instruments(s,z,slist, bnk, instr)
  ENDIF
  iff.close()

  IF bnk THEN getmidiin_icon(name)

EXCEPT DO
  END iff                            -> ALWAYS END the OBJECT before exiting!!!!
  IF exception="bprj" THEN initbanks(bnk)
  IF exception THEN ReThrow()
ENDPROC prefs

EXPORT PROC mergeproject(name,slist:PTR TO lh,bnk:PTR TO bank,numbnk) HANDLE
DEF iff:PTR TO iffparser, f,v,z,l,s,n,skip=FALSE,
    nb:PTR TO bank, instr[NUMBANKS]:ARRAY OF LONG

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
        n:=0
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
    load_instruments(s,z,slist, bnk, instr)
  ENDIF

  iff.close()

EXCEPT DO
  END iff                            -> ALWAYS END the OBJECT before exiting!!!!
  IF exception THEN ReThrow()
ENDPROC


PROC asksfx(name,size)
DEF req=0:PTR TO rtfilerequester,file[109]:ARRAY OF CHAR
DEF ask
  IF (ask:=reqsample(name))=1
    IF req:=RtAllocRequestA(0,0)
      AstrCopy(file,name,109)
      RtChangeReqAttrA(req,[RTFI_DIR,sndpath,NIL])
      IF RtFileRequestA(req,file,getLocStr(STRID_CHOOSEANOTHERSAMPLE),[
                RTFI_FLAGS,FREQF_PATGAD,0])
        StrCopy(sndpath,req.dir,ALL)
        AstrCopy(name,sndpath,size)
        AddPart(name,file,size)
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
      i,ln:PTR TO lln

  NEW iff.iffparser()

  iff.save({preferencesname})
  iff.createchunk("MBFF","FORM")

  iff.createchunk("MBFF","PREF")
    prefs.magic:=MAGIC_VERSION_1_PREFS
    prefs.sndoffs:=SIZEOF mbprefs
    prefs.prjoffs:=StrLen(sndpath) + 1 + SIZEOF mbprefs
    prefs.routeoffs:=prefs.prjoffs + StrLen(prjpath) + 1
    iff.writechunk(prefs,SIZEOF mbprefs)
    iff.writechunk(sndpath,StrLen(sndpath)+1)
    iff.writechunk(prjpath,StrLen(prjpath)+1)
    ln:=mysrclist.head
    WHILE ln.ln.succ
      IF ln.ln.name[] <> " " THEN iff.writechunk(ln.ln.name+2,StrLen(ln.ln.name+2)+1)
      ln:=ln.ln.succ
    ENDWHILE
    iff.writechunk('',1)
  iff.closechunk()

  IF slist.head.succ
    iff.createchunk("MBFF","LSMP")
        ln:=slist.head
        WHILE ln.ln.succ
          snd:=ln.pointer
          IF i:=snd.pathname() THEN iff.writechunk(i,StrLen(i)+1)
          ln:=ln.ln.succ
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
DEF iff=0:PTR TO iffparser,s,z,name[300]:ARRAY OF CHAR

  IF StrLen(projectname) > 0
    AstrCopy(name,prjpath,300); AddPart(name,projectname,300)
    IF load_project_bis(name,slist,bnk,prefs) THEN RETURN
  ENDIF
  defaultsettings(prefs)
  AstrCopy(name,{preferencesname},300)
  IF FileLength(name) = -1 THEN RETURN -1

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
    load_instruments(s,z,slist)
  ENDIF

  iff.close()

EXCEPT DO
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
  
  IF s.magic = MAGIC_VERSION_1_PREFS
    IF l < SIZEOF mbprefs THEN RETURN FALSE
    CopyMem(s,prefs,SIZEOF mbprefs)
  ELSEIF s.magic = MAGIC_VERSION_0_PREFS
    IF l < (SIZEOF mbprefs-8) THEN RETURN FALSE
    CopyMem(s,prefs,SIZEOF mbprefs-8)
    IF prefs.dmaper THEN prefs.mixfreq:=Div(3546895,prefs.dmaper)
    prefs.dmaper:=0
  ELSE
    oprefs:=s;
    IF oprefs.routeoffs <> SIZEOF mbprefs_old THEN RETURN FALSE
    prefs.dmaper:=oprefs.dmaper
    IF prefs.dmaper THEN prefs.mixfreq:=Div(3546895,prefs.dmaper)
    prefs.dmaper:=0
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
      IF v >= (s+l) THEN RETURN TRUE
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
  prefs.dmaper:=0
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
  prefs.ahiaudioid:=0
  prefs.mixfreq:=28375
  StrCopy(prjpath,'PROGDIR:Projects')
  SetStr(sndpath,0)
ENDPROC


preferencesname: CHAR 'PROGDIR:midiIn.pref',0

PROC load_instruments(m,size,slist:PTR TO lh, bn=FALSE:PTR TO bank, instr=FALSE:PTR TO LONG)
DEF i,x,l, buf[512]:ARRAY OF CHAR, snd:PTR TO sfx, all
  i:=m; all:=0; WHILE i<(m+size); i:=i+StrLen(i)+1; INC all; ENDWHILE
  x:=1 -> number of sample
  WHILE size>0
    IF (size:=size-StrLen(m)-1) >= 0
      printstatus(getLocStr(STRID_LOOKINGFORSAMPLE),FilePart(m),0,x,all)
      IF (snd:=addsnd(m, slist, TRUE))=NIL
        AstrCopy(buf,sndpath,512)  -> sample path!
        AddPart(buf,FilePart(m),512) ->  'path/name'
        IF (snd:=addsnd(buf, slist, TRUE))=NIL
          AstrCopy(buf, FilePart(m), 512)
          IF 1=(l:=asksfx(buf,512))
            snd:=addsnd(buf, slist)
          ELSEIF l=2
            size:=0
          ENDIF
        ENDIF
      ENDIF
      IF (bn<>NIL) AND (instr<>NIL) AND (snd<>NIL)
        FOR i:=0 TO NUMBANKS-1 DO IF instr[i]=x THEN setinstr(bn[i], snd)
      ENDIF
    ENDIF
    m:=m+StrLen(m)+1
    INC x
  ENDWHILE
  addingsamplesover()
ENDPROC

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


