OPT MODULE
OPT OSVERSION=37

MODULE 'intuition/intuition','tools/arq','reqtools','tools/EasyGUI',
       'midi','exec/memory','exec/nodes','*mblocale','*mbabout','*mbprogressbar'

EXPORT DEF cxhotkey -> hotkey txt to display
EXPORT DEF dest -> midi dest to flush
EXPORT DEF mh:PTR TO multihandle,
           gh:PTR TO guihandle,       -> main window
           volgh:PTR TO guihandle,    -> volume
           envgh:PTR TO guihandle,    -> envelope
           scopegh:PTR TO guihandle,  -> scope
           mongh:PTR TO guihandle,    -> midimonitor
           setgh:PTR TO guihandle,    -> midi settings
           saugh:PTR TO guihandle     -> audio settings

DEF statgh:PTR TO guihandle, prsbar:PTR TO progressbar,
    aboutgh:PTR TO guihandle, aboutpict:PTR TO aboutpicture
DEF statgd_stat -> status text gadgets
DEF lasttxt -> last txt for statgd
DEF winblocked        -> boolean TRUE=windows are blocked

PROC doreq(text,gadget,animID,title)
DEF result, eestruct:exteasystruct,mytitle[50]:STRING


    StringF(mytitle,'\s \s\c',getLocStr(STRID_MIDIIN),title,$a0) -> do the non-blanking space
    eestruct.animid            := animID
    eestruct.magic             := ARQ_MAGIC

    eestruct.flags             := NIL
    eestruct.sound             := 0
    eestruct.reserved[0]       := NIL
    eestruct.reserved[1]       := NIL
    eestruct.reserved[2]       := NIL

    eestruct.image             := 0

    eestruct.easy.structsize   := SIZEOF easystruct
    eestruct.easy.flags        := NIL
    eestruct.easy.title        := mytitle
    eestruct.easy.textformat   := text
    eestruct.easy.gadgetformat := gadget


    blockallwindows()
    result := EasyRequestArgs( NIL, eestruct.easy, NIL, NIL )
    unblockallwindows()

ENDPROC result

EXPORT PROC blockallwindows()
  winblocked:=winblocked+1
  IF winblocked > 1 THEN RETURN
  IF gh THEN IF gh.wnd THEN blockwin(gh)
  IF setgh THEN IF setgh.wnd THEN blockwin(setgh)
  IF saugh THEN IF saugh.wnd THEN blockwin(saugh)
  IF volgh THEN IF volgh.wnd THEN blockwin(volgh)
  IF envgh THEN IF envgh.wnd THEN blockwin(envgh)
  IF scopegh THEN IF scopegh.wnd THEN blockwin(scopegh)
  IF mongh THEN IF mongh.wnd THEN blockwin(mongh)
ENDPROC

EXPORT PROC unblockallwindows()
  IF winblocked <= 0 THEN RETURN
  winblocked:=winblocked-1
  IF winblocked > 0 THEN RETURN
  IF gh THEN IF gh.wnd THEN unblockwin(gh)
  IF setgh THEN IF setgh.wnd THEN unblockwin(setgh)
  IF saugh THEN IF saugh.wnd THEN unblockwin(saugh)
  IF volgh THEN IF volgh.wnd THEN unblockwin(volgh)
  IF envgh THEN IF envgh.wnd THEN unblockwin(envgh)
  IF scopegh
    IF scopegh.wnd
      unblockwin(scopegh)
      ModifyIDCMP(scopegh.wnd,scopegh.wnd.idcmpflags OR IDCMP_SIZEVERIFY)
    ENDIF
  ENDIF
  IF mongh THEN IF mongh.wnd THEN unblockwin(mongh)
  IF dest THEN FlushMDest(dest)   -> flush midi dest lockwin was too long
ENDPROC

EXPORT PROC reqclear()
DEF s[60]:STRING
  StringF(s,'\s|\s|\s',getLocStr(STRID_UNUSED),getLocStr(STRID_ALL),getLocStr(STRID_CANCEL))
ENDPROC doreq(getLocStr(STRID_AREUSURE),s,ARQ_ID_DELETE,getLocStr(STRID_REQUEST))

EXPORT PROC reqnotundo() IS doreq(getLocStr(STRID_NOTTOUNDO),getLocStr(STRID_CONTINUE),ARQ_ID_EXCLAM,getLocStr(STRID_TROUBLE))
EXPORT PROC reqnotredo() IS doreq(getLocStr(STRID_NOTTOREDO),getLocStr(STRID_CONTINUE),ARQ_ID_EXCLAM,getLocStr(STRID_TROUBLE))

EXPORT PROC reqskipover()
DEF s[60]:STRING
  StringF(s,'\s|\s|\s',getLocStr(STRID_OVERWRITE),getLocStr(STRID_SKIPOVER),getLocStr(STRID_CANCEL))
ENDPROC doreq(getLocStr(STRID_OVERORSKIP),s,ARQ_ID_DELETE,getLocStr(STRID_REQUEST))

EXPORT PROC reqdontfit(numleft)
DEF s[60]:STRING,t[120]:STRING
  StringF(s,'\s|\s',getLocStr(STRID_CONTINUE),getLocStr(STRID_CANCEL))
  StringF(t,getLocStr(STRID_MERGEDONTFIT),numleft)
ENDPROC doreq(t,s,ARQ_ID_DELETE,getLocStr(STRID_REQUEST))

EXPORT PROC reqquit()
DEF s[50]:STRING
  StringF(s,'\s|\s',getLocStr(STRID_LOSE),getLocStr(STRID_BACK))
ENDPROC doreq(getLocStr(STRID_UNSAVED),s,ARQ_ID_QUESTION,getLocStr(STRID_QUESTION))

EXPORT PROC reqexit()
DEF s[50]:STRING
  StringF(s,'\s|\s',getLocStr(STRID_EXIT),getLocStr(STRID_BACK))
ENDPROC doreq(getLocStr(STRID_SUREQUIT),s,ARQ_ID_QUESTION,getLocStr(STRID_QUESTION))

EXPORT PROC reqsample(sname)
DEF s[80]:STRING,g[60]:STRING,sl
  StringF(s,'\s\n\a',getLocStr(STRID_INSTRNOTFOUND))
  IF (sl:=StrLen(sname)) >= 58
    StrAdd(s,'... ')
    StrAdd(s,sname+sl-53,53)
  ELSE
    StrAdd(s,sname,57)
  ENDIF
  StrAdd(s,'\a')
  StringF(g,'\s|\s|\s',getLocStr(STRID_REPLACE),getLocStr(STRID_ABORT),getLocStr(STRID_SKIP))
ENDPROC doreq(s,g,ARQ_ID_INFO,getLocStr(STRID_TROUBLE))

EXPORT PROC report_exception()
DEF s[80]:STRING,e[5]:ARRAY

  IF exception
    IF exception<10000
      StringF(s,'\s #\d!',getLocStr(STRID_UNKNOWN),exception)
    ELSE
      IF (exception="OPEN") OR (exception="oold") OR (exception="onew")
        StrCopy(s,getLocStr(STRID_WRONGFILENAME))
      ELSEIF (exception="oiff") OR (exception="NIFF") OR (exception="MNGL")
        StrCopy(s,getLocStr(STRID_NOTINTERCHANGEFF))
      ELSEIF (exception="init") OR (exception="coll") OR (exception="exit") OR (exception="iffa") OR (exception="popc")
        StrCopy(s,getLocStr(STRID_IFFMANGLED))
      ELSEIF (exception="push") OR (exception="writ")
        StrCopy(s,getLocStr(STRID_WRITEERROR))
      ELSE
        SELECT exception
          CASE "RNGE";  StringF(s,getLocStr(STRID_SETRANGEFIRST))
          CASE "CXER";  StringF(s,getLocStr(STRID_POPKEYERROR),cxhotkey)
          CASE "bprj";  StrCopy(s,getLocStr(STRID_BADPROJECT))
          CASE "AUDB";  StrCopy(s,getLocStr(STRID_AUDIOERROR))
          CASE "NAIF";  StrCopy(s,getLocStr(STRID_NOCOMMONERROR))
          CASE "N8SV";  StrCopy(s,getLocStr(STRID_NOAHDRERROR))
          CASE "UNRE";  StrCopy(s,getLocStr(STRID_UNKNOWNSOUNDFILE))
          CASE "READ";  StrCopy(s,getLocStr(STRID_READERROR))
          CASE "NSND";  StrCopy(s,getLocStr(STRID_NOSSNDERROR))
          CASE "NBDY";  StrCopy(s,getLocStr(STRID_NOBODYERROR))
          CASE "FIBO";  StrCopy(s,'Fibonacci-delta compression not supported!')
          CASE "BWAV";  StrCopy(s,getLocStr(STRID_NOWAVECHUNKS))
          CASE "WAVN";  StrCopy(s,getLocStr(STRID_UNSUPPORTEDWAVE))
          CASE "MEM" ;  StrCopy(s,getLocStr(STRID_MEMERROR))
          CASE "MATH";  StrCopy(s,getLocStr(STRID_MATHERROR))
          CASE "LIB" ;  StringF(s,getLocStr(STRID_LIBRARYERROR),exceptioninfo)
          CASE "ROUT";  StringF(s,getLocStr(STRID_MIDISOURCEERR),exceptioninfo)
          CASE "GT";    StrCopy(s,getLocStr(STRID_GADTOOLSERR))
          CASE "bigg";  StrCopy(s,getLocStr(STRID_WINDOWERROR))
          CASE "GUI";   StrCopy(s,getLocStr(STRID_GADMEMERROR))
          CASE "PREF";  StrCopy(s,getLocStr(STRID_PREFSERROR))
          CASE "iffp";  StrCopy(s,getLocStr(STRID_IFFPARSEERROR))
          CASE "^C";    StrCopy(s,getLocStr(STRID_USERBREAK))
          DEFAULT  
            e[4]:=0
            ^e:=exception
            WHILE e[]=0 DO e++
            StringF(s,'Exception: "\s" 0x\h[8]!\h[8] ',e,exception,exceptioninfo)
        ENDSELECT
      ENDIF
    ENDIF
    doreq(s,getLocStr(STRID_NOWAY),ARQ_ID_EXCLAM,getLocStr(STRID_ERROR))
  ENDIF
ENDPROC

EXPORT PROC reqsumm(n,a,b,c,d,t,usl)
DEF s[400]:STRING,
    m[50]:STRING,ds[15]:STRING,es[15]:STRING,data:PTR TO LONG,node:PTR TO mln,count=0

  MOVE.L  A4,data
  node:=data[-5]
  IF node
    WHILE node.succ ; count:=count+node.pred ; node:=node.succ; ENDWHILE
  ENDIF

  StringF(s,'\s \q\s\q\n\n\s \d\n\s \d\n\s \d\n\s \s \s\n',
    getLocStr(STRID_PROJECTNAME),n,
    getLocStr(STRID_ACTIVEBANKS),a,
    getLocStr(STRID_SAMPLESINLIST),b,
    getLocStr(STRID_SAMPLESINMEMORY),c,
    getLocStr(STRID_TOTALLENGTH),dotnum(d,ds),
    getLocStr(STRID_BYTES))

  StringF(m,'\n * \s\n',getLocStr(STRID_FREEMEMORY))
  StrAdd(s,m)
  StringF(m,'\s \s  \s \s \s\n',getLocStr(STRID_FAST),dotnum(AvailMem(MEMF_FAST),ds),getLocStr(STRID_LARGEST),dotnum(AvailMem(MEMF_FAST OR MEMF_LARGEST),es),getLocStr(STRID_BYTES))
  StrAdd(s,m)
  StringF(m,'\s \s  \s \s \s\n',getLocStr(STRID_CHIP),dotnum(AvailMem(MEMF_CHIP),ds),getLocStr(STRID_LARGEST),dotnum(AvailMem(MEMF_CHIP OR MEMF_LARGEST),es),getLocStr(STRID_BYTES))
  StrAdd(s,m)
  StringF(m,'\n\s \s \s\n',getLocStr(STRID_DATASIZE),dotnum(count,ds),getLocStr(STRID_BYTES))
  StrAdd(s,m)
  StringF(m,'\n\s \d \s\n',getLocStr(STRID_UNDOMEMLEFT),usl,getLocStr(STRID_BYTES))
  StrAdd(s,m)
  StringF(m,'\n\s \d:\z\d[2]:\z\d[2] \s',getLocStr(STRID_RUNTIME),t/3600,Mod(Div(t,60),60),Mod(t,60),getLocStr(STRID_SECONDS))
  StrAdd(s,m)

  doreq(s,'OK',ARQ_ID_INFO,getLocStr(STRID_PROJECTSUMMARY))

ENDPROC

PROC dotnum(num,s)
DEF m[11]:STRING,n[20]:ARRAY OF CHAR,x,l,i,max

  StringF(m,'\d',num)
  l:=StrLen(m); x:=3
  max:=l+((l-1)/3)
  n[max]:=0; DEC max
  FOR i:=l-1 TO 0 STEP -1
    n[max]:=m[i]; DEC max
    DEC x; IF (x=0) AND (i<>0); x:=3; n[max]:=","; DEC max; ENDIF
  ENDFOR
  StrCopy(s,n)
ENDPROC s

EXPORT PROC string_info() IS {versionstr}+5

versionstr: INCBIN 'version.bin'
CHAR 0

/* =======================================================================
                              status window
   ======================================================================= */

EXPORT PROC open_status(screen,ta,font)

  statgh:=addmultiA(mh,'',
    [EQROWS,
      statgd_stat:=[TEXT,'','midiIn:',FALSE,25],
      [BEVELR,[PLUGIN,0 ,NEW prsbar.init(screen,font),FALSE,NIL]]
    ],
      [EG_CLEAN,  {clean_status},
       EG_WTYPE, WTYPE_BASIC,
       EG_HIDE, TRUE,
       EG_SCRN,screen,
       EG_FONT,ta,
       0,0])

ENDPROC


PROC clean_status()
  statgh:=0
ENDPROC

EXPORT PROC closestatus()
  IF statgh.wnd
    printstatus('','','')
    closewin(statgh)
  ENDIF
ENDPROC

EXPORT PROC printstatus(statustext,infotext,typetext,progrs=0,full=0) HANDLE
  IF statgh.wnd=0
    prsbar.settext(infotext,typetext,0,1);  openwin(statgh)
  ENDIF
  IF statustext
    IF statustext<>lasttxt THEN settext(statgh,statgd_stat,statustext)
    lasttxt:=statustext
  ENDIF
  prsbar.settext(infotext,typetext,progrs,full)

EXCEPT
  RETURN FALSE
ENDPROC TRUE

/* =======================================================================
                              about picture
   ======================================================================= */

EXPORT PROC open_aboutpic(screen,ta)

  aboutgh:=addmultiA(mh,'',
      [PLUGIN,{about_act},NEW aboutpict.init(screen), FALSE,NIL],
      [EG_CLEAN,  {clean_about},
       EG_WTYPE, WTYPE_BASIC,
       EG_HIDE, FALSE,
       EG_SCRN,screen,
       EG_FONT,ta,
       0,0])

ENDPROC

PROC about_act(info,abpic:PTR TO aboutpicture) IS closewin(aboutgh)

PROC clean_about()
  aboutgh:=0
  END aboutpict
ENDPROC

EXPORT PROC reqabout()
  IF aboutgh.wnd=NIL
    openwin(aboutgh)
  ELSE
    WindowToFront(aboutgh.wnd)
    ActivateWindow(aboutgh.wnd)
  ENDIF
ENDPROC
