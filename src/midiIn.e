/* MidiIn 32.014b in progress by Royal 98 */

OPT LARGE
OPT OSVERSION=37
OPT PREPROCESS
MODULE 'tools/EasyGUI', 'intuition/intuition','layers',
       'exec/lists','exec/nodes','exec/ports','exec/semaphores',
       'dos/dos','workbench/startup','workbench/workbench','icon',
       '*mbgui','*midibplists','*soundfx','*mbreport','*mbdiskoper',
       '*mbbanks','*mbsetup','*mbplay','*mbleds','*mbwindow','*mbscopes',
       'libraries/midi','midi',
        'amigalib/tasks','other/ecode',
        'mathieeedoubbas',
        'commodities','libraries/commodities',
        'locale','*mblocale',
        'diskfont','graphics/text',
        'devices/timer','exec/io'



DEF timestart,dummy  -> global work time
DEF bd[NUMBANKS]:ARRAY OF bank, cpbn:bank, cpnm[256]:STRING  -> bank's and clipbank data
DEF nbank=0                                                 ->current bank num.
DEF prilist[NUMBANKS]:ARRAY OF LONG                         ->list of bank pri sorted
DEF keybchannels          -> which channel on which keyboard key
    -> ^^^  data 4 pianokeys: for other bank ranges & playing (active) pianokeys

DEF smplist:lh       -> exec list for initialised samples

DEF mainbartext         -> default strings for main window (const)
DEF instrumenttext[32]:STRING,timetext[16]:STRING,wintext[80]:STRING  -> strings for sample list & window title
DEF sndpath[256]:STRING,prjpath[256]:STRING,prjname[32]:STRING
    -> ^^^ path for samples, projects, current project name, midisource name (eg. "MidiIn")
DEF mysrclist:lh  -> exec list for my source listview
DEF mh=NIL:PTR TO multihandle   -> multiple EasyGUI!!?
DEF mp:PTR TO pianokeys   -> EGUI plugins piano
DEF notestr[8]:STRING,basestr[8]:STRING,         -> music note, base note strings
    followb=0,basesetb=0,rangesetb=0,mareaset=255   -> control vars for pianokeys
  -> control vars
 
DEF askquit=FALSE   -> last changes save query
DEF mbprefs:mbprefs                        -> midiIn preferences struct

DEF deffont=0,defta=0:PTR TO textattr,defscreen=0,pubscreenname[120]:STRING   -> program's argument vars


    /* external playing handler task vars: */

DEF packet=0:PTR TO midipacket,   -> MIDI-related
    dest=0:PTR TO mdest,
    minfo:mrouteinfo

DEF oldlock=0, catalog=0  -> dos, locale stuff

CONST EVT_HOTKEY=1                -> Cx related
DEF cxhotkey[60]:STRING,cxpri=0
DEF broker_mp=NIL:PTR TO mp, broker=NIL, filter=NIL, sender=NIL, translate=NIL

RAISE "MEM" IF CreateCxObj()=NIL  -> the CxXXX macros use this
/*-----------------------------------------------------------------------*/

PROC main() HANDLE
DEF res=-1,mid=-1,a,b,sigmidi,sigwnd,cxsigflag,sigtime,signal,type,mmsg,
    msg,msgid,msgtype,reqfirsttime=TRUE,signalmask,
    timermp=0:PTR TO mp,timereq=0:PTR TO timerequest

  newlist(smplist)  -> this must be the first thing!!!
  initbanks(cpbn)
  keybchannels:=[255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
                 255,255,255,255,255,255,255,255,255,255,255,255]:CHAR
  mh:=multiinit()
  IF oldlock:=GetProgramDir() THEN oldlock:=CurrentDir(oldlock)
    ->^ change directory to PROGDIR:
  localebase:=OpenLibrary('locale.library',0)
  iconbase:=OpenLibrary('icon.library',36)
  IF localebase THEN catalog:=OpenCatalogA(NIL,'midiIn.catalog', 0)
  IF (mathieeedoubbasbase:=OpenLibrary('mathieeedoubbas.library',0))=0 THEN Raise("MATH")
  IF (cxbase:=OpenLibrary('commodities.library', 37))=0 THEN Throw("LIB",'commodities v37+')
  IF (reqtoolsbase:=OpenLibrary('reqtools.library',38))=0 THEN Throw("LIB",'reqtools v38+')
  IF (diskfontbase:=OpenLibrary('diskfont.library',37))=0 THEN Throw("LIB",'diskfont v37+')
  IF (layersbase:=OpenLibrary('layers.library',37))=0 THEN Throw("LIB",'layers v37+')
  IF (midibase:=OpenLibrary('midi.library',MIDIVERSION))=0 THEN Throw("LIB",'midi v7.-1')
  IF (dest:=CreateMDest(0,0))=0 THEN Raise("MEM")
  IF (timermp:=CreateMsgPort())=0 THEN Raise("MEM")
  IF (timereq:=CreateIORequest(timermp,SIZEOF timerequest))=0 THEN Raise("MEM")
  IF OpenDevice('timer.device',UNIT_VBLANK,timereq,0)
    DeleteIORequest(timereq); timereq:=0; Raise("TIME")
  ENDIF
  sigtime:=Shl(1, timermp.sigbit)
  triggertime(timereq,1000000/10)

  getargs()
  IF defta THEN IF 0 = (deffont:=OpenDiskFont(defta)) THEN END defta
  IF (broker_mp:=CreateMsgPort())=0 THEN Raise("MEM")
  cxsigflag:=Shl(1, broker_mp.sigbit)

  IF 0 = (broker:=CxBroker([NB_VERSION, 0,
                    getLocStr(STRID_MIDIIN), -> String to identify this broker
                    getLocStr(STRID_CXTITLE),
                    getLocStr(STRID_CXDESCR),
                    NBU_UNIQUE OR NBU_NOTIFY,
                    COF_SHOW_HIDE, cxpri, 0,
                    broker_mp, 0]:newbroker, NIL)) THEN Raise(0)

  filter:=CxFilter(cxhotkey)
  AttachCxObj(broker, filter)
  sender:=CxSender(broker_mp, EVT_HOTKEY)
  AttachCxObj(filter, sender)
  translate:=CxTranslate(NIL)
  AttachCxObj(filter, translate)
  IF CxObjError(filter)<>FALSE THEN Raise("CXER")
  ActivateCxObj(broker, TRUE)

  defscreen:=LockPubScreen(pubscreenname)
  open_aboutpic(defscreen,defta)
  open_status(defscreen,defta)
  initprefs(prjname,bd)
-> WriteF('prefs ok!\n')

  CurrentTime({timestart},{dummy})
  mainbartext:=string_info()
  open_gui(defscreen,defta,deffont)

    sigwnd:=mh.sig  -> multihandle signal!!!
    sigmidi:=Shl(1,dest.destport::mp.sigbit)

    install_playtask()

    signalmask:=sigtime OR sigwnd OR sigmidi OR cxsigflag OR SIGBREAKF_CTRL_C
    WHILE res<>0
      mid:=-1; res:=-1
      signal:=Wait(signalmask)
      IF signal AND sigmidi
        WHILE packet:=GetMidiPacket(dest)
          type:=packet.type;  mmsg:=packet.midimsg; a:=mmsg[1]; b:=mmsg[0]
          FreeMidiPacket(packet); packet:=0
          mid:=-1

          IF ((type=MMF_NOTEON) OR (type=MMF_NOTEOFF)) AND mcontrol
            IF (rangesetb OR basesetb) = FALSE
              checkplayingbanks()
            ENDIF
            updatechannelkeys()
          ENDIF

          IF (type=MMF_NOTEON) AND mcontrol
            mp.autokey(a,SELECTDOWN); pianokeypressed(mp)
            IF (rangesetb OR basesetb) = FALSE
              IF followb THEN follow(a,b AND $F +1)
            ELSE
              mid:=1
            ENDIF
            IF rangesetb AND (basesetb=0)
              IF mareaset<128
                IF mareaset < a THEN mp.bounds(mareaset,a) ELSE mp.bounds(a,mareaset)
                mareaset:=255
              ELSE
                mareaset:=a
              ENDIF
            ENDIF
          ELSEIF (type=MMF_NOTEOFF) AND mcontrol
            IF (rangesetb OR basesetb)=FALSE
              IF pianokeypressed(mp)=a THEN pianokeypressed(mp,255)
            ELSEIF rangesetb AND (basesetb=0)
              IF mareaset=a THEN mareaset:=255
            ENDIF
          ELSEIF mcontrol=0
            mareaset:=255
          ENDIF
          IF mid=1    -> process every midi message!
            mp.keycode:=-1
            plugact(0,mp)
          ENDIF
        ENDWHILE
      ENDIF
      IF signal AND sigwnd
-> WriteF('window:\h\n',volgh.wnd.flags AND WFLG_REPORTMOUSE)
        res:=multimessage(mh)
        IF reqfirsttime; reqabout(); reqfirsttime:=FALSE; ENDIF
      ENDIF
      IF signal AND sigtime
        WaitIO(timereq)
  /* messwctrlers() */
        updatechannelkeys(); checkplayingbanks(); updatemidimonitor()
        triggertime(timereq,1000000/10)
      ENDIF
      IF signal AND cxsigflag
        WHILE msg:=GetMsg(broker_mp)
          msgid:=CxMsgID(msg)
          msgtype:=CxMsgType(msg)
          ReplyMsg(msg)
          SELECT msgtype
          CASE CXM_IEVENT
            IF msgid=EVT_HOTKEY -> We got the message from the sender CxObject
              show();  IF defscreen THEN ScreenToFront(defscreen)
            ENDIF
          CASE CXM_COMMAND
            SELECT msgid
            CASE CXCMD_DISABLE
              ActivateCxObj(broker, FALSE)
            CASE CXCMD_ENABLE
              ActivateCxObj(broker, TRUE)
            CASE CXCMD_KILL
              IF askquit ; IF reqquit() THEN Raise(0)
              ELSE; Raise(0)
              ENDIF
            CASE CXCMD_UNIQUE
              show();   IF defscreen THEN ScreenToFront(defscreen)
            CASE CXCMD_APPEAR
              show();   IF defscreen THEN ScreenToFront(defscreen)
            CASE CXCMD_DISAPPEAR
              hide(0)
            ENDSELECT
          ENDSELECT
        ENDWHILE
      ENDIF
      IF signal AND SIGBREAKF_CTRL_C
        Raise("^C")
      ENDIF
-> IF (res<>-1) OR (mid<>-1) THEN -> WriteF('res=\d mid=\d\n',res,mid)
    ENDWHILE

EXCEPT DO
  freeaudiodevice()
  clearsmplist()
  IF mh THEN cleanmulti(mh)
  UnlockPubScreen(0,defscreen)
  deinstall_playtask()
  freeallMRoutes()
  report_exception()
  IF broker THEN DeleteCxObjAll(broker)
  IF broker_mp
    WHILE msg:=GetMsg(broker_mp) DO ReplyMsg(msg)
    DeleteMsgPort(broker_mp)
  ENDIF
  IF timereq
    AbortIO(timereq); WaitIO(timereq)
    CloseDevice(timereq); DeleteIORequest(timereq)
  ENDIF
  IF timermp THEN DeleteMsgPort(timermp)
  IF dest THEN DeleteMDest(dest)
  IF midibase THEN CloseLibrary(midibase)
  IF layersbase THEN CloseLibrary(layersbase)
  IF deffont THEN CloseFont(deffont)
  IF diskfontbase THEN CloseLibrary(diskfontbase)
  IF reqtoolsbase THEN CloseLibrary(reqtoolsbase)
  IF cxbase THEN CloseLibrary(cxbase)
  IF mathieeedoubbasbase THEN CloseLibrary(mathieeedoubbasbase)
  freemidiin_icon()
  IF iconbase THEN CloseLibrary(iconbase)
  IF localebase THEN CloseCatalog(catalog)
  IF localebase THEN CloseLibrary(localebase)
  IF oldlock THEN CurrentDir(oldlock)
ENDPROC
  

PROC triggertime(tr:PTR TO timerequest,micros)
  tr.io.command:=TR_ADDREQUEST
  tr.time.secs:=0
  tr.time.micro:=micros
  SendIO(tr)
ENDPROC

PROC getargs()
DEF a:PTR TO LONG,argmsg:PTR TO wbstartup, wb_arg:PTR TO wbarg,olddir=0,
    diskobj:PTR TO diskobject,s,l,rdargs,fname=0,fsize=0,i

  a:=[0,0,0,0,0,0]
  IF wbmessage=0
    IF rdargs:=ReadArgs('PROJECT, PUBSCREENNAME, FONTNAME, FONTSIZE/N, CX_POPKEY, CX_PRIORITY/N',a,0)
      IF s:=a[0]
        IF StrLen(s) > 0
          StrCopy(prjname,FilePart(s),ALL)
          IF l:=(PathPart(s)-s) THEN StrCopy(prjpath,s,l)
        ENDIF
      ENDIF
      IF s:=a[2]
        IF fname:=String(StrLen(s)) THEN StrCopy(fname,s,ALL)
      ENDIF
      IF s:=a[3] THEN fsize:=Long(s)
      IF s:=a[1] THEN StrCopy(pubscreenname,s,ALL)
      IF s:=a[4] THEN StrCopy(cxhotkey,s,ALL)
      IF s:=a[5] THEN cxpri:=Long(s)
      FreeArgs(rdargs)
    ENDIF
  ELSE
    IF iconbase
      argmsg:=wbmessage
      wb_arg:=argmsg.arglist  -> Head of the arg list
      i:=argmsg.numargs-1
      REPEAT
        IF wb_arg[i].lock<>NIL
          olddir:=CurrentDir(wb_arg[i].lock)
        ENDIF
        IF diskobj:=GetDiskObject(wb_arg[i].name)
          IF s:=FindToolType(diskobj.tooltypes,'PUBSCREENNAME') THEN IF EstrLen(pubscreenname)=0 THEN StrCopy(pubscreenname,s,ALL)
          IF s:=FindToolType(diskobj.tooltypes,'FONTNAME')
            IF fname=0 THEN IF fname:=String(StrLen(s)) THEN StrCopy(fname,s,ALL)
          ENDIF
          IF s:=FindToolType(diskobj.tooltypes,'FONTSIZE') THEN IF fsize<4 THEN fsize:=Val(s)
          IF s:=FindToolType(diskobj.tooltypes,'CX_POPKEY') THEN IF EstrLen(cxhotkey)=0 THEN StrCopy(cxhotkey,s,ALL)
          IF s:=FindToolType(diskobj.tooltypes,'CX_PRIORITY') THEN cxpri:=Val(s)
          FreeDiskObject(diskobj)
        ENDIF
        IF i>0
          IF l:=wb_arg[i].lock
            IF NameFromLock(l,prjpath,256) THEN SetStr(prjpath,StrLen(prjpath)) ELSE SetStr(prjpath,0)
            StrCopy(prjname,wb_arg[i].name,ALL)
          ENDIF
        ENDIF
        IF olddir THEN CurrentDir(olddir)
        olddir:=0
        i:=IF i>0 THEN 0 ELSE -1
      UNTIL i < 0
    ENDIF
  ENDIF
  IF EstrLen(cxhotkey)=0 THEN StrCopy(cxhotkey,'control shift m')
  IF (fname <> 0) AND (fsize > 3)
    NEW defta
    defta.name:=fname
    defta.ysize:=fsize
  ENDIF
->  WriteF('\q\s\q \d \q\s\q font:\s \d\n',pubscreenname,cxpri,cxhotkey,IF fname THEN fname ELSE '<default>',fsize)
ENDPROC


