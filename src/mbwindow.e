OPT LARGE
OPT OSVERSION=37
OPT PREPROCESS
OPT MODULE

MODULE 'tools/EasyGUI', 'intuition/intuition','libraries/gadtools',
       'reqtools','libraries/reqtools','graphics/text',
       'exec/lists','exec/nodes',
       'libraries/midi','midi','workbench/workbench','workbench/startup',
       '*mbbanks','*mbleds','*mbgui','*soundfx_ahi','*mbsetup','*mbdiskoper',
       '*mbreport','*mbplay','*midibplists','*title','*mbtitle','*mbscopes',
       '*mblocale','*mbenvelope','*mbkeycod','*mbmmonit','*mbundo'

ENUM WINDOW_MAIN, WINDOW_VOLUME, WINDOW_ENVELOPE

EXPORT DEF timestart,dummy  -> global work time

EXPORT DEF mbprefs:PTR TO mbprefs     -> prefs structure
EXPORT DEF sndpath,prjpath            -> prefs paths

EXPORT DEF prjname,instrumenttext,basestr,timetext,monosliderstr,
           wintext  -> strings for text gadgets and win bar
EXPORT DEF mainbartext  -> default strings for main window (const)

EXPORT DEF bd:PTR TO bank, nbank,   -> banks vars
           smplist:PTR TO lh        -> exec list of samples
       DEF cpbd:PTR TO bank, cpsize, cpstr -> clipbank var and clipsize
       DEF rangelo, rangehi
EXPORT DEF mh:PTR TO multihandle    -> multiple EasyGUI!
EXPORT DEF gh:PTR TO guihandle, menuptr,
           volgh:PTR TO guihandle,
           envgh:PTR TO guihandle,
           scopegh:PTR TO guihandle, 
           mongh:PTR TO guihandle -> windows & EGUI stuff
       DEF volwindow:PTR TO window, 
           envwindow:PTR TO window -> flags for hide to reopen!!!
EXPORT DEF mp:PTR TO pianokeys 
DEF leds:PTR TO leds, envp:PTR TO envel_plugin, midimon:PTR TO midimonitor
DEF title_a:PTR TO title_keys     -> ^^^ EGUI plugins

DEF egh_number, egh_envel                                       -> envelope EGUI's gadgets
DEF vgh_volum, vgh_veloc, vgh_panor, vgh_pantx, vgh_pwide, vgh_pitch,      -> volume EGUI's gadgets
    vgh_firstskip,vgh_banknum,vgh_freq,vgh_time,vgh_after,vgh_subaft,
    vgh_fullname,vgh_skipnum,vgh_mctvol,vgh_mctpan
DEF mgh_ctrl

EXPORT DEF gd_notetext,gd_instrtext,gd_midichansl,gd_bankprisl,
    gd_basetx,gd_durmx,gd_groupsl,gd_monochk,gd_monoslide,gd_monosltxt,gd_monovsens,
    gd_loopchk,gd_finetx,gd_finesl,gd_followck,
    gd_smplist,gd_audiock,gd_listtx,gd_mcontr,gd_rangeset,
    gd_undotx       -> main EGUI's gadgets

EXPORT DEF notestr -> string for diplaying note

EXPORT DEF currmcmon

EXPORT DEF followb,basesetb,rangesetb, mareaset,mcontrol   -> booleans for midi and pianokeys

EXPORT DEF askquit            -> change project boolean
EXPORT DEF dest:PTR TO mdest  -> our midi dest port

EXPORT DEF keybchannels       -> which note is currently played (for displaying)

DEF seconds,micros,dblistnum    -> time of last click on samplelist, last position clicked

DEF fullnameptr       -> pointer to ASCIIZ containing full sample path (volwindow)
DEF remlist           -> status of delete sample list button
DEF lastundotype      -> last undotype
/*
  ==================================================================================
  ----------------------------------------------------------------------------------
                            Global window functions
  ----------------------------------------------------------------------------------
  ==================================================================================
*/

EXPORT PROC open_gui(defscreen,defta,deffont)
  open_mainwindow(defscreen,defta)
  open_sauwin(defscreen,defta)
  open_setwin(defscreen,defta)
  open_volwin(defscreen,defta)
  open_envwin(defscreen,defta)
  open_midimonitwin(defscreen,defta,deffont)
  open_scopewindow(defscreen,defta)
  IF defscreen THEN ScreenToFront(defscreen)
  IF gh.wnd THEN ActivateWindow(gh.wnd)
  IF StrLen(prjname) > 0  THEN updategh()
ENDPROC

EXPORT PROC hide(info)
  close_setwin(0)
  IF volgh.wnd THEN closewin(volgh)
  IF envgh.wnd THEN closewin(envgh)
  IF gh.wnd THEN closewin(gh)
ENDPROC

EXPORT PROC show()
  IF gh.wnd
    IF envgh.wnd THEN WindowToFront(envgh.wnd)
    IF volgh.wnd THEN WindowToFront(volgh.wnd)
    WindowToFront(gh.wnd); ActivateWindow(gh.wnd)
  ELSE
    IF volwindow THEN volumewindow(0)
    IF envwindow THEN envelwindow(0)
    mainwindow(0)
  ENDIF
ENDPROC


/*
  ==================================================================================
  ----------------------------------------------------------------------------------
                               [Main window]
  ----------------------------------------------------------------------------------
  ==================================================================================
*/
PROC mainwindow(info)
  IF gh.wnd
    WindowToFront(gh.wnd); ActivateWindow(gh.wnd)
  ELSE
    IF remlist THEN remsfx(info)
    settext(gh,gd_instrtext,''); lastundotype:=0; settext(gh,gd_undotx,'')
    openwin(gh); menuptr:=gh.wnd.menustrip
    updategh()
  ENDIF
ENDPROC

PROC open_mainwindow(screen,ta)
DEF prjwindow:PTR TO window, e,g,m,t,i,d,l,s,z,x,a,c,w,q
  gh:=addmultiA(mh,mainbartext,
    [ROWS,
      [PLUGIN,{plugact},NEW mp.init(screen),FALSE,NIL],
      [BAR],
      [COLS,
        gd_notetext:=[TEXT,'','',TRUE,4],
        [BAR],
        gd_undotx:=[TEXT,'',getLocStr(STRID_MAINUNDO),TRUE,12],
        [BAR],
        gd_rangeset:=[CHECK,{rangeset},getLocStr(STRID_EDITRANGE,{g}),rangesetb,TRUE,0, g ],
        [BAR],
        gd_mcontr:=[CHECK,{midiset},getLocStr(STRID_MIDICTRL,{m}),mcontrol,TRUE,0, m ],
        [BAR],
        gd_audiock:=[CHECK,{audioset},getLocStr(STRID_AUDIO),FALSE,TRUE,0,0],
        [BAR],
        gd_followck:=[CHECK,{followset},getLocStr(STRID_FOLLOW,{e}),FALSE,TRUE,0, e ]
      ],
      [BAR],
      [COLS,
        [ROWS,
          [BEVELR,
            [ROWS,
              [COLS,
                [ROWS,
                  gd_instrtext:=[TEXT,'',getLocStr(STRID_INSTRUMENT),TRUE,12],
                  [COLS,
                    gd_midichansl:=[SLIDE,{midichan},getLocStr(STRID_MIDICHAN,{i}),0,1,16,1,8,'\d[2]',0, i ],
                    [SPACEH],
                    [BAR]
                  ]
                ],
                [ROWS,
                  [SBUTTON,{free},getLocStr(STRID_FREE),0, 0 ],
                  [SBUTTON,{reload},getLocStr(STRID_RELOAD,{t}),0, t ]
                ]
              ],
              [SPACEV],
              [COLS,
                gd_bankprisl:=[SLIDE,{setpri},getLocStr(STRID_BANKPRIORITY),0,1,NUMBANKS,1,6,'\d[2]',0,0],
                gd_groupsl:=[SLIDE,{setgroup},getLocStr(STRID_GROUP,{x}),0,0,16,0,3,'\d[2]',0, x]
              ],
              [SPACEV],
              [COLS,
                gd_durmx:=[MX,{dur},getLocStr(STRID_DURATION,{d}),[getLocStr(STRID_ONOFF),getLocStr(STRID_ONON),getLocStr(STRID_DRUM),0],TRUE,0,0, d ],
                [COLS,
                  [ROWS,
                    gd_monochk:=[CHECK,{monophonic},getLocStr(STRID_MONOPHONIC,{z}),FALSE,TRUE,0, z ,FALSE],
                    gd_loopchk:=[CHECK,{loop},getLocStr(STRID_LOOP,{l}),FALSE,TRUE,0, l ,FALSE]
                  ],
                  [BEVELR,[ROWS,
                    [COLS,
                      gd_monoslide:=[SLIDE,{monoslide},getLocStr(STRID_MONOSLIDE,{w}),0,0,1500,0,8,'',0, w ,TRUE],
                      gd_monosltxt:=[TEXT,getLocStr(STRID_OFF),'',FALSE,4]
                    ],
                    gd_monovsens:=[SLIDE,{monovsens},getLocStr(STRID_MONOVSENS,{q}),0,0,100,0,8,'\d[3]',0, q ,TRUE]
                  ]]
                ]
              ],
              [SPACEV],
              [COLS,
                [BUTTON,{baseset},getLocStr(STRID_SET,{s}),0, s ],
                gd_basetx:=[TEXT,'c 60',getLocStr(STRID_BASE),TRUE,4],
                gd_finesl:=[SLIDE,{fine},getLocStr(STRID_FINE),0,-100,100,0,8,'',0,0],
                gd_finetx:=[NUM,0,'',FALSE,4]
              ]
            ]
          ],
          [BAR],
          [PLUGIN,{ledact},NEW leds.init(screen),FALSE,NIL]
        ],
        [ROWS,
          [COLS,
            gd_listtx:=[TEXT,getLocStr(STRID_INSTRUMENTS),'',FALSE,1],
            [SBUTTON,{clearsfx},getLocStr(STRID_CLEAR,{c}),0, c]
          ],
          gd_smplist:=[LISTV,{samplesel},0,10,2,smplist,0,0,0,-1,0,{applistvproc},FALSE],
          [EQCOLS,
            [SBUTTON,{addsfx},getLocStr(STRID_ADD,{a}),0, a],
            [SBUTTON,{remsfx},getLocStr(STRID_DELETE)]
          ]
        ]
      ]
    ],[EG_MENU,   [NM_TITLE, 0, getLocStr(STRID_MENUPROJECT), 0, 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_MENUNEW),getLocStr(STRID_MENUNEW,-1), 0, 0, {menu_new},
    NM_ITEM, 0, getLocStr(STRID_OPEN),getLocStr(STRID_OPEN,-1), 0, 0, {menu_open},
    NM_ITEM, 0, getLocStr(STRID_MERGE),getLocStr(STRID_MERGE,-1), 0, 0, {menu_addproject},
    NM_ITEM, 0, getLocStr(STRID_RELOADALL),getLocStr(STRID_RELOADALL,-1), 0, 0, {menu_reloadall},
    NM_ITEM, 0, getLocStr(STRID_SAVE),getLocStr(STRID_SAVE,-1), 0, 0, {menu_save},
    NM_ITEM, 0, getLocStr(STRID_SAVEAS),getLocStr(STRID_SAVEAS,-1), 0, 0, {menu_saveas},
    NM_ITEM, 0, getLocStr(STRID_SUMM),getLocStr(STRID_SUMM,-1), 0, 0, {menu_summary},
    NM_ITEM, 0, NM_BARLABEL, 0 , 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_HIDE),getLocStr(STRID_HIDE,-1), 0, 0, {hide},
    NM_ITEM, 0, getLocStr(STRID_ABOUT),getLocStr(STRID_ABOUT,-1), 0, 0, {menu_about},
    NM_ITEM, 0, getLocStr(STRID_QUIT), getLocStr(STRID_QUIT,-1), 0, 0, {closemain},
    NM_TITLE,0, getLocStr(STRID_EDIT), 0, 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_COPY), getLocStr(STRID_COPY,-1), 0, 0, {menu_copy},
    NM_ITEM, 0, getLocStr(STRID_PASTE),getLocStr(STRID_PASTE,-1), 0, 0, {menu_paste},
    NM_ITEM, 0, getLocStr(STRID_MENUDELETE),getLocStr(STRID_MENUDELETE,-1), 0, 0, {menu_delete},
    NM_ITEM, 0, NM_BARLABEL, 0 , 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_MENUSORT),0 , 0, 0, 0,
    NM_SUB, 0, getLocStr(STRID_MENUSORTPRI),0, 0, 0, {menu_sort_pri},
    NM_SUB, 0, getLocStr(STRID_MENUSORTMIDI),0, 0, 0, {menu_sort_midi},
    NM_SUB, 0, getLocStr(STRID_MENUSORTNAME),0, 0, 0, {menu_sort_name},
    NM_SUB, 0, getLocStr(STRID_MENUSORTRANGE),0, 0, 0, {menu_sort_range},
    NM_ITEM, 0, NM_BARLABEL, 0 , 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_MENUUNDO),getLocStr(STRID_MENUUNDO,-1), 0, 0, {menu_undo},
    NM_ITEM, 0, getLocStr(STRID_MENUREDO),getLocStr(STRID_MENUREDO,-1), 0, 0, {menu_redo},
    NM_TITLE, 0, getLocStr(STRID_SETTINGS), 0 , 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_AUDIOENABLE), getLocStr(STRID_AUDIOENABLE,-1),  CHECKIT OR MENUTOGGLE, 0, {menu_audio},
    NM_ITEM, 0, getLocStr(STRID_MENUMIDICTRL), getLocStr(STRID_MENUMIDICTRL,-1),  IF mcontrol THEN CHECKIT OR CHECKED OR MENUTOGGLE ELSE CHECKIT OR MENUTOGGLE, 0, {menu_mcontrol},
    NM_ITEM, 0, getLocStr(STRID_MENUFOLLOW), getLocStr(STRID_MENUFOLLOW,-1),  CHECKIT OR MENUTOGGLE, 0, {menu_followset},
    NM_ITEM, 0, getLocStr(STRID_ADVANCEDMIDI), getLocStr(STRID_ADVANCEDMIDI,-1), 0, 0, {menu_advancedmidi},
    NM_ITEM, 0, getLocStr(STRID_ADVANCEDAUDIO), getLocStr(STRID_ADVANCEDAUDIO,-1), 0, 0, {menu_advancedaudio},
    NM_ITEM, 0, NM_BARLABEL, 0 , 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_SAVESETTINGS), 0, 0, 0, {menu_settings},
    NM_ITEM, 0, getLocStr(STRID_MENUSETLAYOUT), getLocStr(STRID_MENUSETLAYOUT,-1),  CHECKIT OR MENUTOGGLE, 0, {menu_setlayout},
    NM_ITEM, 0, getLocStr(STRID_MENUSETWITHPROJECTS), getLocStr(STRID_MENUSETWITHPROJECTS,-1),  CHECKIT OR CHECKED OR MENUTOGGLE, 0, {menu_setwithprojects},
    NM_ITEM, 0, getLocStr(STRID_MENUSETSAVEICONS), getLocStr(STRID_MENUSETSAVEICONS,-1),  CHECKIT OR CHECKED OR MENUTOGGLE, 0, {menu_setsaveicons},
    NM_ITEM, 0, getLocStr(STRID_MENUSETSAVEUNDO), getLocStr(STRID_MENUSETSAVEUNDO,-1),  CHECKIT OR CHECKED OR MENUTOGGLE, 0, {menu_setsaveundo},
    NM_TITLE, 0, getLocStr(STRID_WINDOWS), 0 , 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_MENUMAIN), getLocStr(STRID_MENUMAIN,-1), 0, 0, {mainwindow},
    NM_ITEM, 0, getLocStr(STRID_MENUVOLUME), getLocStr(STRID_MENUVOLUME,-1), 0, 0, {volumewindow},
    NM_ITEM, 0, getLocStr(STRID_MENUENVELOPE), getLocStr(STRID_MENUENVELOPE,-1), 0, 0, {envelwindow},
    NM_ITEM, 0, getLocStr(STRID_SCOPE), getLocStr(STRID_SCOPE,-1), 0, 0, {scopewindowopen},
    NM_ITEM, 0, getLocStr(STRID_MENUMIDIMON), getLocStr(STRID_MENUMIDIMON,-1), 0, 0, {midimonitoropen},
    NM_END]:newmenu,
       EG_AWPROC, {appwindowproc}, -> app proc!!!
       EG_CLOSE,  {closemain},
       EG_CLEAN,  {cleanmain},
       EG_INFO,   999,
       EG_SCRN,screen,
       EG_TOP, mbprefs.mainwiny,
       EG_LEFT,mbprefs.mainwinx,
       EG_WIDTH,mbprefs.mainwinw,
       EG_HEIGHT,mbprefs.mainwinh,
       EG_FONT,ta,
        0,0])
    prjwindow:=gh.wnd
    SetWindowTitles(prjwindow,-1,mainbartext)
    menuptr:=gh.wnd.menustrip
ENDPROC
  
PROC cleanmain(info)
  gh:=0
ENDPROC

PROC closemain(info)
DEF qt=TRUE
  IF askquit THEN qt:=reqquit() ELSE IF audio_attrs([SFX_GET_AUDIO_STATUS,0,NIL]) THEN qt:=reqexit()
  IF qt
    quitgui()
  ENDIF
ENDPROC

PROC projectname()
DEF t
  IF EstrLen(prjname)=0
    t:=getLocStr(STRID_UNNAMED)
  ELSE
    t:=prjname
  ENDIF
  StringF(wintext,'\s \q\s\q',getLocStr(STRID_PROJECT),t)
  IF askquit THEN StrAdd(wintext,' *',2)
  IF gh.wnd THEN SetWindowTitles(gh.wnd,wintext,mainbartext)
ENDPROC

/*
  ==================================================================================
                            Main window GUI handlers
  ==================================================================================
*/
PROC applistvproc(info,awmsg:PTR TO appmessage)
DEF args:PTR TO wbarg,name[512]:ARRAY OF CHAR,i,test,snd:PTR TO sfx, loadbool=FALSE
  args:=awmsg.arglist
  blockallwindows()
  set_undo(UNDO_PREP_SAMPLELIST,smplist,bd)
  FOR i:=1 TO awmsg.numargs
    IF args.lock
      NameFromLock(args.lock,name,512)
      AddPart(name,args.name,512)
      setlistvlabels(gh,gd_smplist,-1)
      IF loadinstrumentsfrombank(name,smplist)
        test:=TRUE
      ELSE
        printstatus(getLocStr(STRID_ADDINGINSTRUMENTS),FilePart(name),0,i-1,awmsg.numargs)
        snd,test:=addsnd(name,smplist)
      ENDIF
      IF test THEN loadbool:=TRUE
      setlistvlabels(gh,gd_smplist,smplist)
    ENDIF
    args++
  ENDFOR
  IF loadbool
    IF remlist THEN remsfx(info)
    set_undo(UNDO_SET_SAMPLELIST,smplist,bd); update_redoundo()
    checkaskquit()
  ENDIF
  addingsamplesover()
  unblockallwindows()
  IF dest THEN FlushMDest(dest)   -> flush midi for any case:)
ENDPROC

PROC appwindowproc(info,awmsg:PTR TO appmessage) HANDLE
DEF args:PTR TO wbarg,name[512]:ARRAY OF CHAR,ans=TRUE
  IF askquit THEN ans:=reqquit()
  blockallwindows()
  IF ans
    args:=awmsg.arglist
    IF args.lock
      NameFromLock(args.lock,name,512)
      AddPart(name,args.name,512)
      setlistvlabels(gh,gd_smplist,-1)
      StrCopy(prjname,'')
      askquit:=FALSE; flush_undo()
      loadproject(name,smplist,bd)
      StrCopy(prjname,args.name,ALL)
    ENDIF
  ENDIF
EXCEPT DO
  unblockallwindows()
  sortbanks()
  updategh()
  setlistvlabels(gh,gd_smplist,smplist)
  IF exception THEN report_exception() ELSE FlushMDest(dest)   -> flush midi for any case:)
ENDPROC

EXPORT PROC plugact(info,mp:PTR TO pianokeys)
DEF a,b,l,h,nb:PTR TO bank,keybcode

  IF (keybcode:=mp.keycode) = -1
    IF basesetb
      nb:=bd[nbank]
      basesetb:=FALSE
      a:=pianokeypressed(mp)
      IF rangesetb
        set_undo(UNDO_SET_BOUNDS,bd,nbank); update_redoundo()
        b:=a-nb.base;  l:=nb.lobound+b;  h:=nb.hibound+b
        IF h > 127; b:=h-127;  l:=l-b;  h:=h-b;  a:=a-b; ENDIF
        IF l < 0;   b:=-l;  l:=l+b;  h:=h+b;  a:=a+b;    ENDIF
        nb.lobound:=l; nb.hibound:=h
        mp.boundset(TRUE)
        mp.bounds(l,h)
      ENDIF
      set_undo(UNDO_SET_BASE,bd,nbank); update_redoundo()
      nb.base:=a
      settext(gh,gd_basetx,midinote(basestr,a))
      checkaskquit()
    ELSEIF rangesetb
      a,b:=mp.bounds()
      IF a>=0
        set_undo(UNDO_SET_BOUNDS,bd,nbank); update_redoundo()
        bd[nbank].lobound:=a
        bd[nbank].hibound:=b
        checkaskquit()
      ENDIF
    ELSE
      a:=pianokeypressed(mp)
      IF mcontrol=0 THEN signal_playtask(PSG_PLAY,bd[nbank],a)
    ENDIF
  ELSEIF keybcode >= 0
    keybact(keybcode,WINDOW_MAIN)
  ELSEIF keybcode = -2
    IF basesetb THEN baseset(0)
  ENDIF

  mp.keycode:=-1
ENDPROC

PROC keybact(keybcode,winno)
DEF a:REG, b:REG, shift, l,h
  IF keybcode AND MYRAWCODE
    shift:=IF keybcode AND SHIFTQUAL THEN TRUE ELSE FALSE
    keybcode:=keybcode AND $FF
    leds.getrange({l},{h})
    a:=0; b:=0
    SELECT keybcode
      CASE CURSORLEFT
        IF shift; IF h > nbank THEN DEC h ELSE IF l>0 THEN DEC l; leds.setrange(l,h)
        ELSE; a:=nbank; IF a < 1 THEN a:=NUMBANKS; ENDIF
      CASE CURSORRIGHT
        IF shift; IF l < nbank THEN INC l ELSE IF h<(NUMBANKS-1) THEN INC h; leds.setrange(l,h)
        ELSE; a:=nbank+2; IF a > NUMBANKS THEN a:=1; ENDIF
      CASE CURSORUP
        IF shift; IF h > nbank THEN h:=h-10 ELSE l:=l-10
        IF h<nbank THEN h:=nbank; IF l<0 THEN l:=0; leds.setrange(l,h)
        ELSE; a:=nbank-9; IF a < 1 THEN a:=a+NUMBANKS; ENDIF
      CASE CURSORDOWN
        IF shift; IF l < nbank THEN l:=l+10 ELSE h:=h+10
        IF l>nbank THEN l:=nbank; IF h>=NUMBANKS THEN h:=NUMBANKS-1; leds.setrange(l,h)
        ELSE; a:=nbank+11; IF a > NUMBANKS THEN a:=a-NUMBANKS; ENDIF
      DEFAULT
        -> WriteF('F\d\n',keybcode-$4F) ->F1 - F10
    ENDSELECT
    IF a ; leds.setcurrent(a-1); banksel(0,a-1); ENDIF
  ELSE
    SELECT 128 OF keybcode
      CASE ESC_CODE
        SELECT winno
          CASE 1; close_volwin(0)
          CASE 2; close_envwin(0)
        ENDSELECT
      CASE "0" TO "9"
        a:=nbank/10; b:=a*10
        a:=((keybcode-"0") AND $F); IF a = 0 THEN a:=10
        a:=a+b
        leds.setcurrent(a-1); banksel(0,a-1)
      CASE "."
        a:=nbank+11; IF a > NUMBANKS THEN a:=a-NUMBANKS
        leds.setcurrent(a-1); banksel(0,a-1)
      CASE ")"
        b:=bd[nbank].pri+1; IF b > NUMBANKS THEN b:=NUMBANKS
        setslide(gh,gd_bankprisl,b); setpri(0,b)
      CASE "("
        b:=bd[nbank].pri-1; IF b < 1 THEN b:=1
        setslide(gh,gd_bankprisl,b); setpri(0,b)
      CASE "-" , "_"
        b:=bd[nbank].fine-1-FINE_CENTR; IF b < -100 THEN b:=-100
        setslide(gh,gd_finesl,b); fine(0,b)
      CASE "+" , "="
        b:=bd[nbank].fine+1-FINE_CENTR; IF b > 100 THEN b:=100
        setslide(gh,gd_finesl,b); fine(0,b)
      CASE "*"
        setslide(gh,gd_finesl,0); fine(0,0)
      CASE 9
        IF winno=WINDOW_MAIN
          IF volgh.wnd
            volumewindow(0)
          ELSEIF envgh.wnd
            envelwindow(0)
          ENDIF
        ELSEIF winno=WINDOW_VOLUME
          IF envgh.wnd
            envelwindow(0)
          ELSE
            mainwindow(0)
          ENDIF
        ELSEIF winno=WINDOW_ENVELOPE
          mainwindow(0)
        ENDIF
      CASE 13
        pianokeypressed(mp,a:=bd[nbank].base)
        IF mcontrol=0 THEN signal_playtask(PSG_PLAY,bd[nbank],a)
      CASE 32
        IF mcontrol=0 THEN signal_playtask(PSG_PLAY,bd[nbank],255)
      CASE DEL_CODE
        free(0)
      DEFAULT
      ->WriteF('key:\d == \q\c\q\n',keybcode,keybcode)
    ENDSELECT
  ENDIF
ENDPROC


PROC audioset(info,aud) HANDLE
DEF item:PTR TO menuitem,f
  item:=ItemAddress(menuptr, FULLMENUNUM(2,0,0))
  IF rangesetb=0 THEN leds.setactive()
  f:=item.flags AND Not(CHECKED)
  IF aud
    IF audio_attrs([SFX_SET_AUDIO_STATUS,TRUE,NIL])=FALSE
      Raise("AUDB")
    ELSE
      item.flags:=f OR CHECKED
    ENDIF
  ELSE
    audio_attrs([SFX_SET_AUDIO_STATUS,FALSE,NIL])
    item.flags:=f
  ENDIF
EXCEPT
  setcheck(gh,gd_audiock,FALSE)
  report_exception()
ENDPROC

PROC followset(info,fol)
DEF item:PTR TO menuitem,f
  item:=ItemAddress(menuptr, FULLMENUNUM(2,2,0))
  followb:=IF fol THEN TRUE ELSE FALSE
  f:=item.flags AND Not(CHECKED)
  item.flags:=f OR (CHECKED AND followb)
ENDPROC

EXPORT PROC follow(note,midi)
DEF i:REG,n:REG,p:REG,bn:PTR TO bank
  p:= -1
  FOR i:=0 TO NUMBANKS-1
    bn:=bd[i]
    IF bn.instr AND (bn.midi=midi)
      n:=note
      IF (n >= bn.lobound) AND (n <= bn.hibound)
        IF p= -1 THEN IF i < nbank THEN p:=i
        IF i > nbank ; p:=i; JUMP followover; ENDIF
      ENDIF
    ENDIF
  ENDFOR
followover:
  IF p > -1 ; leds.setcurrent(p); banksel(0,p); ENDIF
ENDPROC

PROC rangeset(info,sel)
DEF bn:PTR TO bank
  bn:=bd[nbank]
  mareaset:=255
  IF rangesetb:=sel
    updaterange(bn.lobound,bn.hibound)
    updateleds()
    IF basesetb=FALSE THEN mp.boundset(TRUE)
  ELSE
    checkplayingbanks()
    mp.bounds(255)
    -> mp.boundset(FALSE) automaticaly performed by ^^^
  ENDIF
ENDPROC

PROC midiset(info,sel)
DEF item:PTR TO menuitem,f:REG
  mcontrol:=sel
  item:=ItemAddress(menuptr, FULLMENUNUM(2,1,0))
  f:=item.flags AND Not(CHECKED)
  IF mcontrol THEN item.flags:=f OR CHECKED ELSE item.flags:=f
  clearcontrollers()
ENDPROC

PROC ledact(info,leds:PTR TO leds)
DEF i,f,b,c,l=0,xchg[NUMBANKS]:ARRAY OF INT, xpt:PTR TO CHAR
  xpt:=xchg
  IF leds.act=LEDACT_SETBANK
    banksel(info,leds.setcurrent())
  ELSEIF leds.act=LEDACT_SETRANGE
    leds.getrange({rangelo},{rangehi})
->    WriteF('lo:\d hi:\d\n',rangelo,rangehi)
  ELSEIF leds.act=LEDACT_MOVERANGE
    f:=leds.getrange()
    IF f > rangelo
      i:=rangehi-rangelo; b:=0; c:= -1
    ELSE
      i:=0; b:=rangehi-rangelo; c:= 1
    ENDIF
    WHILE ((c>0) AND (i<=b)) OR ((c<0) AND (i>=b))
      xpt[l]:=rangelo+i; xpt[l+1]:=f+i
      xchgbanksachn(bd[xpt[l++]],bd[xpt[l++]])
      i:=i+c
    ENDWHILE
    set_undo(UNDO_SET_XCHGBANKS,xchg,l)
    updategh()
  ENDIF
ENDPROC

PROC banksel(info,bank)
DEF snd:PTR TO sfx,bn:PTR TO bank,v:REG,bno:PTR TO bank
  bno:=bd[nbank]
  nbank:=bank; rangelo:=nbank; rangehi:=nbank
  bn:=bd[nbank]
  IF snd:=bn.instr
    StrCopy(instrumenttext,IF snd.stereo() THEN '= ' ELSE '- ',2)
    StrAdd(instrumenttext,snd.ln.name)
    settext(gh,gd_instrtext,instrumenttext)
  ELSE
    settext(gh,gd_instrtext,'')
  ENDIF
  IF (v:=bn.midi)<>bno.midi
    setslide(gh,gd_midichansl,v)
  ENDIF
  IF (v:=bn.pri)<>bno.pri
    setslide(gh,gd_bankprisl,v)
  ENDIF
  IF (v:=bn.set AND (B_DUR_ON OR B_DRUM)) <> (bno.set AND (B_DUR_ON OR B_DRUM))
    IF v THEN v:=IF v AND B_DRUM THEN 2 ELSE 1
    setmx(gh,gd_durmx,v)
    setdisabled(gh,gd_loopchk,IF v=2 THEN TRUE ELSE FALSE)
  ENDIF
  IF (v:=bn.group)<>bno.group THEN setslide(gh,gd_groupsl,v)
  IF (v:=bn.set AND B_LOOP)<>(bno.set AND B_LOOP ) THEN setcheck(gh,gd_loopchk,IF v THEN TRUE ELSE FALSE)
  IF (v:=bn.set AND B_MONO)<>(bno.set AND B_MONO ) THEN setcheck(gh,gd_monochk,IF v THEN TRUE ELSE FALSE)
  v:=IF (v=0) OR (bn.set AND B_DRUM) THEN TRUE ELSE FALSE
  setdisabled(gh,gd_monoslide,v); setdisabled(gh,gd_monovsens,IF v OR (bn.monoslide=0) THEN TRUE ELSE FALSE)
  IF (v:=bn.monovsens)<>bno.monovsens THEN setslide(gh,gd_monovsens,v)
  IF (v:=bn.monoslide)<>bno.monoslide
    setslide(gh,gd_monoslide,v:=bn.monoslide)
    IF v THEN StringF(monosliderstr,'\d.\z\d[2] ',v/50,Mod(v,50)*2) ELSE StrCopy(monosliderstr,getLocStr(STRID_OFF))
    settext(gh,gd_monosltxt,monosliderstr)
  ENDIF
  v:=bn.base;  IF rangesetb OR basesetb OR (followb=0) THEN pianokeypressed(mp,v)
  IF v<>bno.base
    settext(gh,gd_basetx,midinote(basestr,v))
  ENDIF
  IF (v:=bn.fine)<>bno.fine; setslide(gh,gd_finesl,v-FINE_CENTR); setnum(gh,gd_finetx,v-FINE_CENTR); ENDIF
  update_volgh()
  update_envgh()
  IF rangesetb
    updateleds()
    updaterange(bn.lobound,bn.hibound)
  ELSE
    checkplayingbanks()
  ENDIF
ENDPROC

PROC midichan(info,chan)
  set_undo(UNDO_SET_MIDI,bd,nbank); update_redoundo()
  bd[nbank].midi:=chan
  IF rangesetb 
    updateleds()
    updaterange(bd[nbank].lobound,bd[nbank].hibound)
  ENDIF
  checkaskquit()
ENDPROC

PROC setpri(info,bankpri)
  set_undo(UNDO_SET_PRI,bd,nbank); update_redoundo()
  bd[nbank].pri:=bankpri
  sortbanks()
  IF rangesetb
    updateleds()
    updaterange(bd[nbank].lobound,bd[nbank].hibound)
  ENDIF
  checkaskquit()
ENDPROC

PROC dur(info,duration)
DEF f:REG,disable=FALSE
  set_undo(UNDO_SET_SET_DUR,bd,nbank); update_redoundo()
  f:=bd[nbank].set AND Not(B_DRUM OR B_DUR_ON)
  SELECT duration
    CASE 1
      f:=f OR B_DUR_ON
    CASE 2
      f:=f OR B_DRUM; disable:=TRUE
  ENDSELECT
  bd[nbank].set:=f
  setdisabled(gh,gd_loopchk,disable)
  disable:=IF disable OR ((bd[nbank].set AND B_MONO)=0) THEN TRUE ELSE FALSE
  setdisabled(gh,gd_monoslide,disable); setdisabled(gh,gd_monovsens,IF disable OR (bd[nbank].monoslide=0) THEN TRUE ELSE FALSE)
  checkaskquit()
ENDPROC

PROC setgroup(info,group)
  set_undo(UNDO_SET_GROUP,bd,nbank); update_redoundo()
  bd[nbank].group:=group
  checkaskquit()
ENDPROC

PROC monophonic(info,mono)
DEF f
  set_undo(UNDO_SET_SET_MONO,bd,nbank); update_redoundo()
  f:=bd[nbank].set
  bd[nbank].set:=(IF mono THEN f OR B_MONO ELSE f AND Not(B_MONO))
  f:=IF (mono=FALSE) OR (bd[nbank].set AND B_DRUM) THEN TRUE ELSE FALSE
  setdisabled(gh,gd_monoslide,f); setdisabled(gh,gd_monovsens,IF f OR (bd[nbank].monoslide=0) THEN TRUE ELSE FALSE)
  checkaskquit()
ENDPROC

PROC monoslide(info,set)
  set_undo(UNDO_SET_MONOSLIDE,bd,nbank); update_redoundo()
  bd[nbank].monoslide:=set
  IF set THEN StringF(monosliderstr,'\d.\z\d[2]',set/50,Mod(set,50)*2) ELSE StrCopy(monosliderstr,getLocStr(STRID_OFF))
  settext(gh,gd_monosltxt,monosliderstr)
  setdisabled(gh,gd_monovsens,IF set THEN FALSE ELSE TRUE)
  checkaskquit()
ENDPROC

PROC monovsens(info,set)
  set_undo(UNDO_SET_MONOVSENS,bd,nbank); update_redoundo()
  bd[nbank].monovsens:=set
  checkaskquit()
ENDPROC

PROC loop(info,loop)
DEF f
  set_undo(UNDO_SET_SET_LOOP,bd,nbank); update_redoundo()
  f:=bd[nbank].set
  bd[nbank].set:=(IF loop THEN f OR B_LOOP ELSE f AND Not(B_LOOP))
  checkaskquit()
ENDPROC

PROC baseset(info)
  pianokeypressed(mp,bd[nbank].base) -> force to draw!
  mp.boundset(FALSE)
  mareaset:=255
  basesetb:=Not(basesetb)
  IF basesetb=0
    IF rangesetb THEN mp.boundset(TRUE)
  ENDIF
ENDPROC

PROC fine(info,fine)
  set_undo(UNDO_SET_FINE,bd,nbank); update_redoundo()
  bd[nbank].fine:=fine+FINE_CENTR
  setnum(gh,gd_finetx,fine)
  checkaskquit()
  signal_playtask(PSG_TUNE,bd[nbank])
ENDPROC

PROC samplesel(info,num) HANDLE
DEF ln:PTR TO lln,i,snd:PTR TO sfx, oldseconds,oldmicros
  i:=num; ln:=smplist; WHILE ln:=ln.ln.succ; DEC i; EXIT i<0; ENDWHILE
  INC num
  IF ln=0 ; dblistnum:=0; RETURN; ENDIF
  oldseconds:=seconds; oldmicros:=micros
  CurrentTime({seconds},{micros}) -> get time for comparision
  IF remlist
    IF (num=dblistnum) AND DoubleClick(oldseconds,oldmicros,seconds,micros)
      setlistvlabels(gh,gd_smplist,-1)
      set_undo(UNDO_SET_SAMPLEDELETE,ln.pointer,bd)
      delsnd(ln.pointer, bd)
      checkaskquit()
      setlistvlabels(gh,gd_smplist,smplist)
      IF smplist.head.succ=0 THEN remsfx(info)
      updategh()
      seconds:=0; micros:=0
    ENDIF
  ELSEIF ((snd:=bd[nbank].instr)=0) OR ((num=dblistnum) AND DoubleClick(oldseconds,oldmicros,seconds,micros))
    seconds:=0; micros:=0
    blockallwindows()
    set_undo(UNDO_SET_INSTR,bd,nbank)
    setinstr(bd[nbank], ln.pointer)
    updategh()
    checkaskquit()
    addingsamplesover()
    unblockallwindows()
  ENDIF
  dblistnum:=num
EXCEPT
  unblockallwindows()
  report_exception()
ENDPROC

PROC free(info)
  IF bd[nbank].instr <> NIL
    set_undo(UNDO_SET_INSTR,bd,nbank)
    clearinstr(bd[nbank])
    updategh()
    checkaskquit()
  ENDIF
ENDPROC

PROC reload(info)
DEF instr[NUMBANKS]:ARRAY OF LONG,
    f,snd
  IF snd:=bd[nbank].instr
    blockallwindows()
    FOR f:=0 TO NUMBANKS-1 DO instr[f]:=IF bd[f].instr=snd THEN clearinstr(bd[f]) ELSE NIL
    FOR f:=0 TO NUMBANKS-1 DO IF instr[f]=snd THEN setinstr(bd[f],snd)
    unblockallwindows()
    updategh()
    addingsamplesover()
  ENDIF
ENDPROC

PROC addsfx(info) HANDLE
DEF req=0:PTR TO rtfilerequester,name[260]:ARRAY OF CHAR,file[109]:ARRAY OF CHAR,
    fentry:PTR TO rtfilelist,flist=0,
    snd:PTR TO sfx, test, loadbool=FALSE,i=0,total=0

  blockallwindows()
  set_undo(UNDO_PREP_SAMPLELIST,smplist,bd)
  IF req:=RtAllocRequestA(0,0)
    file[]:=0; name[]:=0
    RtChangeReqAttrA(req,[RTFI_DIR,sndpath,NIL])
    IF flist:=RtFileRequestA(req,file,getLocStr(STRID_ADDSAMPLES),[
              RT_WINDOW,gh.wnd,
              RTFI_FLAGS,FREQF_MULTISELECT OR FREQF_PATGAD,0])
      StrCopy(sndpath,req.dir,ALL)
      IF fentry:=flist
        total:=1;  WHILE fentry:=fentry.next DO INC total
        fentry:=flist
      ENDIF
      WHILE fentry
        AstrCopy(name,sndpath,260)     -> directory path to <name>
        AddPart(name,fentry.name,260)  -> add filename to <name> (dos 36+)
        setlistvlabels(gh,gd_smplist,-1)
        IF loadinstrumentsfrombank(name,smplist)
          test:=TRUE
        ELSE
          printstatus(getLocStr(STRID_ADDINGINSTRUMENTS),FilePart(name),0,i++,total)
          snd,test:=addsnd(name,smplist)
        ENDIF
        IF test THEN loadbool:=TRUE
        setlistvlabels(gh,gd_smplist,smplist)
        fentry:=fentry.next
      ENDWHILE
    ENDIF
    IF dest THEN FlushMDest(dest)   -> flush midi dest lockwin was too long
  ENDIF
EXCEPT DO
  IF loadbool
    IF remlist THEN remsfx(info)
    set_undo(UNDO_SET_SAMPLELIST,smplist,bd); update_redoundo()
    checkaskquit()
  ENDIF
  addingsamplesover()
  unblockallwindows()
  IF flist THEN RtFreeFileList(flist)
  IF req THEN RtFreeRequest(req)
  IF exception THEN report_exception()
ENDPROC

PROC remsfx(info)
  IF remlist
    remlist:=FALSE;    settext(gh,gd_listtx,getLocStr(STRID_INSTRUMENTS))
  ELSE
    IF smplist.head.succ
      remlist:=TRUE
      settext(gh,gd_listtx,getLocStr(STRID_DELETELIST))
    ENDIF
  ENDIF
ENDPROC

PROC clearsfx(info)
DEF x,ln:PTR TO ln
  ln:=smplist.head
  IF ln.succ
    IF x:=reqclear()
      set_undo(IF x=1 THEN UNDO_PREP_SAMPLELIST ELSE UNDO_PREP_SAMPLELISTINSTR,smplist,bd)
      setlistvlabels(gh,gd_smplist,-1)
      IF clearsmplist(bd,smplist, IF x=1 THEN TRUE ELSE FALSE)
        checkaskquit()
        set_undo(IF x=1 THEN UNDO_SET_SAMPLELIST ELSE UNDO_SET_SAMPLELISTINSTR,smplist,bd)
      ENDIF
      setlistvlabels(gh,gd_smplist,smplist)
      updategh()
    ENDIF
  ENDIF
  IF remlist THEN remsfx(info)
ENDPROC

/*
  ==================================================================================
  ----------------------------------------------------------------------------------
                                [Volume window]
  ----------------------------------------------------------------------------------
  ==================================================================================
*/
PROC update_volgh()
DEF bn:PTR TO bank,a,b,n,snd:PTR TO sfx
  bn:=bd[nbank]
  setnum(volgh,vgh_banknum,nbank+1)
  setnum(volgh,vgh_freq,IF snd:=bn.instr THEN snd.basefreq() ELSE 0)
  IF snd
    n:=Div(Mul(snd.frames(),1000),snd.basefreq())
    b:=n; a:=b/1000; b:=b-(a*1000)
    fullnameptr:=snd.pathname()
  ELSE
    a:=0; b:=0
    fullnameptr:=''
  ENDIF
  StringF(timetext,'\d.\d s',a,b)
  settext(volgh,vgh_time,timetext)
  settext(volgh,vgh_fullname,fullnameptr)
  setslide(volgh,vgh_volum,Div(Mul(100,bn.volume)+128,256))
  setslide(volgh,vgh_veloc,bn.velsens)
  setslide(volgh,vgh_after,bn.aftersens)
  setcheck(volgh,vgh_subaft,IF (bn.set AND B_ADDAFTERT)=0 THEN TRUE ELSE FALSE)
  setslide(volgh,vgh_panor,bn.panorama-128); setnum(volgh,vgh_pantx,bn.panorama-128)
  setslide(volgh,vgh_pwide,bn.panwide)
  setslide(volgh,vgh_pitch,bn.pitchsens)
  setslide(volgh,vgh_firstskip,bn.firstskip)
  setcheck(volgh,vgh_mctvol,bn.mctrlvol)
  setcheck(volgh,vgh_mctpan,bn.mctrlpan)
  a:=IF snd THEN n ELSE bn.firstskip
  setnum(volgh,vgh_skipnum,IF bn.firstskip < a THEN bn.firstskip ELSE a)
ENDPROC


PROC volumewindow(info)
  IF volgh.wnd
    ActivateWindow(volgh.wnd)
    WindowToFront(volgh.wnd)
  ELSE
    settext(volgh,vgh_fullname,'')
    openwin(volgh)
    settext(volgh,vgh_fullname,fullnameptr)
    IF volwindow:=volgh.wnd THEN SetWindowTitles(volwindow,-1,mainbartext)
  ENDIF
ENDPROC

PROC close_volwin(info)
  IF volgh.wnd
    volwindow:=0; closewin(volgh)
  ENDIF
ENDPROC

PROC clean_volwin(info)
  volgh:=0; volwindow:=0
ENDPROC

PROC open_volwin(screen,ta)
DEF v,l,p,w,c,r,s,z,x,y,q,a,b,d
  volgh:=addmultiA(mh,getLocStr(STRID_VOLUMECTRLWIN),
  [ROWS,
    [BEVEL,
      [ROWS,
        [COLS,
          vgh_banknum:=[NUM,1,getLocStr(STRID_BANKNUM),TRUE,3],
          vgh_freq:=[NUM,0,getLocStr(STRID_FREQNUM),TRUE,7],
          vgh_time:=[TEXT,'',getLocStr(STRID_TIMENUM),TRUE,5]
        ],
        vgh_fullname:=[TEXT,'',NIL,TRUE,15],
        [BEVELR,[BEVEL,
          [COLS,
            vgh_firstskip:=[SLIDE,{skip_slide},getLocStr(STRID_FIRSTSKIP,{s}),0,0,3000,0,6,'',0, s ],
            vgh_skipnum:=[NUM,0,NIL,TRUE,4]
          ]
        ]],
        [BEVELR,
          [ROWS,
            [COLS,
              [BUTTON,{vol_center},getLocStr(STRID_HUNPERCENT,{z}),0, z ],
              vgh_volum:=[SLIDE,{vol_slide},getLocStr(STRID_VOLUMESET,{v}),0,0,200,100,6,'\d[3]',0, v ],
              [BUTTON,{vol_maxvol},getLocStr(STRID_MAXVOLUME,{x}),0, x ]
            ],
            [ROWS,
              [COLS,
                vgh_veloc:=[SLIDE,{vel_slide},getLocStr(STRID_VELOCITY,{l}),0,0,100,0,6,'\d[3]',0, l ],
                [BUTTON,{after_vel},getLocStr(STRID_AFTERVEL,{q}),0, q ]
              ],
              [COLS,
                vgh_after:=[SLIDE,{after_slide},getLocStr(STRID_AFTERTOUCH,{y}),0,0,100,0,6,'\d[3]',0, y ],
                vgh_subaft:=[CHECK,{after_sub},getLocStr(STRID_SUBAFTERT,{d}),FALSE,TRUE,0, d ]
              ]
            ],
            [COLS,
            vgh_panor:=[SLIDE,{pan_slide},getLocStr(STRID_PANORAMA,{p}),0,-128,128,0,6,'',0, p ],
            vgh_pantx:=[NUM,0,'',FALSE,4]
            ],
            [COLS,
              vgh_pwide:=[SLIDE,{wid_slide},getLocStr(STRID_WIDE,{w}),0,0,30,0,6,'\d[2]',0, w ],
              [BUTTON,{pan_center},getLocStr(STRID_CENTER,{c}),0, c ]
            ],
            [EQCOLS,
              vgh_mctvol:=[CHECK,{mctvol_set},getLocStr(STRID_MCTRLVOL,{a}),FALSE,TRUE,0, a ],
              vgh_mctpan:=[CHECK,{mctpan_set},getLocStr(STRID_MCTRLPAN,{b}),FALSE,TRUE,0, b ]
            ]
          ]
        ],
        [PLUGIN, {title_act}, NEW title_a.setup(getLocStr(STRID_PITCHBENDER), TITLE_NORMAL) ,FALSE,NIL],
        vgh_pitch:=[SLIDE,{ben_slide},getLocStr(STRID_NOTERANGE,{r}),0,0,12,0,6,'\d[2]',0, r ]
      ]
    ]
  ],[EG_MENU,  boommenu(),
     EG_CLOSE,{close_volwin},
     EG_CLEAN,{clean_volwin},
     EG_HIDE, mbprefs.volumehide,
     EG_SCRN,screen,
     EG_TOP, mbprefs.volumewiny,
     EG_LEFT,mbprefs.volumewinx,
     EG_WIDTH,mbprefs.volumewinw,
     EG_HEIGHT,mbprefs.volumewinh,
     EG_FONT,ta,
      0,0])

    update_volgh()
    IF volwindow:=volgh.wnd THEN SetWindowTitles(volwindow,-1,mainbartext)

ENDPROC

PROC title_act(info,tl:PTR TO title_keys)
  IF tl.keycode>=0 THEN keybact(tl.keycode,WINDOW_VOLUME)
ENDPROC

PROC ben_slide(info,ben)
  set_undo(UNDO_SET_PITCHSENS,bd,nbank); update_redoundo()
  bd[nbank].pitchsens:=ben
  checkaskquit()
ENDPROC
PROC vol_slide(info,vol)
  set_undo(UNDO_SET_VOLUME,bd,nbank); update_redoundo()
  bd[nbank].volume:=Div(Mul(256,vol),100)
  checkaskquit()
  signal_playtask(PSG_VOLUME,bd[nbank])
ENDPROC
PROC vol_center(info)
  IF bd[nbank].volume<>256
    set_undo(UNDO_SET_VOLUME,bd,nbank); update_redoundo()
    bd[nbank].volume:=256
    checkaskquit()
    update_volgh()
    signal_playtask(PSG_VOLUME,bd[nbank])
  ENDIF
ENDPROC
PROC vol_maxvol(info)
DEF snd:PTR TO sfx,v
  v:=IF snd:=bd[nbank].instr THEN snd.maxvolume() ELSE 512
  IF bd[nbank].volume <> v
    set_undo(UNDO_SET_VOLUME,bd,nbank); update_redoundo()
    bd[nbank].volume:=v
    checkaskquit()
    update_volgh()
    signal_playtask(PSG_VOLUME,bd[nbank])
  ENDIF
ENDPROC
PROC vel_slide(info,vel)
  set_undo(UNDO_SET_VELSENS,bd,nbank); update_redoundo()
  bd[nbank].velsens:=vel
  checkaskquit()
  signal_playtask(PSG_VOLUME,bd[nbank])
ENDPROC
PROC after_slide(info,after)
  set_undo(UNDO_SET_AFTERSENS,bd,nbank); update_redoundo()
  bd[nbank].aftersens:=after
  checkaskquit()
  signal_playtask(PSG_VOLUME,bd[nbank])
ENDPROC
PROC after_sub(info, bool)
DEF b:REG
  set_undo(UNDO_SET_SET_ADDAFTERT,bd,nbank); update_redoundo()
  b:=bd[nbank].set
  bd[nbank].set:=IF bool THEN b AND Not(B_ADDAFTERT) ELSE b OR B_ADDAFTERT
  checkaskquit()
  signal_playtask(PSG_VOLUME,bd[nbank])
ENDPROC
PROC after_vel(info)
  IF bd[nbank].aftersens <> bd[nbank].velsens
    set_undo(UNDO_SET_AFTERSENS,bd,nbank); update_redoundo()
    bd[nbank].aftersens:=bd[nbank].velsens
    checkaskquit()
    update_volgh()
    signal_playtask(PSG_VOLUME,bd[nbank])
  ENDIF
ENDPROC
PROC pan_slide(info,pan)
  setnum(volgh,vgh_pantx,pan)
  set_undo(UNDO_SET_PANORAMA,bd,nbank); update_redoundo()
  bd[nbank].panorama:=pan+128
  checkaskquit()
  signal_playtask(PSG_VOLUME,bd[nbank])
ENDPROC
PROC pan_center(info)
  IF bd[nbank].panorama <> 128
    set_undo(UNDO_SET_PANORAMA,bd,nbank); update_redoundo()
    bd[nbank].panorama:=128
    checkaskquit()
    update_volgh()
    signal_playtask(PSG_VOLUME,bd[nbank])
  ENDIF
ENDPROC
PROC wid_slide(info,pwide)
  set_undo(UNDO_SET_PANWIDE,bd,nbank); update_redoundo()
  bd[nbank].panwide:=pwide
  checkaskquit()
  signal_playtask(PSG_VOLUME,bd[nbank])
ENDPROC
PROC skip_slide(info,skip)
DEF snd:PTR TO sfx,n
  set_undo(UNDO_SET_FIRSTSKIP,bd,nbank); update_redoundo()
  bd[nbank].firstskip:=skip
  checkaskquit()
  n:=IF snd:=bd[nbank].instr THEN Div(Mul(snd.frames(),1000),snd.basefreq()) ELSE skip
  setnum(volgh,vgh_skipnum,IF skip < n THEN skip ELSE n)
ENDPROC
PROC mctvol_set(info,set)
  set_undo(UNDO_SET_MCTRLVOL,bd,nbank); update_redoundo()
  bd[nbank].mctrlvol:=IF set THEN 100 ELSE 0
  checkaskquit()
  signal_playtask(PSG_VOLUME,bd[nbank])
ENDPROC
PROC mctpan_set(info,set)
  set_undo(UNDO_SET_MCTRLPAN,bd,nbank); update_redoundo()
  bd[nbank].mctrlpan:=IF set THEN 100 ELSE 0
  checkaskquit()
  signal_playtask(PSG_VOLUME,bd[nbank])
ENDPROC
/*
  ==================================================================================
  ----------------------------------------------------------------------------------
                               [Envelope window]
  ----------------------------------------------------------------------------------
  ==================================================================================
*/
PROC update_envgh()
DEF bn:PTR TO bank
  bn:=bd[nbank]
  setnum(envgh,egh_number,nbank+1)
  envp.setenvelope(bn.attack,bn.decay,bn.sustainlev,bn.release)
ENDPROC

PROC open_envwin(screen,ta)

  envgh:=addmultiA(mh,getLocStr(STRID_VOLUMEENVELWIN),
  [BEVEL,
   [ROWS,
    [EQCOLS,
      [TEXT,'','',FALSE,1],
      egh_number:=[NUM,0,getLocStr(STRID_BANKNUM),TRUE,3],
      [TEXT,'','',FALSE,1]
    ],
    [BEVELR,[BEVEL,
      egh_envel:=[PLUGIN,{envel_act}, NEW envp.init(screen), FALSE, 0]
    ]]
   ]
  ],[EG_MENU,  boommenu(),
     EG_CLOSE,{close_envwin},
     EG_CLEAN,{clean_envwin},
     EG_HIDE, mbprefs.envelhide,
     EG_SCRN,screen,
     EG_TOP, mbprefs.envelwiny,
     EG_LEFT,mbprefs.envelwinx,
     EG_WIDTH,mbprefs.envelwinw,
     EG_HEIGHT,mbprefs.envelwinh,
     EG_FONT,ta,
      0,0])

    update_envgh()
    IF envwindow:=envgh.wnd THEN SetWindowTitles(envwindow,-1,mainbartext)

ENDPROC

PROC envel_act(info,envp:PTR TO envel_plugin)
DEF attack,decay,sustain,release, nb:PTR TO bank, change=FALSE, keybcode
  nb:=bd[nbank]
  IF (keybcode:=envp.keycode) = -1
    envp.getenvelope({attack},{decay},{sustain},{release})
    IF nb.attack <> attack; set_undo(UNDO_SET_ATTACK,bd,nbank); nb.attack:=attack; change:=TRUE; ENDIF
    IF nb.decay <> decay; set_undo(UNDO_SET_DECAY,bd,nbank); nb.decay:=decay; change:=TRUE; ENDIF
    IF nb.sustainlev <> sustain; set_undo(UNDO_SET_SUSTAINLEV,bd,nbank); nb.sustainlev:=sustain; change:=TRUE; ENDIF
    IF nb.release <> release; set_undo(UNDO_SET_RELEASE,bd,nbank); nb.release:=release; change:=TRUE; ENDIF
    IF change ; checkaskquit(); ; update_redoundo(); ENDIF
  ELSEIF keybcode = -2
    envp.setenvelope(nb.attack,nb.decay,nb.sustainlev,nb.release)
  ELSEIF keybcode>=0
    keybact(keybcode,WINDOW_ENVELOPE)
  ENDIF
ENDPROC


PROC envelwindow(info)
  IF envgh.wnd
    ActivateWindow(envgh.wnd)
    WindowToFront(envgh.wnd)
  ELSE
    openwin(envgh)
    IF envwindow:=envgh.wnd THEN SetWindowTitles(envwindow,-1,mainbartext)
  ENDIF
ENDPROC

PROC close_envwin(info)
  IF envgh.wnd
    envwindow:=0; closewin(envgh)
  ENDIF
ENDPROC

PROC clean_envwin(info)
  envgh:=0; envwindow:=0
ENDPROC

/*
  ==================================================================================
  ----------------------------------------------------------------------------------
                               [Midimonitor]
  ----------------------------------------------------------------------------------
  ==================================================================================
*/

PROC midimonitoropen(info)
  IF mongh.wnd
    WindowToFront(mongh.wnd); ActivateWindow(mongh.wnd)
  ELSE
    openwin(mongh)
  ENDIF
ENDPROC

PROC open_midimonitwin(defscreen,defta,deffont)
DEF a,mmwindow:PTR TO window
  mongh:=addmultiA(mh,getLocStr(STRID_MIDIMONITORWIN),
  [ROWS,
    [BEVEL,
      mgh_ctrl:=[CYCLE,{mm_setmctrl},getLocStr(STRID_MIDICONTROLLER,{a}),[
          getLocStr(STRID_MIDIVOLUME),
          getLocStr(STRID_MIDIPAN),
          getLocStr(STRID_MIDIPITCHBEND),0],mm_setmctrl(0,Not(currmcmon)),0, a ]
    ],
    [BEVEL,
      [PLUGIN, {mm_act}, NEW midimon.init(defscreen,deffont) ,FALSE,NIL]
    ]
  ],[EG_CLOSE,{close_midimonwin},
     EG_CLEAN,{clean_midimonwin},
     EG_HIDE, mbprefs.midimonhide,
     EG_SCRN, defscreen,
     EG_TOP, mbprefs.midimonwiny,
     EG_LEFT,mbprefs.midimonwinx,
     EG_WIDTH,mbprefs.midimonwinw,
     EG_HEIGHT,mbprefs.midimonwinh,
     EG_FONT,defta,
      0,0])

    IF mmwindow:=mongh.wnd THEN SetWindowTitles(mmwindow,-1,mainbartext)

ENDPROC

PROC mm_setmctrl(info,set)
DEF l:PTR TO INT,i=0
  l:=[MC_VOLUME,MC_PAN,32]:INT
  IF set < 0
    set:=Not(set)
    FOR i:=0 TO 2 DO IF set = l[i] THEN RETURN i
    RETURN 0
  ELSE
    currmcmon:=l[set]
  ENDIF
ENDPROC

PROC mm_act(info,mm:PTR TO midimonitor)
  IF mm.keycode=ESC_CODE
    close_midimonwin(0)
  ELSEIF mm.keycode=9
    IF gh.wnd
      WindowToFront(gh.wnd); ActivateWindow(gh.wnd)
    ENDIF
  ENDIF
ENDPROC

PROC close_midimonwin(info)
  IF mongh.wnd
    closewin(mongh)
  ENDIF
ENDPROC

PROC clean_midimonwin(info)
  END midimon
  mongh:=0
ENDPROC

/*
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ==================================================================================
  ----------------------------------------------------------------------------------

                                    M E N U S

  ----------------------------------------------------------------------------------
  ==================================================================================
  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
*/
PROC boommenu() IS [NM_TITLE, 0, getLocStr(STRID_MENUPROJECT), 0, 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_MENUNEW),getLocStr(STRID_MENUNEW,-1), 0, 0, {menu_new},
    NM_ITEM, 0, getLocStr(STRID_OPEN),getLocStr(STRID_OPEN,-1), 0, 0, {menu_open},
    NM_ITEM, 0, getLocStr(STRID_MERGE),getLocStr(STRID_MERGE,-1), 0, 0, {menu_addproject},
    NM_ITEM, 0, getLocStr(STRID_RELOADALL),getLocStr(STRID_RELOADALL,-1), 0, 0, {menu_reloadall},
    NM_ITEM, 0, getLocStr(STRID_SAVE),getLocStr(STRID_SAVE,-1), 0, 0, {menu_save},
    NM_ITEM, 0, getLocStr(STRID_SAVEAS),getLocStr(STRID_SAVEAS,-1), 0, 0, {menu_saveas},
    NM_ITEM, 0, getLocStr(STRID_SUMM),getLocStr(STRID_SUMM,-1), 0, 0, {menu_summary},
    NM_ITEM, 0, NM_BARLABEL, 0 , 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_HIDE),getLocStr(STRID_HIDE,-1), 0, 0, {hide},
    NM_ITEM, 0, getLocStr(STRID_ABOUT),getLocStr(STRID_ABOUT,-1), 0, 0, {menu_about},
    NM_ITEM, 0, getLocStr(STRID_QUIT), getLocStr(STRID_QUIT,-1), 0, 0, {closemain},
    NM_TITLE,0, getLocStr(STRID_EDIT), 0, 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_COPY), getLocStr(STRID_COPY,-1), 0, 0, {menu_copy},
    NM_ITEM, 0, getLocStr(STRID_PASTE),getLocStr(STRID_PASTE,-1), 0, 0, {menu_paste},
    NM_ITEM, 0, getLocStr(STRID_MENUDELETE),getLocStr(STRID_MENUDELETE,-1), 0, 0, {menu_delete},
    NM_ITEM, 0, NM_BARLABEL, 0 , 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_MENUSORT),0 , 0, 0, 0,
    NM_SUB, 0, getLocStr(STRID_MENUSORTPRI),0, 0, 0, {menu_sort_pri},
    NM_SUB, 0, getLocStr(STRID_MENUSORTMIDI),0, 0, 0, {menu_sort_midi},
    NM_SUB, 0, getLocStr(STRID_MENUSORTNAME),0, 0, 0, {menu_sort_name},
    NM_SUB, 0, getLocStr(STRID_MENUSORTRANGE),0, 0, 0, {menu_sort_range},
    NM_ITEM, 0, NM_BARLABEL, 0 , 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_MENUUNDO),getLocStr(STRID_MENUUNDO,-1), 0, 0, {menu_undo},
    NM_ITEM, 0, getLocStr(STRID_MENUREDO),getLocStr(STRID_MENUREDO,-1), 0, 0, {menu_redo},
    NM_TITLE, 0, getLocStr(STRID_WINDOWS), 0 , 0, 0, 0,
    NM_ITEM, 0, getLocStr(STRID_MENUMAIN), getLocStr(STRID_MENUMAIN,-1), 0, 0, {mainwindow},
    NM_ITEM, 0, getLocStr(STRID_MENUVOLUME), getLocStr(STRID_MENUVOLUME,-1), 0, 0, {volumewindow},
    NM_ITEM, 0, getLocStr(STRID_MENUENVELOPE), getLocStr(STRID_MENUENVELOPE,-1), 0, 0, {envelwindow},
    NM_ITEM, 0, getLocStr(STRID_SCOPE), getLocStr(STRID_SCOPE,-1), 0, 0, {scopewindowopen},
    NM_ITEM, 0, getLocStr(STRID_MENUMIDIMON), getLocStr(STRID_MENUMIDIMON,-1), 0, 0, {midimonitoropen},
    NM_END]:newmenu

PROC menu_reloadall(info)
DEF instr[NUMBANKS]:ARRAY OF LONG,i,f=0,l=0
  blockallwindows()
  FOR i:=0 TO NUMBANKS-1
    instr[i]:=clearinstr(bd[i])
    IF instr[i]<>NIL THEN INC l
  ENDFOR
  FOR i:=0 TO NUMBANKS-1
    IF instr[i]<>NIL THEN printstatus(0,0,0,f++,l)
    setinstr(bd[i],instr[i])
  ENDFOR
  unblockallwindows()
  updategh()
  addingsamplesover()
ENDPROC

PROC menu_followset(info)
DEF item:PTR TO menuitem
  item:=ItemAddress(menuptr, FULLMENUNUM(2,2,0))
  followb:=IF item.flags AND CHECKED THEN TRUE ELSE FALSE
  setcheck(gh,gd_followck,followb)
ENDPROC

PROC menu_audio(info) HANDLE
DEF item:PTR TO menuitem,aud
  item:=ItemAddress(menuptr, FULLMENUNUM(2,0,0))
  IF rangesetb=0 THEN leds.setactive()
  aud:=IF item.flags AND CHECKED THEN TRUE ELSE FALSE
  IF aud
    IF audio_attrs([SFX_SET_AUDIO_STATUS,TRUE,NIL])=FALSE
      Raise("AUDB")
    ELSE
      setcheck(gh,gd_audiock,TRUE)
    ENDIF
  ELSE
    audio_attrs([SFX_SET_AUDIO_STATUS,FALSE,NIL])
    setcheck(gh,gd_audiock,FALSE)
  ENDIF
EXCEPT
  item.flags:=item.flags AND Not(CHECKED)
  report_exception()
ENDPROC

PROC menu_mcontrol(info)
  DEF item:PTR TO menuitem
  item:=ItemAddress(menuptr, FULLMENUNUM(2,1,0))
  mcontrol:=IF item.flags AND CHECKED THEN TRUE ELSE FALSE
  setcheck(gh,gd_mcontr,mcontrol)
  clearcontrollers()
ENDPROC

PROC menu_about(info) IS reqabout()

PROC menu_summary(info)
DEF a=0,b=0,c=0,d=0, i, ln:PTR TO lln, snd:PTR TO sfx,l,
    time,t

  FOR i:=0 TO NUMBANKS-1 DO IF bd[i].instr<>FALSE THEN INC a
  ln:=smplist.head
  WHILE ln.ln.succ
    INC b
    snd:=ln.pointer
    IF (l:=snd.length()) <> -1 ; INC c; d:=d+l; ENDIF
    ln:=ln.ln.succ
  ENDWHILE
  CurrentTime({time},{dummy})
  IF (t:=time-timestart) < 0 THEN t:=-t
  reqsumm(IF EstrLen(prjname)=0 THEN getLocStr(STRID_UNNAMED) ELSE prjname,a,b,c,d,t,undoleft())

ENDPROC

PROC menu_copy(info) HANDLE
DEF snd:PTR TO sfx, i, n, p, t
  IF cpbd <> NIL
    IF cpstr <> NIL THEN DisposeLink(cpstr); cpstr:=NIL
    Dispose(cpbd); cpbd:=NIL
  ENDIF
  cpsize:=rangehi-rangelo+1
  IF (cpbd:=New(cpsize * SIZEOF bank))=NIL THEN Raise("MEM")
  CopyMem(bd[rangelo], cpbd, cpsize * SIZEOF bank)
  FOR i:=0 TO cpsize-1
    IF snd:=cpbd[i].instr
      p:=snd.pathname()
      IF n:=cpstr
        WHILE StrCmp(n,p)=FALSE
          EXIT Next(n)=NIL; n:=Next(n)
        ENDWHILE
      ELSE
        n:=''
      ENDIF
      IF StrCmp(n,p)=TRUE
        t:=n
      ELSE
        IF NIL=(t:=String(StrLen(p))) THEN Raise("MEM")
        StrCopy(t,p)
        IF cpstr THEN Link(n,t) ELSE cpstr:=t
      ENDIF
      cpbd[i].instr:=t
    ENDIF
  ENDFOR
EXCEPT
  IF cpstr <> NIL THEN DisposeLink(cpstr); cpstr:=NIL
  IF cpbd <> NIL THEN Dispose(cpbd); cpbd:=NIL
  cpsize:=0
  report_exception()
ENDPROC

PROC menu_paste(info)
DEF i,name,prgrs=0,total=0
  IF (cpsize=NIL) OR (cpbd=NIL) THEN RETURN
  blockallwindows()
  set_undo(UNDO_PREP_ALL,smplist,bd,rangehi-rangelo+1,rangelo)
  FOR i:=0 TO Min(rangehi-rangelo,cpsize-1) DO IF cpbd[i].instr <> NIL THEN INC total
  FOR i:=rangelo TO Min(rangehi,rangelo+cpsize-1)
    IF name:=cpbd[i-rangelo].instr
      setlistvlabels(gh,gd_smplist,-1)
      printstatus(0,0,0,prgrs++,total)
      setinstrname(bd[i],smplist,name)
      cpbd[i-rangelo].instr:=bd[i].instr
      setlistvlabels(gh,gd_smplist,smplist)
    ENDIF
    setbank(bd[i],cpbd[i-rangelo])
    cpbd[i-rangelo].instr:=name
  ENDFOR
  FOR i:=i TO rangehi DO deletebank(bd[i])
  set_undo(UNDO_SET_ALL,smplist,bd)
  checkaskquit()
  sortbanks(); updategh()
  unblockallwindows()
  addingsamplesover()
ENDPROC

PROC menu_delete(info)
DEF i
  set_undo(UNDO_SET_BANKS,bd,rangehi-rangelo+1,rangelo)
  FOR i:=rangelo TO rangehi DO deletebank(bd[i])
  checkaskquit()
  sortbanks(); updategh()
ENDPROC

PROC menu_sort_pri(info)
  sortbanksrange(SORT_PRI)
ENDPROC
PROC menu_sort_midi(info)
  sortbanksrange(SORT_MIDI)
ENDPROC
PROC menu_sort_name(info)
  sortbanksrange(SORT_NAME)
ENDPROC
PROC menu_sort_range(info)
  sortbanksrange(SORT_RANGE)
ENDPROC

PROC sortbanksrange(type) HANDLE
DEF i,f,c,xpt=0
  IF (f:=rangehi-rangelo)=0 THEN Raise("RNGE")
  xpt:=NewR(f*(f+1))
  i:=rangelo+1;  c:=0
  WHILE i<=rangehi
    f:=i
    WHILE cmpbanks(bd[f-1],bd[f],type) = -1
      xpt[c++]:=f-1; xpt[c++]:=f
      xchgbanksachn(bd[f-1],bd[f])
      DEC f; EXIT f <= rangelo
    ENDWHILE
    INC i
  ENDWHILE
  IF c; set_undo(UNDO_SET_XCHGBANKS,xpt,c); checkaskquit(); ENDIF
  updategh()
EXCEPT DO
  IF xpt THEN Dispose(xpt)
  IF exception THEN report_exception()
ENDPROC


PROC menu_undo(info) HANDLE
DEF n
  setlistvlabels(gh,gd_smplist,-1)
  blockallwindows()
  IF (n:=do_undo(bd, smplist)) >= 0 THEN leds.setcurrent(nbank:=n)
  IF asksaveundo() THEN askquit:=FALSE ELSE askquit:=TRUE
EXCEPT DO
  unblockallwindows()
  setlistvlabels(gh,gd_smplist,smplist)
  IF remlist THEN remsfx(info)
  updategh()
  IF exception THEN report_exception()
ENDPROC

PROC menu_redo(info) HANDLE
DEF n
  setlistvlabels(gh,gd_smplist,-1)
  blockallwindows()
  IF (n:=do_redo(bd, smplist)) >= 0 THEN leds.setcurrent(nbank:=n)
  IF asksaveundo() THEN askquit:=FALSE ELSE askquit:=TRUE
EXCEPT DO
  unblockallwindows()
  setlistvlabels(gh,gd_smplist,smplist)
  IF remlist THEN remsfx(info)
  updategh()
  IF exception THEN report_exception()
ENDPROC

PROC menu_save(info)
  IF EstrLen(prjname)=0 THEN save(TRUE) ELSE save(FALSE)
ENDPROC
PROC menu_saveas(info) IS save(TRUE)

PROC save(askr) HANDLE
DEF req=0:PTR TO rtfilerequester,file[109]:ARRAY OF CHAR
DEF name[260]:ARRAY OF CHAR,ans=TRUE

  blockallwindows()
  IF askr
    ans:=FALSE
    IF req:=RtAllocRequestA(0,0)
      AstrCopy(file,prjname,32)
      RtChangeReqAttrA(req,[RTFI_DIR,prjpath,NIL])
      IF RtFileRequestA(req,file,getLocStr(STRID_SAVEPROJECTAS),[
                RT_WINDOW,gh.wnd,RTFI_FLAGS,FREQF_SAVE,0])
        StrCopy(prjpath,req.dir,ALL)
        StrCopy(prjname,file,ALL)
        ans:=TRUE
      ENDIF
    ELSE
      DisplayBeep(0)
    ENDIF
  ENDIF
  IF ans
    AstrCopy(name,prjpath,260)
    AddPart(name,prjname,260)
    saveproject(name,smplist,bd)
    askquit:=FALSE; remembersaveundo()
    projectname()
  ENDIF
EXCEPT DO
  unblockallwindows()
  IF req THEN RtFreeRequest(req)
  IF exception THEN report_exception() ELSE FlushMDest(dest)   -> flush midi for any case:)
ENDPROC

PROC menu_open(info) HANDLE
DEF req=0:PTR TO rtfilerequester,file[109]:ARRAY OF CHAR
DEF ans=TRUE,name[260]:ARRAY OF CHAR
  IF askquit THEN ans:=reqquit()
  blockallwindows()
  IF ans
    IF req:=RtAllocRequestA(0,0)
      file[]:=0 ->    AstrCopy(file,prjname,32)
      RtChangeReqAttrA(req,[RTFI_DIR,prjpath,NIL])
      IF RtFileRequestA(req,file,getLocStr(STRID_CHOOSEPROJECT),[
                RT_WINDOW,gh.wnd,0])

        StrCopy(prjpath,req.dir,ALL)
        AstrCopy(name,prjpath,260)
        AddPart(name,file,260)
        setlistvlabels(gh,gd_smplist,-1)
        StrCopy(prjname,'')
        askquit:=FALSE; flush_undo()
        loadproject(name,smplist,bd)
        StrCopy(prjname,file,ALL)
      ENDIF
    ELSE
      DisplayBeep(0)
    ENDIF
  ENDIF
EXCEPT DO
  unblockallwindows()
  sortbanks()
  updategh()
  setlistvlabels(gh,gd_smplist,smplist)
  IF req THEN RtFreeRequest(req)
  IF exception THEN report_exception() ELSE FlushMDest(dest)   -> flush midi for any case:)
ENDPROC

PROC menu_addproject(info) HANDLE
DEF req=0:PTR TO rtfilerequester,file[109]:ARRAY OF CHAR
DEF name[260]:ARRAY OF CHAR
  blockallwindows()
  set_undo(UNDO_PREP_ALL,smplist,bd,NUMBANKS-nbank,nbank)
  IF req:=RtAllocRequestA(0,0)
    file[]:=0 ->    AstrCopy(file,prjname,32)
    RtChangeReqAttrA(req,[RTFI_DIR,prjpath,NIL])
    IF RtFileRequestA(req,file,getLocStr(STRID_CHOOSEPROJECT),[
              RT_WINDOW,gh.wnd,0])
      StrCopy(prjpath,req.dir,ALL)
      AstrCopy(name,prjpath,260)
      AddPart(name,file,260)
      setlistvlabels(gh,gd_smplist,-1)
      mergeproject(name,smplist,bd,nbank)
      set_undo(UNDO_SET_ALL,smplist,bd)
      checkaskquit()
    ENDIF
  ELSE
    DisplayBeep(0)
  ENDIF

EXCEPT DO
  unblockallwindows()
  sortbanks()
  updategh()
  setlistvlabels(gh,gd_smplist,smplist)
  IF req THEN RtFreeRequest(req)
  IF (exception <> NIL) AND (exception <> "cncl") THEN report_exception() ELSE FlushMDest(dest)   -> flush midi for any case:)
ENDPROC


PROC menu_new(info)
DEF answer=TRUE, ln:PTR TO lln, snd:PTR TO sfx
  IF askquit THEN answer:=reqquit()
  IF answer
    StrCopy(prjname,'')
    initbanks(bd)
    ln:=smplist.head
    WHILE ln.ln.succ
      snd:=ln.pointer
      snd.end()
      ln:=ln.ln.succ
    ENDWHILE
    freemidiin_icon()
    askquit:=FALSE; flush_undo()
    updategh()
  ENDIF
ENDPROC

/*
  ==================================================================================
                                updating functions
  ==================================================================================
*/

EXPORT PROC updatemidimonitor()
  IF midimon THEN midimon.updatemidimonitor(getmidicontrolarray(currmcmon))
ENDPROC

EXPORT PROC updatechannelkeys()
DEF a:REG,b:REG,f:REG
  FOR a:=0 TO 31
    b:=getchannelnote(a) AND 255
    f:=keybchannels[a]
    IF (f <> b)
      IF f<128 THEN mp.setplaying(f,FALSE)
      IF b<128 THEN mp.setplaying(b,TRUE)
      keybchannels[a]:=b
    ENDIF
  ENDFOR
ENDPROC

PROC updaterange(l,h)
DEF f:REG,chan:REG,i:REG,pri:REG,nb:PTR TO bank,
    rangeother[128]:ARRAY OF CHAR

  FOR f:=0 TO 127 DO rangeother[f]:=0
  chan:=bd[nbank].midi
  pri:=bd[nbank].pri
  FOR f:=0 TO NUMBANKS-1
    IF f<>nbank           -> nie bieûâcy bank
      nb:=bd[f]
      IF (nb.instr) AND (nb.midi=chan)  -> ten sam kanaî
        IF (nb.hibound < 128) AND (nb.lobound <= nb.hibound)
          FOR i:=nb.lobound TO nb.hibound
            IF (nb.pri<= pri)
              rangeother[i]:=RANGE_HIPRI
            ELSEIF rangeother[i]=0
              rangeother[i]:=RANGE_LOPRI
            ENDIF
          ENDFOR
        ENDIF
      ENDIF
    ENDIF
  ENDFOR
  mp.bounds(l,h,rangeother)
ENDPROC

PROC updateleds()
DEF f:REG,chan:REG,pri:REG,bn:PTR TO bank
    chan:=bd[nbank].midi
    pri:=bd[nbank].pri
    FOR f:=0 TO NUMBANKS-1
      bn:=bd[f]
      IF bn.instr=0
        leds.setenabled(f,FALSE)
      ELSE
        IF rangesetb AND (bn.midi=chan)
          IF bn.pri <= pri THEN leds.setactive(f,LSETFULL) ELSE leds.setactive(f,LSETINNER)
        ELSE
          leds.setenabled(f)
        ENDIF
      ENDIF
    ENDFOR
ENDPROC

EXPORT PROC checkplayingbanks()
DEF a[NUMBANKS]:ARRAY OF CHAR,i:REG
  IF rangesetb=FALSE
    FOR i:=0 TO NUMBANKS-1 DO a[i]:=0
    whichbankisplaying(a,bd)
    FOR i:=0 TO NUMBANKS-1 DO IF a[i] THEN leds.setactive(i,LSETFULL) ELSE leds.setactive(i)
  ENDIF
ENDPROC

PROC updategh()
DEF bn:PTR TO bank,v:REG,snd:PTR TO sfx
->WriteF('upd!!!!\n')
  bn:=bd[nbank]
  IF snd:=bn.instr
    StrCopy(instrumenttext,IF snd.stereo() THEN '= ' ELSE '- ',2)
    StrAdd(instrumenttext,snd.ln.name)
    settext(gh,gd_instrtext,instrumenttext)
  ELSE
    settext(gh,gd_instrtext,'')
  ENDIF
  setslide(gh,gd_midichansl,bn.midi)
  setslide(gh,gd_bankprisl,bn.pri)
  IF v:=bn.set AND (B_DUR_ON OR B_DRUM) THEN v:=IF v AND B_DRUM THEN 2 ELSE 1
  setmx(gh,gd_durmx,v)
  setdisabled(gh,gd_loopchk,IF v=2 THEN TRUE ELSE FALSE)
  setslide(gh,gd_groupsl,bn.group)
  v:=bn.set AND B_LOOP
  setcheck(gh,gd_loopchk,IF v THEN TRUE ELSE FALSE)
  v:=bn.set AND B_MONO
  setcheck(gh,gd_monochk,IF v THEN TRUE ELSE FALSE)
  v:=IF (v=0) OR (bn.set AND B_DRUM) THEN TRUE ELSE FALSE
  setdisabled(gh,gd_monoslide,v); setdisabled(gh,gd_monovsens,IF v OR (bn.monoslide=0) THEN TRUE ELSE FALSE)
  setslide(gh,gd_monovsens,bn.monovsens)
  setslide(gh,gd_monoslide,v:=bn.monoslide)
  IF v THEN StringF(monosliderstr,'\d.\z\d[2] ',v/50,Mod(v,50)*2) ELSE StrCopy(monosliderstr,getLocStr(STRID_OFF))
  settext(gh,gd_monosltxt,monosliderstr)
  v:=bn.base;  pianokeypressed(mp,v)
  settext(gh,gd_basetx,midinote(basestr,v))
  v:=bn.fine
  setslide(gh,gd_finesl,v-FINE_CENTR)
  setnum(gh,gd_finetx,v-FINE_CENTR)
  updateleds()
  IF rangesetb
    updaterange(bn.lobound,bn.hibound)
  ENDIF
  update_redoundo()
  projectname()
  update_volgh()
  update_envgh()
ENDPROC

PROC update_redoundo()
DEF t
  IF (t:=nextundo()) <> lastundotype
    lastundotype:=t
    settext(gh,gd_undotx,undotypetext(t))
  ENDIF
ENDPROC

PROC undotypetext(type)
  SELECT type
   CASE UNDO_SET_SAMPLELIST; RETURN getLocStr(STRID_SET_SAMPLELIST)
   CASE UNDO_SET_SAMPLELISTINSTR; RETURN getLocStr(STRID_SET_SAMPLELIST)
   CASE UNDO_SET_ALL; RETURN getLocStr(STRID_SET_ALL)
   CASE UNDO_SET_BANKS; RETURN getLocStr(STRID_SET_BANKS)
   CASE UNDO_SET_XCHGBANKS; RETURN getLocStr(STRID_SET_XCHGBANKS)
   CASE UNDO_SET_SAMPLEDELETE; RETURN getLocStr(STRID_SET_SAMPLEDELETE)
   CASE UNDO_SET_SAMPLEUNDELETE; RETURN getLocStr(STRID_SET_SAMPLEDELETE)
   CASE UNDO_SET_INSTR; RETURN getLocStr(STRID_SET_INSTR)
   CASE UNDO_SET_MIDI; RETURN getLocStr(STRID_SET_MIDI)
   CASE UNDO_SET_PRI; RETURN getLocStr(STRID_SET_PRI)
   CASE UNDO_SET_BASE; RETURN getLocStr(STRID_SET_BASE)
   CASE UNDO_SET_FINE; RETURN getLocStr(STRID_SET_FINE)
   CASE UNDO_SET_SET_LOOP; RETURN getLocStr(STRID_SET_SET_LOOP)
   CASE UNDO_SET_SET_DUR; RETURN getLocStr(STRID_SET_SET_DUR)
   CASE UNDO_SET_SET_MONO; RETURN getLocStr(STRID_SET_SET_MONO)
   CASE UNDO_SET_SET_ADDAFTERT; RETURN getLocStr(STRID_SET_SET_ADDAFTERT)
   CASE UNDO_SET_BOUNDS; RETURN getLocStr(STRID_SET_BOUNDS)
   CASE UNDO_SET_VOLUME; RETURN getLocStr(STRID_SET_VOLUME)
   CASE UNDO_SET_VELSENS; RETURN getLocStr(STRID_SET_VELSENS)
   CASE UNDO_SET_RELEASE; RETURN getLocStr(STRID_SET_RELEASE)
   CASE UNDO_SET_PANORAMA; RETURN getLocStr(STRID_SET_PANORAMA)
   CASE UNDO_SET_PANWIDE; RETURN getLocStr(STRID_SET_PANWIDE)
   CASE UNDO_SET_PITCHSENS; RETURN getLocStr(STRID_SET_PITCHSENS)
   CASE UNDO_SET_ATTACK; RETURN getLocStr(STRID_SET_ATTACK)
   CASE UNDO_SET_DECAY; RETURN getLocStr(STRID_SET_DECAY)
   CASE UNDO_SET_SUSTAINLEV; RETURN getLocStr(STRID_SET_SUSTAINLEV)
   CASE UNDO_SET_AFTERSENS; RETURN getLocStr(STRID_SET_AFTERSENS)
   CASE UNDO_SET_FIRSTSKIP; RETURN getLocStr(STRID_SET_FIRSTSKIP)
   CASE UNDO_SET_MCTRLVOL; RETURN getLocStr(STRID_SET_MCTRLVOL)
   CASE UNDO_SET_MCTRLPAN; RETURN getLocStr(STRID_SET_MCTRLPAN)
   CASE UNDO_SET_GROUP; RETURN getLocStr(STRID_SET_GROUP)
   CASE UNDO_SET_MONOVSENS; RETURN getLocStr(STRID_SET_MONOVSENS)
   CASE UNDO_SET_MONOSLIDE; RETURN getLocStr(STRID_SET_MONOSLIDE)
  ENDSELECT
ENDPROC ''
/*
  ==================================================================================
                           other usefull functions
  ==================================================================================
*/

PROC checkaskquit()
  IF askquit=FALSE
    askquit:=TRUE; projectname()
  ENDIF
ENDPROC

EXPORT PROC pianokeypressed(mp:PTR TO pianokeys,key=-1)
DEF x
  x:=mp.keypressed(key)
  IF key=-1 THEN key:=x
  settext(gh,gd_notetext,IF key<128 THEN midinote(notestr,key) ELSE '')
ENDPROC x

PROC midinote(str,n) IS StringF(str,'\s\d',ListItem(
['c ','c#','d ','d#','e ','f ','f#','g ','g#','a ','a#','h '],Mod(n,12)),n)

