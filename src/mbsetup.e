OPT PREPROCESS
OPT OSVERSION=37
OPT MODULE


MODULE  'intuition/intuition','exec/lists','exec/nodes','tools/EasyGUI',
        'libraries/midi','midi','libraries/midibase',
        '*midibplists','*soundfx','*mbdiskoper','*mbreport',
        '*mblocale','*title','*mbtitle','*mbplay','*mbbanks','*mbkeycod'

EXPORT DEF dest:PTR TO mdest,
           minfo:PTR TO mrouteinfo    -> whole midi stuff
EXPORT DEF mysrclist:PTR TO lh    -> midi sources list for listview
EXPORT DEF smplist:PTR TO lh      -> sample list
EXPORT DEF mainbartext            -> screen text for window
EXPORT DEF mh:PTR TO multihandle,
           gh:PTR TO guihandle,       -> main window
           volgh:PTR TO guihandle,    -> volume
           envgh:PTR TO guihandle,    -> envelope
           scopegh:PTR TO guihandle,  -> scope
           mongh:PTR TO guihandle,    -> midimonitor
           setgh:PTR TO guihandle     -> settings
          ->^ window stuff
EXPORT DEF menuptr  -> mainmenuptr
EXPORT DEF maxchan,maskmaxchan, mcontrol, ledbool, nbank,
           mbprefs:PTR TO mbprefs, msgflags, chanflags
EXPORT DEF currmcmon
                    ->^ prefs stuff
DEF routes:PTR TO LONG  -> array of current midi routes (last entry = -1)
DEF dmaper  -> custom prefs stuff
DEF sgh_dmahz, sgh_chan, sgh_msrclist, sgh_ledchk, -> gadgdets
    sgh_noteoff, sgh_noteon, sgh_keypress, sgh_ctrl, sgh_chanpress, sgh_pitchbend,
    sgh_c1,sgh_c2,sgh_c3,sgh_c4,sgh_c5,sgh_c6,sgh_c7,sgh_c8,
    sgh_c9,sgh_ca,sgh_cb,sgh_cc,sgh_cd,sgh_ce,sgh_cf,sgh_cg
DEF title_m:PTR TO title_keys     -> ^^^ EGUI plugins
DEF setwithprojects, setlayout, setsaveicons    -> menus status



EXPORT PROC initprefs(pname,bnk)  -> set initial prefs value & set soundfx

  newlist(mysrclist)  -> this must be the first thing!!!
  setwithprojects:=TRUE
  setlayout:=FALSE
  setsaveicons:=TRUE
  refresh_srclist()
  loadsettings(smplist,mbprefs,bnk,pname)
->WriteF('mainx=\d mainy=\d\n',mbprefs.mainwinx,mbprefs.mainwiny)
->WriteF('volx=\d voly=\d hide=\d\n',mbprefs.volumewinx,mbprefs.volumewiny,mbprefs.volumehide)
->WriteF('scx=\d scy=\d hide=\d\n',mbprefs.scopewinx,mbprefs.scopewiny,mbprefs.scopehide)
  dmaper:=mbprefs.dmaper;  currentdmacycle()
  setDMAperiod(dmaper)
->WriteF('dma period set!\n')
  ledbool:=IF mbprefs.led <> 0 THEN TRUE ELSE FALSE
  mcontrol:=IF mbprefs.midictrl <> 0 THEN TRUE ELSE FALSE
  IF (maxchan:=mbprefs.maxchannels) > 31 THEN maxchan:=31
  IF maxchan < 1 THEN maxchan:=1
  maskmaxchan:=Shl(1,maxchan+1)-1
  mbprefs.maxchannels:=maxchan
  msgflags:=mbprefs.msgflags AND (MMF_NOTEOFF OR MMF_NOTEON OR MMF_PITCHBEND OR MMF_POLYPRESS OR MMF_CTRL OR MMF_CHANPRESS)
  mbprefs.msgflags:=msgflags
  chanflags:=mbprefs.chanflags
  IF (currmcmon:=mbprefs.currentmcm) > 32 THEN currmcmon:=32
  IF currmcmon < 0 THEN currmcmon:=0
  mbprefs.currentmcm:=currmcmon
  refresh_srclist()
  changeMRoutes()
ENDPROC

EXPORT PROC menu_setlayout(info)
DEF item:PTR TO menuitem,flag
  item:=ItemAddress(menuptr, FULLMENUNUM(2,6,0))
  flag:=IF item.flags AND CHECKED THEN TRUE ELSE FALSE
  setlayout:=IF flag THEN TRUE ELSE FALSE
ENDPROC

EXPORT PROC menu_setwithprojects(info)
DEF item:PTR TO menuitem,flag
  item:=ItemAddress(menuptr, FULLMENUNUM(2,7,0))
  flag:=IF item.flags AND CHECKED THEN TRUE ELSE FALSE
  setwithprojects:=IF flag THEN TRUE ELSE FALSE
ENDPROC

EXPORT PROC menu_setsaveicons(info)
DEF item:PTR TO menuitem,flag
  item:=ItemAddress(menuptr, FULLMENUNUM(2,8,0))
  flag:=IF item.flags AND CHECKED THEN TRUE ELSE FALSE
  setsaveicons:=IF flag THEN TRUE ELSE FALSE
ENDPROC

PROC updatesetwin()   -> update setwin
  IF setgh
    setcycle(setgh,sgh_dmahz,currentdmacycle())
    setslide(setgh,sgh_chan,maxchan+1)
    pb_refresh(0)
    setcheck(setgh,sgh_ledchk,ledbool)
    setcheck(setgh,sgh_noteoff,msgflags AND MMF_NOTEOFF)
    setcheck(setgh,sgh_noteon,msgflags AND MMF_NOTEON)
    setcheck(setgh,sgh_keypress,msgflags AND MMF_POLYPRESS)
    setcheck(setgh,sgh_ctrl,msgflags AND MMF_CTRL)
    setcheck(setgh,sgh_chanpress,msgflags AND MMF_CHANPRESS)
    setcheck(setgh,sgh_pitchbend,msgflags AND MMF_PITCHBEND)
    setcheck(setgh,sgh_c1,IF chanflags AND $1 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_c2,IF chanflags AND $2 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_c3,IF chanflags AND $4 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_c4,IF chanflags AND $8 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_c5,IF chanflags AND $10 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_c6,IF chanflags AND $20 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_c7,IF chanflags AND $40 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_c8,IF chanflags AND $80 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_c9,IF chanflags AND $100 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_ca,IF chanflags AND $200 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_cb,IF chanflags AND $400 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_cc,IF chanflags AND $800 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_cd,IF chanflags AND $1000 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_ce,IF chanflags AND $2000 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_cf,IF chanflags AND $4000 THEN TRUE ELSE FALSE)
    setcheck(setgh,sgh_cg,IF chanflags AND $8000 THEN TRUE ELSE FALSE)
  ENDIF
ENDPROC

EXPORT PROC saveproject(name,slist:PTR TO lh,bnk:PTR TO bank)
DEF tmprefs:mbprefs
  CopyMem(mbprefs,tmprefs,SIZEOF mbprefs)
  tmprefs.dmaper:=dmaper
  tmprefs.led:=ledbool
  tmprefs.midictrl:=mcontrol
  tmprefs.activeb:=nbank
  tmprefs.maxchannels:=maxchan
  tmprefs.msgflags:=msgflags
  tmprefs.chanflags:=chanflags
  tmprefs.currentmcm:=currmcmon
  prefs_layout(tmprefs)
  save_project(name,slist,bnk,tmprefs,setsaveicons)
ENDPROC

EXPORT PROC loadproject(name,slist:PTR TO lh,bnk:PTR TO bank) HANDLE
DEF tmpprefs:mbprefs
  IF setwithprojects
    setlistvlabels(setgh,sgh_msrclist,-1)
    refresh_srclist()
    IF tmpprefs:=load_project(name,slist,bnk,tmpprefs)
      dmaper:=tmpprefs.dmaper;  currentdmacycle()
      setDMAperiod(dmaper)
      led_filt(0,tmpprefs.led)
  ->  mcontrol:=IF tmpprefs.midictrl <> 0 THEN TRUE ELSE FALSE
      IF (maxchan:=tmpprefs.maxchannels) > 31 THEN maxchan:=31
      IF maxchan < 1 THEN maxchan:=1
      maskmaxchan:=Shl(1,maxchan+1)-1
      msgflags:=tmpprefs.msgflags AND (MMF_NOTEOFF OR MMF_NOTEON OR MMF_PITCHBEND OR MMF_POLYPRESS OR MMF_CTRL OR MMF_CHANPRESS)
      chanflags:=tmpprefs.chanflags
    ENDIF
    updatesetwin()
  ELSE
    load_project(name,slist,bnk,NIL)
  ENDIF
EXCEPT DO
  IF setwithprojects THEN setlistvlabels(setgh,sgh_msrclist,mysrclist)
  IF exception THEN ReThrow()
ENDPROC


EXPORT PROC menu_settings(info)
  mbprefs.dmaper:=dmaper
  mbprefs.led:=ledbool
  mbprefs.midictrl:=mcontrol
  mbprefs.activeb:=nbank
  mbprefs.maxchannels:=maxchan
  mbprefs.msgflags:=msgflags
  mbprefs.chanflags:=chanflags
  mbprefs.currentmcm:=currmcmon
  prefs_layout(mbprefs)
  savesettings(smplist,mbprefs)
ENDPROC

PROC prefs_layout(prefs:PTR TO mbprefs)
DEF window:PTR TO window
 IF setlayout
  IF window:=gh.wnd
    prefs.mainwinx:=window.leftedge
    prefs.mainwiny:=window.topedge
    prefs.mainwinw:=window.width
    prefs.mainwinh:=window.height
->WriteF('mainx=\d mainy=\d\n',window.leftedge,window.topedge)
  ENDIF
  IF window:=volgh.wnd
    prefs.volumewinx:=window.leftedge
    prefs.volumewiny:=window.topedge
    prefs.volumewinw:=window.width
    prefs.volumewinh:=window.height
    prefs.volumehide:=FALSE
->WriteF('volx=\d voly=\d\n',window.leftedge,window.topedge)
  ELSE
    prefs.volumehide:=TRUE
  ENDIF
  IF window:=envgh.wnd
    prefs.envelwinx:=window.leftedge
    prefs.envelwiny:=window.topedge
    prefs.envelwinw:=window.width
    prefs.envelwinh:=window.height
    prefs.envelhide:=FALSE
->WriteF('envx=\d envy=\d\n',window.leftedge,window.topedge)
  ELSE
    prefs.envelhide:=TRUE
  ENDIF
  IF window:=scopegh.wnd
    prefs.scopewinx:=window.leftedge
    prefs.scopewiny:=window.topedge
    prefs.scopewinw:=window.width
    prefs.scopewinh:=window.height
    prefs.scopehide:=FALSE
->WriteF('scopex=\d scopey=\d\n',window.leftedge,window.topedge)
  ELSE
    prefs.scopehide:=TRUE
  ENDIF
  IF window:=mongh.wnd
    prefs.midimonwinx:=window.leftedge
    prefs.midimonwiny:=window.topedge
    prefs.midimonwinw:=window.width
    prefs.midimonwinh:=window.height
    prefs.midimonhide:=FALSE
->WriteF('midimonx=\d midimony=\d\n',window.leftedge,window.topedge)
  ELSE
    prefs.midimonhide:=TRUE
  ENDIF
 ENDIF
ENDPROC

EXPORT PROC menu_advanced(info)
  IF setgh.wnd
    ActivateWindow(setgh.wnd)
    WindowToFront(setgh.wnd)
  ELSE
    openwin(setgh)
    IF setgh.wnd THEN SetWindowTitles(setgh.wnd,-1,mainbartext)
  ENDIF
ENDPROC

EXPORT PROC close_setwin(info)
  IF setgh.wnd
    closewin(setgh)
  ENDIF
ENDPROC

PROC clean_setwin(info)
  setgh:=0
ENDPROC

PROC setwin_act(info,tl:PTR TO title_keys)
  IF tl.keycode=ESC_CODE
    close_setwin(info)
  ENDIF
ENDPROC

EXPORT PROC open_setwin(screen,ta)
DEF d,c,l,p,aa,bb,cc,dd,ee,ff

  setgh:=addmultiA(mh,getLocStr(STRID_MIDIINADVANCED),
  [ROWS,
    [BEVEL,[BEVELR,
      [ROWS,
        sgh_dmahz:=[CYCLE,{dmaset},getLocStr(STRID_DMA,{d}),[
        '48 kHz','41 kHz','28 kHz','20 kHz','14 kHz',0],currentdmacycle(),0, d ],
        sgh_chan:=[SLIDE,{chanset},getLocStr(STRID_CHANPOLY,{c}),0,2,32,maxchan+1,6,'%2ld',0, c ],
        [COLS,
          [SBUTTON,{undoset},getLocStr(STRID_UNDO),0, 0 ],
          sgh_ledchk:=[CHECK,{led_filt},getLocStr(STRID_LEDFILTER,{l}),ledbool,TRUE,0, l ]
        ]
      ]
    ]],
    [PLUGIN, {setwin_act}, NEW title_m.setup(getLocStr(STRID_MIDIMESANDCHAN), TITLE_NORMAL) ,FALSE,NIL],
    [COLS,
      [ROWS,
       [BEVEL,[EQROWS,
        sgh_noteoff:=[CHECK,{chk_noteoff},getLocStr(STRID_NOTEOFF,{bb}),msgflags AND MMF_NOTEOFF,FALSE,0, bb],
        sgh_noteon:=[CHECK,{chk_noteon},getLocStr(STRID_NOTEON,{aa}),msgflags AND MMF_NOTEON,FALSE,0, aa],
        sgh_keypress:=[CHECK,{chk_keypress},getLocStr(STRID_KEYPRESS,{cc}),msgflags AND MMF_POLYPRESS,FALSE,0, cc],
        sgh_ctrl:=[CHECK,{chk_ctrl},getLocStr(STRID_CTRL,{dd}),msgflags AND MMF_CTRL,FALSE,0, dd],
        sgh_chanpress:=[CHECK,{chk_chanpress},getLocStr(STRID_CHANPRESS,{ee}),msgflags AND MMF_CHANPRESS,FALSE,0, ee],
        sgh_pitchbend:=[CHECK,{chk_pitchbend},getLocStr(STRID_SETPITCHBEND,{ff}),msgflags AND MMF_PITCHBEND,FALSE,0, ff]
       ]],
       sgh_msrclist:=[LISTV,{pb_routechange},getLocStr(STRID_PATCHBAY),8,3,mysrclist,FALSE,0,0],
       [SBUTTON,{pb_refresh},getLocStr(STRID_REFRESH,{p}),0, p ]
      ],
      [ROWS,
        [SBUTTON,{undomidi},getLocStr(STRID_UNDO),0, 0 ],
        [BEVEL,[EQCOLS,
          [EQROWS,
            sgh_c1:=[CHECK,{chk_mc1},getLocStr(STRID_C01),chanflags AND $1,0, 0],
            sgh_c2:=[CHECK,{chk_mc2},getLocStr(STRID_C02),chanflags AND $2,0, 0],
            sgh_c3:=[CHECK,{chk_mc3},getLocStr(STRID_C03),chanflags AND $4,FALSE,0, 0],
            sgh_c4:=[CHECK,{chk_mc4},getLocStr(STRID_C04),chanflags AND $8,FALSE,0, 0],
            sgh_c5:=[CHECK,{chk_mc5},getLocStr(STRID_C05),chanflags AND $10,FALSE,0, 0],
            sgh_c6:=[CHECK,{chk_mc6},getLocStr(STRID_C06),chanflags AND $20,FALSE,0, 0],
            sgh_c7:=[CHECK,{chk_mc7},getLocStr(STRID_C07),chanflags AND $40,FALSE,0, 0],
            sgh_c8:=[CHECK,{chk_mc8},getLocStr(STRID_C08),chanflags AND $80,FALSE,0, 0]
          ],
          [EQROWS,
            sgh_c9:=[CHECK,{chk_mc9},getLocStr(STRID_C09),chanflags AND $100,0, 0],
            sgh_ca:=[CHECK,{chk_mc10},getLocStr(STRID_C10),chanflags AND $200,0, 0],
            sgh_cb:=[CHECK,{chk_mc11},getLocStr(STRID_C11),chanflags AND $400,FALSE,0, 0],
            sgh_cc:=[CHECK,{chk_mc12},getLocStr(STRID_C12),chanflags AND $800,FALSE,0, 0],
            sgh_cd:=[CHECK,{chk_mc13},getLocStr(STRID_C13),chanflags AND $1000,FALSE,0, 0],
            sgh_ce:=[CHECK,{chk_mc14},getLocStr(STRID_C14),chanflags AND $2000,FALSE,0, 0],
            sgh_cf:=[CHECK,{chk_mc15},getLocStr(STRID_C15),chanflags AND $4000,FALSE,0, 0],
            sgh_cg:=[CHECK,{chk_mc16},getLocStr(STRID_C16),chanflags AND $8000,FALSE,0, 0]
          ]
        ]]
      ]
    ]
  ],[EG_CLOSE,{close_setwin},
     EG_CLEAN,{clean_setwin},
     EG_SCRN,screen,
     EG_HIDE, TRUE,
     EG_FONT,ta,
      0,0])

ENDPROC

dmapresets:
INT 73,86,125,172,250

PROC currentdmacycle()
DEF i,t:PTR TO INT
  t:={dmapresets}
  FOR i:=0 TO 4
    IF dmaper=t[i] THEN RETURN i
  ENDFOR
  i:=2; dmaper:=t[i]
ENDPROC i

PROC modifyMRouteInfos(chnf,msgf)
DEF i=0:REG,mr:REG
  minfo.msgflags:=msgf AND (MMF_NOTEOFF OR MMF_NOTEON)
  minfo.chanflags:=chnf
  minfo.chanoffset:=0
  minfo.noteoffset:=0
  minfo.sysexmatch::rimatch.flags:=0
  minfo.ctrlmatch::rimatch.flags:=0
  IF routes
    WHILE routes[i++]<>-1 DO IF mr:=routes[i-1] THEN ModifyMRoute(mr,minfo)
  ENDIF
  minfo.msgflags:=msgf
  signal_playtask(PSG_MINFO)   -> signal play task "we've changed minfo"
ENDPROC TRUE
/* ======================== GUI Stuff ============================ */

PROC undoset(info)
  dmaper:=mbprefs.dmaper;  currentdmacycle()
  IF mbprefs.led <> 0 THEN ledbool:=TRUE ELSE ledbool:=FALSE
  maxchan:=mbprefs.maxchannels;  maskmaxchan:=Shl(1,maxchan+1)-1
  setDMAperiod(dmaper)
  updatesetwin()
ENDPROC

PROC dmaset(info,set)
DEF t:PTR TO INT
  t:={dmapresets}
  dmaper:=t[set]
  setDMAperiod(dmaper)
ENDPROC

PROC chanset(info,chan)
  maxchan:=chan-1
  maskmaxchan:=Shl(1,maxchan+1)-1
  soundoff(Not(maskmaxchan))
ENDPROC

PROC led_filt(info,bool)
  ledbool:=IF bool THEN TRUE ELSE FALSE
  ledchange()
ENDPROC

PROC undomidi(info)
  chanflags:=mbprefs.chanflags
  msgflags:=mbprefs.msgflags
  modifyMRouteInfos(chanflags,msgflags)
  updatesetwin()
ENDPROC

PROC chk_mc1(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $1 ELSE chanflags AND $FFFE,msgflags)
PROC chk_mc2(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $2 ELSE chanflags AND $FFFD,msgflags)
PROC chk_mc3(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $4 ELSE chanflags AND $FFFB,msgflags)
PROC chk_mc4(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $8 ELSE chanflags AND $FFF7,msgflags)
PROC chk_mc5(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $10 ELSE chanflags AND $FFEF,msgflags)
PROC chk_mc6(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $20 ELSE chanflags AND $FFDF,msgflags)
PROC chk_mc7(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $40 ELSE chanflags AND $FFBF,msgflags)
PROC chk_mc8(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $80 ELSE chanflags AND $FF7F,msgflags)
PROC chk_mc9(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $100 ELSE chanflags AND $FEFF,msgflags)
PROC chk_mc10(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $200 ELSE chanflags AND $FDFF,msgflags)
PROC chk_mc11(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $400 ELSE chanflags AND $FBFF,msgflags)
PROC chk_mc12(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $800 ELSE chanflags AND $F7FF,msgflags)
PROC chk_mc13(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $1000 ELSE chanflags AND $EFFF,msgflags)
PROC chk_mc14(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $2000 ELSE chanflags AND $DFFF,msgflags)
PROC chk_mc15(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $4000 ELSE chanflags AND $BFFF,msgflags)
PROC chk_mc16(info,check) IS modifyMRouteInfos(chanflags:=IF check THEN chanflags OR $8000 ELSE chanflags AND $7FFF,msgflags)

PROC chk_noteoff(info,check) IS modifyMRouteInfos(chanflags,msgflags:=IF check THEN msgflags OR MMF_NOTEOFF ELSE msgflags AND Not(MMF_NOTEOFF))
PROC chk_noteon(info,check) IS modifyMRouteInfos(chanflags,msgflags:=IF check THEN msgflags OR MMF_NOTEON ELSE msgflags AND Not(MMF_NOTEON))
PROC chk_keypress(info,check) IS modifyMRouteInfos(chanflags,msgflags:=IF check THEN msgflags OR MMF_POLYPRESS ELSE msgflags AND Not(MMF_POLYPRESS))
PROC chk_ctrl(info,check) IS modifyMRouteInfos(chanflags,msgflags:=IF check THEN msgflags OR MMF_CTRL ELSE msgflags AND Not(MMF_CTRL))
PROC chk_chanpress(info,check) IS modifyMRouteInfos(chanflags,msgflags:=IF check THEN msgflags OR MMF_CHANPRESS ELSE msgflags AND Not(MMF_CHANPRESS))
PROC chk_pitchbend(info,check) IS modifyMRouteInfos(chanflags,msgflags:=IF check THEN msgflags OR MMF_PITCHBEND ELSE msgflags AND Not(MMF_PITCHBEND))

PROC pb_refresh(info)
  setlistvlabels(setgh,sgh_msrclist,-1)
  refresh_srclist()
  changeMRoutes()
  setlistvlabels(setgh,sgh_msrclist,mysrclist)
ENDPROC

PROC pb_routechange(info,i)
DEF mysrcnode:PTR TO ln
  mysrcnode:=mysrclist
  WHILE mysrcnode:=mysrcnode.succ
    DEC i; EXIT i<0
  ENDWHILE
  setlistvlabels(setgh,sgh_msrclist,-1)
  IF mysrcnode
     IF mysrcnode.name[]=" " THEN mysrcnode.name[]:="+" ELSE mysrcnode.name[]:=" "
  ENDIF
  refresh_srclist()
  changeMRoutes()
  setlistvlabels(setgh,sgh_msrclist,mysrclist)
ENDPROC

/* ========================================================================
                           source list functions
   ======================================================================== */

PROC refresh_srclist() HANDLE
DEF mbase:PTR TO midibase,
    mysrcnode:PTR TO ln, nextnode:PTR TO ln,
    source:PTR TO msource,name,len

/* sprawdziê i pokasowaê liste sourców */

  LockMidiBase()

  mysrcnode:=mysrclist.head
  WHILE nextnode:=mysrcnode.succ
    mysrcnode.name:=mysrcnode.name+2
    IF FALSE=FindMSource(mysrcnode.name)
      Remove(mysrcnode); FastDispose(mysrcnode,StrLen(mysrcnode.name)+3+SIZEOF ln)
    ENDIF
    mysrcnode:=nextnode
  ENDWHILE

/* sprawdziê nowe source'y i dodaê je */
  mbase:=midibase
  source:=mbase.sourcelist.head
  WHILE source.ln::ln.succ
    IF FALSE=FindName(mysrclist,name:=source.ln::ln.name)
      IF mysrcnode:=FastNew((len:=StrLen(name)+1)+2+SIZEOF ln)
        mysrcnode.name:=mysrcnode+2+SIZEOF ln
        AstrCopy(mysrcnode.name-2,'  ',3)
        AstrCopy(mysrcnode.name,name,len)
        addsorted(mysrclist,mysrcnode)
      ENDIF
    ENDIF
    source:=source.ln::ln.succ
  ENDWHILE


EXCEPT DO
  UnlockMidiBase()
  mysrcnode:=mysrclist.head
  WHILE nextnode:=mysrcnode.succ
    mysrcnode.name:=mysrcnode.name-2
    mysrcnode:=nextnode
  ENDWHILE
  IF exception THEN report_exception()
ENDPROC

PROC changeMRoutes()
DEF mr:REG,i:REG,mysrcnode:PTR TO ln

/* policzyê routy */
  i:=0; mysrcnode:=mysrclist.head
  WHILE mysrcnode.succ
    IF mysrcnode.name[] <> " " THEN i++
    mysrcnode:=mysrcnode.succ
  ENDWHILE

/* pokasowaê stare routy */
  freeallMRoutes()

/* utworzyê routy na podstawie nazw */
  minfo.msgflags:=msgflags AND (MMF_NOTEOFF OR MMF_NOTEON)
  minfo.chanflags:=chanflags
  minfo.chanoffset:=0
  minfo.noteoffset:=0
  minfo.sysexmatch::rimatch.flags:=0
  minfo.ctrlmatch::rimatch.flags:=0
  IF i
    NEW routes[i+1]; routes[i]:= -1
    i:=0; mysrcnode:=mysrclist.head
    WHILE mysrcnode.succ
      IF mysrcnode.name[] <> " "
        IF mr:=MrouteDest(mysrcnode.name+2,dest,minfo) THEN mysrcnode.name[]:="+" ELSE mysrcnode.name[]:="-"
        routes[i++]:=mr
      ENDIF
      mysrcnode:=mysrcnode.succ
    ENDWHILE
  ENDIF

/* utworzyê routy w subtasku */
  minfo.msgflags:=msgflags
  signal_playtask(PSG_CHANGE)   -> signal play task "we've changed routes"
ENDPROC

EXPORT PROC freeallMRoutes()
DEF i=0:REG,mr:REG
  IF routes
    WHILE routes[i++]<>-1 DO IF mr:=routes[i-1] THEN DeleteMRoute(mr)
    END routes[i]
  ENDIF
ENDPROC



