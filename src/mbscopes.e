OPT OSVERSION=37
OPT MODULE

MODULE 'tools/EasyGUI','intuition/intuition','graphics/text','intuition/screens',
       'amigalib/tasks','other/ecode','exec/semaphores','graphics/view','*mbsetup',
       '*mbdiskoper','*soundfx','*MBLocale','*mbkeycod'


OBJECT scope OF plugin
  code:LONG
PRIVATE

  xpl:INT
  xpr:INT
  yp:INT
  radw:INT
  radh:INT
ENDOBJECT

CONST ARROWLEN=4,ARROWCOORDLEN=8,MAXWIDTH=256

EXPORT DEF scopedata:PTR TO LONG, scopelen  -> pointer for sound realtime data
EXPORT DEF mbprefs:PTR TO mbprefs           -> prefs 4 window pos.

EXPORT DEF mainbartext            -> screen text for window
EXPORT DEF mh:PTR TO multihandle
EXPORT DEF scopegh:PTR TO guihandle ->, scgd_channum
EXPORT DEF gh:PTR TO guihandle  -> guihandle for mainwindow address
DEF scopetask                                             -> task that displays scope
DEF scoperpsem:PTR TO ss, scopetasksem:PTR TO ss, scoperp -> semaphores & rport 4 scope
DEF scopechange   -> =TRUE : do not block old scope pixels
DEF scplug:PTR TO scope   -> scope plugin 4 EasyGUI

DEF greeencolour,colormap -> colour pen, colormap for Obtain/Release Pen

EXPORT PROC open_scopewindow(screen,ta) HANDLE
DEF scopecode,window:PTR TO window

  NEW scoperpsem,scopetasksem
  InitSemaphore(scoperpsem)
  InitSemaphore(scopetasksem)
  ObtainSemaphore(scoperpsem) ->; WriteF('obtain\n')

  scopegh:=addmultiA(mh,getLocStr(STRID_MIDIINSCOPE),
      [BEVEL,[PLUGIN,{scopeact},NEW scplug.init(screen), FALSE,NIL]],
      [EG_CLOSE,  {close_scope},
       EG_CLEAN,  {clean_scope},
       EG_HIDE, mbprefs.scopehide,
       EG_INFO, 111,
       EG_SCRN, screen,
       EG_TOP, mbprefs.scopewiny,
       EG_LEFT, mbprefs.scopewinx,
       EG_FONT, ta,
       0,0])

  IF window:=scopegh.wnd
    IF mbprefs.scopewinw <> -1
      IF (mbprefs.scopewinw <> window.width) OR (mbprefs.scopewinh <> window.height)
        sizewin(scopegh,mbprefs.scopewinw,mbprefs.scopewinh)
      ENDIF
    ENDIF
    ModifyIDCMP(window,window.idcmpflags OR IDCMP_SIZEVERIFY)
    SetWindowTitles(window,-1,mainbartext)
  ENDIF

  ObtainSemaphore(scopetasksem)
  IF NIL=(scopecode:=eCodeTask({subtaskscopes})) THEN Raise("ECOD")
  IF NIL=(scopetask:=createTask('midiIn_SCOPE', -25, scopecode, 8192)) THEN Raise("TASK")
  ReleaseSemaphore(scopetasksem)

EXCEPT
  IF scoperpsem
    IF scoperpsem.nestcount THEN ReleaseSemaphore(scoperpsem)
  ENDIF
  IF scopetasksem
    IF scopetasksem.nestcount THEN ReleaseSemaphore(scopetasksem)
  ENDIF
  ReThrow()
ENDPROC

EXPORT PROC scopewindowopen(info)
DEF window:PTR TO window
  IF scopegh.wnd
    ActivateWindow(scopegh.wnd)
    WindowToFront(scopegh.wnd)
  ELSE
    openwin(scopegh)
    IF window:=scopegh.wnd
      ModifyIDCMP(window,window.idcmpflags OR IDCMP_SIZEVERIFY)
      SetWindowTitles(window,-1,mainbartext)
    ENDIF
  ENDIF
ENDPROC

PROC close_scope(info)
  IF scopegh.wnd
    closewin(scopegh)
  ENDIF
ENDPROC

PROC clean_scope(info)
  IF colormap THEN ReleasePen(colormap,greeencolour)
  SetTaskPri(FindTask(0),0)
  SetTaskPri(scopetask,1)
  scopetask:=0
  ObtainSemaphore(scopetasksem)
  ReleaseSemaphore(scopetasksem)
  scopegh:=0; scoperp:=0
  ReleaseSemaphore(scoperpsem)
ENDPROC

/********************************************************************************/

PROC subtaskscopes()
DEF sdata,w:REG,h:REG,xl:REG,xr:REG,y,rp:REG,cleared=TRUE,
    olddata[MAXWIDTH]:ARRAY OF LONG,    tadata[MAXWIDTH]:ARRAY OF LONG

  ObtainSemaphore(scopetasksem)
->  sinus:={sincostable}+512; cosinus:={sincostable}+1024
  WHILE scopetask

    WaitTOF()

    IF AttemptSemaphore(scoperpsem)
      IF sdata:=scopedata
        cleared:=FALSE
        MOVE.L sdata,A0; MOVE.L (A0),A0; MOVE.L A0,sdata
        w:=scplug.radw; LSL.L #1,w; SUBQ.L #2,w; IF w >= MAXWIDTH THEN w:=MAXWIDTH-1
        h:=scplug.radh; ASR.L #1,h
        MOVE.L sdata,A0; MOVE.L w,D0 ; MOVE.L tadata,A1

scopeloop1:
        MOVEM.W (A0)+,D1-D2
        MULS  h,D1
        DIVS  #32767,D1
        MOVE.W D1,(A1)+; -> tadatal[a]:=p[]++*h/32767
        MULS  h,D2
        DIVS  #32767,D2
        MOVE.W D2,(A1)+; -> tadatar[a]:=p[]++*h/32767
        DBRA  D0,scopeloop1

        xl:=scplug.xpl; xr:=scplug.xpr; y:=scplug.yp-h
        MOVE.L w,D0; ADDQ.L #1,D0; ASR.L #1,D0; ADD.L D0,xl; ADD.L D0,xr -> +(w/2)

        MOVE.L gfxbase,A6; MOVE.L olddata,A2; MOVE.L tadata,A3
        MOVE.L scoperp,rp
        MOVE.L greeencolour,D0
        MOVE.L  rp,A1
        JSR    SetAPen(A6)
scopedrawloop:
          MOVE.L  (A3)+,h   ->tadata l/r
          MOVE.L #$7FFF7FFF,D2
          TST.L scopechange
          BNE scopedrawskip
            MOVE.L  (A2)+,D2
            CMP.L   D2,h   ->olddata
            BEQ  scopedrawskiptotal
            MOVEQ #1,D0
            MOVE.L  rp,A1
            JSR   SetAPen(A6)
            CMP.W   D2,h
            BEQ.S   scopeskipdrawright
            MOVE.L  xr,D0; SUB.L w,D0
            MOVE.W  D2,D1; EXT.L D1; ADD.L y,D1
            MOVE.L  rp,A1
            JSR   WritePixel(A6)
            SWAP  h; SWAP D2; CMP.W D2,h; BNE.S scopedrawoldleft
            BRA.S scopedrawoldend
scopedrawskip:
            SWAP  h; BRA.S scopedrawskip2
scopeskipdrawright:
            SWAP  D2; SWAP h
scopedrawoldleft:
            MOVE.L  xl,D0; SUB.L w,D0
            MOVE.W  D2,D1; EXT.L D1; ADD.L y,D1
            MOVE.L  rp,A1
            JSR   WritePixel(A6)
scopedrawoldend:
          MOVE.L greeencolour,D0
          MOVE.L  rp,A1
          JSR    SetAPen(A6)
          CMP.W   D2,h; BEQ.S scopedrawdrawr
scopedrawskip2:
          MOVE.L  xl,D0; SUB.L w,D0
          MOVE.W  h,D1; EXT.L D1; ADD.L y,D1
          MOVE.L  rp,A1
          JSR   WritePixel(A6)
scopedrawdrawr:
          SWAP  h; SWAP D2; CMP.W D2,h; BEQ.S scopedrawskiptotal
          MOVE.L  xr,D0; SUB.L w,D0
          MOVE.W  h,D1; EXT.L D1; ADD.L y,D1
          MOVE.L  rp,A1
          JSR   WritePixel(A6)
scopedrawskiptotal:
        DBRA  w,scopedrawloop

        MOVE.L olddata,D0; MOVE.L tadata,olddata; MOVE.L D0,tadata

        scopechange:=FALSE
      ELSE
        IF cleared=FALSE
          SetAPen(scoperp,1)
          xl:=scplug.x; y:=scplug.y
          RectFill(scoperp,xl,y,xl+scplug.xs-1,y+scplug.ys-1)
          cleared:=TRUE
          scopechange:=TRUE
        ENDIF
      ENDIF
      ReleaseSemaphore(scoperpsem)
    ENDIF
  ENDWHILE
  ReleaseSemaphore(scopetasksem)
  RemTask(0)
ENDPROC

/***************************************************************************
 ***************************************************************************
 ***************************************************************************/
PROC min_size(ta,fh) OF scope IS 200,20

PROC init(screen) OF scope
DEF scr:PTR TO screen
  IF screen=0 THEN scr:=LockPubScreen(NIL) ELSE scr:=screen
  IF scr
    colormap:=scr.viewport.colormap
    IF screen=0 THEN UnlockPubScreen(NIL,scr)
    IF KickVersion(39)
      greeencolour:=ObtainBestPenA(colormap,$10101010,$aaaaaaaa,$30303030,0)
    ELSE
      greeencolour:= -1
    ENDIF
    IF greeencolour = -1
      greeencolour:=0
      colormap:=0
    ENDIF
  ENDIF
ENDPROC

PROC clear_render(win:PTR TO window) OF scope

  ObtainSemaphore(scoperpsem) -> ; WriteF('in: obtain\n')
  
ENDPROC TRUE

PROC render(ta,x,y,xs,ys,win:PTR TO window) OF scope

  self.radh:=ys-1
  self.yp:=y+self.radh
  self.xpl:=xs/4+x
  self.radw:=xs/4-4
  self.xpr:=xs/2+self.xpl
  scoperp:=win.rport
  SetAPen(scoperp,1)
  RectFill(scoperp,x,y,x+xs-1,y+ys-1)
  scopechange:=TRUE
  ReleaseSemaphore(scoperpsem)
  IF scoperpsem.nestcount THEN ReleaseSemaphore(scoperpsem) ->; WriteF('in:rel\n')
  
ENDPROC TRUE

PROC message_test(imsg:PTR TO intuimessage,win:PTR TO window) OF scope
DEF class:REG,code:REG
  class:=imsg.class
  code:=imsg.code
  IF class AND IDCMP_SIZEVERIFY THEN ObtainSemaphore(scoperpsem)
  IF class AND IDCMP_VANILLAKEY
    IF (code=9) OR (code=ESC_CODE) THEN RETURN TRUE
  ENDIF
ENDPROC FALSE

PROC message_action(class,qual,code,window:PTR TO window) OF scope
 IF class AND IDCMP_VANILLAKEY
   self.code:=code; RETURN TRUE
 ENDIF
ENDPROC FALSE


PROC scopeact(info,sc:PTR TO scope)
  IF sc.code=ESC_CODE
    close_scope(0)
  ELSEIF sc.code=9
    IF gh.wnd
      WindowToFront(gh.wnd); ActivateWindow(gh.wnd)
    ENDIF
  ENDIF
ENDPROC

/*
sincostable:
INCBIN 'sincostable.bin'
*/
/*
    IF sdata:=scopedata
      MOVEQ #0,D1; MOVEQ #0,D2; MOVEQ #0,D3; MOVEQ #0,D4
      MOVE.L sdata,A0; MOVE.L (A0),A0 ; MOVE.L scopelen,D5
      BEQ.S nodatajump; MOVE.L D5,D0; SUBQ.W #1,D0
jestdataloop:
      MOVEM.W (A0)+,D3-D4
          TST.W D3; BPL.S plusleft; NEG.W D3
plusleft: TST.W D4; BPL.S plusright; NEG.W D4
plusright: ADD.L D3,D1; ADD.L D4,D2; DBRA D0,jestdataloop
      DIVU D5,D1; DIVU D5,D2
      LSR.W #7,D1; EXT.L D1;  LSR.W #7,D2; EXT.L D2
nodatajump:
      MOVE.L D1,a; MOVE.L D2,b
    ELSE
      a:=0; b:=0
    ENDIF      
    IF AttemptSemaphore(scoperpsem)
      IF (a<>averl) AND (scopechange=FALSE)
        SetAPen(scoperp,0); Move(scoperp,leftp[ARROWCOORDLEN-2],leftp[ARROWCOORDLEN-1])
        PolyDraw(scoperp,ARROWLEN,leftp)
      ENDIF
      IF (b<>averr) AND (scopechange=FALSE)
        SetAPen(scoperp,0); Move(scoperp,rightp[ARROWCOORDLEN-2],rightp[ARROWCOORDLEN-1])
        PolyDraw(scoperp,ARROWLEN,rightp)
      ENDIF
      IF (scopechange=TRUE) OR (a<>averl)
        IF (a-averl)>10 THEN a:=averl+10
        IF (a-averl)<-10 THEN a:=averl-10
        leftp[0]:=scplug.xpl-(scplug.radw/5*cosinus[a-32]/16384)
        leftp[1]:=scplug.yp-(scplug.radh/5*sinus[a-32]/16384)
        leftp[2]:=scplug.xpl-(scplug.radw*cosinus[a]/16384)
        leftp[3]:=scplug.yp-(scplug.radh*sinus[a]/16384)
        leftp[4]:=scplug.xpl-(scplug.radw/5*cosinus[a+32]/16384)
        leftp[5]:=scplug.yp-(scplug.radh/5*sinus[a+32]/16384)
        leftp[6]:=scplug.xpl; leftp[7]:=scplug.yp
        averl:=a
        SetAPen(scoperp,1); Move(scoperp,leftp[ARROWCOORDLEN-2],leftp[ARROWCOORDLEN-1])
        PolyDraw(scoperp,ARROWLEN,leftp)
      ENDIF
      IF (scopechange=TRUE) OR (b<>averr)
        IF (b-averr)>10 THEN b:=averr+10
        IF (b-averr)<-10 THEN b:=averr-10
        rightp[0]:=scplug.xpr-(scplug.radw/5*cosinus[b-32]/16384)
        rightp[1]:=scplug.yp-(scplug.radh/5*sinus[b-32]/16384)
        rightp[2]:=scplug.xpr-(scplug.radw*cosinus[b]/16384)
        rightp[3]:=scplug.yp-(scplug.radh*sinus[b]/16384)
        rightp[4]:=scplug.xpr-(scplug.radw/5*cosinus[b+32]/16384)
        rightp[5]:=scplug.yp-(scplug.radh/5*sinus[b+32]/16384)
        rightp[6]:=scplug.xpr; rightp[7]:=scplug.yp
        averr:=b
        SetAPen(scoperp,1); Move(scoperp,rightp[ARROWCOORDLEN-2],rightp[ARROWCOORDLEN-1])
        PolyDraw(scoperp,ARROWLEN,rightp)
      ENDIF
      ReleaseSemaphore(scoperpsem)
    ENDIF
*/
