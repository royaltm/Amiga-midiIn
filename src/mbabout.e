OPT MODULE
OPT OSVERSION=37
OPT LARGE

MODULE 'tools/EasyGUI','intuition/intuition','graphics/rastport','intuition/screens',
       'graphics/text','graphics/gfx','graphics/view','layers'


CONST P_WIDTH=256,
      P_HEIGHT=192

EXPORT OBJECT aboutpicture OF plugin
  tmpbitmap:PTR TO bitmap
  colors[64]:ARRAY OF CHAR
  font:PTR TO textfont
  oldregion:LONG
  fillpen:LONG
  shinepen:LONG
  highlightpen:LONG
  obtainpenflag:INT
  regionbottom:INT
  regionright:INT
  texty:INT
  txtoffset:INT
  xd:INT
  lsx:INT
  dx:INT
  mx:INT
ENDOBJECT

EXPORT PROC will_resize() OF aboutpicture IS FALSE

EXPORT PROC min_size(ta:PTR TO textattr,fontheight) OF aboutpicture IS P_WIDTH,P_HEIGHT+fontheight

EXPORT PROC render(ta:PTR TO textattr,x,y,xs,ys,win:PTR TO window) OF aboutpicture
DEF cmap,ptr:PTR TO CHAR,src:PTR TO CHAR,n:REG,r:REG,g:REG,b:REG,colour:REG,
    temprp:rastport, rp:PTR TO rastport,
    array[P_WIDTH]:ARRAY OF CHAR, tmparray[P_WIDTH]:ARRAY OF CHAR,
    region=0, txex:textextent

  ptr:={palette}
  cmap:=win.wscreen.viewport.colormap
  IF KickVersion(39)
    self.obtainpenflag:=TRUE
    FOR n:=0 TO 63
      MOVE.L ptr,A0
      MOVEQ #0,D0; MOVE.B (A0)+,D0; MOVE.L D0,r; SWAP D0; MOVE.W r,D0; MOVE.L D0,r; LSL.L #8,D0; OR.L D0,r
      MOVEQ #0,D0; MOVE.B (A0)+,D0; MOVE.L D0,g; SWAP D0; MOVE.W g,D0; MOVE.L D0,g; LSL.L #8,D0; OR.L D0,g
      MOVEQ #0,D0; MOVE.B (A0)+,D0; MOVE.L D0,b; SWAP D0; MOVE.W b,D0; MOVE.L D0,b; LSL.L #8,D0; OR.L D0,b
      MOVE.L A0,ptr
      EXIT -1 = (colour:=ObtainBestPenA(cmap,r,g,b,[OBP_PRECISION,PRECISION_EXACT,NIL,NIL]))
      self.colors[n]:=colour
    ENDFOR
  ELSE
    colour:= -1
  ENDIF
  IF colour = -1
    FOR n:=0 TO 63 DO self.colors[n]:=self.fillpen
    self.colors[1]:=self.highlightpen
    self.colors[2]:=self.shinepen
    self.obtainpenflag:=FALSE
  ENDIF

  src:={picturebody}
  stdrast:=rp:=win.rport
  IF self.font <> NIL THEN SetFont(rp, self.font)
  self.regionbottom:=y+rp.txheight-1
  self.regionright:=x+xs-1
  self.texty:=y+rp.txbaseline
  Box(x,y,self.regionright,self.regionbottom,self.fillpen)

  CopyMem(rp,temprp,SIZEOF rastport); temprp.layer:=NIL;  temprp.bitmap:=self.tmpbitmap
  cmap:=self.colors
  FOR n:=0 TO P_HEIGHT-1
    MOVE.L src,A0; MOVE.L tmparray,A1; LEA P_WIDTH/8*6(A1),A2
bodyloop:
    MOVEQ  #0,D0;  MOVE.B (A0)+,D0;  BPL.S bodycopy;  NEG.B D0;  MOVE.B (A0)+,D1
bodyfill:
    MOVE.B  D1,(A1)+;    DBRA    D0,bodyfill
    CMPA.L  A2,A1;    BCS.S   bodyloop;    BRA.S   bodyend
bodycopy:
    MOVE.B  (A0)+,(A1)+;    DBRA    D0,bodycopy
    CMPA.L  A2,A1;    BCS.S   bodyloop
bodyend:
    MOVE.L  A0,src

    r:=P_WIDTH/8
    MOVE.L  tmparray,A0;    MOVE.L  array,A1;    LEA  P_WIDTH(A1),A2
chunkyloop:
    MOVEQ   #15,D2
chunkypixloop:
    MOVE.L  A0,A3; MOVEQ  #5,D1
chunkywordloop:
    LSL.W   #1,(A3);    ROXR.B  #1,D0;  ADD.L r,A3; DBRA D1,chunkywordloop
  LSR.B   #2,D0
    MOVE.B  D0,(A1)+
    DBRA    D2,chunkypixloop
    ADDQ.L  #2,A0
    CMPA.L  A2,A1
    BCS.S   chunkyloop

    MOVE.L array,A0; MOVE.L cmap,A2
    MOVE.W #P_WIDTH-1,D1; MOVEQ #0,D0
renderloop:
    MOVE.B (A0),D0; MOVE.B 0(A2,D0.W),(A0)+; DBRA D1,renderloop
    WritePixelLine8(rp,x,self.regionbottom+n+1,P_WIDTH,array,temprp)
  ENDFOR

  IF region:=NewRegion()
    IF OrRectRegion(region,[x,y,self.regionright,self.regionbottom]:rectangle)
      region:=InstallClipRegion(win.wlayer,region)
    ELSE
      DisposeRegion(region); region:=0
    ENDIF
  ENDIF
  self.oldregion:=region
  SetAPen(stdrast,self.highlightpen); SetBPen(stdrast,self.fillpen)
  IF n:=self.txtoffset
    IF r:=TextFit(rp,{scrollertxt}+n-1, n, txex, NIL, -1, 
                  self.regionright+self.xd-x+2, 32767)
      Move(rp,self.regionright+self.xd-txex.width+1,self.texty)
      Text(rp,{scrollertxt}+n-r,r)
    ENDIF
  ENDIF
ENDPROC

EXPORT PROC clear_render(win:PTR TO window) OF aboutpicture
DEF cmap,i,colors:PTR TO CHAR,region
  IF self.obtainpenflag
    cmap:=win.wscreen.viewport.colormap
    colors:=self.colors
    FOR i:=0 TO 63 DO ReleasePen(cmap,colors[i])
  ENDIF
  IF region:=InstallClipRegion(win.wlayer,self.oldregion) THEN DisposeRegion(region)
ENDPROC

EXPORT PROC message_test(imsg:PTR TO intuimessage,win:PTR TO window) OF aboutpicture
DEF class,dx:REG,x:REG,y:REG,code
  class:=imsg.class
  IF class AND (IDCMP_INTUITICKS OR IDCMP_RAWKEY OR IDCMP_VANILLAKEY OR IDCMP_INACTIVEWINDOW)
    RETURN TRUE
  ELSEIF class AND IDCMP_MOUSEMOVE
    x:=imsg.mousex; IF (dx:=self.mx-x) <= 0 THEN dx:=1
    IF dx > 25 THEN dx:=25
    self.mx:=x; self.dx:=dx
    RETURN IF dx > 1 THEN TRUE ELSE FALSE
  ELSEIF class AND IDCMP_MOUSEBUTTONS
    x:=imsg.mousex; y:=imsg.mousey
    IF (code:=imsg.code)=SELECTDOWN
      IF (x>=self.x) AND (x<=self.regionright) AND (y>=self.y) AND (y<=self.regionbottom)
        self.mx:=x; ReportMouse(TRUE,win); RETURN FALSE
      ENDIF
    ELSEIF code=SELECTUP
      ReportMouse(FALSE,win); RETURN FALSE
    ENDIF
    RETURN TRUE
  ENDIF
ENDPROC FALSE

EXPORT PROC message_action(class,qual,code,win:PTR TO window) OF aboutpicture
DEF rp:PTR TO rastport,txt,l,t,v,dx
  rp:=stdrast:=win.rport
  IF class AND (IDCMP_INTUITICKS OR IDCMP_MOUSEMOVE)
    txt:={scrollertxt}
    ScrollRaster(rp, dx:=self.dx, 0, self.x, self.y, self.regionright, self.regionbottom)
    t:=self.txtoffset; l:=self.xd-dx
    REPEAT
      Move(rp,self.regionright+l+1,self.texty); Text(rp,txt+t,1)
      IF (v:=l+self.lsx) <= 0
        l:=v; t++; IF txt[t]=0 THEN t:=0
        self.lsx:=TextLength(rp,txt+t,1)
      ENDIF
    UNTIL v >= 0
    self.xd:=l; self.txtoffset:=t; DEC dx; self.dx:=IF dx < 1 THEN 1 ELSE dx
    RETURN FALSE
  ENDIF
ENDPROC TRUE

EXPORT PROC init(screen) OF aboutpicture
DEF bitmap=0:PTR TO bitmap, scr:PTR TO screen, drinfo:PTR TO drawinfo,penless,x:PTR TO INT,
    font=NIL,raster,i
  IF KickVersion(39)
    IF 0=(bitmap:=AllocBitMap(P_WIDTH,1,8,BMF_CLEAR OR BMF_INTERLEAVED, NIL)) THEN Raise("MEM")
  ELSE
    NEW bitmap; InitBitMap(bitmap, 8, P_WIDTH, 1 )
    IF raster:=AllocRaster(P_WIDTH, 8 )
      FOR i:=0 TO 7 DO bitmap.planes[i]:=P_WIDTH/8*i+raster
    ELSE
      END bitmap; Raise("MEM")
    ENDIF
  ENDIF
  self.tmpbitmap:=bitmap
  IF screen=NIL THEN scr:=LockPubScreen(NIL) ELSE scr:=screen
  self.shinepen:=2; self.fillpen:=3; self.highlightpen:=2
  IF scr
    IF drinfo:=GetScreenDrawInfo(scr)
      penless:=drinfo.numpens
      x:=drinfo.pens
      IF penless>FILLPEN THEN self.fillpen:=x[FILLPEN]
      IF penless>SHINEPEN THEN self.shinepen:=x[SHINEPEN]
      IF penless>HIGHLIGHTTEXTPEN THEN self.highlightpen:=x[HIGHLIGHTTEXTPEN]
      IF font=NIL THEN font:=drinfo.font
      FreeScreenDrawInfo(scr,drinfo)
    ENDIF
    IF font=NIL THEN font:=scr.font
    IF screen=0 THEN UnlockPubScreen(NIL,scr)
  ENDIF
  self.font:=font
  self.txtoffset:=0; self.lsx:=0; self.xd:=0; self.dx:=1
ENDPROC

EXPORT PROC end() OF aboutpicture
DEF bitmap:PTR TO bitmap
  IF bitmap:=self.tmpbitmap
    self.tmpbitmap:=0
    IF KickVersion(39)
      FreeBitMap(bitmap)
    ELSE
      FreeRaster(bitmap.planes[], P_WIDTH, 8)
      END bitmap
    ENDIF
  ENDIF
ENDPROC

scrollertxt:
  INCBIN 'abouttext.bin'
picturebody:
  INCBIN 'bodyraw.bin'
palette:
  INCBIN 'cmapraw.bin'

