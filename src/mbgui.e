OPT MODULE
OPT OSVERSION=37


MODULE 'tools/EasyGUI','intuition/intuition','intuition/screens',
       'graphics/text','graphics/gfx','graphics/view','graphics/gfxbase',
       'devices/inputevent','*mbkeycod','*drawpattern','exec/libraries'


EXPORT ENUM KB_NONE=0,KB_FUN,KB_SPACE,KB_LOOP,KB_DUR,KB_RETURN,KB_PRI,KB_ELSE

ENUM KEY_REFRESH,KEY_SET, KEY_UNSET, KEY_PRHI, KEY_PRLO, KEY_PRNO,
     KEY_PLAYON, KEY_PLAYOFF, KEY_CURRENT

SET STS_LO, STS_HI, STS_SET, STS_PLAY
CONST STS_CURRENT=STS_LO OR STS_HI OR STS_SET OR STS_PLAY,
      STSMASK_PLAY=STS_PLAY,
      STSMASK_RANGE=STS_HI OR STS_LO OR STS_SET,
      STSMASK_STATUS=STS_PLAY OR STS_HI OR STS_LO OR STS_SET,
      STSBITS_SHIFT=4
EXPORT CONST RANGE_HIPRI=STS_HI, RANGE_LOPRI=STS_LO

EXPORT OBJECT pianokeys OF plugin
  keycode:LONG
PRIVATE
  rport:LONG
  darkpen:LONG
  activepen:LONG
  backpen:LONG
  bgrnpen:LONG
  currentpen:LONG
  shadowpen:LONG
  ptrnalloc:LONG
  shinepen:LONG
  xm:INT
  ym:INT
  xx:INT
  yy:INT
  xsize:INT
  keysize:INT
  smallsize:INT
  windowreport:INT
  currentkey:CHAR
  loarea:CHAR
  hiarea:CHAR
  boundset:CHAR
  boundselect:CHAR
  allocpens:CHAR
  status[128]:ARRAY OF CHAR
  playingkeys[128]:ARRAY OF CHAR
ENDOBJECT

EXPORT DEF basesetb

DEF lasttick

EXPORT PROC min_size(ta,fh) OF pianokeys IS 462,38

EXPORT PROC init(screen) OF pianokeys
DEF drinfo:PTR TO drawinfo,scr:PTR TO screen,penless,x:PTR TO INT,f:REG
    

  FOR f:=0 TO 127 ; self.status[f]:=0; self.playingkeys[f]:=0; ENDFOR
  lasttick:=0
  self.keycode:=-1
  self.boundset:=0
  self.boundselect:=0
  self.currentkey:=255
  self.loarea:=255
  self.hiarea:=127
  self.darkpen:=1
  self.activepen:=2
  self.backpen:=0
  self.bgrnpen:=0
  self.currentpen:=3
  self.shadowpen:=0
  self.allocpens:=0
  self.shinepen:=3
  IF screen=0 THEN scr:=LockPubScreen(NIL) ELSE scr:=screen
  IF scr
    IF drinfo:=GetScreenDrawInfo(scr)
      penless:=drinfo.numpens
      x:=drinfo.pens
      IF penless>SHADOWPEN THEN self.darkpen:=x[SHADOWPEN]
      IF penless>FILLPEN THEN self.activepen:=x[FILLPEN]
      IF penless>BACKGROUNDPEN THEN self.backpen:=x[BACKGROUNDPEN]
      IF penless>BACKGROUNDPEN THEN self.bgrnpen:=x[BACKGROUNDPEN]
      IF penless>BACKGROUNDPEN THEN self.shadowpen:=x[BACKGROUNDPEN]
      IF penless>HIGHLIGHTTEXTPEN THEN self.currentpen:=x[HIGHLIGHTTEXTPEN]
      IF penless>SHINEPEN THEN IF x[SHINEPEN]<>self.currentpen THEN self.backpen:=x[SHINEPEN]
      IF penless>SHINEPEN THEN self.shinepen:=x[SHINEPEN]
      FreeScreenDrawInfo(scr,drinfo)
    ENDIF
    IF screen=0 THEN UnlockPubScreen(NIL,scr)
  ENDIF


-> WriteF('curr=\d act=\d\n',self.currentpen,self.activepen)
ENDPROC

EXPORT PROC clear_render(win:PTR TO window) OF pianokeys
DEF x,cmap
  self.rport:=0
  IF self.allocpens<>0
    cmap:=win.wscreen.viewport.colormap; x:=1
    IF self.allocpens AND x THEN ReleasePen(cmap,self.darkpen)
    x:=Shl(x,1)
    IF self.allocpens AND x THEN ReleasePen(cmap,self.activepen)
    x:=Shl(x,1)
    IF self.allocpens AND x THEN ReleasePen(cmap,self.backpen)
    x:=Shl(x,1)
    IF self.allocpens AND x THEN ReleasePen(cmap,self.currentpen)
    x:=Shl(x,1)
    IF self.allocpens AND x THEN ReleasePen(cmap,self.shadowpen)
    self.allocpens:=0
  ENDIF
  self.ptrnalloc:=freepattern(self.ptrnalloc)
ENDPROC

EXPORT PROC render(ta,x,y,xs,ys,win:PTR TO window) OF pianokeys
DEF keysize,smallkey,xsize,f,xx:REG,yy:REG,t,b,sh,c,bsize:REG,yn:REG,xn:REG,
    graphicsbase:PTR TO lib,cmap,col,bit, pt:PTR TO INT,x1,x2,y1,y2

  graphicsbase:=gfxbase
  IF graphicsbase.version >=39
    cmap:=win.wscreen.viewport.colormap; bit:=1
    IF (col:=ObtainBestPenA(cmap,$0,$0,$0,[OBP_PRECISION,PRECISION_EXACT,NIL,NIL]))>=0 THEN self.darkpen:=col
    IF col>=0 THEN self.allocpens:=bit; bit:=Shl(bit,1)
    IF (col:=ObtainBestPenA(cmap,$00000000,$88888888,$FFFFFFFF,[OBP_PRECISION,PRECISION_EXACT,NIL,NIL]))>=0 THEN self.activepen:=col
    IF col>=0 THEN self.allocpens:=self.allocpens OR bit; bit:=Shl(bit,1)
    IF (col:=ObtainBestPenA(cmap,$FFFFFFFF,$FFFFFFFF,$FFFFFFFF,[OBP_PRECISION,PRECISION_EXACT,NIL,NIL]))>=0 THEN self.backpen:=col
    IF col>=0 THEN self.allocpens:=self.allocpens OR bit; bit:=Shl(bit,1)
    IF (col:=ObtainBestPenA(cmap,$FFFFFFFF,$C0C0C0C0,$00000000,[OBP_PRECISION,PRECISION_EXACT,NIL,NIL]))>=0 THEN self.currentpen:=col
    IF col>=0 THEN self.allocpens:=self.allocpens OR bit; bit:=Shl(bit,1)
    IF (col:=ObtainBestPenA(cmap,$80808080,$80808080,$80808080,[OBP_PRECISION,PRECISION_EXACT,NIL,NIL]))>=0 THEN self.shadowpen:=col
    IF col>=0 THEN self.allocpens:=self.allocpens OR bit
    self.ptrnalloc:=drawpattern(win.rport,cmap,x,y,xs,ys,{patterncmap},{pattern},100,100)
  ENDIF
  t:=self.darkpen
  b:=self.backpen
  sh:=self.shadowpen

  keysize:=ys/4*3
  smallkey:=keysize/3*2-1
  xsize:=(xs-12)/75
  bsize:=xsize/3

  self.xsize:=xsize

  xx:=(xs-(xsize*75))/2+x;  yy:=ys-keysize+y
  self.xx:=xx
  self.yy:=yy

  DEC keysize
  self.keysize:=keysize

  self.rport:=stdrast:=win.rport

  IF self.ptrnalloc=0 THEN Box(x,y,x+xs-1,y+ys-1,self.activepen)
  Box(xx-1,yy-9,xx+33,yy-1,t)
  Line(xx+34,yy-1,xx+(xsize*75)+1,yy-1,t)
  Line(xx-1,yy,xx-1,yy+keysize-2,t)
  Line(xx+(xsize*75)+1,yy,xx+(xsize*75)+1,yy+keysize-2,t)

  Line(x,y,x+xs-2,y,self.shinepen)
  Line(x,y+1,x,y+ys-1,self.shinepen)
  Line(x+1,y+ys-1,x+xs-1,y+ys-1,self.darkpen)
  Line(x+xs-1,y,x+xs-1,y+ys-1,self.darkpen)

  pt:={midiin_text}
  yn:=yy-7
  WHILE (c:=pt[]++)<>-1
    WHILE (x1:=pt[]++)<>-1
      y1:=pt[]++; x2:=pt[]++; y2:=pt[]++
      Line(xx+x1+c+1,yn+y1,xx+x2+c+1,yn+y2,self.currentpen)
    ENDWHILE
  ENDWHILE

  c:=0; yn:=yy+keysize-1; xn:=xx+xsize-1
  Box(xx+1,yy,74*xsize+xn,yn,b)
  FOR f:=0 TO 74
    Line(xx,yy,xx,yn,t); Line(xx+2,yn+1,xn,yn+1,t)
    Line(xx+2,yn,xn,yn,sh); Line(xx,yn+1,xx+1,yn+1,sh)
    IF c AND (c<>3)   ->  ### (c=1) OR (c=2) OR (c=4) OR (c=5) OR (c=6)
      Box(xx-bsize,yy+1,xx+bsize,yy+smallkey,t); Plot(xx-bsize+1,yy+2,b)
      Plot(xx-bsize,yy+1,sh)
    ENDIF
    INC c; IF c>6 THEN c:=0
    MOVE.L  xsize,D0
    ADD.L   D0,xx;  ADD.L D0,xn
  ENDFOR
  Line(xx,yy,xx,yy+keysize-1,t)
  self.smallsize:=smallkey

  FOR f:=0 TO 127 DO self.pushkey(f,KEY_REFRESH)

ENDPROC TRUE


EXPORT PROC keypressed(key=-1) OF pianokeys ->-1 only reads 0-127 sets key >127 unsets key
DEF x:REG
  x:=self.currentkey
  IF key >= 0 THEN self.pushkey(key AND 255,KEY_CURRENT)
ENDPROC IF x<128 THEN x ELSE -1

EXPORT PROC bounds(l=-1,h=127,rangeother=NIL) OF pianokeys -> () returns only data, (>127) unsets bounds
DEF lo:REG,hi:REG,f:REG
  lo:=self.loarea;  hi:=self.hiarea
  IF l >= 0
    IF l > 127
      self.loarea:=255; self.hiarea:=127; self.boundselect:=0; self.boundset:=0
      IF lo < 128 THEN FOR f:=0 TO 127 DO self.pushkey(f,KEY_UNSET)
    ELSEIF l <= h
      self.loarea:=l; self.hiarea:=h
      IF rangeother <> NIL
        FOR f:=0 TO 127
          IF rangeother[f]=STS_LO
            self.status[f]:=self.status[f] AND Not(STS_LO OR STS_HI) OR STS_LO
          ELSEIF rangeother[f]=STS_HI
            self.status[f]:=self.status[f] AND Not(STS_LO OR STS_HI) OR STS_HI
          ELSE
            self.status[f]:=self.status[f] AND Not(STS_LO OR STS_HI)
          ENDIF
          self.pushkey(f,IF (f>=l) AND (f<=h) THEN KEY_SET ELSE KEY_UNSET)
        ENDFOR
      ELSE
        IF l > 0 THEN FOR f:=0 TO l-1 DO self.pushkey(f,KEY_UNSET)
        FOR f:=l TO h DO self.pushkey(f,KEY_SET)
        IF h < 127 THEN FOR f:=h+1 TO 127 DO self.pushkey(f,KEY_UNSET)
      ENDIF
    ENDIF
  ENDIF
  IF (lo<128) AND (lo<=hi) THEN RETURN lo,hi
ENDPROC -1

EXPORT PROC boundset(set) OF pianokeys
  IF set THEN self.boundset:=1 ELSE self.boundset:=0
  self.boundselect:=0
ENDPROC

EXPORT PROC setplaying(x,on=TRUE) OF pianokeys
  IF (x AND 127) <> x THEN RETURN
  self.pushkey(x,IF on THEN KEY_PLAYON ELSE KEY_PLAYOFF)
ENDPROC

EXPORT PROC message_test(imsg:PTR TO intuimessage,win:PTR TO window) OF pianokeys
  DEF code:REG,class:REG,xm:REG,ym:REG

  code:=imsg.code
  class:=imsg.class
  IF class AND (IDCMP_MOUSEBUTTONS OR IDCMP_MOUSEMOVE)
    xm:=imsg.mousex-self.xx
    ym:=imsg.mousey-self.yy
    IF code = SELECTDOWN
      IF (xm < 0) OR (ym < 0) OR (xm >= (self.xsize*75) ) OR (ym >= self.keysize) THEN RETURN FALSE
    ENDIF
    IF (code = SELECTUP) OR (class AND IDCMP_MOUSEMOVE)
      IF self.windowreport=FALSE THEN RETURN FALSE
    ENDIF
    self.xm:=xm; self.ym:=ym; RETURN TRUE
  ELSEIF class AND IDCMP_RAWKEY
      IF (code >= CURSORUP) AND (code <= KEYCODE_F10) THEN RETURN TRUE
  ELSEIF class AND IDCMP_VANILLAKEY
      SELECT 128 OF code
        CASE "0" TO "9"; RETURN TRUE
        CASE "."; RETURN TRUE
        CASE "_"; RETURN TRUE
        CASE ")"; RETURN TRUE
        CASE "("; RETURN TRUE
        CASE "-"; RETURN TRUE
        CASE "+"; RETURN TRUE
        CASE "="; RETURN TRUE
        CASE "*"; RETURN TRUE
        CASE 9  ; RETURN TRUE
        CASE 13 ; RETURN TRUE
        CASE 32 ; RETURN TRUE
        CASE DEL_CODE ; RETURN TRUE
        DEFAULT; RETURN FALSE
      ENDSELECT
  ELSEIF class AND IDCMP_INTUITICKS
      IF basesetb THEN RETURN TRUE
  ELSEIF class AND IDCMP_INACTIVEWINDOW
      RETURN TRUE
  ENDIF
ENDPROC FALSE

EXPORT PROC message_action(class,qual,code,win:PTR TO window) OF pianokeys
DEF xsize:REG,xm:REG,ym,oct:REG,mod:REG,key:REG

  IF class AND IDCMP_RAWKEY
    self.keycode:=code OR MYRAWCODE OR IF qual AND (IEQUALIFIER_LSHIFT OR IEQUALIFIER_RSHIFT) THEN SHIFTQUAL ELSE 0
  ELSEIF class AND IDCMP_VANILLAKEY
    self.keycode:=code
  ELSEIF class AND IDCMP_INTUITICKS
    IF basesetb
      IF lasttick
        self.pushkey(255,KEY_CURRENT)
      ELSE
        self.pushkey(self.currentkey,KEY_CURRENT)
      ENDIF
      lasttick:=Not(lasttick)
    ENDIF
    RETURN FALSE
  ELSEIF class AND IDCMP_INACTIVEWINDOW
    ReportMouse(FALSE,win); self.windowreport:=FALSE
    IF basesetb;      self.keycode:= -2 ;      RETURN TRUE;  ENDIF
    RETURN FALSE
  ELSEIF class AND (IDCMP_MOUSEBUTTONS OR IDCMP_MOUSEMOVE)

    self.keycode:=-1

    IF code = SELECTUP
      IF self.windowreport THEN ReportMouse(FALSE,win)
      self.windowreport:=FALSE
      IF self.boundselect=0 THEN RETURN FALSE
    ENDIF
    IF class AND IDCMP_MOUSEMOVE
      IF self.windowreport THEN ReportMouse(FALSE,win)
      IF self.boundselect=0
        self.windowreport:=FALSE; RETURN FALSE
      ENDIF
      code:=SELECTDOWN
    ENDIF

    xm:=self.xm
    ym:=self.ym
    IF (xm < 0) OR (ym < 0) OR (xm >= (self.xsize*75) ) OR (ym >= self.keysize) THEN RETURN FALSE


    xsize:=self.xsize

    ym:=IF ym < self.smallsize THEN TRUE ELSE FALSE

    key:=-1
    IF ym
      mod:=(xm+(xsize/2))/xsize
      oct:=mod/7
      mod:=Mod(mod,7)
      IF mod<>3 THEN key:=mod*2-(mod/4)-1
    ENDIF

    IF key < 0
      mod:=xm/xsize
      oct:=mod/7
      mod:=Mod(mod,7)
      key:=mod*2-((mod+1)/4)
    ENDIF

    key:=key+(oct*12)
    IF key > 127 THEN key:=127

    self.autokey(key,code)

    IF code = SELECTDOWN
      self.windowreport:=TRUE
      ReportMouse(TRUE,win)
    ENDIF

  ELSE
    RETURN FALSE
  ENDIF

ENDPROC TRUE

EXPORT PROC autokey(key,code) OF pianokeys
DEF lo:REG,hi:REG,bnd:REG,f:REG

  IF (code = SELECTDOWN) THEN self.boundselect:=0
  lo:=self.loarea; hi:=self.hiarea
  IF self.boundset>0
    bnd:=self.boundselect
    IF bnd=0  -> means key/extrdowns
      IF lo<128
        IF ((hi-lo)=1) AND (key=lo)
          self.hiarea:=key; self.boundselect:=2
          self.pushkey(hi,KEY_UNSET)
        ELSEIF (Abs(key-lo) < Abs(key-hi)) OR ((lo=hi) AND (key<=lo))
          self.loarea:=key; self.boundselect:=1
          IF key<lo THEN FOR f:=key TO lo-1 DO self.pushkey(f,KEY_SET)
          IF lo<key THEN FOR f:=lo TO key-1 DO self.pushkey(f,KEY_UNSET)
        ELSE
          self.hiarea:=key; self.boundselect:=2
          IF key>hi THEN FOR f:=hi+1 TO key DO self.pushkey(f,KEY_SET)
          IF hi>key THEN FOR f:=key+1 TO hi DO self.pushkey(f,KEY_UNSET)
        ENDIF
      ENDIF
    ELSEIF bnd=1  -> means keyup
      IF key>hi THEN key:=hi
      self.loarea:=key;  self.boundselect:=0
      IF key<lo THEN FOR f:=key TO lo-1 DO self.pushkey(f,KEY_SET)
      IF lo<key THEN FOR f:=lo TO key-1 DO self.pushkey(f,KEY_UNSET)
    ELSEIF bnd=2  -> means keyup
      IF key<lo THEN key:=lo
      self.hiarea:=key; self.boundselect:=0
      IF key>hi THEN FOR f:=hi+1 TO key DO self.pushkey(f,KEY_SET)
      IF hi>key THEN FOR f:=key+1 TO hi DO self.pushkey(f,KEY_UNSET)
    ELSE
      self.boundselect:=0
    ENDIF
  ELSE
    self.pushkey(key,KEY_CURRENT)
  ENDIF

ENDPROC TRUE

PROC pushkey(key,col) OF pianokeys
DEF x:REG,col1,col2,col3,a,k:REG,bsize,xsize:REG,y:REG,smallsize:REG,top,bottom,
    bt2=0,tp2=0

  k:=key AND 127

  x:=self.status[k]
  SELECT col
  CASE KEY_SET
    x:=x OR STS_SET
  CASE KEY_UNSET
    x:=x AND Not(STS_SET)
  CASE KEY_PRHI
    x:=x AND Not(STS_LO) OR STS_HI
  CASE KEY_PRLO
    x:=x AND Not(STS_HI) OR STS_LO
  CASE KEY_PRNO
    x:=x AND Not(STS_HI OR STS_LO)
  CASE KEY_PLAYON
    self.playingkeys[k]:=self.playingkeys[k]+1;
    x:=x OR STS_PLAY
  CASE KEY_PLAYOFF
    IF (a:=self.playingkeys[k]-1) <= 0
      x:=x AND Not(STS_PLAY); a:=0
    ELSE
      x:=x OR STS_PLAY
    ENDIF
    self.playingkeys[k]:=a
  CASE KEY_CURRENT
    k:=self.currentkey; self.currentkey:=key
    IF (key <> k) AND (k < 128) THEN self.pushkey(k,KEY_REFRESH)
    IF key > 127; self.currentkey:=k; RETURN; ENDIF
    k:=key
  DEFAULT
    IF col<>KEY_REFRESH THEN RETURN
  ENDSELECT

  IF col<>KEY_REFRESH
    y:=Shr(x,STSBITS_SHIFT) AND STSMASK_STATUS
    IF k=self.currentkey
      IF y=STS_CURRENT; self.status[k]:=x; RETURN; ENDIF
    ELSEIF self.loarea > 127
      IF (x AND STSMASK_PLAY)=y; self.status[k]:=x; RETURN; ENDIF
    ELSE
      IF (x AND STSMASK_RANGE)=y; self.status[k]:=x; RETURN; ENDIF
      IF (x AND STS_SET)
        IF (STSMASK_RANGE-STS_LO AND x) = (y AND Not(STS_LO)); self.status[k]:=x; RETURN; ENDIF
      ENDIF
    ENDIF
  ENDIF
  
  smallsize:=self.smallsize
  top:=2; bottom:=self.keysize-2

  IF k=self.currentkey
    col1:=self.currentpen
    col2:=self.currentpen
    y:=STS_CURRENT
  ELSE
    y:=x AND STSMASK_STATUS
    IF self.loarea > 127
      IF (y:=y AND STSMASK_PLAY)
        col1:=self.activepen
        col2:=col1
      ELSE
        col1:=self.backpen
        col2:=self.darkpen
      ENDIF
    ELSE
      y:=y AND STSMASK_RANGE
      col1:=self.backpen
      col2:=self.darkpen
      col3:=self.activepen
      IF y AND STS_SET
        IF y AND STS_HI THEN tp2:=2 ELSE tp2:=smallsize/2
        bt2:=(self.keysize-smallsize)/2+smallsize
      ELSEIF y AND STS_HI
        tp2:=2;  bt2:=smallsize/2+1
      ELSEIF y AND STS_LO
        tp2:=smallsize/2+2;  bt2:=smallsize-1
      ENDIF
    ENDIF
  ENDIF
  self.status[k]:=x AND STSMASK_STATUS OR Shl(y,STSBITS_SHIFT)


  IF (stdrast:=self.rport)=0 THEN RETURN

  xsize:=self.xsize
  bsize:=xsize/3
  x:=k/12*(7*xsize)+self.xx
  y:=self.yy

  k:=Mod(k,12)
  IF Shl(1,k) AND %010101001010
    DEC bsize
    x:=((k+2)/2)*xsize+x
    Box(x-bsize,y+top,x+bsize,y+smallsize-1,col2)
    IF bt2 THEN Box(x-bsize,y+tp2,x+bsize,y+IF (bt2 >= smallsize) THEN (smallsize-1) ELSE bt2,col3)
    Plot(x-bsize,y+3,self.shadowpen)
    Plot(x-bsize,y+2,self.backpen)
  ELSE
    INC bsize
    k:=(k+1)/2
    x:=k*xsize+x
    a:=Mod(k-(k/5),3)

    Box(x+(IF (a+1) AND 2 THEN bsize ELSE 1),y+top,x+xsize-(IF a AND 2 THEN 1 ELSE bsize),y+smallsize,col1)
    IF bt2 THEN  Box(x+(IF (a+1) AND 2 THEN bsize ELSE 1),y+tp2,x+xsize-(IF a AND 2 THEN 1 ELSE bsize),y+bt2,col3)
    Box(x+1,y+smallsize+1,x+xsize-1,y+bottom,col1)
    IF bt2 > smallsize THEN Box(x+1,y+smallsize+1,x+xsize-1,y+bt2,col3)

  ENDIF
ENDPROC

midiin_text:
INT 0, 0,0,0,4, 1,3,1,4, 1,0,2,0, 3,1,4,1, 5,0,6,0, 7,0,7,4, 6,3,6,4, -1
INT 9, 0,0,0,4, 1,3,1,4, -1
INT 12,0,0,0,2, 0,4,2,4, 0,3,1,3, 1,0,2,0, 3,1,4,2, 4,3,3,4, -1
INT 18, 0,3,0,4, 1,0,1,4, -1
INT 21, 0,0,2,0, 1,1,1,2, 1,3,2,3, 0,4,2,4, -1
INT 25, 0,0,0,4, 1,3,1,4, 1,1,3,3, 4,3,4,4, 5,0,5,4, -1
INT -1

pattern:
INCBIN 'mbguichunky.bin'
patterncmap:
LONG $00100000	/* Record Header */
LONG $00000000,$00000000,$00000000
LONG $FFFFFFFF,$FFFFFFFF,$FFFFFFFF
LONG $EBBBBBBB,$EAAAAAAA,$F6666666
LONG $D9999999,$D8888888,$EFFFFFFF
LONG $C7777777,$C6666666,$E7777777
LONG $B5555555,$B5555555,$DFFFFFFF
LONG $A5555555,$A4444444,$D7777777
LONG $96666666,$95555555,$D0000000
LONG $84444444,$86666666,$C6666666
LONG $69999999,$68888888,$ACCCCCCC
LONG $53333333,$50000000,$93333333
LONG $41111111,$3CCCCCCC,$7CCCCCCC
LONG $31111111,$29999999,$63333333
LONG $23333333,$1AAAAAAA,$4BBBBBBB
LONG $15555555,$0FFFFFFF,$32222222
LONG $0BBBBBBB,$06666666,$1AAAAAAA
LONG $0
