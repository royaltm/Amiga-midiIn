OPT MODULE
OPT OSVERSION=37


MODULE 'tools/EasyGUI','graphics/text','graphics/rastport',
       'intuition/screens','intuition/intuition',
       'exec/libraries', '*MBbanks'


SET BS_ENABLED, BS_SETLO, BS_SETFULL

ENUM SETCURRENT=$FFFF, SETOTHER

ENUM MOVEMENT_NONE=0, MOVEMENT_SET, MOVEMENT_DRAG

EXPORT ENUM LSETNONE=0, LSETINNER, LSETFULL

EXPORT ENUM LEDACT_SETBANK=1, LEDACT_SETRANGE, LEDACT_MOVERANGE

EXPORT OBJECT leds OF plugin
  act:LONG
PRIVATE
  rport:PTR TO rastport
  win:LONG
  select:INT
  dragpos:INT
  ledx:INT
  ledh:INT
  margl:INT
  margt:INT
  nextrow:INT
  current:CHAR
  lorange:CHAR  ->  [====
  hirange:CHAR  ->  =====]
  movement:CHAR -> [none | setting range | dragging range]
  movepos:CHAR
  bankstat[NUMBANKS]:ARRAY OF CHAR
  colrinner:CHAR
  colrback:CHAR
  colrcurrent:CHAR
  colrset:CHAR
ENDOBJECT

DEF shadowpen,shinepen

PROC min_size(ta,fh) OF leds IS 8*(NUMBANKS/2),fh*2

PROC init(screen) OF leds
	DEF drinfo:PTR TO drawinfo,scr,penless,x:PTR TO INT,f

  self.current:=0; self.lorange:=0; self.hirange:=0; self.movement:=MOVEMENT_NONE
  self.select:=-1; self.dragpos:=-1; self.movepos:=0
  FOR f:=0 TO NUMBANKS-1 DO self.bankstat[f]:=0
  self.colrinner:=1
  self.colrset:=3
  self.colrback:=0
  self.colrcurrent:=2
  IF screen=0 THEN scr:=LockPubScreen(NIL) ELSE scr:=screen
  IF scr
  	IF drinfo:=GetScreenDrawInfo(scr)
  		penless:=drinfo.numpens
  		x:=drinfo.pens
  		IF penless>SHINEPEN THEN shinepen:=x[SHINEPEN]
  		IF penless>SHADOWPEN THEN shadowpen:=x[SHADOWPEN]
  		IF penless>FILLPEN THEN self.colrset:=x[FILLPEN]
  		IF penless>SHINEPEN THEN self.colrinner:=x[SHINEPEN]
  		IF penless>BACKGROUNDPEN THEN self.colrback:=x[BACKGROUNDPEN]
  		IF penless>HIGHLIGHTTEXTPEN THEN self.colrcurrent:=x[HIGHLIGHTTEXTPEN]
      IF self.colrinner=self.colrcurrent THEN self.colrinner:=1
  		FreeScreenDrawInfo(scr,drinfo)
  	ENDIF
    IF screen=0 THEN UnlockPubScreen(NIL,scr)
  ENDIF

ENDPROC

PROC clear_render(win:PTR TO window) OF leds
  self.rport:=0; self.win:=0
ENDPROC

PROC render(ta,x,y,xs,ys,win:PTR TO window) OF leds
DEF f:REG

  f:=xs/(NUMBANKS/2); self.ledx:=f
  self.margl:=f/4
  f:=ys/3; self.ledh:=f
  self.margt:=(ys/2-f)/2
  self.nextrow:=ys/2

  self.rport:=win.rport
  self.win:=win

  FOR f:=0 TO NUMBANKS-1 DO self.drawled(f,self.bankstat[f],TRUE)
  
  self.movement:=MOVEMENT_NONE
  self.dragpos:=-1
  self.drawrange(self.lorange, self.hirange, SETCURRENT)

ENDPROC

PROC setcurrent(v=-1) OF leds

  IF (v >= 0) AND (v < NUMBANKS)
    IF self.movement<>MOVEMENT_NONE
      IF self.win THEN ReportMouse(FALSE,self.win)
      self.movement:=MOVEMENT_NONE
    ENDIF
    self.setrange(v,v)
    self.drawled(v,SETCURRENT)
  ENDIF

ENDPROC self.current

PROC setenabled(v=-1,enable=TRUE) OF leds
DEF f:REG,st:REG, b,e

  IF (v >= 0) AND (v < NUMBANKS)
    b:=v; e:=v
  ELSEIF v=-1
    b:=0; e:=NUMBANKS-1
  ELSE
    RETURN
  ENDIF

  IF enable THEN st:=BS_ENABLED ELSE st:=0
  FOR f:=b TO e
    IF self.bankstat[f] <> st THEN self.drawled(f,st)
  ENDFOR
ENDPROC

PROC setactive(v=-1,typ=LSETNONE) OF leds
DEF f:REG,st:REG, b,e

  IF (v >= 0) AND (v < NUMBANKS)
    b:=v; e:=v
  ELSEIF v=-1
    b:=0; e:=NUMBANKS-1
  ELSE
    RETURN
  ENDIF

  IF typ=LSETINNER
    st:=BS_SETLO OR BS_ENABLED
  ELSEIF typ=LSETFULL
    st:=BS_SETFULL OR BS_ENABLED
  ENDIF

  FOR f:=b TO e
    IF typ=LSETNONE THEN st:=self.bankstat[f] AND BS_ENABLED
    IF self.bankstat[f] <> st THEN self.drawled(f,st)
  ENDFOR
ENDPROC

PROC message_test(imsg:PTR TO intuimessage,win:PTR TO window) OF leds
DEF x:REG,y:REG,code:REG,class:REG

  IF (class:=imsg.class) AND (IDCMP_MOUSEBUTTONS OR IDCMP_MOUSEMOVE)
    x:=imsg.mousex-self.x
    y:=imsg.mousey-self.y
    IF class AND IDCMP_MOUSEMOVE
      IF self.movement <> MOVEMENT_NONE
        IF (x >= 0) AND (y >= 0) AND (x < self.xs) AND (y < self.ys)
          x:=x/self.ledx + IF y < self.nextrow THEN 0 ELSE NUMBANKS/2
        ELSE
          x:=-1
        ENDIF
        IF self.select <> x ; self.select:=x; RETURN TRUE; ENDIF
      ENDIF
    ELSEIF (code:=imsg.code) = SELECTDOWN
      IF (x >= 0) AND (y >= 0) AND (x < self.xs) AND (y < self.ys)
        self.select:=x/self.ledx + IF y < self.nextrow THEN 0 ELSE NUMBANKS/2
        RETURN TRUE
      ENDIF
    ELSEIF code = SELECTUP
      IF self.movement <> MOVEMENT_NONE THEN RETURN TRUE
    ENDIF
  ELSEIF class AND IDCMP_INACTIVEWINDOW
    IF self.movement THEN RETURN TRUE
  ENDIF
ENDPROC FALSE

PROC message_action(class,qual,code,win:PTR TO window) OF leds
DEF sel:REG, mv:REG
  sel:=self.select
  IF (mv:=self.movement) <> MOVEMENT_NONE THEN ReportMouse(FALSE,win)
  IF class AND IDCMP_MOUSEMOVE
    IF mv = MOVEMENT_SET
      IF sel=-1
        self.setrange(self.current,self.current)
      ELSEIF sel < self.current
        self.setrange(sel,self.current)
      ELSE
        self.setrange(self.current,sel)
      ENDIF
      ReportMouse(TRUE,win)
    ELSEIF mv = MOVEMENT_DRAG
      self.setrange()
      ReportMouse(TRUE,win)
    ENDIF
  ELSEIF class AND IDCMP_MOUSEBUTTONS  
    IF code = SELECTDOWN
      IF (sel >= self.lorange) AND (sel <= self.hirange) AND ((self.hirange-self.lorange) < (NUMBANKS-1))
        self.movepos:=sel-self.lorange
        self.movement:=MOVEMENT_DRAG; self.setrange()
        ReportMouse(TRUE,win)
      ELSE
        self.movement:=MOVEMENT_SET
        self.setrange(sel,sel)
        self.drawled(sel,SETCURRENT)
        ReportMouse(TRUE,win)
        self.act:=LEDACT_SETBANK; RETURN TRUE
      ENDIF
    ELSEIF code = SELECTUP
      self.movement:=MOVEMENT_NONE
      IF mv = MOVEMENT_SET
        self.act:=LEDACT_SETRANGE
      ELSEIF mv = MOVEMENT_DRAG
        IF self.dragpos=self.lorange
          self.setrange(sel,sel); self.drawled(sel,SETCURRENT)
          self.act:=LEDACT_SETBANK; RETURN TRUE
        ENDIF
        self.setrange()
        IF self.select < 0 THEN RETURN FALSE
        self.act:=LEDACT_MOVERANGE
      ELSE
        RETURN FALSE
      ENDIF
      RETURN TRUE
    ENDIF
  ELSEIF class AND IDCMP_INACTIVEWINDOW
    self.movement:=MOVEMENT_NONE
    self.setrange()
  ENDIF
ENDPROC FALSE

PROC getrange(lo=FALSE,hi=FALSE) OF leds
DEF x
  IF lo THEN ^lo:=self.lorange
  IF hi THEN ^hi:=self.hirange
  IF (x:=self.select-self.movepos) < 0 THEN x:=0
  IF (self.hirange-self.lorange+x) >= 60 THEN x:=59-self.hirange+self.lorange
ENDPROC x

PROC setrange(lo=-1,hi=-1) OF leds
DEF l:REG, h:REG, x:REG
  l:=self.lorange; h:=self.hirange
  IF (x:=self.dragpos) >= 0
    self.dragpos:=-1; self.drawrange(x,h-l+x)
  ENDIF
  IF (lo >= 0) AND (hi < NUMBANKS) AND (lo <= hi)
    self.drawrange(l,h);  self.drawrange(lo,hi,SETCURRENT)
    self.lorange:=lo; self.hirange:=hi
  ELSE
    IF (x-1 <= h) AND (h-l+x+1 >= l) THEN self.drawrange(l,h,SETCURRENT)
    IF (self.movement = MOVEMENT_DRAG) AND (self.select >= 0)
      x:=self.select-self.movepos; IF x < 0 THEN x:=0
      IF (h-l+x) >= NUMBANKS THEN x:=l-h+NUMBANKS-1
      self.drawrange(x,h-l+x,SETOTHER); self.dragpos:=x
    ENDIF
  ENDIF
ENDPROC

PROC drawrange(l,h,colr=-1) OF leds
DEF x1:REG, x2:REG, y1:REG, y2:REG, ye:REG, maxx, minx, col
  IF (stdrast:=self.rport)=0 THEN RETURN
  x1:=(IF NUMBANKS/2<=l THEN l-(NUMBANKS/2) ELSE l)*self.ledx + self.margl + self.x - 2
  x2:=(IF NUMBANKS/2<=h THEN h-(NUMBANKS/2) ELSE h)*self.ledx + self.margl + self.x
  x2:=x2+self.ledx - self.margl
  y1:=self.y + self.margt - 2 ; y2:=y1
  IF NUMBANKS/2<=l THEN y1:=y1+self.nextrow
  IF NUMBANKS/2<=h THEN y2:=y2+self.nextrow
  maxx:=NUMBANKS/2*self.ledx+self.x
  minx:=self.x+self.margl-2
  ye:=self.ledh + 3
  SELECT colr
    CASE SETCURRENT
      col:=self.colrcurrent
    CASE SETOTHER
      col:=self.colrset
    DEFAULT
      col:=self.colrback
  ENDSELECT
  IF y1=y2
    Line(x1,y1, x2,y1, col); Line(x1,y1+ye, x2,y1+ye, col)
  ELSE
    Line(x1,y1, maxx, y1, col); Line(x1,y1+ye, maxx, y1+ye, col)
    Line(minx,y2, x2, y2, col); Line(minx,y2+ye, x2, y2+ye, col)
  ENDIF
  Line(x1,y1, x1,y1+ye, col); Line(x2,y2, x2,y2+ye, col)
  IF (colr=SETOTHER) AND (l <= self.hirange) AND (h >= self.lorange)
    SetBPen(stdrast,self.colrcurrent); self.rport.lineptrn:=$9999
    l:=self.lorange; h:=self.hirange
    x1:=Max(x1,(IF NUMBANKS/2<=l THEN l-(NUMBANKS/2) ELSE l)*self.ledx + self.margl + self.x - 2)
    x2:=Min(x2,(IF NUMBANKS/2<=h THEN h-(NUMBANKS/2) ELSE h)*self.ledx + self.margl + self.x + self.ledx - self.margl)
    IF y1=y2
      Line(x1,y1, x2,y1, col); Line(x1,y1+ye, x2,y1+ye, col)
    ELSE
      Line(x1,y1, maxx, y1, col); Line(x1,y1+ye, maxx, y1+ye, col)
      Line(minx,y2, x2, y2, col); Line(minx,y2+ye, x2, y2+ye, col)
    ENDIF
    SetBPen(stdrast,self.colrback); self.rport.lineptrn:=$FFFF
  ENDIF

ENDPROC

PROC drawled(number,type,drawborder=FALSE) OF leds
DEF coltype, colin, colshad, colshin, x:REG, y:REG, xe:REG, ye:REG, num:REG,
    rp:PTR TO rastport
  num:=number

  IF (num >= NUMBANKS) OR (num < 0) THEN RETURN
  IF type=SETCURRENT
    x:=self.current; self.current:=num
    IF (x<>num) THEN self.drawled(x,self.bankstat[x])
    type:=self.bankstat[num]
  ELSE
    self.bankstat[num]:=type
  ENDIF


  IF (stdrast:=self.rport)=0 THEN RETURN

  IF type AND BS_ENABLED
    IF type AND BS_SETFULL
      coltype:=1
      colin:=self.colrset
    ELSEIF type AND BS_SETLO
      coltype:=2
      colin:=self.colrset
    ELSE
      coltype:=1
      colin:=self.colrcurrent
    ENDIF
  ELSE
    coltype:=0
    colin:=self.colrinner
  ENDIF

  IF self.current=num
    colshin:=self.colrback;  colshad:=shinepen
  ELSE
    colshin:=shinepen; colshad:=self.colrback
  ENDIF

  x:=(IF NUMBANKS/2<=num THEN num-(NUMBANKS/2) ELSE num)*self.ledx + self.margl + self.x
  xe:=x+self.ledx - self.margl - 2
  y:=self.y + self.margt
  IF NUMBANKS/2<=num THEN y:=y+self.nextrow
  ye:=y + self.ledh -1

  IF drawborder
    Box(x,y-1,xe,ye+1,shadowpen)
    Box(x-1,y,xe+1,ye,shadowpen)
  ENDIF
  Line(x+1,y, xe-1,y, colshin); Line(x,y+1, x, ye-1, colshin) 
  Line(xe,y+1, xe,ye-1, colshad); Line(x+1,ye, xe-1,ye, colshad)

  SELECT coltype
   CASE 1
    Box(x+1,y+1,xe-1,ye-1,colin)
   CASE 2
    Box(x+1,y+1,xe-1,ye-1,self.colrback)
    Line(x+1,y+1,xe-1,ye-1,colin);  Line(xe-1,y+1,x+1,ye-1,colin)
   DEFAULT
    Box(x+1,y+1,xe-1,ye-1,self.colrback)
    rp:=stdrast; rp.areaptrn:=[$5555,$AAAA]:INT; rp.areaptsz:=1
    Box(x+1,y+1,xe-1,ye-1,colin)
    rp.areaptrn:=0; rp.areaptsz:=0
  ENDSELECT

ENDPROC TRUE
