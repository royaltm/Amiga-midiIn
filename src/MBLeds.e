OPT MODULE
OPT OSVERSION=37


MODULE 'tools/EasyGUI','intuition/intuition','graphics/text','intuition/screens',
       '*MBbanks'


SET BS_ENABLED, BS_SETLO, BS_SETFULL

CONST SETCURRENT=$FFFF

EXPORT ENUM LSETNONE=0, LSETINNER, LSETFULL

EXPORT OBJECT leds OF plugin
PRIVATE
  rport:LONG
  xm:INT
  ym:INT
  ledx:INT
  ledh:INT
  margl:INT
  margt:INT
  nextrow:INT
  current:CHAR
  bankstat[NUMBANKS]:ARRAY OF CHAR
  colrinner:CHAR
  colrback:CHAR
  colrcurrent:CHAR
  colrset:CHAR
ENDOBJECT

DEF shadowpen,shinepen

EXPORT PROC min_size(ta,fh) OF leds IS 8*(NUMBANKS/2),fh*2

EXPORT PROC init(screen) OF leds
	DEF drinfo:PTR TO drawinfo,scr,penless,x:PTR TO INT,f

  self.current:=0
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

EXPORT PROC clear_render(win:PTR TO window) OF leds
  self.rport:=0
ENDPROC

EXPORT PROC render(ta,x,y,xs,ys,win:PTR TO window) OF leds
DEF f:REG

  f:=xs/(NUMBANKS/2); self.ledx:=f
  self.margl:=f/4
  f:=ys/3; self.ledh:=f
  self.margt:=(ys/2-f)/2
  self.nextrow:=ys/2

  self.rport:=win.rport

  FOR f:=0 TO NUMBANKS-1 DO self.drawled(f,self.bankstat[f])

ENDPROC

EXPORT PROC setcurrent(v=-1) OF leds

  IF (v >= 0) AND (v < NUMBANKS) THEN   self.drawled(v,SETCURRENT)

ENDPROC self.current

EXPORT PROC setenabled(v=-1,enable=TRUE) OF leds
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

EXPORT PROC setactive(v=-1,typ=LSETNONE) OF leds
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

EXPORT PROC message_test(imsg:PTR TO intuimessage,win:PTR TO window) OF leds
  DEF x:REG,y:REG,xx:REG,yy:REG

  IF imsg.class = IDCMP_MOUSEBUTTONS
    IF imsg.code = SELECTDOWN
      x:=imsg.mousex
      y:=imsg.mousey
      xx:=self.x
      yy:=self.y
      IF (x >= xx) AND (y >= yy) AND (x < (xx+self.xs)) AND (y < (yy+self.ys))
        self.xm:=x-xx
        self.ym:=IF (y-yy) < self.nextrow THEN 0 ELSE NUMBANKS/2
        RETURN TRUE
      ENDIF
    ENDIF
  ENDIF
ENDPROC FALSE

EXPORT PROC message_action(class,qual,code,win:PTR TO window) OF leds

    self.drawled(self.xm/self.ledx+self.ym,SETCURRENT)

ENDPROC TRUE


PROC drawled(number,type) OF leds
DEF colobw, colin, colshad, colshin, x:REG, y:REG, xe:REG, ye:REG, num:REG

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
      colobw:=self.colrset
      colin:=colobw
    ELSEIF type AND BS_SETLO
      colobw:=self.colrback
      colin:=self.colrset
    ELSE
      colobw:=self.colrinner
      colin:=colobw
    ENDIF
  ELSE
    colobw:=self.colrback
    colin:=colobw
  ENDIF

  x:=(IF NUMBANKS/2<=num THEN num-(NUMBANKS/2) ELSE num)*self.ledx + self.margl + self.x
  xe:=x+self.ledx - self.margl - 1
  y:=self.y + self.margt
  IF NUMBANKS/2<=num THEN y:=y+self.nextrow
  ye:=y + self.ledh -1

  IF self.current=num
    colshin:=shadowpen;   colshad:=shinepen
    Box(x-1,y-1,xe+1,ye+1,self.colrcurrent)
    IF colobw=self.colrinner
      colobw:=self.colrcurrent; colin:=colobw
    ENDIF
  ELSE
    colshin:=shinepen;    colshad:=shadowpen
    Box(x-1,y-1,xe+1,ye+1,shinepen)
    Box(x-1,y-1,xe-1,ye-1,shadowpen)
  ENDIF
  Line(xe,y  ,xe,ye-1,colshad); Line(x  ,ye,xe  ,ye,colshad)
  Line(x,y  ,x,ye-1,colshin); Line(x  ,y,xe ,y,colshin)
  Box(x+1,y+1,xe-1,ye-1,colobw); IF colin<>colobw THEN Box(x+2,y+2,xe-2,ye-2,colin)

ENDPROC TRUE
