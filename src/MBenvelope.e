OPT MODULE
OPT OSVERSION=37

MODULE 'intuition/intuition', 'graphics/text', 'tools/EasyGUI', 'graphics/rastport',
       'intuition/screens',
       '*mblocale','*mbkeycod'

CONST COORDSNUM=5
ENUM SETNOTHING=0,SETATTACK,SETDECAY,SETSUSTAIN,SETRELEASE

OBJECT coordinate
  x:INT
  y:INT
ENDOBJECT

EXPORT OBJECT envel_plugin OF plugin
  keycode:LONG
PRIVATE
  rport:PTR TO rastport
  textpen:LONG  -> {
  paperpen:LONG ->  init plz!
  barpen:LONG   -> }
  x1:INT
  x2:INT
  y1:INT
  y2:INT
  ssize:INT
  bottomtexty:INT
  minw:INT
  coords[COORDSNUM]:ARRAY OF coordinate
  t1:CHAR
  t2:CHAR
  slev:CHAR -> init plz!
  t3:CHAR
  xm:INT
  ym:INT
  whichm:INT -> init plz!
  lastm:INT
  secposx:INT
  wait:INT
ENDOBJECT



PROC min_size(ta,fh) OF envel_plugin IS 18*fh, 10*fh

PROC clear_render(win) OF envel_plugin
  self.rport:=0
ENDPROC

PROC render(ta:PTR TO textattr,x,y,xs,ys,win:PTR TO window) OF envel_plugin
DEF ypos:REG,axh,axix:REG,a:REG,s:REG,t,temp:REG,i
  
  stdrast:=win.rport
  self.rport:=stdrast
  self.whichm:=SETNOTHING
  a:=xs/20
  s:=getLocStr(STRID_HUNPERCENT);  self.ssize:=TextLength(stdrast,s,StrLen(s))
  self.minw:=win.rport.txwidth
  self.x1:=x+a+5
  self.x2:=x+xs-3
  self.y1:=y+3+win.rport.txheight
  self.bottomtexty:=y+ys-win.rport.txheight+win.rport.txbaseline-2
  self.y2:=self.bottomtexty-win.rport.txbaseline-5-(ys/20)
  t:=self.textpen
  axix:=a/3*2+x+3; Line(axix,self.y1,axix,self.y2,t); DEC axix
  a:=a/2
  axh:=Div(Shl(self.y2-self.y1,16),20)
  ypos:=Shl(self.y1,16)
  s:=a
  FOR i:=0 TO 19
    IF i=10 THEN s:=a/2
    temp:=ypos; SWAP temp; EXT.L temp
    Line(axix,temp,axix-s,temp,t)
    ypos:=ypos+axh
    s:=a/4
  ENDFOR
  Line(axix,self.y2,axix-a,self.y2,t)
  TextF(x,y+win.rport.txbaseline+1,'\s',getLocStr(STRID_OFVOLUME))
  Box(self.x1-1,self.y1-1,self.x2+1,self.y2+1,self.barpen)
  Box(self.x1,self.y1,self.x2,self.y2,self.paperpen)
  self.redrawenvelope(TRUE)
  self.updatebottomarea(TRUE)
  ReportMouse(TRUE,win)
ENDPROC

PROC updatebottomarea(update=FALSE) OF envel_plugin
DEF y:REG,y2:REG,tx:REG, coords:PTR TO coordinate

  IF stdrast:=self.rport
    coords:=self.coords
    y:=self.y2+3; y2:=y+(self.ys/20)
    Box(self.x, y, self.x+self.xs, self.y+self.ys-1, self.rport.bgpen)
    SetAPen(stdrast,self.textpen)
    IF update
      tx:=self.bottomtexty
      prtfixvalue( coords[0].x, coords[1].x, self.t1, y, y2, tx)
      prtfixvalue( coords[1].x, coords[2].x, self.t2, y, y2, tx)
      prtfixvalue( coords[2].x, coords[3].x, -self.slev-1, y, y2, tx)
      prtfixvalue( coords[3].x, coords[4].x, self.t3, y, y2, tx)
    ELSE
      tx:=ListItem([getLocStr(STRID_EATTACK),getLocStr(STRID_EDECAY),getLocStr(STRID_ESUSTAIN),getLocStr(STRID_ERELEASE)
                    ],self.whichm-1)
      self.secposx:=TextLength(stdrast,tx,StrLen(tx))+self.x1
      TextF(self.x1,self.bottomtexty, tx)
    ENDIF
  ENDIF

ENDPROC

PROC prtfixvalue(x1,x2,val,y1,y2,ytxt)
DEF s[6]:STRING,t:REG,c:REG,x:REG,y:REG
  IF val < 0
    StringF(s,'\d%',100*(-val) / 256)
  ELSE
    Move(stdrast,x1,y2); Draw(stdrast,x1,y1); Draw(stdrast,x2,y1); Draw(stdrast,x2,y2)
    t:=val;  IF t<10 THEN t:=10
    c:=Div(Mul(Shl(x2-x1+1,16),10),t)
    x:=Shl(x1,16); y:=(y2-y1)/2+y1;  WHILE (t:=Shr(x,16)) < x2;    Line(t,y1,t,y); x:=x+c;  ENDWHILE
    IF val = 0 
      StringF(s,'\d',0)
    ELSEIF val < 10
      StringF(s,'.\d',val)
    ELSE
      StringF(s,'\d.\d',val/10,Mod(val,10))
    ENDIF
  ENDIF
  c:=TextLength(stdrast,s,EstrLen(s)) ; t:=(x2-x1)
  TextF(t/2-(c/2)+x1,ytxt,'\s', s)
ENDPROC

PROC updatebottomvalue(val) OF envel_plugin

  IF stdrast:=self.rport
    SetAPen(stdrast,self.textpen)
    IF val >= 0
      TextF(self.secposx, self.bottomtexty,'\d.\d \s ',val/10,Mod(val,10),getLocStr(STRID_SECONDS))
    ELSE
      val:=100*(-val) / 256
      TextF(self.secposx, self.bottomtexty,'\d% ',val)
    ENDIF
  ENDIF

ENDPROC

PROC updatecoords(coords:PTR TO coordinate) OF envel_plugin
DEF w:REG,s:REG,susth,attx:REG,decx:REG,relx:REG,n

  susth:=(self.y2-self.y1-2) * (256-self.slev) /256 + self.y1+1
  w:=self.x2-self.x1-self.ssize
  s:=self.t1+self.t2+self.t3+3
  n:=self.minw
  IF (attx:=w*self.t1/s)< n THEN attx:=n
  IF (decx:=w*self.t2/s)< n THEN decx:=n
  IF (relx:=w*self.t3/s)< n THEN relx:=n
  WHILE (attx+decx+relx) > w
    IF relx >= decx
      IF relx >= attx THEN DEC relx ELSE DEC attx
    ELSE
      IF attx >= decx THEN DEC attx ELSE DEC decx
    ENDIF
  ENDWHILE
  w:=self.x1; s:=self.x2
  coords.x:=w; coords.y:=self.y2; coords++
  coords.x:=w+attx; coords.y:=self.y1+1; coords++
  coords.x:=w+attx+decx; coords.y:=susth; coords++
  coords.x:=s-relx; coords.y:=susth; coords++
  coords.x:=s; coords.y:=self.y2

ENDPROC

PROC redrawenvelope(still=FALSE) OF envel_plugin
DEF rp:REG,i:REG,c:REG,p,x,y,x2,y2,tx:REG,nx:REG,paperp,textp,barp,y3,f,chgx,
    coords:PTR TO coordinate,newcoords[COORDSNUM]:ARRAY OF coordinate

  IF rp:=self.rport
    stdrast:=rp
    coords:=self.coords
    IF still
      self.updatecoords(coords)
      SetAPen(rp,self.barpen)
      y:=self.y1; y2:=self.y2
      FOR i:=1 TO COORDSNUM-2
        tx:=coords[i].x; Move(rp,tx,coords[i].y); Draw(rp,tx,y2)
      ENDFOR
      SetAPen(rp,self.textpen); Move(rp,coords[0].x,coords[0].y); PolyDraw(rp,COORDSNUM-1,coords[1])
      Line(coords[2].x,coords[2].y,coords[3].x,y2,self.barpen); Line(coords[2].x,y2,coords[3].x,coords[3].y,self.barpen)
    ELSE
      paperp:=self.paperpen; textp:=self.textpen; barp:=self.barpen
      self.updatecoords(newcoords)
      c:=0; p:=1; x2:=coords[0].x; x:=newcoords[0].x
      y:=self.y1; y2:=self.y2; chgx:=FALSE
      FOR i:=1 TO COORDSNUM-2
        IF (tx:=coords[i].x) <> (nx:=newcoords[i].x)
          Line(tx,y,tx,y2,paperp)
          c:=-1
          IF chgx=FALSE
            IF (i=2) OR (i=3) THEN chgx:=1
          ENDIF
        ELSEIF coords[i].y <> (y3:=newcoords[i].y)
          Line(tx,y,tx,y3,paperp)
          c:=-1
          IF chgx=FALSE
            IF (i=2) OR (i=3) THEN chgx:=1
          ENDIF
        ELSE
          IF c > 0
            SetAPen(rp,paperp); Move(rp,x2,coords[p-1].y); PolyDraw(rp,i-p,coords[p])
            IF chgx=1 ; chgx:=TRUE; Line(coords[2].x,coords[2].y,coords[3].x,y2,paperp); Line(coords[2].x,y2,coords[3].x,coords[3].y,paperp); ENDIF
            SetAPen(rp,textp); Move(rp,x,newcoords[p-1].y); PolyDraw(rp,i-p,newcoords[p])
            FOR f:=p-1 TO i-2 DO Line(newcoords[f].x,newcoords[f].y,newcoords[f].x,y2,barp)
            c:=0
          ELSEIF c < 0
            c:=1
          ENDIF
          IF c=0 ; p:=i+1; x2:=tx; x:=nx ; ENDIF
        ENDIF
      ENDFOR
      IF c
        IF c < 0 THEN INC i
        SetAPen(rp,paperp); Move(rp,x2,coords[p-1].y); PolyDraw(rp,i-p,coords[p])
        IF chgx=1 ; chgx:=TRUE; Line(coords[2].x,coords[2].y,coords[3].x,y2,paperp); Line(coords[2].x,y2,coords[3].x,coords[3].y,paperp); ENDIF
        SetAPen(rp,textp); Move(rp,x,newcoords[p-1].y); PolyDraw(rp,i-p,newcoords[p])
        FOR f:=p-1 TO i-2 DO Line(newcoords[f].x,newcoords[f].y,newcoords[f].x,y2,barp)
        IF chgx=TRUE; Line(newcoords[2].x,newcoords[2].y,newcoords[3].x,y2,barp); Line(newcoords[2].x,y2,newcoords[3].x,newcoords[3].y,barp); ENDIF
      ENDIF
      CopyMem(newcoords[1],coords[1],COORDSNUM-2 * SIZEOF coordinate)
    ENDIF
  ELSE
    self.updatecoords(self.coords)
  ENDIF

ENDPROC

PROC message_test(imsg:PTR TO intuimessage,win:PTR TO window) OF envel_plugin
DEF class:REG,xm:REG,ym:REG,code:REG
  code:=imsg.code
  class:=imsg.class
  SELECT class
   CASE IDCMP_MOUSEBUTTONS
    xm:=imsg.mousex; ym:=imsg.mousey
    IF (code = SELECTDOWN) OR (code=MENUDOWN)
      IF self.whichm = SETNOTHING
        IF (ym >= self.y1) AND (ym <= self.y2) AND (xm >= self.x1) AND (xm <= self.x2)
          self.xm:=xm; self.ym:=ym; RETURN TRUE
        ELSE
          rmbtrap(FALSE,win)
        ENDIF
      ELSE
        RETURN TRUE
      ENDIF
    ELSEIF (code = SELECTUP) OR (code=MENUUP)
      IF self.whichm <> SETNOTHING THEN RETURN TRUE
    ENDIF
   CASE IDCMP_MOUSEMOVE
    xm:=imsg.mousex; ym:=imsg.mousey
    IF self.whichm = SETNOTHING
      code:=FALSE
      IF (ym >= self.y1)
        IF (ym <= self.y2)
          IF (xm >= self.x1)
            IF (xm <= self.x2) THEN code:=TRUE
          ENDIF
        ENDIF
      ENDIF
      rmbtrap(code,win)
    ELSE
      IF (Abs(self.wait) <> 1);  self.xm:=xm; self.ym:=ym; RETURN TRUE;  ENDIF
    ENDIF
   CASE IDCMP_ACTIVEWINDOW
    xm:=imsg.mousex; ym:=imsg.mousey
    rmbtrap(IF (ym >= self.y1) AND (ym <= self.y2) AND (xm >= self.x1) AND (xm <= self.x2) THEN TRUE ELSE FALSE,win)
   CASE IDCMP_INACTIVEWINDOW
    rmbtrap(FALSE,win)
    IF self.whichm <> SETNOTHING THEN RETURN TRUE
   CASE IDCMP_INTUITICKS
    IF self.whichm <> SETNOTHING
      IF (xm:=self.wait) > 1
        self.wait:=xm-1
        IF xm= 2 THEN self.lastm:= 0
      ELSEIF xm < -1
        self.wait:=xm+1
        IF xm= -2 THEN self.lastm:= 0
      ELSEIF xm <> 0
        RETURN TRUE
      ENDIF
    ENDIF
   CASE IDCMP_RAWKEY
    IF (code >= CURSORUP) AND (code <= KEYCODE_F10) THEN RETURN TRUE
   CASE IDCMP_VANILLAKEY
    SELECT 128 OF code
      CASE ESC_CODE; RETURN TRUE
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
      DEFAULT; RETURN FALSE
    ENDSELECT
  ENDSELECT

ENDPROC FALSE

PROC message_action(class,qual,code,win:PTR TO window) OF envel_plugin
DEF v:REG,t:REG,w:REG,coords:PTR TO coordinate

  IF class AND IDCMP_RAWKEY
    self.keycode:=code OR MYRAWCODE; RETURN TRUE
  ELSEIF class AND IDCMP_VANILLAKEY
    self.keycode:=code; RETURN TRUE
  ELSEIF class AND IDCMP_INTUITICKS
    w:=self.whichm
    IF (t:=self.lastm) < 30
      self.lastm:=t+1
      IF (t = 0) OR (t>3) THEN v:=self.wait ELSE v:=0
    ELSE
      v:=self.wait*10
    ENDIF
    SELECT w
      CASE SETATTACK
        t:=self.t1+v; IF t > 255 THEN t:=255
        IF t < 0 THEN t:=0
        self.t1:=t
      CASE SETDECAY
        t:=self.t2+v; IF t > 255 THEN t:=255
        IF t < 0 THEN t:=0
        self.t2:=t
      CASE SETRELEASE
        t:=self.t3+v; IF t > 255 THEN t:=255
        IF t < 0 THEN t:=0
        self.t3:=t
      CASE SETSUSTAIN
        t:=self.slev+v; IF t > 255 THEN t:=255
        IF t < 0 THEN t:=0
        self.slev:=t; t:=-t-1
    ENDSELECT
    self.redrawenvelope(); self.updatebottomvalue(t)
  ELSEIF class AND IDCMP_INACTIVEWINDOW
    self.whichm:=SETNOTHING
    self.updatebottomarea(TRUE)
    self.keycode:= -2
    RETURN TRUE
   ELSEIF class AND IDCMP_MOUSEBUTTONS
    IF (code = SELECTDOWN) OR (code = MENUDOWN)
      IF self.whichm = SETNOTHING
        v:=self.xm; coords:=self.coords
        IF v < coords[1].x
          self.whichm:=SETATTACK; t:=self.t1
        ELSEIF v < coords[2].x
          self.whichm:=SETDECAY; t:=self.t2
        ELSEIF v > coords[3].x
          self.whichm:=SETRELEASE; t:=self.t3
        ELSE
          self.whichm:=SETSUSTAIN; v:=self.ym; t:=-self.slev-1
        ENDIF
        self.lastm:=v
        self.updatebottomarea(); self.updatebottomvalue(t)
        self.wait:=IF code = SELECTDOWN THEN 4 ELSE -4
        RETURN FALSE
      ELSE
        self.whichm:=SETNOTHING
        self.updatebottomarea(TRUE)
        self.keycode:= -2
        RETURN TRUE
      ENDIF
    ELSEIF (code = SELECTUP) OR (code = MENUUP)
      self.whichm:=SETNOTHING
      self.updatebottomarea(TRUE)
      self.keycode:= -1
      RETURN TRUE
    ENDIF
  ELSEIF (class AND IDCMP_MOUSEMOVE)
    ReportMouse(FALSE,win)
    self.wait:= 0
    w:=self.whichm
    IF w=SETSUSTAIN
      v:=self.y2-self.ym; t:=self.y2-self.y1
      t:=256*v/t; IF t > 255 THEN t:=255
      IF t < 0 THEN t:=0
      self.slev:=t; t:=-t-1
    ELSE
      v:=self.xm; t:=v-self.lastm
      v:=self.minw
      IF (-v<t) AND (t<v)
        t:=0
      ELSEIF (-(v*2)<t) AND (t<(v*2))
        t:=Sign(t); self.lastm:=self.xm
      ELSE
        self.lastm:=self.xm
      ENDIF
      SELECT w
       CASE SETATTACK
        t:=self.t1+t; IF t > 255 THEN t:=255
        IF t < 0 THEN t:=0
        self.t1:=t
       CASE SETDECAY
        t:=self.t2+t; IF t > 255 THEN t:=255
        IF t < 0 THEN t:=0
        self.t2:=t
       CASE SETRELEASE
        t:=self.t3+t; IF t > 255 THEN t:=255
        IF t < 0 THEN t:=0
        self.t3:=t
      ENDSELECT
    ENDIF
    self.redrawenvelope(); self.updatebottomvalue(t)
    ReportMouse(TRUE,win)
  ENDIF
ENDPROC FALSE

PROC rmbtrap(on,window:PTR TO window)
  IF on
    MOVE.L  window,A0
    ORI.L   #WFLG_RMBTRAP,24(A0)
  ELSE
    MOVE.L  window,A0
    MOVE.L  #WFLG_RMBTRAP,D0
    NOT.L   D0
    AND.L  D0,24(A0)
  ENDIF
ENDPROC

EXPORT PROC setenvelope(attack,decay,sustain,release) OF envel_plugin
DEF w=FALSE, val
  IF self.t1<>attack; self.t1:=attack; w:=TRUE; ENDIF
  IF self.t2<>decay; self.t2:=decay; w:=TRUE; ENDIF
  IF self.slev<>sustain; self.slev:=sustain; w:=TRUE; ENDIF
  IF self.t3<>release; self.t3:=release; w:=TRUE; ENDIF
  IF w=FALSE THEN RETURN
  self.redrawenvelope()
  IF self.whichm = SETNOTHING
    self.updatebottomarea(TRUE)
  ELSE
    w:=self.whichm
    IF w=SETSUSTAIN
      val:=-self.slev-1
    ELSE
      SELECT w
       CASE SETATTACK
        val:=self.t1
       CASE SETDECAY
        val:=self.t2
       CASE SETRELEASE
        val:=self.t3
      ENDSELECT
    ENDIF
    self.updatebottomvalue(val)
  ENDIF
ENDPROC

EXPORT PROC getenvelope(attack,decay,sustain,release) OF envel_plugin
  IF attack THEN ^attack:=self.t1
  IF decay THEN ^decay:=self.t2
  IF sustain THEN ^sustain:=self.slev
  IF release THEN ^release:=self.t3
ENDPROC

EXPORT PROC init(screen) OF envel_plugin
DEF drinfo:PTR TO drawinfo,scr,x:PTR TO INT,penless
  self.keycode:=-1
  self.slev:=255
  self.textpen:=1
  self.paperpen:=2
  self.barpen:=3
  IF screen=0 THEN scr:=LockPubScreen(NIL) ELSE scr:=screen
  IF scr
    IF drinfo:=GetScreenDrawInfo(scr)
      penless:=drinfo.numpens
      x:=drinfo.pens
      IF penless>TEXTPEN THEN self.textpen:=x[TEXTPEN]
      IF penless>FILLPEN THEN self.barpen:=x[FILLPEN]
      IF penless>SHINEPEN THEN self.paperpen:=x[SHINEPEN]
      FreeScreenDrawInfo(scr,drinfo)
    ENDIF
    IF screen=0 THEN UnlockPubScreen(NIL,scr)
  ENDIF
ENDPROC
