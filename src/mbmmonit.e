OPT PREPROCESS
OPT MODULE

MODULE 'tools/EasyGUI',
       'intuition/screens','intuition/intuition','graphics/rastport','graphics/gfx',
       '*mblocale','*mbkeycod'

#define MIN(a,b) (IF a < b THEN a ELSE b)
#define MAX(a,b) (IF a > b THEN a ELSE b)

OBJECT displaystatus
  value:INT
  lastx:INT
  mintxt:INT
  maxtxt:INT
  miny:INT
  maxy:INT
  txty:INT
ENDOBJECT

EXPORT OBJECT midimonitor OF plugin
  keycode:LONG
PRIVATE
  rport:LONG
  textpen:LONG
  fillpen:LONG
  shinepen:LONG
  font:LONG
  minx:INT
  midx:INT
  maxx:INT
  ds[16]:ARRAY OF displaystatus
ENDOBJECT


PROC min_size(ta,fh) OF midimonitor IS fh*8,fh+3 *16+1

PROC render(ta,x,y,xs,ys,win:PTR TO window) OF midimonitor
DEF h,yo,i,rp:PTR TO rastport,s[10]:STRING,v=0,t,y1,y2,c,w,dss:PTR TO displaystatus
  stdrast:=rp:=win.rport
  IF self.font THEN SetFont(rp,self.font)
  h:=ys-1 /16
  yo:=h-2-rp.txheight /2 + y
  FOR i:=1 TO 16 DO IF (t:=TextLength(rp,StringF(s,'\d[2]',i),2))>v THEN v:=t
  SetAPen(rp,self.textpen); SetDrMd(rp,RP_JAM1)
  t:=yo+rp.txbaseline+1; self.minx:=x+v+3; self.maxx:=x+xs-3
  dss:=self.ds
  FOR i:=0 TO 15
    dss.txty:=h*i+t
    Move(rp,x+1,dss.txty); Text(rp,StringF(s,'\d[2]',i+1),2)
    Move(rp,self.minx-1,y1:=i*h+yo); Draw(rp,self.maxx+1,y1); Draw(rp,self.maxx+1,y2:=y1+rp.txheight+1)
    Draw(rp,self.minx-1,y2); Draw(rp,self.minx-1,y1)
    dss.miny:=y1+1; dss.maxy:=y2-1
    StringF(s,IF (v:=dss.value) < 0 THEN getLocStr(STRID_NOTAVAILABLE) ELSE '\r\d[5]',v)
    w:=self.maxx-self.minx; self.midx:=w/2 +self.minx
    dss.lastx:=IF v < 0 THEN self.midx ELSE w*v/16383 +self.minx
    Box(self.minx,dss.miny, self.maxx,dss.maxy, self.shinepen)
    IF self.midx < dss.lastx
      Box(self.midx,dss.miny, dss.lastx,dss.maxy, self.fillpen)
    ELSEIF v <> -1
      Box(dss.lastx,dss.miny, self.midx,dss.maxy, self.fillpen)
    ENDIF
    c:=TextLength(rp,s,EstrLen(s)-2)
    dss.mintxt:=self.midx-c
    dss.maxtxt:=dss.mintxt+TextLength(rp,s,EstrLen(s))-1
    SetAPen(rp,self.textpen);Move(rp,dss.mintxt,dss.txty);Text(rp,s,EstrLen(s))
    dss++
  ENDFOR
  self.rport:=rp
ENDPROC

PROC clear_render(win:PTR TO window) OF midimonitor
  self.rport:=0
ENDPROC

PROC setstatus(num,value) OF midimonitor
DEF dss:PTR TO displaystatus,l,t,rp:REG,s[10]:STRING,
    otx1,otx2,y1,y2,ox:REG,midx,nx:REG,tx1:REG,tx2:REG,shp,flp,txp
 dss:=self.ds[num]
 IF dss.value <> value
  dss.value:=value
  IF stdrast:=rp:=self.rport
    shp:=self.shinepen; flp:=self.fillpen; txp:=self.textpen
    y1:=dss.miny; y2:=dss.maxy
    midx:=self.midx; ox:=dss.lastx
    dss.lastx:=nx:=IF value < 0 THEN midx ELSE self.maxx-self.minx *value/16383 +self.minx
    otx1:=dss.mintxt; otx2:=dss.maxtxt
    StringF(s,IF value < 0 THEN getLocStr(STRID_NOTAVAILABLE) ELSE '\r\d[5]',value)
    dss.mintxt:=tx1:=midx-TextLength(rp,s,l:=EstrLen(s)-2)
    dss.maxtxt:=tx2:=midx+TextLength(rp,s+l,2)-1
    SetDrMd(rp,RP_JAM2); SetBPen(rp,shp)
    IF value < 0
      SetAPen(rp,txp); Move(rp,tx1,dss.txty); Text(rp,s,l+2)
      IF (t:=MIN(ox,otx1)) < tx1
        Box(t,y1, tx1-1,y2, shp)
      ELSEIF (t:=MAX(ox,otx2)) > tx2
        Box(tx2+1,y1, t,y2, shp)
      ENDIF
    ELSEIF nx >= midx
      IF (t:=MIN(ox,otx1)) < tx1 THEN Box(t,y1, tx1-1,y2, shp)
      SetAPen(rp,txp); Move(rp,tx1,dss.txty); Text(rp,s,l)
      IF nx >= tx2
        SetBPen(rp,flp); Move(rp,midx,dss.txty); Text(rp,s+l,2)
        IF ox <= nx
          IF nx < otx2
            Box(nx+1,y1, otx2,y2, shp); IF nx > tx2 THEN Box(tx2+1,y1, nx,y2, flp)
          ELSEIF ox <= MAX(otx2,tx2)
            IF nx > tx2 THEN Box(tx2+1,y1, nx,y2,flp)
          ELSE
            IF ox < nx THEN Box(ox+1,y1, nx,y2,flp)
            IF tx2 < otx2 THEN Box(tx2+1,y1, otx2,y2,flp)
          ENDIF
        ELSE
          Box(nx+1,y1, MAX(ox,otx2),y2,shp)
          IF tx2 < (t:=MIN(nx,otx2)) THEN Box(tx2+1,y1, t,y2, flp)
        ENDIF
      ELSE
        Box(midx,y1, nx,y2, flp)
        IF nx < (t:=MAX(otx2,ox)) THEN Box(nx+1,y1, t,y2, shp)
        SetDrMd(rp,RP_JAM1); SetAPen(rp,txp) 
        Move(rp,midx,dss.txty); Text(rp,s+l,2)
      ENDIF      
    ELSE
      IF (t:=MAX(ox,otx2)) > tx2 THEN Box(tx2+1,y1, t,y2, shp)
      SetAPen(rp,txp); Move(rp,midx,dss.txty); Text(rp,s+l,2)
      IF nx <= tx1
        SetBPen(rp,flp); Move(rp,tx1,dss.txty); Text(rp,s,l)
        IF ox >= nx
          IF nx > otx1
            Box(otx1,y1, nx-1,y2, shp); IF nx < tx1 THEN Box(nx,y1, tx1-1,y2, flp)
          ELSEIF ox >= MIN(otx1,tx1)
            IF nx < tx1 THEN Box(nx,y1, tx1-1,y2,flp)
          ELSE
            IF ox > nx THEN Box(nx,y1, ox-1,y2,flp)
            IF tx1 > otx1 THEN Box(otx1,y1, tx1-1,y2,flp)
          ENDIF
        ELSE
          Box(MIN(ox,otx1),y1, nx-1,y2,shp)
          IF tx1 > (t:=MAX(nx,otx1)) THEN Box(t,y1, tx1-1,y2, flp)
        ENDIF
      ELSE
        Box(nx,y1, midx,y2, flp)
        IF nx > (t:=MIN(otx1,ox)) THEN Box(t,y1, nx-1,y2, shp)
        SetDrMd(rp,RP_JAM1); SetAPen(rp,txp) 
        Move(rp,tx1,dss.txty); Text(rp,s,l)
      ENDIF
    ENDIF
  ENDIF
 ENDIF
ENDPROC


PROC message_test(imsg:PTR TO intuimessage,win:PTR TO window) OF midimonitor
DEF code
  code:=imsg.code
  IF imsg.class AND IDCMP_VANILLAKEY
    IF (code=9) OR (code=ESC_CODE) THEN RETURN TRUE
  ENDIF
ENDPROC FALSE

PROC message_action(class,qual,code,win:PTR TO window) OF midimonitor
 IF class AND IDCMP_VANILLAKEY
   self.keycode:=code; RETURN TRUE
 ENDIF
ENDPROC FALSE


/* ---------------------- external control procedures --------------------- */


EXPORT PROC updatemidimonitor(mc:PTR TO INT) OF midimonitor
DEF i,dss:PTR TO displaystatus
  IF mc
    dss:=self.ds
    FOR i:=0 TO 15 DO IF dss.value++ <> mc[]++ THEN self.setstatus(i,mc[-1])
  ELSE
    FOR i:=0 TO 15 DO IF dss.value++ <> -1 THEN self.setstatus(i,-1)
  ENDIF
ENDPROC

EXPORT PROC init(screen,font) OF midimonitor
DEF scr:PTR TO screen, drinfo:PTR TO drawinfo,penless,x:PTR TO INT,i
  IF screen=NIL THEN scr:=LockPubScreen(NIL) ELSE scr:=screen
  self.textpen:=1; self.shinepen:=2; self.fillpen:=3
  IF scr
    IF drinfo:=GetScreenDrawInfo(scr)
      penless:=drinfo.numpens
      x:=drinfo.pens
      IF penless>TEXTPEN THEN self.textpen:=x[TEXTPEN]
      IF penless>FILLPEN THEN self.fillpen:=x[FILLPEN]
      IF penless>SHINEPEN THEN self.shinepen:=x[SHINEPEN]
      IF font=NIL THEN font:=drinfo.font
      FreeScreenDrawInfo(scr,drinfo)
    ENDIF
    IF font=NIL THEN font:=scr.font
    IF screen=0 THEN UnlockPubScreen(NIL,scr)
  ENDIF
  self.font:=font
  FOR i:=0 TO 15 DO self.ds[i].value:=-1
ENDPROC
