OPT MODULE
OPT OSVERSION=37
OPT LARGE

MODULE 'tools/EasyGUI','intuition/intuition','graphics/rastport','intuition/screens',
       'graphics/text','graphics/gfx','graphics/view','layers'



EXPORT OBJECT progressbar OF plugin PRIVATE
  rport:PTR TO rastport
  win:PTR TO window
  font:PTR TO textfont
  text1:PTR TO CHAR
  text2:PTR TO CHAR
  progress:INT
  full:INT
  textpen:LONG
  shinepen:LONG
  backpen:LONG
  fillpen:LONG
ENDOBJECT

EXPORT PROC min_size(ta:PTR TO textattr,fontheight) OF progressbar IS 100,self.font.ysize

EXPORT PROC render(ta:PTR TO textattr,x,y,xs,ys,win:PTR TO window) OF progressbar
  stdrast:=win.rport; self.rport:=stdrast
  self.win:=win
  self.settext(self.text1,self.text2,self.progress,self.full)
ENDPROC

EXPORT PROC clear_render(win:PTR TO window) OF progressbar
  self.rport:=NIL
ENDPROC

EXPORT PROC init(screen,font) OF progressbar
DEF scr:PTR TO screen, drinfo:PTR TO drawinfo,penless,x:PTR TO INT
  self.textpen:=1
  self.shinepen:=2
  self.backpen:=0
  self.fillpen:=3
  self.progress:=0
  self.full:=1
  IF screen=NIL THEN scr:=LockPubScreen(NIL) ELSE scr:=screen
  IF scr
    IF drinfo:=GetScreenDrawInfo(scr)
      penless:=drinfo.numpens
      x:=drinfo.pens
      IF penless>BACKGROUNDPEN THEN self.backpen:=x[BACKGROUNDPEN]
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
ENDPROC

EXPORT PROC settext(text1,text2,progress,full) OF progressbar
DEF l=0,k=0,region,xp,t1x,t2x,xmax,ymax,ytx
  IF text1 THEN self.text1:=text1 ELSE text1:=self.text1
  IF text2 THEN self.text2:=text2 ELSE text2:=self.text2
  IF full>0
    WHILE full > 32767; full:=Shr(full,1); progress:=Shr(progress,1); ENDWHILE
    IF progress > full THEN progress:=full
    self.progress:=progress
    self.full:=full
  ELSE
    progress:=self.progress
    full:=self.full
  ENDIF

  IF (stdrast:=self.rport)=NIL THEN RETURN

  xmax:=self.x+self.xs-1; ymax:=self.y+self.ys-1; ytx:=self.y+self.font.baseline
  IF text2 THEN WHILE (l:=TextLength(stdrast,text2,StrLen(text2))) > (self.xs/2) DO INC text2
  IF text1 THEN WHILE (k:=TextLength(stdrast,text1,StrLen(text1))) > (self.xs-l-13) DO INC text1
  t2x:=xmax-l; t1x:=self.x+3
  xp:=(self.xs-1)*progress/full+self.x
  SetDrMd(stdrast, RP_JAM2 )
  SetFont(stdrast,self.font)
  IF xp > self.x
    IF region:=NewRegion()
      IF OrRectRegion(region,[self.x,self.y, xp,ymax]:rectangle)
        region:=InstallClipRegion(self.win.wlayer,region)
      ELSE
        DisposeRegion(region); region:=0; RETURN
      ENDIF
      IF t1x > self.x THEN Box(self.x,self.y,t1x-1,ymax,self.textpen)
      SetBPen(stdrast,self.textpen); SetAPen(stdrast,self.shinepen)
      IF k>0 THEN TextF(t1x,ytx,'\s',text1)
      IF xp >= (t1x+k) THEN Box(t1x+k,self.y, Min(xp,t2x-1),ymax,self.textpen)
      SetAPen(stdrast,self.shinepen)
      IF (l>0) AND (t2x <= xp) THEN TextF(t2x,ytx,'\s',text2)
      region:=InstallClipRegion(self.win.wlayer,region); ClearRegion(region)
      IF xp < xmax
        IF OrRectRegion(region,[xp+1,self.y, xmax,ymax]:rectangle)
          region:=InstallClipRegion(self.win.wlayer,region)
        ELSE
          DisposeRegion(region); region:=0; RETURN
        ENDIF
        SetAPen(stdrast,self.textpen); SetBPen(stdrast,self.backpen)
        IF (t1x+k-1) > xp THEN TextF(t1x,ytx,'\s',text1)
        IF t2x > (xp+1) THEN Box(Max(xp+1,t1x+k),self.y, t2x,ymax,self.backpen)
        SetAPen(stdrast,self.textpen)
        IF l>0 THEN TextF(t2x,ytx,'\s',text2)
        region:=InstallClipRegion(self.win.wlayer,region); DisposeRegion(region); region:=0
      ELSE    
        DisposeRegion(region)
      ENDIF
    ENDIF
  ELSE
    Box(t1x+k,self.y, t2x,ymax,self.backpen)
    SetAPen(stdrast,self.textpen); SetBPen(stdrast,self.backpen)
    IF k>0 THEN TextF(t1x,ytx,'\s',text1)
    IF l>0 THEN TextF(t2x,ytx,'\s',text2)
  ENDIF
ENDPROC
