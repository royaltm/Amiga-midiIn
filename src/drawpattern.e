OPT MODULE

MODULE 'graphics/rastport','graphics/view','graphics/gfx'

EXPORT PROC drawpattern(rp,cm,x,y,w,h,ctab:PTR TO LONG,pattern,pw,ph) HANDLE
DEF tmprp:rastport, bm=0, coltab=0:PTR TO LONG, tmpar=0,
    i,acth,pch,pcw,tmpptr,cnum

  cnum:=Shr(ctab[]++,16)
  NEW coltab[cnum+2]; coltab[0]:=cnum; coltab[1]:=cm
  FOR i:=2 TO cnum+1 DO coltab[i]:=-1
  FOR i:=2 TO cnum+1
    coltab[i]:=ObtainBestPenA(cm,ctab[]++,ctab[]++,ctab[]++,[OBP_PRECISION,PRECISION_EXACT,NIL,NIL])
    IF coltab[i] < 0 THEN Raise("NCOL")
  ENDFOR
  IF (bm:=AllocBitMap((Min(w,pw)+15) AND $FFFFFFF0 ,1,8, BMF_CLEAR OR BMF_INTERLEAVED, NIL))=0 THEN Raise("MEM")
  CopyMem(rp,tmprp,SIZEOF rastport); tmprp.layer:=0; tmprp.bitmap:=bm
  tmpar:=NewR((Min(w,pw)+15) AND $FFFFFFF0)

  acth:=0; pch:=ph-h/2; WHILE pch < 0 DO pch:=pch+ph
  pcw:=pw-w/2; WHILE pcw < 0 DO pcw:=pcw+pw
  WHILE acth < h
    tmpptr:=pch*pw+pattern
    FOR i:=0 TO Min(w,pw)-1
      tmpar[i]:=coltab[tmpptr[pcw++]+2]; IF pcw >= pw THEN pcw:=0
    ENDFOR
    WritePixelLine8(rp,x,y+acth,Min(w,pw),tmpar,tmprp)
    INC pch; IF pch >= ph THEN pch:=0
    INC acth
  ENDWHILE
  pcw:=x
  WHILE w > pw
    w:=w-pw; pcw:=pcw+pw
    ClipBlit(rp, x, y, rp, pcw, y, Min(w,pw), h, $0C0)
  ENDWHILE

EXCEPT DO
  FreeBitMap(bm)
  IF tmpar THEN Dispose(tmpar)
  IF exception THEN coltab:=freepattern(coltab)
ENDPROC coltab

EXPORT PROC freepattern(coltab:PTR TO LONG)
DEF i,n,cm
  IF coltab
    n:=coltab[0]; cm:=coltab[1]
    FOR i:=2 TO n+1
      IF coltab[i]>=0 THEN ReleasePen(cm,coltab[i])
    ENDFOR
    END coltab[n+2]
  ENDIF
ENDPROC NIL
