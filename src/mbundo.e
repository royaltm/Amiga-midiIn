OPT OSVERSION=37
OPT MODULE

MODULE  'exec/memory','exec/nodes','exec/lists',
        '*mbbanks','*soundfx_ahi','*midibplists','*mbreport','*mbplay','*mblocale'

EXPORT ENUM UNDO_PREP_SAMPLELIST=1,
            UNDO_SET_SAMPLELIST,      -> full sample list
            UNDO_PREP_SAMPLELISTINSTR,
            UNDO_SET_SAMPLELISTINSTR, -> bn.nstr*NUMBANKS, full sample list
            UNDO_PREP_ALL,
            UNDO_SET_ALL,             -> bankstart, banknum, banks, full sample list
            UNDO_SET_BANKS,           -> bankstart, banknum, banks, sample list
            UNDO_SET_XCHGBANKS,       -> bankstart, banknum, table of: swap1,swap2
            UNDO_SET_SAMPLEDELETE,    -> samplepath+name, table of: numbanks with instr=sample
            UNDO_SET_SAMPLEUNDELETE,  -> samplepath+name
            UNDO_SET_INSTR,           -> banknum, samplepath+name OR NIL (len=257)
            UNDO_SET_MIDI,            -> banknum, midi
            UNDO_SET_PRI,             -> ...
            UNDO_SET_BASE,
            UNDO_SET_FINE,          -> TUNE
            UNDO_SET_SET_LOOP,
            UNDO_SET_SET_DUR,
            UNDO_SET_SET_MONO,
            UNDO_SET_SET_ADDAFTERT, -> VOLUME
            UNDO_SET_BOUNDS,          -> banknum, lobound, hibound
            UNDO_SET_VOLUME,        -> VOLUME
            UNDO_SET_VELSENS,       -> VOLUME
            UNDO_SET_RELEASE,
            UNDO_SET_PANORAMA,      -> VOLUME
            UNDO_SET_PANWIDE,       -> VOLUME
            UNDO_SET_PITCHSENS,
            UNDO_SET_ATTACK,
            UNDO_SET_DECAY,
            UNDO_SET_SUSTAINLEV,
            UNDO_SET_AFTERSENS,     -> VOLUME
            UNDO_SET_FIRSTSKIP,
            UNDO_SET_MCTRLVOL,      -> VOLUME
            UNDO_SET_MCTRLPAN,      -> VOLUME
            UNDO_SET_GROUP,
            UNDO_SET_MONOVSENS,
            UNDO_SET_MONOSLIDE,
            UNDO_SET_MAX


CONST UNDO_REDO_FLAG=$4000, UNDO_SET_MASK=$3FFF

OBJECT undohd
  mln:mln
  size:LONG
  type:INT
ENDOBJECT

DEF undopool,                 -> undo memory pool pointer
    undolh:PTR TO mlh,        -> undo list header
    undocurr:PTR TO undohd,   -> undo current node (=header if nothing to undo)
    undonew:PTR TO undohd,    -> pointer to newly allocated node
    undosave:PTR TO undohd,   -> pointer to node on last save project (no asterix)
    undosize                  -> undo memory left

/*
  ----------------------------------------------------------------------------------
                            Undo pool allocation procedures
  ----------------------------------------------------------------------------------
*/

EXPORT PROC init_undo(max)
  IF undopool THEN RETURN
  undosize:=max-16; undolh:=NIL; undocurr:=NIL; undonew:=NIL
  IF (undopool:=CreatePool(MEMF_ANY,max,max))=NIL THEN Raise("MEM")
  undolh:=AllocPooled(undopool,SIZEOF mlh)
  IF undolh=NIL; free_undo(); Raise("MEM"); ENDIF
  newlist(undolh); undocurr:=undolh; remembersaveundo()
ENDPROC

EXPORT PROC flush_undo()
DEF ln:PTR TO ln, u:PTR TO undohd
  IF ln:=undolh.head
    WHILE ln.succ
      u:=ln; ln:=ln.succ; Remove(ln.pred)
      freeundonode(u)
    ENDWHILE
  ENDIF
  undocurr:=undolh; remembersaveundo()
ENDPROC

EXPORT PROC free_undo()
DEF u
  IF u:=undopool;    undopool:=0; DeletePool(u);  ENDIF
ENDPROC

/*
  ----------------------------------------------------------------------------------
                            other external functions
  ----------------------------------------------------------------------------------
*/

EXPORT PROC remembersaveundo()
  undosave:=undocurr
ENDPROC

EXPORT PROC asksaveundo() IS undocurr=undosave

EXPORT PROC undoleft() IS undosize

EXPORT PROC nextundo() IS IF undocurr <> undolh THEN undocurr.type ELSE NIL

EXPORT PROC do_undo(banks:PTR TO bank, slist:PTR TO lh)
DEF u:PTR TO undohd, nbank
  u:=IF undocurr <> undolh THEN undocurr ELSE NIL
  IF u=NIL; reqnotundo(); Raise(); ENDIF
  IF u:=undoreplace(u, banks, slist, {nbank})
    Insert(undolh,u,undocurr); Remove(undocurr)
    freeundonode(undocurr)
    undocurr:=u
  ENDIF
  undocurr:=undocurr.mln.pred
ENDPROC nbank

EXPORT PROC do_redo(banks:PTR TO bank, slist:PTR TO lh)
DEF u:PTR TO undohd, nbank
  u:=undocurr.mln.succ
  IF u.mln.succ <> NIL THEN undocurr:=u ELSE u:=NIL
  IF u=NIL; reqnotredo(); Raise(); ENDIF
  IF u:=undoreplace(u, banks, slist, {nbank})
    Insert(undolh,u,undocurr); Remove(undocurr)
    freeundonode(undocurr)
    undocurr:=u
  ENDIF
ENDPROC nbank

EXPORT PROC exportundo(node:PTR TO undohd)
DEF size=0,cks=0,redo=FALSE
  IF node
    node:=node-SIZEOF mln
    redo:=IF node.type AND UNDO_REDO_FLAG THEN TRUE ELSE FALSE
    node.type:=node.type AND UNDO_SET_MASK
  ENDIF
  IF redo=FALSE
    node:=IF node=NIL THEN undocurr ELSE node.mln.pred
    IF node=undolh; node:=NIL; redo:=TRUE; ENDIF
  ENDIF
  IF redo
    node:=IF node=NIL THEN undocurr.mln.succ ELSE node.mln.succ
    IF node.mln.succ=NIL THEN node:=NIL
  ENDIF
  IF node
    IF redo THEN node.type:=node.type OR UNDO_REDO_FLAG
    size:=node.size-SIZEOF mln
    node:=node+SIZEOF mln
    cks:=get_chksum(node,size)
  ENDIF
ENDPROC node,size,cks

EXPORT PROC importundo(ptr,cksum,numb,bsize) HANDLE
DEF node:undohd,l,m,bn:PTR TO bank,type,k,d,i,endp
  CopyMem(ptr-SIZEOF mln,node,SIZEOF undohd)
  IF get_chksum(ptr,node.size-SIZEOF mln)<>cksum THEN RETURN NIL
  ptr:=ptr-SIZEOF mln+SIZEOF undohd; endp:=ptr+node.size-SIZEOF undohd
  type:=node.type AND UNDO_SET_MASK
  IF (type=UNDO_SET_ALL) OR (type=UNDO_SET_BANKS)
    d:=ptr[1]; l:=d * SIZEOF bank+2; k:=node.size-SIZEOF undohd-(d*bsize+2)
    m:=allocundonode(l+k,type)
    bn:=m+2; m[0]:=ptr[]++; m[1]:=ptr[]++ 
    FOR i:=0 TO d-1
      bn[i].instr:=NIL; deletebank(bn); CopyMem(ptr,bn,bsize); ptr:=ptr+bsize
    ENDFOR
    IF k>0 THEN CopyMem(ptr,m+l,k)
  ELSEIF type=UNDO_SET_SAMPLELISTINSTR
    d:=numb*4; l:=NUMBANKS * 4; k:=node.size-SIZEOF undohd-d
    m:=allocundonode(l+k,type)
    FOR i:=0 TO d-1 DO m[i]:=ptr[i]
    FOR i:=d TO l-1 DO m[i]:=0
    IF k>0 THEN CopyMem(ptr+d,m+l,k)
  ELSE
    m:=allocundonode(node.size-SIZEOF undohd,type)
    CopyMem(node+SIZEOF undohd,m,node.size-SIZEOF undohd)
  ENDIF
  l:=undocurr; undoaddnew()
  IF node.type AND UNDO_REDO_FLAG THEN undocurr:=l
  remembersaveundo()
EXCEPT
  report_exception(); RETURN FALSE
ENDPROC endp


EXPORT PROC set_undo(type,obj,data,dat2=NIL,dat3=NIL) HANDLE
DEF bn:PTR TO bank, bns:PTR TO bank, lh:PTR TO lh, ln:PTR TO lln,
    x,k,i,l,m, snd:PTR TO sfx, instr:PTR TO LONG
  SELECT type
    CASE UNDO_SET_SAMPLEDELETE /* obj:sfx  data:allbanks */
      snd:=obj; bn:=data; l:=StrLen(k:=snd.pathname())+1
      FOR i:=0 TO NUMBANKS-1 DO IF bn[i].instr=snd THEN INC l
      m:=allocundonode(l,type)
      AstrCopy(m,k,l); m:=m+StrLen(m)+1
      FOR i:=0 TO NUMBANKS-1 DO IF bn[i].instr=snd THEN m[]++:=i
    CASE UNDO_SET_INSTR /* obj:allbanks  data:numbank */
      bn:=obj; bn:=bn[data]; l:=257
      m:=allocundonode(l,type); m[]++:=data
      IF snd:=bn.instr THEN AstrCopy(m,snd.pathname(),l-1) ELSE m[]:=0
    CASE UNDO_SET_XCHGBANKS /* obj:ptr to xchgdata data:xchglen */
      l:=data+1; m:=allocundonode(l,type)
      m[]++:=0; CopyMem(obj,m,data)
    CASE UNDO_SET_BANKS /* obj:banks data:numbanks dat2:nbank */
      bn:=obj; bn:=bn[dat2]; l:=data*SIZEOF bank + 2
      FOR i:=0 TO data-1
        IF snd:=bn[i].instr
          FOR k:=0 TO i-1 DO IF bn[k].instr=snd THEN snd:=NIL
          IF snd THEN l:=l+StrLen(snd.pathname())+1
        ENDIF
      ENDFOR
      m:=allocundonode(l,type); m[]++:=dat2; m[]++:=data
      CopyMem(bn,m,data*SIZEOF bank); bns:=m; m:=bns[data]
      FOR i:=0 TO data-1
        IF snd:=bns[i].instr THEN FOR k:=i+1 TO data-1 DO IF bns[k].instr=snd THEN bns[k].instr:=NIL
      ENDFOR
      x:=1
      FOR i:=0 TO data-1
        IF snd:=bns[i].instr
          l:=StrLen(snd.pathname())+1; AstrCopy(m,snd.pathname(),l)
          bns[i].instr:=x; INC x; m:=m+l
        ELSEIF snd:=bn[i].instr
          k:=0; WHILE k<i DO EXIT bn[k++].instr=snd
          IF bn[k-1].instr=snd THEN bns[i].instr:=bns[k-1].instr
        ENDIF
      ENDFOR
    DEFAULT
      IF (type=UNDO_PREP_SAMPLELIST) OR (type=UNDO_PREP_ALL) OR (type=UNDO_PREP_SAMPLELISTINSTR)
        lh:=obj; ln:=lh.head; k:=0; IF dat2=0 THEN dat2:=NUMBANKS
        IF type=UNDO_PREP_ALL
          k:=dat2 * SIZEOF bank + 2
        ELSEIF type=UNDO_PREP_SAMPLELISTINSTR
          k:=NUMBANKS * 4
        ENDIF
        l:=k
        WHILE ln.ln.succ
          snd:=ln.pointer; l:=l+StrLen(snd.pathname())+1; ln:=ln.ln.succ
        ENDWHILE
        m:=allocundonode(l,type)
        /* copying */
        IF type=UNDO_PREP_ALL /* dat2=numbanks dat3=startbank */
          bns:=data; bn:=m+2; CopyMem(bns[dat3], bn, dat2 * SIZEOF bank)
          FOR i:=0 TO dat2-1 DO IF snd:=bn[i].instr THEN bn[i].instr:=convlnptrtonum(snd,lh)
          m[0]:=dat3; m[1]:=dat2
        ELSEIF type=UNDO_PREP_SAMPLELISTINSTR
          bn:=data; instr:=m
          FOR i:=0 TO NUMBANKS-1 DO instr[i]:=IF snd:=bn[i].instr THEN convlnptrtonum(snd,lh) ELSE NIL
        ENDIF
        m:=m+k
        ln:=lh.head;
        WHILE ln.ln.succ
          snd:=ln.pointer; i:=StrLen(l:=snd.pathname())+1;
          AstrCopy(m,l,i); m:=m+i; ln:=ln.ln.succ
        ENDWHILE
        RETURN
      ELSEIF type=UNDO_SET_SAMPLELISTINSTR
        IF undonew
          IF undonew.type <> UNDO_PREP_SAMPLELISTINSTR THEN RETURN
          undonew.type:=type
        ENDIF
      ELSEIF type=UNDO_SET_SAMPLELIST
        IF undonew
          IF undonew.type <> UNDO_PREP_SAMPLELIST THEN RETURN
          undonew.type:=type
        ENDIF
      ELSEIF type=UNDO_SET_ALL
        IF undonew
          IF undonew.type <> UNDO_PREP_ALL THEN RETURN
          undonew.type:=type
        ENDIF
      ELSE /* obj:allbanks  data:numbank */
        IF undocurr <> undolh
          IF undocurr.type=type
            m:=undocurr + SIZEOF undohd
            IF m[]=data THEN RETURN
          ENDIF
        ENDIF
        bn:=obj; bn:=bn[data]
        SELECT type
          CASE UNDO_SET_MIDI
            l:=2; i:=bn.midi
          CASE UNDO_SET_PRI
            l:=2; i:=bn.pri
          CASE UNDO_SET_BASE
            l:=2; i:=bn.base
          CASE UNDO_SET_FINE
            l:=2; i:=bn.fine
          CASE UNDO_SET_SET_LOOP
            l:=2; i:=bn.set
          CASE UNDO_SET_SET_DUR
            l:=2; i:=bn.set
          CASE UNDO_SET_SET_MONO
            l:=2; i:=bn.set
          CASE UNDO_SET_SET_ADDAFTERT
            l:=2; i:=bn.set
          CASE UNDO_SET_BOUNDS
            l:=3; i:=Shl(bn.lobound,8) + bn.hibound
          CASE UNDO_SET_VOLUME
            l:=3; i:=bn.volume
          CASE UNDO_SET_VELSENS
            l:=2; i:=bn.velsens
          CASE UNDO_SET_RELEASE
            l:=2; i:=bn.release
          CASE UNDO_SET_PANORAMA
            l:=3; i:=bn.panorama
          CASE UNDO_SET_PANWIDE
            l:=2; i:=bn.panwide
          CASE UNDO_SET_PITCHSENS
            l:=2; i:=bn.pitchsens
          CASE UNDO_SET_ATTACK
            l:=2; i:=bn.attack
          CASE UNDO_SET_DECAY
            l:=2; i:=bn.decay
          CASE UNDO_SET_SUSTAINLEV
            l:=2; i:=bn.sustainlev
          CASE UNDO_SET_AFTERSENS
            l:=2; i:=bn.aftersens
          CASE UNDO_SET_FIRSTSKIP
            l:=3; i:=bn.firstskip
          CASE UNDO_SET_MCTRLVOL
            l:=2; i:=bn.mctrlvol
          CASE UNDO_SET_MCTRLPAN
            l:=2; i:=bn.mctrlpan
          CASE UNDO_SET_GROUP
            l:=2; i:=bn.group
          CASE UNDO_SET_MONOVSENS
            l:=2; i:=bn.monovsens
          CASE UNDO_SET_MONOSLIDE
            l:=3; i:=bn.monoslide
          DEFAULT
            RETURN
        ENDSELECT
        m:=allocundonode(l,type); m[]++:=data
        IF l < 3 THEN m[]:=i ELSE PutInt(m,i)
      ENDIF
  ENDSELECT
  undoaddnew()
EXCEPT
  report_exception()
ENDPROC
/*
  ----------------------------------------------------------------------------------
                              internal functions
  ----------------------------------------------------------------------------------
*/
PROC undoreplace(uhd:PTR TO undohd, banks:PTR TO bank, slist:PTR TO lh, banknum)
DEF type, m,l,i,k,x,d, snd:PTR TO sfx, bn:PTR TO bank, bns:PTR TO bank,
    ln:PTR TO lln, instr:PTR TO LONG,prgrs=0,total=0
  ^banknum:=-1
  type:=uhd.type AND UNDO_SET_MASK
  l:=uhd.size - SIZEOF undohd; m:=uhd + SIZEOF undohd
  IF type=UNDO_SET_SAMPLEDELETE /* samplename + numbanks active */
    uhd.type:=UNDO_SET_SAMPLEUNDELETE
    IF (snd:=addsnd(m, slist))=NIL
      undosave:=NIL
    ELSE
      l:=uhd + uhd.size; m:=m+StrLen(m)+1
      WHILE m < l
        IF (i:=m[]++) < NUMBANKS THEN IF setinstr(banks[i], snd)=FALSE THEN undosave:=NIL
      ENDWHILE
    ENDIF
    addingsamplesover()
    uhd:=NIL
  ELSEIF type=UNDO_SET_SAMPLEUNDELETE
    IF ln:=FindName(slist,FilePart(m)) THEN snd:=ln.pointer ELSE snd:=0
    IF snd THEN delsnd(snd, banks)
    uhd.type:=UNDO_SET_SAMPLEDELETE
    uhd:=NIL
  ELSEIF (type=UNDO_SET_SAMPLELIST) OR (type=UNDO_SET_ALL) OR (type=UNDO_SET_SAMPLELISTINSTR)
    /* new store */
    ln:=slist.head; k:=0
    IF type=UNDO_SET_ALL
      d:=m[1]; x:=m[0]; k:=d * SIZEOF bank + 2
    ELSEIF type=UNDO_SET_SAMPLELISTINSTR
      k:=NUMBANKS * 4
    ENDIF
    l:=k
    WHILE ln.ln.succ
      snd:=ln.pointer; l:=l+StrLen(snd.pathname())+1; ln:=ln.ln.succ
    ENDWHILE
    m:=allocundonode(l,type,TRUE)
    /* copying */
    IF type=UNDO_SET_ALL
      bn:=m+2; CopyMem(banks[x], bn, d * SIZEOF bank)
      FOR i:=0 TO d-1 DO IF snd:=bn[i].instr THEN bn[i].instr:=convlnptrtonum(snd,slist)
      m[0]:=d; m[1]:=x
    ELSEIF type=UNDO_SET_SAMPLELISTINSTR
      instr:=m
      FOR i:=0 TO NUMBANKS-1 DO instr[i]:=IF snd:=banks[i].instr THEN convlnptrtonum(snd,slist) ELSE NIL
    ENDIF
    m:=m+k
    ln:=slist.head
    WHILE ln.ln.succ
      snd:=ln.pointer; i:=StrLen(l:=snd.pathname())+1;
      AstrCopy(m,l,i); m:=m+i; ln:=ln.ln.succ
    ENDWHILE
    /* //////// real un/redo //////// */
    l:=uhd + uhd.size; ln:=slist.head; instr:=uhd + SIZEOF undohd; bn:=instr + 2
    WHILE ln.ln.succ
      m:=uhd + SIZEOF undohd + k
      WHILE m < l
        EXIT StrCmp(FilePart(m),ln.ln.name,ALL); m:=m+StrLen(m)+1
      ENDWHILE
      snd:=ln.pointer
      ln:=ln.ln.succ
      IF m >= l THEN delsnd(snd, banks)
    ENDWHILE
    m:=uhd + SIZEOF undohd + k
    IF (prgrs:=m) < l
      total:=0; WHILE prgrs < l; prgrs:=prgrs+StrLen(prgrs)+1; INC total; ENDWHILE
      prgrs:=0
    ENDIF
    k:=1
    WHILE m < l
      printstatus(getLocStr(STRID_LOOKINGFORSAMPLE),FilePart(m),0,prgrs++,total)
      IF (snd:=addsnd(m,slist))=NIL THEN undosave:=NIL
      IF type=UNDO_SET_ALL THEN FOR i:=0 TO d-1 DO IF bn[i].instr=k THEN bn[i].instr:=snd
      IF type=UNDO_SET_SAMPLELISTINSTR THEN FOR i:=0 TO NUMBANKS-1 DO IF instr[i]=k THEN instr[i]:=snd
      m:=m+StrLen(m)+1; INC k
    ENDWHILE
    total:=0; prgrs:=0
    IF type=UNDO_SET_ALL
      FOR i:=0 TO d-1 DO IF bn[i].instr <> NIL THEN INC total
      FOR i:=0 TO d-1
        IF bn[i].instr <> NIL THEN printstatus(0,0,0,prgrs++,total)
        IF setbank(banks[i+x], bn[i])=FALSE THEN undosave:=NIL
      ENDFOR
    ELSEIF type=UNDO_SET_SAMPLELISTINSTR
      FOR i:=0 TO NUMBANKS-1 DO IF instr[i]<>NIL THEN INC total
      FOR i:=0 TO NUMBANKS-1
        IF instr[i]<>NIL THEN printstatus(0,0,0,prgrs++,total)
        IF setinstr(banks[i],instr[i])=FALSE THEN undosave:=NIL
      ENDFOR
    ENDIF
    addingsamplesover()
    uhd:=undonew; undonew:=NIL
  ELSEIF type=UNDO_SET_BANKS /* bankstart + numbanks + banksdata + samples */
    x:=m[]++; bn:=banks[x]; d:=m[]; l:=d*SIZEOF bank + 2
    FOR i:=0 TO d-1
      IF snd:=bn[i].instr
        FOR k:=0 TO i-1 DO IF bn[k].instr=snd THEN snd:=NIL
        IF snd THEN l:=l+StrLen(snd.pathname())+1
      ENDIF
    ENDFOR
    m:=allocundonode(l,type,TRUE); m[]++:=x; m[]++:=d
    CopyMem(bn,m,d*SIZEOF bank); bns:=m; m:=bns[d]
    FOR i:=0 TO d-1
      IF snd:=bns[i].instr THEN FOR k:=i+1 TO d-1 DO IF bns[k].instr=snd THEN bns[k].instr:=NIL
    ENDFOR
    x:=1
    FOR i:=0 TO d-1
      IF snd:=bns[i].instr
        l:=StrLen(snd.pathname())+1; AstrCopy(m,snd.pathname(),l)
        bns[i].instr:=x; INC x; m:=m+l
      ELSEIF snd:=bn[i].instr
        k:=0; WHILE k<i DO EXIT bn[k++].instr=snd
        IF bn[k-1].instr=snd THEN bns[i].instr:=bns[k-1].instr
      ENDIF
    ENDFOR
    /* //////// real set ///////// */
    m:=uhd + SIZEOF undohd + 2; l:=uhd + uhd.size; bns:=m
    total:=0; prgrs:=0; FOR i:=0 TO d-1 DO IF bns[i].instr <> NIL THEN INC total
    FOR i:=0 TO d-1
      IF (x:=bns[i].instr) <> NIL
        printstatus(0,0,0,prgrs++,total)
        m:=bns[d]; WHILE (m < l) AND (x-- > 0) DO m:=m+StrLen(m)+1
        IF setinstrname(bn[i], slist, m)=FALSE THEN undosave:=0
        bns[i].instr:=bn[i].instr
      ENDIF
      setbank(bn[i], bns[i])
    ENDFOR
    addingsamplesover()
    uhd:=undonew; undonew:=NIL
  ELSEIF type=UNDO_SET_XCHGBANKS /* m:tab of xgchbanks l:len */
    uhd:=NIL; l:=l-1
    IF m[] <> 0
      i:=0; l:=l-2; d:= 2
    ELSE
      i:=l-2; l:=0; d:= -2
    ENDIF
    m[]++:=Not(m[])
    WHILE ((d > 0) AND (i<=l)) OR ((d < 0) AND (i>=l))
      k:=m[i]; x:=m[i+1]; IF (k < NUMBANKS) AND (x < NUMBANKS) THEN xchgbanksachn(banks[k],banks[x])
      i:=i+d
    ENDWHILE
  ELSE
    uhd:=NIL
    IF (i:=m[]++) >= NUMBANKS THEN RETURN NIL
    ^banknum:=i
    bn:=banks[i]
    SELECT type
    CASE UNDO_SET_INSTR /* numbank + soundname */
      snd:=bn.instr
      IF setinstrname(bn, slist, m)=FALSE THEN undosave:=NIL
      IF snd THEN AstrCopy(m,snd.pathname(),l-1) ELSE m[]:=0
      addingsamplesover()
    CASE UNDO_SET_MIDI
      i:=bn.midi; bn.midi:=m[]
    CASE UNDO_SET_PRI
      i:=bn.pri; bn.pri:=m[]
    CASE UNDO_SET_BASE
      i:=bn.base; bn.base:=m[]
    CASE UNDO_SET_FINE
      i:=bn.fine; bn.fine:=m[]; signal_playtask(PSG_TUNE,bn)
    CASE UNDO_SET_SET_LOOP
      i:=bn.set; bn.set:=m[]
    CASE UNDO_SET_SET_DUR
      i:=bn.set; bn.set:=m[]
    CASE UNDO_SET_SET_MONO
      i:=bn.set; bn.set:=m[]
    CASE UNDO_SET_SET_ADDAFTERT
      i:=bn.set; bn.set:=m[]; signal_playtask(PSG_VOLUME,bn)
    CASE UNDO_SET_BOUNDS
      i:=Shl(bn.lobound,8) + bn.hibound
      bn.lobound:=m[0]; bn.hibound:=m[1]
    CASE UNDO_SET_VOLUME
      i:=bn.volume; bn.volume:=Int(m); signal_playtask(PSG_VOLUME,bn)
    CASE UNDO_SET_VELSENS
      i:=bn.velsens; bn.velsens:=m[]; signal_playtask(PSG_VOLUME,bn)
    CASE UNDO_SET_RELEASE
      i:=bn.release; bn.release:=m[]
    CASE UNDO_SET_PANORAMA
      i:=bn.panorama; bn.panorama:=Int(m); signal_playtask(PSG_VOLUME,bn)
    CASE UNDO_SET_PANWIDE
      i:=bn.panwide; bn.panwide:=m[]; signal_playtask(PSG_VOLUME,bn)
    CASE UNDO_SET_PITCHSENS
      i:=bn.pitchsens; bn.pitchsens:=m[]
    CASE UNDO_SET_ATTACK
      i:=bn.attack; bn.attack:=m[]
    CASE UNDO_SET_DECAY
      i:=bn.decay; bn.decay:=m[]
    CASE UNDO_SET_SUSTAINLEV
      i:=bn.sustainlev; bn.sustainlev:=m[]
    CASE UNDO_SET_AFTERSENS
      i:=bn.aftersens; bn.aftersens:=m[]; signal_playtask(PSG_VOLUME,bn)
    CASE UNDO_SET_FIRSTSKIP
      i:=bn.firstskip; bn.firstskip:=Int(m)
    CASE UNDO_SET_MCTRLVOL
      i:=bn.mctrlvol; bn.mctrlvol:=m[]; signal_playtask(PSG_VOLUME,bn)
    CASE UNDO_SET_MCTRLPAN
      i:=bn.mctrlpan; bn.mctrlpan:=m[]; signal_playtask(PSG_VOLUME,bn)
    CASE UNDO_SET_GROUP
      i:=bn.group; bn.group:=m[]
    CASE UNDO_SET_MONOVSENS
      i:=bn.monovsens; bn.monovsens:=m[]
    CASE UNDO_SET_MONOSLIDE
      i:=bn.monoslide; bn.monoslide:=Int(m)
    DEFAULT
      RETURN NIL
    ENDSELECT
    IF l=2 THEN m[]:=i
    IF l=3 THEN PutInt(m,i)
  ENDIF      
ENDPROC uhd

PROC freeundonode(u:PTR TO undohd)
DEF size:REG
  IF u=NIL THEN RETURN
  size:=(u.size+7) AND $FFFFFFF8
  undosize:=undosize+size; FreePooled(undopool,u,u.size)
  IF u=undosave THEN undosave:=NIL
ENDPROC

PROC allocundonode(size,type,leavecurr=FALSE)
DEF u:PTR TO undohd
  size:=size + SIZEOF undohd
  IF undopool=NIL THEN Raise("MEM")
  IF u:=undonew
     undonew:=NIL; freeundonode(u)
  ENDIF
  WHILE (undosize <= size) OR (undonew=NIL)
    IF undosize > size THEN undonew:=AllocPooled(undopool,size)
    EXIT undonew <> NIL
    IF (undocurr=undolh.head) OR (undocurr=undolh)
      IF (u:=RemTail(undolh))=NIL THEN Raise("UNDO")
      freeundonode(u)
      IF u=undocurr
        undocurr:=undolh
        IF leavecurr THEN Raise("UNDO")
      ENDIF
    ELSE
      IF (u:=RemHead(undolh))=NIL THEN Raise("UNDO")
      freeundonode(u)
    ENDIF
  ENDWHILE
  undonew.size:=size; undonew.type:=type
  undonew.mln.succ:=NIL; undonew.mln.pred:=NIL;
  size:=(size+7) AND $FFFFFFF8; undosize:=undosize-size
ENDPROC undonew + SIZEOF undohd

PROC undoaddnew()
DEF u:PTR TO undohd
  IF undonew=NIL THEN RETURN
  WHILE undocurr <> undolh.tailpred
    IF u:=RemTail(undolh) THEN freeundonode(u)
  ENDWHILE
  AddTail(undolh,undonew); undocurr:=undonew; undonew:=NIL
-> wypiszinfos()
ENDPROC

PROC get_chksum(start,size)
DEF csum:REG
    MOVE.L size,D2; MOVE.L start,A0; MOVEQ #0,csum; SUBQ.W #1,D2
chkmainloop:
    MOVEQ #1,D0
chksumloop:
    MOVE.B (A0)+,D1
    LSL.L #8,D0
    BCS.S chkaddit
    OR.B  D1,D0
    DBRA  D2,chksumloop
    ADD.L D0,csum
    BRA.S chksumend
chkaddit:
    OR.B  D1,D0
    ADD.L D0,csum
    DBRA  D2,chkmainloop
chksumend:
ENDPROC csum
/*

PROC wypiszinfos()
DEF u:PTR TO undohd
  u:=undolh.head; WriteF('Undolist start:\n')
  WHILE u.mln.succ; WriteF('uhd:\h type:\d size:\d\n',u-undolh,u.type,u.size - SIZEOF undohd); u:=u.mln.succ; ENDWHILE
  WriteF('undocurr:\h undosize=\d\n',undocurr-undolh,undosize)
ENDPROC
*/
