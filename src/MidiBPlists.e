OPT MODULE
OPT EXPORT

MODULE 'exec/lists','exec/nodes','utility'

OBJECT lln
  ln:ln
  pointer:LONG
ENDOBJECT

PROC newlist(lh=NIL:PTR TO lh,type=0)
  IF lh=NIL THEN lh:=NEW lh
  lh.head:=lh+4
  lh.tailpred:=lh
  lh.tail:=0
ENDPROC lh

PROC addsorted(lh:PTR TO lh,ln:PTR TO ln)
DEF n:REG,g:PTR TO ln

  n:=ln.name; g:=lh.head
  WHILE g.succ
    IF Stricmp(g.name,n) >= 0
      Insert(lh,ln,g.pred)
      RETURN
    ENDIF
    g:=g.succ
  ENDWHILE
  AddTail(lh,ln)

ENDPROC

PROC convlnptrtonum(ptr,lh:PTR TO lh)
DEF i,ln:PTR TO lln
  ln:=lh.head; i:=1
  WHILE ln.ln.succ
    EXIT ptr=ln.pointer
    ln:=ln.ln.succ; INC i
  ENDWHILE
  IF ptr <> ln.pointer THEN i:=0
ENDPROC i

/*
PROC getvarafterln(ln:PTR TO ln)
DEF x
    x:=ln+SIZEOF ln;    x:=^x
ENDPROC x
*/
/*
PROC numfromnode(curr:PTR TO ln, node:PTR TO ln)
DEF i=0
  IF node
    curr:=curr.succ
    WHILE curr.succ
      INC i
      IF curr=node THEN RETURN i
      curr:=curr.succ
    ENDWHILE
  ENDIF
ENDPROC FALSE
*/
