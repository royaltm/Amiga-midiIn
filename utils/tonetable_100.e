MODULE 'mathieeedoubbas','mathieeedoubtrans','tools/exceptions'

RAISE	"^C" IF CtrlC()<>0

PROC main() HANDLE
DEF mathname,x,n,i,a,b,c,d,e,f,g,h,noteval:PTR TO INT,notetab:PTR TO LONG
DEF fh=0

mathname:='mathieeedoubbas.library'

NEW notetab[(100*13+1)*2]

noteval:=[831,880,932,988,1046,1108,1174,1244,1318,1398,1480,1568,1662,1760]:INT

IF 0=(mathieeedoubbasbase:=OpenLibrary(mathname,0)) THEN Throw("LIB",mathname)
WriteF('\n***************************************\n')

x:=0
FOR i:=0 TO 12

  a,b:=IeeeDPFlt(noteval[i])	->val0
  c,d:=IeeeDPFlt(noteval[i+1])	->(val1)
  e,f:=IeeeDPSub(c,d,a,b)	->val1-val0

  WriteF('\nNote [\s]:',ListItem(['A','A#','H','C','C#','D','D#','E','F','F#','G','G#','A'],i))
	WriteF('\d \n',IeeeDPFix(a,b))
  FOR n:=0 TO 99
    c,d:=IeeeDPFlt(n)
    g,h:=IeeeDPMul(e,f,c,d)	->(val1-val0)*n
    c,d:=IeeeDPFlt(100)
    g,h:=IeeeDPDiv(g,h,c,d)	->(val1-val0)*n/100
    g,h:=IeeeDPAdd(a,b,g,h)
    notetab[x++]:=g
    notetab[x++]:=h
->    WriteF('[33m\d#[31m\d[0m\t',n,IeeeDPFix(g,h))
  CtrlC()
  ENDFOR

ENDFOR
  g,h:=IeeeDPFlt(noteval[i])
    notetab[x++]:=g
    notetab[x++]:=h
->    WriteF('[33m\d#[31m\d[0m\t',n,IeeeDPFix(g,h))

IF 0=(fh:=Open('RAM:tonetable.bin',NEWFILE)) THEN Throw("OPEN",'file')
Write(fh,notetab,(100*13+1)*2*4)

 displaytable(notetab)

EXCEPT DO
IF mathieeedoubbasbase THEN CloseLibrary(mathieeedoubbasbase)
IF fh THEN Close(fh)
report_exception()
ENDPROC

PROC displaytable(nt:PTR TO LONG) HANDLE
DEF i,x,a,b,c,s[10]:STRING
  
  IF 0=(mathieeedoubtransbase:=OpenLibrary('mathieeedoubtrans.library',0)) THEN Throw("LIB",'mathieeedoubtrans')
  FOR i:=0 TO 12
    WriteF('\nNote [\s]:\n',ListItem(['A','A#','H','C','C#','D','D#','E','F','F#','G','G#','A'],i))
    FOR x:=0 TO 99
      a:=nt[]++ ;b:=nt[]++
      c:=IeeeDPTieee(a,b)
      WriteF('\z\d[3]:\s[9]  ',x,RealF(s,c,8))
      CtrlC()
  	ENDFOR
  ENDFOR
  a:=nt[]++ ;b:=nt[]++
  c:=IeeeDPTieee(a,b)
  WriteF('\d[3]:\s[9]  ',x,RealF(s,c,8))
EXCEPT DO
  IF mathieeedoubtransbase THEN CloseLibrary(mathieeedoubtransbase)
  IF exception THEN ReThrow()
ENDPROC
