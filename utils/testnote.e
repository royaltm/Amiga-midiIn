MODULE 'mathieeedoubbas','mathieeedoubtrans','tools/exceptions','*notecalc'

RAISE	"^C" IF CtrlC()<>0

PROC main() HANDLE
DEF a,b,s[10]:STRING,rate[2]:ARRAY OF LONG,note,fine,base


  IF 0=(mathieeedoubbasbase:=OpenLibrary('mathieeedoubbas.library',0)) THEN Throw("LIB",'mathieeedoubbas')
  IF 0=(mathieeedoubtransbase:=OpenLibrary('mathieeedoubtrans.library',0)) THEN Throw("LIB",'mathieeedoubtrans')

  a,b:=IeeeDPFlt(100000)
  rate[0]:=a; rate[1]:=b
  note:=60-24;  base:=60
  WriteF('********\n')
  FOR fine:=0 TO 200
    a,b:=noterate(rate,note,fine,base)
    WriteF('fine:\d[4] \s[10] \h[8] \z\h[8]\n',fine-100,RealF(s,IeeeDPTieee(a,b),8),a,b)
    CtrlC()
  ENDFOR

EXCEPT DO
  IF mathieeedoubtransbase THEN CloseLibrary(mathieeedoubtransbase)
  IF mathieeedoubbasbase THEN CloseLibrary(mathieeedoubbasbase)
  report_exception()
ENDPROC
