PROC main()
DEF a,b,i,s[20]:STRING,x,table:PTR TO INT,index=0,fh=0,v
  NEW table[2048]
  WriteF('**************************\n')
  WriteF('INT ')
  x:=4; v:=!Fexp(1.0)-1.0
  FOR i:=2047 TO 0 STEP -1
    a:=!(i!)/2047.0
    b:=!Fexp(a)-1.0
    b:=!32767.0*b/v
    StringF(s,'\d',!b!)
    table[index++]:=!b!
    x:=x+EstrLen(s)+1
    IF x > 80
      x:=4
      WriteF('\s\nINT ',s)
    ELSE
      WriteF('\s,',s)
    ENDIF
  ENDFOR
  WriteF('\ntable[1024]=\d\n',table[1024])
  IF fh:=Open('RAM:exptable.bin',NEWFILE)
    Write(fh,table,2048*2)
    Close(fh)
  ENDIF
  Inp(stdout)
ENDPROC
