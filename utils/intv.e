MODULE 'exec/lists',
       'exec/nodes',
       'exec/interrupts',
       'exec/execbase'

PROC main()
DEF i,iv:PTR TO iv,x:PTR TO execbase,s
x:=execbase
iv:=x.ivtbe
FOR i:=0 TO 15
  s:=iv[i].node.name; IF s=0 THEN s:='<no name>'
  PrintF('\d=\h \s\n',i,iv[i].code,s)
ENDFOR

ENDPROC
