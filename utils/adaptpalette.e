OPT OSVERSION=37
OPT LARGE

CONST P_WIDTH=256,
      P_HEIGHT=192


PROC main()
DEF i,c,ptr:PTR TO CHAR,src:PTR TO CHAR,n:REG,r:REG,
    array[P_WIDTH]:ARRAY OF CHAR, tmparray[P_WIDTH]:ARRAY OF CHAR,
    colors[64]:ARRAY OF INT,colord[64]:ARRAY OF CHAR,f=0


  FOR i:=0 TO 63 DO colors[i]:=0
  ptr:={palette}
  src:={picturebody}

  FOR n:=0 TO P_HEIGHT-1
    MOVE.L src,A0; MOVE.L tmparray,A1; LEA P_WIDTH/8*6(A1),A2
bodyloop:
    MOVEQ  #0,D0;  MOVE.B (A0)+,D0;  BPL.S bodycopy;  NEG.B D0;  MOVE.B (A0)+,D1
bodyfill:
    MOVE.B  D1,(A1)+;    DBRA    D0,bodyfill
    CMPA.L  A2,A1;    BCS.S   bodyloop;    BRA.S   bodyend
bodycopy:
    MOVE.B  (A0)+,(A1)+;    DBRA    D0,bodycopy
    CMPA.L  A2,A1;    BCS.S   bodyloop
bodyend:
    MOVE.L  A0,src

    r:=P_WIDTH/8
    MOVE.L  tmparray,A0;    MOVE.L  array,A1;    LEA  P_WIDTH(A1),A2
chunkyloop:
    MOVEQ   #15,D2
chunkypixloop:
    MOVE.L  A0,A3; MOVEQ  #5,D1
chunkywordloop:
    LSL.W   #1,(A3);    ROXR.B  #1,D0;  ADD.L r,A3; DBRA D1,chunkywordloop
  LSR.B   #2,D0
    MOVE.B  D0,(A1)+
    DBRA    D2,chunkypixloop
    ADDQ.L  #2,A0
    CMPA.L  A2,A1
    BCS.S   chunkyloop

    FOR i:=0 TO P_WIDTH-1
      c:=array[i]; colors[c]:=colors[c]+1
    ENDFOR
  ENDFOR
  FOR i:=0 TO 63
    WriteF('\d=\d \h \h \h\n',i,colors[i],ptr[3*i],ptr[3*i+1],ptr[3*i+2])
    colord[i]:=i
  ENDFOR
  colors[1]:=32767
  colors[2]:=32766
  FOR i:=1 TO 63
    n:=i
    WHILE colors[n] > colors[n-1]
      c:=colors[n]; colors[n]:=colors[n-1]; colors[n-1]:=c
      c:=colord[n]; colord[n]:=colord[n-1]; colord[n-1]:=c
      DEC n; EXIT n<=0
    ENDWHILE
  ENDFOR

  WriteF('[')
  FOR i:=0 TO 1
    FOR n:=0 TO 31 DO WriteF('\d,',colord[i*32+n])
    WriteF('\n')
  ENDFOR

ENDPROC

picturebody:
  INCBIN '/bodyraw.bin'
palette:
  INCBIN '/cmapraw.bin'

