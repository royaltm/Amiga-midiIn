PROC main()
DEF a,b,x,seg,s[10]:STRING

  IF seg:=LoadSeg('w:modules/pr1')

    WriteF('And the winner is:\h\n',Long(seg*4+4))
    MOVE.L seg,D0
    LSL.L  #2,D0
    ADDQ.L #4,D0
    MOVE.L D0,A0

    MOVE.L #$11555555,D0
    MOVE.L #8,D1

    JSR   (A0)

    MOVE.L D1,a
    MOVE.L D2,b

/*
    b:=!(b!)/($7FFFFFFF!)
    RealF(s,b,8)
*/
    WriteF('And the winner is:\h,\d\n',a,b)
  ENDIF

  UnLoadSeg(seg)

/*

  MOVEA.L #2,A0

  MOVEM.L A0/A4,-(A7)
  MOVEM.L (A7)+,D0/D1

/*
  MOVE.L #$0000F000,D0
  MOVE.L #$12340014,D1
  MOVEQ   #32,D2
  SUB.W   D1,D2
  LONG $EDC01862 ->  bfffo d0{d1:d2},d1
  NOT.W  D1
  BCLR   D1,D0
  NEG.W  D1
*/
  MOVE.L D0,r
  MOVE.L D1,a
/*
  BEQ.S zero
  r:=TRUE
  JUMP dupa
zero:
  r:=FALSE
dupa:
*/
*/

ENDPROC
