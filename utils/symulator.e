DEF volume=512,
    velocity=64,
    velsens=100,
    aftch=128,
    aftersens=60,
    set=1


PROC main()
DEF i
  FOR i:=0 TO 127 STEP 1
    aftch:=i
    WriteF('v:\d vel:\d vs:\d aft:\d afs:\d = \d\n',volume,velocity,velsens,aftch,aftersens,
    calcvolume())
  ENDFOR

ENDPROC

PROC calcvolume()
DEF voll:REG,volr:REG,vol:REG,pan:REG,vel:REG,vtab:PTR TO INT

  vtab:={exptable} -> 2048 * INT

  vol:=velocity; MOVEQ #127,vel; SUB.L vol,vel; LSL.L #4,vel
  vol:=velsens; MULU vol,vol;  MULU vol,vel; DIVU #10000,vel; EXT.L vel
  /* vel:=(127-velocity)*16*(velsens^2)/10000 */
  IF set AND 1
    vol:=aftch; CMPI.W #128,vol; BCS.S aftertouch_additive
    MOVE.L  vel,vol; BRA.S aftertouch_over
aftertouch_additive:
    pan:=aftersens; MULU pan,pan; LSL.L #4,vol; MULU pan,vol; DIVU #10000,vol
    MULU vel,vol; DIVU #2032,vol; EXT.L vol; NEG.L vol; ADD.L vel,vol
    /* vol:=vel-((aftch[f]*16*(aftersens^2)/10000)*vel/2032) */
  ELSE
    pan:=aftch; MOVEQ #127,vol; SUB.L pan,vol; BCC.S aftertouch_yes
    MOVE.L  vel,vol; BRA.S aftertouch_over
aftertouch_yes:
    pan:=aftersens; MULU pan,pan; LSL.L #4,vol; SUB.L vel,vol
    MULS pan,vol; DIVS #10000,vol; EXT.L vol; ADD.L vel,vol
    /* vol:=((127-aftch[f])*16-vel)*(aftersens^2)/10000+vel */
aftertouch_over:
  ENDIF
  vol:=volume*vtab[vol]/32767

ENDPROC vol

exptable: INCBIN '/exptable.bin'
