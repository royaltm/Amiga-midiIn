OPT OSVERSION=37
OPT MODULE
MODULE 'exec/lists','exec/nodes','exec/ports','exec/semaphores',
       '*midibplists','*soundfx_ahi','*mbbanks',
       'libraries/midi','midi',
       'amigalib/tasks','other/ecode',
       'mathieeedoubbas'

SET NOTEON_ON,NOTEON_OFF

CONST MM_ALLSOUNDOFF=$78 -> not defined in midi.m !!!

OBJECT audiochan
  bank:PTR TO bank
  noteoff:CHAR
  midichan:CHAR
  playnote:CHAR
  velocity:CHAR
ENDOBJECT

OBJECT midi_controllers
  pitchbend[16]:ARRAY OF INT
  volume[16]:ARRAY OF INT
  pan[16]:ARRAY OF INT
ENDOBJECT

OBJECT internal_message
  mn:mn
  type:LONG
  val1:LONG
  val2:LONG
ENDOBJECT

EXPORT DEF maxchan,maskmaxchan -> maximum channel number (<=31) and mask set by prefs

EXPORT DEF mcontrol,basesetb,rangesetb -> control vars
EXPORT DEF prilist:PTR TO LONG  ->current banks list.
EXPORT DEF minfo:PTR TO mrouteinfo,
           mysrclist:PTR TO lh  -> internal list of midi sources names

DEF playtask, playport:PTR TO mp, playtasksem:PTR TO ss, extport:PTR TO mp

    -> MIDI-related
DEF dest:PTR TO mdest,
    routes:PTR TO LONG  -> array of all custom routes -1 terminated

DEF ach:PTR TO audiochan,   -> array of audio channels (32)
    mctrl:PTR TO midi_controllers,
    aftch:PTR TO CHAR       -> array of aftertouch values 4 each channel (32)

DEF lastchan -> last played channel

DEF midicontrolarray:PTR TO LONG -> array of pointers to mcontrollers

CONST PSG_ALLOCOK=TRUE,
      PSG_ALLOCFAILED=FALSE

EXPORT ENUM PSG_QUIT=11111,
            PSG_CHANGE,
            PSG_PLAY,
            PSG_VOLUME,
            PSG_MINFO,
            PSG_TUNE
/*
  ------------------------------------------------------------------
                        play control procs
  ------------------------------------------------------------------
*/

EXPORT PROC getchannelnote(f)
  IF ach
    IF chanfree(f)=TRUE THEN RETURN -1
    RETURN ach[f].playnote
  ENDIF
ENDPROC -1


EXPORT PROC whichbankisplaying(bf:PTR TO CHAR,bd:PTR TO bank)
DEF i:REG,b:REG,ac:PTR TO audiochan
  IF ac:=ach
    FOR i:=0 TO maxchan
      IF (chanfree(i)=FALSE) AND (b:=ac.bank)
        b:=(b-bd) / SIZEOF bank
        bf[b]:=1
      ENDIF
      ac++
    ENDFOR
  ENDIF
ENDPROC

EXPORT PROC clearcontrollers()
DEF f, pbn:PTR TO INT, vol:PTR TO INT, pan:PTR TO INT
  IF mctrl
    pbn:=mctrl.pitchbend
    vol:=mctrl.volume
    pan:=mctrl.pan
    FOR f:=0 TO 15
      pbn[f]:=PITCHBENDCENTER
      vol[f]:=16383
      pan[f]:=8192
    ENDFOR
  ENDIF
ENDPROC

/*
  -------------------------------------------------------------
                subtask procedures
  -------------------------------------------------------------
*/
PROC subtaskallocall()
  IF 0=(playport:=CreateMsgPort()) THEN RETURN FALSE
  IF 0=(dest:=CreateMDest(0,0)) THEN RETURN FALSE
ENDPROC TRUE

PROC subtaskplay() -> here it starts!!!
DEF quit=FALSE,waitsig,sigport,sigmidi,sig,
    type,mmsg,a:REG,b:REG,c,f:REG,
    nb:PTR TO bank,snd:PTR TO sfx,freech:REG,i:REG
DEF ac:PTR TO audiochan, packet:PTR TO midipacket
DEF playnote, ctrlbank:PTR TO bank, msg:PTR TO internal_message, mymsg:internal_message

 ObtainSemaphore(playtasksem)
 OpenLibrary('mathieeedoubbas.library',0) -> must be opened cause parent task does it too
 OpenLibrary('midi.library',MIDIVERSION) -> ----||----
 mymsg.mn.ln.pri:=0
 mymsg.mn.replyport:=0
 mymsg.mn.length:=12
 IF subtaskallocall() THEN mymsg.type:=PSG_ALLOCOK ELSE mymsg.type:=PSG_ALLOCFAILED
 PutMsg(extport,mymsg)
 IF mymsg.type=PSG_ALLOCOK
  makeMRoutes()      -> join with MIDI routes
  FOR f:=0 TO 31
    ac:=ach[f]
    ac.midichan:=1;   ac.noteoff:=0;  ac.playnote:=255; ac.bank:=0
  ENDFOR
  clearcontrollers()
  sigmidi:=Shl(1,dest.destport::mp.sigbit)
  sigport:=Shl(1,playport.sigbit)
  waitsig:=sigmidi OR sigport
  REPEAT
    sig:=Wait(waitsig)
    lockbanksaccess()
    IF sig AND sigmidi
      WHILE packet:=GetMidiPacket(dest)
        type:=packet.type;  mmsg:=packet.midimsg; a:=mmsg[1]; b:=(mmsg[0] AND $F)+1
        c:=mmsg[2] -> velocity
        FreeMidiPacket(packet)
        IF mcontrol
         SELECT type
         CASE MMF_NOTEON
          IF (rangesetb OR basesetb)=FALSE
            freech:=0; i:=0
            FOR f:=0 TO maxchan
              IF chanfree(f)=FALSE
                ac:=ach[f]
                IF (ac.noteoff AND NOTEON_ON) AND (ac.playnote=a) AND (ac.midichan=b)
                  IF i:=ac.bank.release
                    ac.noteoff:=0; releasechan(f,i)
                  ELSE
                    MOVEQ #1,D0; LSL.L f,D0; OR.L D0,freech
                  ENDIF
                ENDIF
              ENDIF
            ENDFOR
            IF freech THEN soundsoff(freech)
            IF (freech OR i)=0
              FOR i:=0 TO NUMBANKS-1
                nb:=prilist[i]
                IF snd:=nb.instr
                  IF nb.midi=b
                    IF (a>=nb.lobound) AND (a<=nb.hibound)
                      playbank(snd,nb,a,b,c,mctrl.pitchbend[b-1])
                    ENDIF
                  ENDIF
                ENDIF
              ENDFOR
            ENDIF
          ENDIF
         CASE MMF_NOTEOFF
          IF (rangesetb OR basesetb)=FALSE
            freech:=0; i:=0 
            FOR f:=0 TO maxchan
              IF chanfree(f)=FALSE
                ac:=ach[f]
                IF (ac.noteoff AND NOTEON_OFF) AND (ac.playnote=a) AND (ac.midichan=b)
                  IF i:=ac.bank.release
                    ac.noteoff:=0; releasechan(f,i)
                  ELSE
                    MOVEQ #1,D0; LSL.L f,D0; OR.L D0,freech
                  ENDIF
                ENDIF
              ENDIF
            ENDFOR
            IF freech THEN soundsoff(freech)
          ENDIF
         CASE MMF_PITCHBEND
          ac:=ach
          a:=c*128+a
          mctrl.pitchbend[b-1]:=a
          FOR f:=0 TO maxchan
            IF (chanfree(f)=FALSE) AND (ac.midichan=b)  -> pitchbender for whole channel!!!
              IF nb:=ac.bank
                IF snd:=nb.instr
                  snd.changepitch(f,a,nb.pitchsens,ac.playnote,nb.fine-FINE_CENTR+100,nb.base)
                ENDIF
              ENDIF
            ENDIF
            ac++
          ENDFOR
         CASE MMF_POLYPRESS
          ac:=ach
          FOR f:=0 TO maxchan
            IF chanfree(f)=FALSE
              IF (ac.playnote=a) AND (ac.midichan=b)
                aftch[f]:=c; i,freech:=calcvolume(f); changevolume(f,i,freech)
              ENDIF
            ENDIF
            ac++
          ENDFOR
         CASE MMF_CHANPRESS
          ac:=ach
          FOR f:=0 TO maxchan
            IF chanfree(f)=FALSE
              IF ac.midichan=b
                aftch[f]:=a; i,freech:=calcvolume(f); changevolume(f,i,freech)
              ENDIF
            ENDIF
            ac++
          ENDFOR
         CASE MMF_CTRL
          IF a=MM_ALLSOUNDOFF
            soundsoff(maskmaxchan)
          ELSEIF a=MM_ALLOFF
            freech:=0; i:=0
            FOR f:=0 TO maxchan
              IF chanfree(f)=FALSE
                ac:=ach[f]
                 IF i:=nb.release
                   ac.noteoff:=0; releasechan(f,i)
                 ELSE
                   MOVEQ #1,D0; LSL.L f,D0; OR.L D0,freech
                 ENDIF
              ENDIF
            ENDFOR
            IF freech THEN soundsoff(freech)
          ELSE
            SELECT a
             CASE MM_RESETCTRL
              clearcontrollers()
             CASE MC_VOLUME
              mctrl.volume[b-1]:=Shl(c,7) + c
             CASE MC_VOLUME+$20
              mctrl.volume[b-1]:=mctrl.volume[b-1] AND $3F80 + c
             CASE MC_PAN
              mctrl.pan[b-1]:=Shl(c,7) + c
             CASE MC_PAN+$20
              mctrl.pan[b-1]:=mctrl.pan[b-1] AND $3F80 + c
             DEFAULT
              a:= -1
            ENDSELECT
            IF a <> -1
              ac:=ach
              FOR f:=0 TO maxchan
                IF chanfree(f)=FALSE
                  IF ac.midichan=b
                    i,freech:=calcvolume(f); changevolume(f,i,freech)
                  ENDIF
                ENDIF
                ac++
              ENDFOR
            ENDIF
          ENDIF
         ENDSELECT
        ENDIF
      ENDWHILE
    ENDIF
    IF sig AND sigport
      WHILE msg:=GetMsg(playport)
        type:=msg.type; ctrlbank:=msg.val1; playnote:=msg.val2
        ReplyMsg(msg)
        SELECT type
        CASE PSG_TUNE
          ac:=ach; nb:=ctrlbank
          FOR f:=0 TO maxchan
            IF chanfree(f)=FALSE
              IF nb=ac.bank
                IF snd:=nb.instr
                  snd.changepitch(f,mctrl.pitchbend[ac.midichan-1],nb.pitchsens,ac.playnote,nb.fine-FINE_CENTR+100,nb.base)
                ENDIF
              ENDIF
            ENDIF
            ac++
          ENDFOR
        CASE PSG_CHANGE
          makeMRoutes()
        CASE PSG_MINFO
          changeMRouteInfo()
        CASE PSG_PLAY
          IF mcontrol=0
            nb:=ctrlbank
            IF (snd:=nb.instr) AND (playnote < 128)
              a:=playnote
              b:=nb.midi
              freech:=0; i:=0
              FOR f:=0 TO maxchan
                IF chanfree(f)=FALSE
                  ac:=ach[f]
                  IF ac.noteoff AND (ac.playnote=a) AND (ac.bank=nb)
                    IF i:=nb.release
                      ac.noteoff:=0; releasechan(f,i)
                    ELSE
                      MOVEQ #1,D0; LSL.L f,D0; OR.L D0,freech
                    ENDIF
                  ENDIF
                ENDIF
              ENDFOR
              IF freech THEN soundsoff(freech)
              IF (freech OR i)=0
                playbank(snd,nb,a,b,127,mctrl.pitchbend[b-1])
              ENDIF
            ELSE
              IF playnote > 127 THEN soundsoff(maskmaxchan)
            ENDIF
          ENDIF
        CASE PSG_VOLUME
          ac:=ach; nb:=ctrlbank
          FOR f:=0 TO maxchan
            IF chanfree(f)=FALSE
              IF ac.bank=nb ; i,freech:=calcvolume(f); changevolume(f,i,freech); ENDIF
            ENDIF
            ac++
          ENDFOR
        CASE PSG_QUIT
          quit:=TRUE
        ENDSELECT
        EXIT (SetSignal(0,0) AND sigport) <> 0
      ENDWHILE
    ENDIF
    releasebanksaccess()
  UNTIL quit
 ENDIF
 freeMRoutes()
 IF dest THEN DeleteMDest(dest)
 IF playport THEN DeleteMsgPort(playport)
 CloseLibrary(midibase)
 CloseLibrary(mathieeedoubbasbase)
 playtask:=0
 ReleaseSemaphore(playtasksem)
 RemTask(0)   -> OK. Sayonara!
ENDPROC

PROC calcvolume(f)
DEF ac:PTR TO audiochan,nb:PTR TO bank,
    volume:REG,vol:REG,pan:REG,vel:REG,vtab:PTR TO INT

  vtab:={exptable} -> 2048 * INT

  ac:=ach[f]
  IF (nb:=ac.bank)=0 THEN RETURN 0,0
  vol:=ac.velocity; MOVEQ #127,vel; SUB.L vol,vel; LSL.L #4,vel
  vol:=nb.velsens; MULU vol,vol;  MULU vol,vel; DIVU #10000,vel; EXT.L vel
  /* vel:=(127-ac.velocity)*16*(nb.velsens^2)/10000 */
  IF nb.set AND B_ADDAFTERT
    vol:=aftch[f]; CMPI.W #128,vol; BCS.S aftertouch_additive
    MOVE.L  vel,vol; BRA.S aftertouch_over
aftertouch_additive:
    pan:=nb.aftersens; MULU pan,pan; LSL.L #4,vol; MULU pan,vol; DIVU #10000,vol
    MULU vel,vol; DIVU #2032,vol; EXT.L vol; NEG.L vol; ADD.L vel,vol
    /* vol:=vel-((aftch[f]*16*(nb.aftersens^2)/10000)*vel/2032) */
  ELSE
    pan:=aftch[f]; MOVEQ #127,vol; SUB.L pan,vol; BCC.S aftertouch_yes
    MOVE.L  vel,vol; BRA.S aftertouch_over
aftertouch_yes:
    pan:=nb.aftersens; MULU pan,pan; LSL.L #4,vol; SUB.L vel,vol
    MULS pan,vol; DIVS #10000,vol; EXT.L vol; ADD.L vel,vol
    /* vol:=((127-aftch[f])*16-vel)*(nb.aftersens^2)/10000+vel */
aftertouch_over:
  ENDIF
  volume:=nb.volume*vtab[vol]/32767; LSL.L #5,volume
  IF nb.mctrlvol THEN volume:=volume*mctrl.volume[ac.midichan-1]/16383
  LSL.L #3,volume

  pan:=nb.panorama+(ac.playnote-(nb.hibound+nb.lobound/2) * nb.panwide)
  IF pan > 256 THEN pan:=256
  IF pan < 0 THEN pan:=0
  LSL.L #6,pan
  IF nb.mctrlpan
    IF (vel:=mctrl.pan[ac.midichan-1]) < 8192
      pan:=pan*vel/8192
    ELSEIF (vel:=vel-8192) > 0
      pan:=(16384-pan)*vel/8192+pan
    ENDIF
  ENDIF
  LSL.L #2,pan
ENDPROC volume,pan

PROC findchannel(pri)
DEF ac:PTR TO audiochan,f:REG,i:REG,d:REG,p:REG
  d:=lastchan
  IF f:=channelsfree(maskmaxchan)
    ROR.L   d,f
findloop:
    ADDQ.L  #1,d
    LSR.L   #1,f
    BCC.S   findloop
    SUBQ.L  #1,d
    AND.W   #31,d
    MOVE.L  d,lastchan
    RETURN  d
  ELSE
    f:= -1
    IF d > maxchan THEN d:=0
    DEC pri
    i:=d; ac:=ach[i]
    REPEAT
      INC i; ac++
      IF i > maxchan ; i:=0; ac:=ach; ENDIF
      IF (p:=ac.bank.pri) > pri; pri:=p; f:=i; ENDIF
    UNTIL i=d
    IF f >= 0 THEN lastchan:=f
  ENDIF
ENDPROC f

PROC playbank(snd:PTR TO sfx,nb:PTR TO bank,note,midichan,vel,pitch)
DEF chan,volume,pan,ac:PTR TO audiochan,loop,
    act:PTR TO audiochan,nbt:PTR TO bank,f:REG,r:REG,g:REG
  IF (nb.set AND B_MONO) AND ((g:=nb.monoslide)<>0)
    ac:=ach
    FOR chan:=0 TO maxchan
      IF chanfree(chan)=FALSE
        IF (ac.bank=nb) AND (ac.noteoff<>0)
          act:=ac
          FOR f:=chan+1 TO maxchan
            act++
            IF act.bank=nb
              IF r:=nb.release
                act.noteoff:=0; releasechan(f,r)
              ELSE
                soundoff(f)
              ENDIF
            ENDIF
          ENDFOR
          IF r:=nb.monovsens
            MOVE.L vel,D0; LSL.L #7,D0; MULU r,D0; DIVU #100,D0
            MULU g,D0; DIVU #16256,D0; EXT.L D0; SUB.L D0,g
            /* vel:=vel*128*r)/100; r:=g*vel/16256; g:=g-r */
          ENDIF
          ac.playnote:=note
          ac.midichan:=midichan
          snd.changepitch(chan,pitch,nb.pitchsens,note,nb.fine-FINE_CENTR+100,nb.base,g)
          RETURN
        ENDIF
      ENDIF
      ac++
    ENDFOR
  ENDIF
  IF (chan:=findchannel(nb.pri)) < 0 THEN RETURN
  ac:=ach[chan]
  aftch[chan]:=128
  ac.velocity:=vel
  ac.bank:=nb
  ac.playnote:=note
  ac.midichan:=midichan
  volume,pan:=calcvolume(chan)
  IF nb.set AND B_DRUM
    ac.noteoff:=0; loop:=FALSE
  ELSE
    ac.noteoff:=IF (nb.set AND B_DUR_ON) THEN NOTEON_ON ELSE NOTEON_OFF
    loop:=IF nb.set AND B_LOOP THEN TRUE ELSE FALSE
  ENDIF
  IF nb.set AND B_MONO
    act:=ach
    FOR f:=0 TO maxchan
      IF (act<>ac) AND (act.bank=nb)
        IF r:=nb.release
          act.noteoff:=0; releasechan(f,r)
        ELSE
          soundoff(f)
        ENDIF
      ENDIF
      act++
    ENDFOR
  ENDIF
  IF g:=nb.group
    act:=ach
    FOR f:=0 TO maxchan
      IF (act<>ac)
        IF (nbt:=act.bank) <> nb
          IF g=nbt.group
            IF r:=nbt.release
              act.noteoff:=0; releasechan(f,r)
            ELSE
              soundoff(f)
            ENDIF          
          ENDIF
        ENDIF
      ENDIF
      act++
    ENDFOR
  ENDIF
  snd.play(chan,loop,note,nb.fine-FINE_CENTR+100,nb.base,volume,pan,pitch,nb.pitchsens,nb.attack,nb.decay,nb.sustainlev,nb.firstskip)
ENDPROC


PROC changeMRouteInfo()
DEF i=0:REG,mr:REG
  IF routes
    WHILE routes[i++]<>-1 DO IF mr:=routes[i-1] THEN ModifyMRoute(mr,minfo)
  ENDIF
ENDPROC

PROC makeMRoutes() HANDLE
DEF mysrcnode:PTR TO ln,i

  i:=0; mysrcnode:=mysrclist.head
  WHILE mysrcnode.succ
    IF mysrcnode.name[] = "+" THEN i++
    mysrcnode:=mysrcnode.succ
  ENDWHILE

  freeMRoutes()

  IF i
    NEW routes[i+1]; routes[i]:= -1
    i:=0; mysrcnode:=mysrclist.head
    WHILE mysrcnode.succ
      IF mysrcnode.name[] = "+"
        routes[i++]:=MrouteDest(mysrcnode.name+2,dest,minfo)
      ENDIF
      mysrcnode:=mysrcnode.succ
    ENDWHILE
  ENDIF
EXCEPT
  NOP
ENDPROC

PROC freeMRoutes()
DEF i=0:REG,mr:REG
  IF routes
    WHILE routes[i++]<>-1 DO IF mr:=routes[i-1] THEN DeleteMRoute(mr)
    END routes[i]
  ENDIF
ENDPROC

/*
  =========================================================================
                            play task control
  =========================================================================
*/

EXPORT PROC install_playtask() HANDLE
DEF playcode,msg:PTR TO internal_message
    NEW mctrl, ach[32], aftch[32], midicontrolarray[33], playtasksem
    InitSemaphore(playtasksem)
    IF (extport:=CreateMsgPort())=0 THEN Raise("MEM")
    SetTaskPri(FindTask(0),0)
    ObtainSemaphore(playtasksem)
    IF NIL=(playcode:=eCodeTask({subtaskplay})) THEN Raise("ECOD")
    IF NIL=(playtask:=createTask('midiIn_PLAY', 5, playcode, 4096)) THEN Raise("TASK")
    ReleaseSemaphore(playtasksem)
    REPEAT;  WaitPort(extport);  UNTIL msg:=GetMsg(extport)
    IF msg.type=PSG_ALLOCFAILED
      ObtainSemaphore(playtasksem) -> wait for task to end itself
      Raise("TASK")  -> something went wrong!!
    ENDIF
    midicontrolarray[MC_VOLUME]:=mctrl.volume
    midicontrolarray[MC_PAN]:=mctrl.pan
    midicontrolarray[32]:=mctrl.pitchbend
EXCEPT
  IF playtasksem
    IF playtasksem.nestcount THEN ReleaseSemaphore(playtasksem)
  ENDIF
  ReThrow()
ENDPROC

EXPORT PROC deinstall_playtask()
  IF playtask
    signal_playtask(PSG_QUIT) -> signal slave to harakiri
    ObtainSemaphore(playtasksem)
    ReleaseSemaphore(playtasksem)
  ENDIF
  IF extport THEN DeleteMsgPort(extport)
  END mctrl, ach[32], aftch[32], midicontrolarray[33], playtasksem
ENDPROC

EXPORT PROC signal_playtask(type,bn=0,v=0)
DEF msg:internal_message
 IF playtask
  msg.mn.ln.pri:=0
  msg.mn.replyport:=extport
  msg.mn.length:=12
  msg.type:=type
  msg.val1:=bn
  msg.val2:=v
  PutMsg(playport,msg)
  REPEAT; WaitPort(extport); UNTIL msg:=GetMsg(extport)
 ENDIF
ENDPROC TRUE

EXPORT PROC getmidicontrolarray(mcm) IS IF midicontrolarray THEN midicontrolarray[mcm] ELSE 0

EXPORT PROC xchgbanksachn(bank1,bank2)
DEF i,ac:PTR TO audiochan
  lockbanksaccess()
  xchgbanks(bank1,bank2)
  ac:=ach
  FOR i:=0 TO 31
    IF ac.bank=bank1
      ac.bank:=bank2
    ELSEIF ac.bank=bank2
      ac.bank:=bank1
    ENDIF
    ac++
  ENDFOR
  releasebanksaccess()
ENDPROC


/*
EXPORT PROC messwctrlers()
DEF ct:PTR TO INT,i,a
  ct:=mctrl.pan
  FOR i:=0 TO 15 STEP 2
    IF (a:=ct[i]+(i*5)+1) > 16383 THEN a:=a-16383
    ct[i]:=a
    IF (a:=ct[i+1]-(i*5)-1) < 0 THEN a:=a+16383
    ct[i+1]:=a
  ENDFOR
  ct[1]:=(ct[0]-1 * 75) AND 16383
  ct[0]:=(ct[1]-1 * 75) AND 16383
  signal_playtask(PSG_VOLUME)
ENDPROC
*/

exptable: INCBIN 'exptable.bin'
