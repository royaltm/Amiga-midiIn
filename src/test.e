MODULE '*soundFX','mathieeedoubbas'

PROC main() HANDLE
DEF snd=0:PTR TO sfx,f,i
  IF (mathieeedoubbasbase:=OpenLibrary('mathieeedoubbas.library',0))=0 THEN Raise("MATH")

  NEW snd.init('Comm:Work/Play14/samples/kl_Piano1.aiff')
  
  snd.load()
  WriteF('')
  IF allocaudiodevice()=FALSE THEN Raise("CHUJ")
  snd.playstereo(0,-1,50,0,60,256,256)
Inp(stdout)
  FOR i:=0 TO 16383 STEP 800
    snd.playstereo(0,-1,50,0,60,256,256,i,10)
    FOR f:=i TO 16383 STEP 800
      snd.changepitch(0,f,2,50,0,60)
      Delay(1)
      IF CtrlC() THEN Raise("^C")
    ENDFOR
    FOR f:=16383 TO 0 STEP -800
      snd.changepitch(0,f,2,50,0,60)
      Delay(1)
      IF CtrlC() THEN Raise("^C")
    ENDFOR
    IF CtrlC() THEN Raise("^C")
  ENDFOR
WriteF('juz!:')
/*
FOR f:=0 TO 31
  snd.playstereo(f,-1,f,0,15,8*(31-f),8*f)
WriteF('\d ',f)
  Delay(5)
ENDFOR
*/
/*
  FOR f:=0 TO 256 STEP 10
    FOR i:=256 TO 0 STEP -40
      Delay(1)
      setvolume(0,i*f/256,i)
      IF CtrlC() THEN Raise("  ^C")
    ENDFOR
    snd.playstereo(0,-1,60,0,60,256*f/256,256)
  ENDFOR
*/
Inp(stdout)
  soundoff(-1)


EXCEPT DO
  IF exception THEN WriteF('exc:\s[4]\n',{exception})
  freeaudiodevice()
  END snd
  IF mathieeedoubbasbase THEN CloseLibrary(mathieeedoubbasbase)
ENDPROC
