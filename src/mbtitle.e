OPT OSVERSION=37
OPT MODULE
MODULE 'intuition/intuition','*title','*mbkeycod'

EXPORT OBJECT title_keys OF title_plugin
  keycode:LONG
ENDOBJECT

/*
  ==================================================================================
                              title plugin
  ==================================================================================
*/
EXPORT PROC message_test(imsg:PTR TO intuimessage,window:PTR TO window) OF title_keys
DEF code:REG,class:REG

  code:=imsg.code
  class:=imsg.class
  IF class AND IDCMP_RAWKEY
      IF (code >= CURSORUP) AND (code <= KEYCODE_F10) THEN RETURN TRUE
  ELSEIF class AND IDCMP_VANILLAKEY
      SELECT 128 OF code
        CASE ESC_CODE; RETURN TRUE
        CASE "0" TO "9"; RETURN TRUE
        CASE "."; RETURN TRUE
        CASE "_"; RETURN TRUE
        CASE ")"; RETURN TRUE
        CASE "("; RETURN TRUE
        CASE "-"; RETURN TRUE
        CASE "+"; RETURN TRUE
        CASE "="; RETURN TRUE
        CASE "*"; RETURN TRUE
        CASE 9  ; RETURN TRUE
        CASE 13 ; RETURN TRUE
        CASE 32 ; RETURN TRUE
        DEFAULT; RETURN FALSE
      ENDSELECT
  ENDIF
ENDPROC FALSE

EXPORT PROC message_action(class,qual,code,window:PTR TO window) OF title_keys

 SELECT class
  CASE IDCMP_RAWKEY
    self.keycode:=code OR MYRAWCODE
  CASE IDCMP_VANILLAKEY
    self.keycode:=code
  DEFAULT
    RETURN FALSE
 ENDSELECT

ENDPROC TRUE

