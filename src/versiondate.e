MODULE 'dos/dos','dos/datetime'

PROC main()
DEF dt:datetime,date[20]:ARRAY OF CHAR
  dt.format:=FORMAT_DOS
  dt.flags:=0
  dt.strday:=NIL
  dt.strdate:=date
  dt.strtime:=NIL
  DateStamp(dt.stamp)
  DateToStr(dt)
  WriteF('$VER:midiIn 32.020b (\s) (c) by Najakotiva Software 1997-99',date)
ENDPROC
