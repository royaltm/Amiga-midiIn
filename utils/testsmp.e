MODULE '*/play14unlim','*/notecalc','*/extloader',
    'exec/ports','exec/memory','exec/nodes','devices/audio','exec/io',
    'mathieeedoubbas'


PROC main() HANDLE
DEF t:sampleinfo, name,type

  name:=arg
  WriteF('\s \d\n',name,FileLength(name))

  IF loader_recon(name,t)=FALSE THEN Raise("UNRE")

  type:=t.type
  WriteF('File: \a\s\a \q\s[4]\q \s\n',name,{type},t.descr)

  IF loader_get(name,t)=FALSE THEN Raise("UNRE")

  WriteF('Sample rate: \d\n', t.rate)
  WriteF('Num channels: \d\n', t.channels)
  WriteF('Frames: \d\n', t.frames)
  WriteF('Byte len: \d\n', t.bytelength)
  WriteF('Start at: \h\n', t.start)
  WriteF('Loop pt: \d\n', t.loop)
EXCEPT
  WriteF('ERROR: \s[4]\n',{exception})

ENDPROC

