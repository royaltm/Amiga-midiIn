MODULE '*softmix'


OBJECT data
  outsampledata:PTR TO INT -> 16bit, stereo
  channeldata:PTR TO LONG
  channelmask:LONG
  bufwork:PTR TO LONG -> 32bit, stereo
  workframes:LONG ->numbuffer frames-1
  mixfreq:LONG
  sampledata:LONG
ENDOBJECT

PROC main()
DEF a:PTR TO data,b
a,b:=mxcmd_setup(256,$40000,60,32,10000)
WriteF('userdata=\h mix=\h\n',a,b)
WriteF('buf=\h frms=\d mix=\d\n',a.outsampledata, a.workframes, a.mixfreq)

mxcmd_end(a)

ENDPROC

