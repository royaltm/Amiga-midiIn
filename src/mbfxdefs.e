OPT MODULE

EXPORT OBJECT envelope -> realtime update volume for each channel
  attack:CHAR
  decay:CHAR
  sustain:CHAR
ENDOBJECT

EXPORT OBJECT modulation
  data:PTR TO INT
  length:LONG
  speed:LONG -> fixed
  amplitude:LONG -> fixed
ENDOBJECT


