OPT MODULE
OPT EXPORT


CONST WAVE_FORMAT_PCM=$0001, /* Microsoft Pulse Code Modulation (PCM) format */
      IBM_FORMAT_MULAW=$0101, /* IBM mu-law format */
      IBM_FORMAT_ALAW=$0102, /* IBM a-law format */
      IBM_FORMAT_ADPCM=$0103  /* IBM AVC Adaptive Differential Pulse Code Modulation format */



OBJECT formatchunk	/* "fmt " */
  wformattag:INT              /* Format category */
  wchannels:INT          /* Number of channels */
  dwsamplespersec:LONG         /* Sampling rate */
  dwavgbytespersec:LONG        /* For buffer */
  wblockalign:INT        /* Data block size */
ENDOBJECT

