OPT MODULE
OPT EXPORT


OBJECT commonchunk	/* "COMM" */
  numchannels:INT       /* 1=mono 2=stereo */
  numsampleframes:LONG  /* number of samples for each channel */
  samplesize:INT        /* sample size in bits (1 to 32) */
  samplerate[10]:ARRAY OF CHAR  /* IEEE 754 Format sample rate */
ENDOBJECT

OBJECT sounddatachunk   /* "SSND" */
  offset:LONG           /* offset to first block */
  blocksize:LONG        /* block size            */
  sounddata[1]:ARRAY OF CHAR  /* sample size varies from 1 to 4 bytes */
ENDOBJECT                    /* depends on samplesize in commonchunk */
