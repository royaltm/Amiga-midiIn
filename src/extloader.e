OPT MODULE

MODULE '*simpleiffparse','dos/dos','mathieeedoubbas',
       '*svx','*loadsvxbody',
       '*aiff','*loadssnd',
       '*wave','*loadwavedata',
       '*MBLocale'

EXPORT OBJECT sampleinfo
  descr:LONG  -> opis ptr to string
  type:LONG   -> 4 byte type as "AIFF"
  start:LONG  -> start in memory
  loop:LONG   -> loopstart sampleframe (not byte) 0 = loop whole
  frames:LONG -> num sampleframes (not bytes)
  channels:LONG -> num channels (1 or 2)
  bytelength:LONG -> len of sample
  rate:LONG     -> samples per sec.
ENDOBJECT

EXPORT PROC loader_recon(name,si:PTR TO sampleinfo) HANDLE
DEF typeptr:PTR TO LONG,fh=0,descr=0,type=0,l,s,buff[256]:ARRAY OF CHAR,
    status=FALSE

  typeptr:=[getLocStr(STRID_AIFFNAME),"AIFF",0,'FORM',8,'AIFF',-1,
            getLocStr(STRID_8SVXNAME),"8SVX",0,'FORM',8,'8SVX',-1,
            getLocStr(STRID_WAVENAME),"WAVE",0,'RIFF',8,'WAVE',-1,
            0]:LONG

  IF 0=(fh:=Open(name,OLDFILE)) THEN Raise("OPEN")

  REPEAT
    descr:=typeptr[]++  -> description string
    type:=typeptr[]++   -> type
    status:=TRUE
    WHILE (l:=typeptr[]++) <> -1      -> first offset
      s:=typeptr[]++    -> first string to compare
      IF Seek(fh,l,OFFSET_BEGINNING) <> -1
        IF (l:=StrLen(s)) > 256 THEN l:=256 -> no more than buffer size
        Read(fh,buff,l)
        IF StrCmp(buff,s,l)=FALSE THEN status:=FALSE -> doesn't match
      ELSE
        status:=FALSE  -> file too small
      ENDIF
      IF status=FALSE
        WHILE (typeptr[] <> -1) DO typeptr++
      ENDIF
    ENDWHILE
  UNTIL (status = TRUE) OR (typeptr[] = 0)

EXCEPT DO
  IF fh THEN Close(fh)
  IF status
    IF si
      si.descr:=descr
      si.type:=type
    ENDIF
  ENDIF
  IF exception THEN ReThrow()
ENDPROC status

EXPORT PROC loader_get(name,si:PTR TO sampleinfo) HANDLE
DEF fh=0, status=FALSE, sel

  IF 0=(fh:=Open(name,OLDFILE)) THEN Raise("OPEN")
  sel:=si.type
  SELECT sel
    CASE  "AIFF"
      status:=loadaiff(fh,si)
    CASE "8SVX"
      status:=load8svx(fh,si)
    CASE "WAVE"
      status:=loadwave(fh,si)
    DEFAULT
      status:=FALSE
  ENDSELECT

EXCEPT DO
  IF fh THEN Close(fh)
  IF exception THEN ReThrow()
ENDPROC status

/*=====================================================================*/
/*==================== single loader procedures =======================*/

->----------------------------------------------------------------------<-
->                               8SVX                                   <-
->----------------------------------------------------------------------<-

PROC load8svx(fh,si:PTR TO sampleinfo)
DEF t, vhdr:voiceheader, l, k, chn=1, bits=8

  t:="8SVX"
  IF seekIFFHeader(fh,{t}) = -1 THEN Raise("READ")
  t:="VHDR"
  IF (l:=seekIFFChunk(fh,{t})) <> SIZEOF voiceheader THEN Raise("N8SV")
  IF Read(fh,vhdr,SIZEOF voiceheader) <> SIZEOF voiceheader THEN Raise("READ")
  IF vhdr.scompression <> 0 THEN Raise("FIBO")
  t:=0
  IF (l:=seekIFFChunk(fh,{t})) = -1 THEN Raise("NBDY")
  WHILE t <> "BODY"
    IF ((t="CHAN") OR (t="BITS")) AND (l=4)
      IF Read(fh,{k},4) <> 4 THEN Raise("READ")
      IF t="BITS"
        bits:=k
      ELSE
        MOVE.L  k,D1; MOVEQ #0,D2; MOVEQ  #0,D0
svxchancnt:
        LSR.L #1,D1; ADDX.B D2,D0; TST.L  D1; BNE.S svxchancnt
        MOVE.L  D0,chn
      ENDIF
      t:=0
      IF (l:=seekIFFChunk(fh,{t})) = -1 THEN Raise("NBDY")
    ELSE
      t:=0
      IF (l:=seekIFFChunk(fh,{t},TRUE)) = -1 THEN Raise("NBDY") ->omit this one
    ENDIF
  ENDWHILE

  IF (k:=vhdr.oneshothisamples+vhdr.repeathisamples)=0   ->frames
    k:=Div(l,chn); IF bits > 8 THEN k:=k/2
  ENDIF
  IF chn > 1 THEN chn:=Div(l,chn) ELSE chn:=0 -> chanoffset
  t,l:=loadsvxBODY(fh,k,bits,chn)
	si.start:=t
	si.loop:=IF vhdr.repeathisamples<>0 THEN vhdr.oneshothisamples ELSE 0
	si.bytelength:=l
	si.channels:=IF chn THEN 2 ELSE 1
	si.frames:=k
  si.rate:=vhdr.samplespersec
ENDPROC TRUE




->----------------------------------------------------------------------<-
->                               AIFF                                   <-
->----------------------------------------------------------------------<-

PROC loadaiff(fh,si:PTR TO sampleinfo)
DEF t, com:commonchunk, l, k, rate[2]:ARRAY OF LONG

  t:="AIFF"
  IF seekIFFHeader(fh,{t}) = -1 THEN Raise("READ")
  t:="COMM"
  IF (l:=seekIFFChunk(fh,{t})) <> SIZEOF commonchunk THEN Raise("NAIF")
  IF Read(fh,com,SIZEOF commonchunk) <> SIZEOF commonchunk THEN Raise("READ")
  t:="SSND"
  IF (l:=seekIFFChunk(fh,{t})) < 9 THEN Raise("NSND")
  IF Seek(fh,8,OFFSET_CURRENT) = -1 THEN Raise("READ")

  /* start , length */
	t,k:=loadSSND(fh,com.numsampleframes,com.samplesize,com.numchannels)
	si.start:=t
	si.loop:=0
	si.bytelength:=k
	si.channels:=IF com.numchannels > 1 THEN 2 ELSE 1
	si.frames:=com.numsampleframes
  si.rate:=16576
  IF convertrate(com.samplerate,rate)
    IF mathieeedoubbasbase
      si.rate:=IeeeDPFix(rate[0],rate[1])
    ENDIF
  ENDIF
ENDPROC TRUE

PROC convertrate(val:PTR TO LONG,dbl:PTR TO LONG)
DEF a:REG,b:REG,c:REG
/* conwersja 80 bitow IEEE754 na dblieee */
      MOVE.L	val,A0
      MOVE.L	(A0)+,a
      MOVE.W	(A0)+,b
      SWAP		a
      MOVE.W	a,c		->cecha
      MOVE.W	b,a		->mantysa
      MOVE.L	(A0)+,b	->mantysa2
      MOVEQ		#63,D0
mant_loop:
			LSL.L		#1,b
			ROXL.L	#1,a
			DBCS		D0,mant_loop
			BCS.S		mant_not_zero
	RETURN  FALSE
mant_not_zero:
			MOVEQ		#63,D1
			SUB.W		D0,D1
			SUB.W		D1,c
			MOVE.W	c,D0
			LSL.W		#4,D0
			AND.W		#$3FF0,D0
			AND.W		#$C000,c
			OR.W		D0,c		->cecha
      MOVEQ		#11,D0
mant2_loop:
			LSR.L		#1,a
			ROXR.L	#1,b
			DBRA		D0,mant2_loop
			SWAP		c
			SUB.W		c,c
			OR.L		c,a

			MOVE.L	dbl,A0
			MOVE.L	a,(A0)+
			MOVE.L	b,(A0)+
ENDPROC	TRUE

->----------------------------------------------------------------------<-
->                               WAVE                                   <-
->----------------------------------------------------------------------<-

PROC loadwave(fh,si:PTR TO sampleinfo)
DEF fmt:formatchunk, p, len, chan, align, frames

  IF (len:=seek_RIFF_CHUNK(fh,"fmt "))< SIZEOF formatchunk THEN Raise("BWAV")
  IF Read(fh,fmt,SIZEOF formatchunk) <> SIZEOF formatchunk THEN Raise("READ")
  IF (len:=seek_RIFF_CHUNK(fh,"data"))<= 0 THEN Raise("BWAV")

  IF get_little_endian(fmt.wformattag,2) <> WAVE_FORMAT_PCM THEN Raise("WAVN")
  IF (chan:=get_little_endian(fmt.wchannels,2)) < 1 THEN Raise("BWAV")
  IF (align:=get_little_endian(fmt.wblockalign,2)) < 1 THEN Raise("BWAV")
  frames:=Div(Div(len,chan),align)

  /* start , length */
	p,len:=loadWAVEdata(fh,frames,chan,align)
	si.start:=p
	si.loop:=0
	si.bytelength:=len
	si.channels:=IF chan > 1 THEN 2 ELSE 1
	si.frames:=frames
  si.rate:=get_little_endian(fmt.dwsamplespersec,4)
ENDPROC TRUE

PROC get_little_endian(val,size)
  MOVE.L  size,D1;  SUBQ.L  #1,D1; MOVEQ #0,D0; MOVE.L val,D2
gle_loop:
  LSL.L #8,D0; MOVE.B  D2,D0; LSR.L #8,D2; ; DBRA D1,gle_loop
  MOVE.L  D0,val
ENDPROC val

PROC seek_RIFF_CHUNK(fh,chunk)
DEF buff[3]:ARRAY OF LONG,k,t,l
  IF Seek(fh,0,OFFSET_BEGINING)=-1 THEN Raise("READ")
  IF Read(fh,buff,12)<>12 THEN Raise("READ")
  l:=get_little_endian(buff[1],4)+12   -> size of 'RIFF'
  REPEAT
    IF Read(fh,buff,8)<>8 THEN RETURN FALSE
    t:=get_little_endian(buff[1],4)+1 AND $FFFFFFFE -> size of another chunk
    IF buff[0] <> chunk
      IF -1 = (k:=Seek(fh,t,OFFSET_CURRENT)) THEN RETURN FALSE
      IF (k+t) > l THEN RETURN FALSE
    ENDIF
  UNTIL buff[0]=chunk
ENDPROC t
