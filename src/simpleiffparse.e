OPT MODULE
CONST O_BEGINNING=-1,O_CURRENT=0,O_END=1

/*
  *******  simple IFF multi-purpose parsing module *******

length = seekIFFHeader(fh,{type},next=0) ;(next optionally)

fh     = filehandle to iff file
{type} = pointer to type variable: store a FORM name for IFF type identification
         if type=0 the first FORM type found is returned and stored in type var
f.e.: "ILBM"
next   = 0    FORM is seeking from current position of fh
       = TRUE next FORM is seeking. *** You can use this opt only after
         previous use of this function; after Reading the contents of the FORM
         use this procedure with next=0 ***

length = length of FORM or -1 if EOF was reached

length = seekIFFChunk(fh,{id},next=0) ;(next optionally)

fh     = filehandle to iff file
{id}   = pointer to id chunk name to look for
         if id=0 the first chunk found is returned and stored in id variable
f.e.: "BODY"
next   = 0    chunk is seeking from current position of fh
       = TRUE next chunk is seeking. *** You can use this opt only after
         previous use of this function; after Reading the contents of chunk
         use this procedure with next=0 ***

length = length of chunk or -1 if EOF was reached or another FORM was found

ERRORS that may occur:
"READ" disk error
"NIFF" not an IFF file
"MNGL" IFF file mangled

have a nice...   ....yyyy...   day.

*/


EXPORT PROC	seekIFFHeader(file,tptr:PTR TO LONG,next=0)
	DEF type,left,buffer[3]:ARRAY OF LONG,curr

  type:=tptr[]
  IF (curr:=Seek(file,0,O_END))=-1 THEN Raise("READ")
  IF next THEN IF (curr:=curr-8)<0 THEN Raise("READ")
  IF (left:=Seek(file,curr,O_BEGINNING))=-1 THEN Raise("READ")
  left:=left-curr
  IF next
    IF Read(file,buffer,4)<>4 THEN Raise("READ")
    buffer[]:=Shl(Shr(buffer[]+1,1),1)
    IF (left:=left-4-buffer[])<0 THEN Raise("MNGL")
    IF Seek(file,buffer[],O_CURRENT)=-1 THEN Raise("READ")
  ENDIF
  WHILE left>0
    IF left<8 THEN Raise("NIFF")
    IF Read(file,buffer,12)<8 THEN Raise("READ")
    IF buffer[]<>"FORM" THEN Raise("NIFF")
    IF (buffer[2]=type) OR (type=0)
      tptr[]:=buffer[2]
      RETURN buffer[1]
    ENDIF
    buffer[1]:=Shl(Shr(buffer[1]+1,1),1)
    IF (left:=left-8-buffer[1])<0 THEN Raise("MNGL")
    IF Seek(file,buffer[1]-4,O_CURRENT)=-1 THEN Raise("MNGL")
  ENDWHILE
ENDPROC -1

EXPORT PROC	seekIFFChunk(file,tptr:PTR TO LONG,next=0)
	DEF type,left,buffer[2]:ARRAY OF LONG,curr

  type:=tptr[]
  IF (curr:=Seek(file,0,O_END))=-1 THEN Raise("READ")
  IF next THEN IF (curr:=curr-4)<0 THEN Raise("READ")
  IF (left:=Seek(file,curr,O_BEGINNING))=-1 THEN Raise("READ")
  left:=left-curr
  IF next
    IF Read(file,buffer,4)<>4 THEN Raise("READ")
    buffer[]:=Shl(Shr(buffer[]+1,1),1)
    IF (left:=left-4-buffer[])<0 THEN Raise("MNGL")
    IF Seek(file,buffer[],O_CURRENT)=-1 THEN Raise("READ")
  ENDIF
  WHILE left>0
    IF left<8 THEN Raise("NIFF")
    IF Read(file,buffer,8)<8 THEN Raise("READ")
    IF buffer[]="FORM"
      IF Seek(file,-8,O_CURRENT)=-1 THEN Raise("READ")
      RETURN -1
    ENDIF
    buffer[1]:=Shl(Shr(buffer[1]+1,1),1)
    IF (buffer[]=type) OR (type=0)
      tptr[]:=buffer[]
      RETURN buffer[1]
    ENDIF
    IF (left:=left-8-buffer[1])<0 THEN Raise("MNGL")
    IF Seek(file,buffer[1],O_CURRENT)=-1 THEN Raise("MNGL")
  ENDWHILE
ENDPROC -1

