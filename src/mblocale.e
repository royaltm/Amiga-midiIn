OPT MODULE

MODULE 'locale','utility'

EXPORT DEF catalog

EXPORT ENUM STRID_MIDIIN,
            STRID_CXTITLE,
            STRID_CXDESCR,
            STRID_AIFFNAME,
            STRID_8SVXNAME,
            STRID_WAVENAME,
            STRID_LOOKINGFORSAMPLE,
            STRID_LOADINGSAMPLE,
            STRID_CHOOSEANOTHERSAMPLE,
            STRID_MIDIINSCOPE,
            STRID_MIDIINADVANCEDAUDIO,
            STRID_MIDIINADVANCED,
            STRID_AHIMODE,
            STRID_MFREQ,
            STRID_CHANPOLY,
            STRID_UNDO,
            STRID_APPLY,
            STRID_AHIAUDIOINFO,
            STRID_AHIINFONAME,
            STRID_AHIINFODRIVER,
            STRID_AHIINFOAUTHOR,
            STRID_AHIINFOVERSION,
            STRID_AHIINFOCOPYRIGHT,
            STRID_MIDIMESANDCHAN,
            STRID_NOTEOFF,
            STRID_NOTEON,
            STRID_KEYPRESS,
            STRID_CTRL,
            STRID_CHANPRESS,
            STRID_SETPITCHBEND,
            STRID_PATCHBAY,
            STRID_REFRESH,
            STRID_C01,
            STRID_C02,
            STRID_C03,
            STRID_C04,
            STRID_C05,
            STRID_C06,
            STRID_C07,
            STRID_C08,
            STRID_C09,
            STRID_C10,
            STRID_C11,
            STRID_C12,
            STRID_C13,
            STRID_C14,
            STRID_C15,
            STRID_C16,
            STRID_FOLLOW,
            STRID_EDITRANGE,
            STRID_AUDIO,
            STRID_MIDICTRL,
            STRID_INSTRUMENT,
            STRID_FREE,
            STRID_MIDICHAN,
            STRID_RELOAD,
            STRID_BANKPRIORITY,
            STRID_DURATION,
            STRID_ONOFF,
            STRID_ONON,
            STRID_DRUM,
            STRID_GROUP,
            STRID_MONOVSENS,
            STRID_MONOSLIDE,
            STRID_OFF,
            STRID_MONOPHONIC,
            STRID_LOOP,
            STRID_SET,
            STRID_BASE,
            STRID_FINE,
            STRID_CLEAR,
            STRID_ADD,
            STRID_DELETE,
            STRID_MENUPROJECT,
            STRID_MENUNEW,
            STRID_OPEN,
            STRID_MERGE,
            STRID_RELOADALL,
            STRID_SAVE,
            STRID_SAVEAS,
            STRID_SUMM,
            STRID_HIDE,
            STRID_ABOUT,
            STRID_QUIT,
            STRID_EDIT,
            STRID_COPY,
            STRID_PASTE,
            STRID_MENUDELETE,
            STRID_MENUSORT,
            STRID_MENUSORTPRI,
            STRID_MENUSORTMIDI,
            STRID_MENUSORTNAME,
            STRID_MENUSORTRANGE,
            STRID_MENUUNDO,
            STRID_MENUREDO,
            STRID_SETTINGS,
            STRID_AUDIOENABLE,
            STRID_MENUMIDICTRL,
            STRID_MENUFOLLOW,
            STRID_ADVANCEDMIDI,
            STRID_ADVANCEDAUDIO,
            STRID_SAVESETTINGS,
            STRID_MENUSETLAYOUT,
            STRID_MENUSETWITHPROJECTS,
            STRID_MENUSETSAVEICONS,
            STRID_MENUSETSAVEUNDO,
            STRID_WINDOWS,
            STRID_MENUMAIN,
            STRID_MENUVOLUME,
            STRID_MENUENVELOPE,
            STRID_SCOPE,
            STRID_MENUMIDIMON,
            STRID_ADDSAMPLES,
            STRID_ADDINGINSTRUMENTS,
            STRID_INSTRUMENTS,
            STRID_DELETELIST,
            STRID_SECONDS,
            STRID_VOLUMECTRLWIN,
            STRID_FREQNUM,
            STRID_TIMENUM,
            STRID_VOLUMESET,
            STRID_HUNPERCENT,
            STRID_MAXVOLUME,
            STRID_VELOCITY,
            STRID_AFTERTOUCH,
            STRID_SUBAFTERT,
            STRID_AFTERVEL,
            STRID_PANORAMA,
            STRID_WIDE,
            STRID_CENTER,
            STRID_MCTRLVOL,
            STRID_MCTRLPAN,
            STRID_SUSTAIN,
            STRID_PITCHBENDER,
            STRID_NOTERANGE,
            STRID_FIRSTSKIP,
            STRID_UNNAMED,
            STRID_SAVEPROJECTAS,
            STRID_CHOOSEPROJECT,
            STRID_PROJECT,
            STRID_AREUSURE,
            STRID_UNUSED,
            STRID_ALL,
            STRID_CANCEL,
            STRID_REQUEST,
            STRID_OVERORSKIP,
            STRID_NOTTOUNDO,
            STRID_NOTTOREDO,
            STRID_OVERWRITE,
            STRID_SKIPOVER,
            STRID_MERGEDONTFIT,
            STRID_CONTINUE,
            STRID_UNSAVED,
            STRID_LOSE,
            STRID_BACK,
            STRID_QUESTION,
            STRID_SUREQUIT,
            STRID_EXIT,
            STRID_INSTRNOTFOUND,
            STRID_REPLACE,
            STRID_ABORT,
            STRID_SKIP,
            STRID_TROUBLE,
            STRID_UNKNOWN,
            STRID_WRONGFILENAME,
            STRID_NOTINTERCHANGEFF,
            STRID_IFFMANGLED,
            STRID_WRITEERROR,
            STRID_SETRANGEFIRST,
            STRID_POPKEYERROR,
            STRID_BADPROJECT,
            STRID_AUDIOERROR,
            STRID_NOCOMMONERROR,
            STRID_NOAHDRERROR,
            STRID_UNKNOWNSOUNDFILE,
            STRID_READERROR,
            STRID_NOSSNDERROR,
            STRID_NOBODYERROR,
            STRID_NOWAVECHUNKS,
            STRID_UNSUPPORTEDWAVE,
            STRID_MEMERROR,
            STRID_MATHERROR,
            STRID_LIBRARYERROR,
            STRID_MIDISOURCEERR,
            STRID_GADTOOLSERR,
            STRID_WINDOWERROR,
            STRID_GADMEMERROR,
            STRID_PREFSERROR,
            STRID_IFFPARSEERROR,
            STRID_USERBREAK,
            STRID_NOWAY,
            STRID_ERROR,
            STRID_VOLUMEENVELWIN,
            STRID_BANKNUM,
            STRID_EATTACK,
            STRID_EDECAY,
            STRID_ESUSTAIN,
            STRID_ERELEASE,
            STRID_OFVOLUME,
            STRID_MIDIMONITORWIN,
            STRID_MIDICONTROLLER,
            STRID_MIDIVOLUME,
            STRID_MIDIPAN,
            STRID_MIDIPITCHBEND,
            STRID_NOTAVAILABLE,
            STRID_PROJECTSUMMARY,
            STRID_PROJECTNAME,
            STRID_ACTIVEBANKS,
            STRID_SAMPLESINLIST,
            STRID_SAMPLESINMEMORY,
            STRID_TOTALLENGTH,
            STRID_BYTES,
            STRID_FREEMEMORY,
            STRID_FAST,
            STRID_LARGEST,
            STRID_CHIP,
            STRID_DATASIZE,
            STRID_UNDOMEMLEFT,
            STRID_RUNTIME,
            STRID_SET_SAMPLELIST,
            STRID_SET_ALL,
            STRID_SET_BANKS,
            STRID_SET_XCHGBANKS,
            STRID_SET_SAMPLEDELETE,
            STRID_SET_INSTR,
            STRID_SET_MIDI,
            STRID_SET_PRI,
            STRID_SET_BASE,
            STRID_SET_FINE,
            STRID_SET_SET_LOOP,
            STRID_SET_SET_DUR,
            STRID_SET_SET_MONO,
            STRID_SET_SET_ADDAFTERT,
            STRID_SET_BOUNDS,
            STRID_SET_VOLUME,
            STRID_SET_VELSENS,
            STRID_SET_RELEASE,
            STRID_SET_PANORAMA,
            STRID_SET_PANWIDE,
            STRID_SET_PITCHSENS,
            STRID_SET_ATTACK,
            STRID_SET_DECAY,
            STRID_SET_SUSTAINLEV,
            STRID_SET_AFTERSENS,
            STRID_SET_FIRSTSKIP,
            STRID_SET_MCTRLVOL,
            STRID_SET_MCTRLPAN,
            STRID_SET_GROUP,
            STRID_SET_MONOVSENS,
            STRID_SET_MONOSLIDE,
            STRID_AHIREQUESTER,
            STRID_MAINUNDO,
            STRID_MAX


EXPORT PROC getLocStr(id,gadchar=FALSE)
DEF strings:PTR TO LONG,ret,i
  strings:=['midiIn',   -> STRID_MIDIIN
            'midiIn: 16bit unlimited sample player', -> STRID_CXTITLE
            'MIDI controlled, up to 32 sound chan.', -> STRID_CXDESCR
            'Audio IFF file', ->  STRID_AIFFNAME
            '8svx IFF file',  ->  STRID_8SVXNAME
            'WAVE RIFF file', ->  STRID_WAVENAME
            'Looking for instrument...', -> STRID_LOOKINGFORSAMPLE
            'Loading instrument...',     -> STRID_LOADINGSAMPLE
            'Choose another instrument file', -> STRID_CHOOSEANOTHERSAMPLE
            'midiIn scope window', -> STRID_MIDIINSCOPE
            'midiIn ahi settings', -> STRID_MIDIINADVANCEDAUDIO
            'midiIn advanced settings', -> STRID_MIDIINADVANCED
            'A_hi ID', -> STRID_AHIMODE
            '_Mix freq', -> STRID_MFREQ
            'Max _polyphony:     ', -> STRID_CHANPOLY
            '_Undo', -> STRID_UNDO
            '_Apply', -> STRID_APPLY
            'Ahi Info', -> STRID_AHIAUDIOINFO
            'Name', -> STRID_AHIINFONAME
            'Driver', -> STRID_AHIINFODRIVER
            'Author', -> STRID_AHIINFOAUTHOR
            'Version', -> STRID_AHIINFOVERSION
            '(c)', -> STRID_AHIINFOCOPYRIGHT
            'midi messages and channels', -> STRID_MIDIMESANDCHAN
            'Note Off', -> STRID_NOTEOFF
            'Note On', -> STRID_NOTEON
            'Key Pressure', -> STRID_KEYPRESS
            'Control Change', -> STRID_CTRL
            'Channel Pressure', -> STRID_CHANPRESS
            'Pitch Bender', -> STRID_SETPITCHBEND
            'midi sources', -> STRID_PATCHBAY
            '_Refresh', -> STRID_REFRESH
            ' 1', -> STRID_C01
            ' 2', -> STRID_C02
            ' 3', -> STRID_C03
            ' 4', -> STRID_C04
            ' 5', -> STRID_C05
            ' 6', -> STRID_C06
            ' 7', -> STRID_C07
            ' 8', -> STRID_C08
            ' 9', -> STRID_C09
            '10', -> STRID_C10
            '11', -> STRID_C11
            '12', -> STRID_C12
            '13', -> STRID_C13
            '14', -> STRID_C14
            '15', -> STRID_C15
            '16', -> STRID_C16
            'F_ollow', -> STRID_FOLLOW
            '_Edit', -> STRID_EDITRANGE
            'Audio', -> STRID_AUDIO
            '_Midi', -> STRID_MIDICTRL
            'Instrument:', -> STRID_INSTRUMENT
            'Free', -> STRID_FREE
            'M_idi channel:   ', -> STRID_MIDICHAN
            '_Reload', -> STRID_RELOAD
            'Bank priority:    ', -> STRID_BANKPRIORITY
            '_dur.', -> STRID_DURATION
            'ON->OFF', -> STRID_ONOFF
            'ON->ON', -> STRID_ONON
            'DRUM', -> STRID_DRUM
            'Mute _gr.   ', -> STRID_GROUP
            '_Velocity:     ', -> STRID_MONOVSENS
            '_Portamento', -> STRID_MONOSLIDE
            'Off', -> STRID_OFF
            'Mo_no', -> STRID_MONOPHONIC
            '_Loop', -> STRID_LOOP
            '_Set', -> STRID_SET
            'Base', -> STRID_BASE
            'Fine:', -> STRID_FINE
            '_Clear', -> STRID_CLEAR
            '_Add', -> STRID_ADD
            'Delete', -> STRID_DELETE
            'Project', -> STRID_MENUPROJECT
            'N\0New', -> STRID_MENUNEW
            'O\0Open', -> STRID_OPEN
            'Add Project', -> STRID_MERGE
            'Reload All', -> STRID_RELOADALL
            'W\0Save', -> STRID_SAVE
            'Save as', -> STRID_SAVEAS
            'I\0Info', -> STRID_SUMM
            'H\0Hide', -> STRID_HIDE
            '?\0About', -> STRID_ABOUT
            'Q\0Quit', -> STRID_QUIT
            'Edit', -> STRID_EDIT
            'C\0Copy', -> STRID_COPY
            'V\0Paste', -> STRID_PASTE
            'X\0Delete', -> STRID_MENUDELETE
            'Sort by', -> STRID_MENUSORT
            'bank priority', -> STRID_MENUSORTPRI
            'MIDI channel', -> STRID_MENUSORTMIDI
            'instrument', -> STRID_MENUSORTNAME
            'range', -> STRID_MENUSORTRANGE
            'U\0Undo', -> STRID_MENUUNDO,
            'Z\0Redo', -> STRID_MENUREDO,
            'Settings', -> STRID_SETTINGS
            'A\0Audio enable', -> STRID_AUDIOENABLE
            'M\0MIDI control', -> STRID_MENUMIDICTRL
            'F\0Follow mode', -> STRID_MENUFOLLOW
            'P\0Midi Advanced...', -> STRID_ADVANCEDMIDI
            'D\0Audio...', -> STRID_ADVANCEDAUDIO
            'Save defaults', -> STRID_SAVESETTINGS
            'L\0Save layout?', -> STRID_MENUSETLAYOUT
            ';\0Load with projects?', -> STRID_MENUSETWITHPROJECTS
            'Save icons?', -> STRID_MENUSETSAVEICONS
            'Save undo history?', -> STRID_MENUSETSAVEUNDO
            'Windows', -> STRID_WINDOWS
            '1\0Main', -> STRID_MENUMAIN
            '2\0Details', -> STRID_MENUVOLUME
            '3\0Envelope', -> STRID_MENUENVELOPE
            '4\0Scope', -> STRID_SCOPE
            '5\0Monitor', -> STRID_MENUMIDIMON
            'Add instruments', -> STRID_ADDSAMPLES
            'Adding instruments...', -> STRID_ADDINGINSTRUMENTS
            'Instruments:',  -> STRID_INSTRUMENTS
            'Delete:', -> STRID_DELETELIST
            'seconds', -> STRID_SECONDS
            'midiIn details window', -> STRID_VOLUMECTRLWIN
            'Hz:', -> STRID_FREQNUM
            'time:', -> STRID_TIMENUM
            '_Volume:     ', -> STRID_VOLUMESET
            '_Nor', -> STRID_HUNPERCENT
            '_Max', -> STRID_MAXVOLUME
            'Ve_locity:     ', -> STRID_VELOCITY
            'Af_tertouch:     ', -> STRID_AFTERTOUCH
            'Su_b', -> STRID_SUBAFTERT
            '_Same', -> STRID_AFTERVEL
            '_Panorama:', -> STRID_PANORAMA
            '_Autopan:     ', -> STRID_WIDE
            '_Center', -> STRID_CENTER
            'midi Vol_ume', -> STRID_MCTRLVOL
            'mid_i Pan', -> STRID_MCTRLPAN
            '_Sustain:', -> STRID_SUSTAIN
            'Pitch Bender', -> STRID_PITCHBENDER
            'note _range:      ', -> STRID_NOTERANGE
            '_Offset [ms]', -> STRID_FIRSTSKIP
            '<Unnamed>', -> STRID_UNNAMED
            'Save project as', -> STRID_SAVEPROJECTAS
            'Choose project to Open', -> STRID_CHOOSEPROJECT
            'Project:', -> STRID_PROJECT
            'Are U sure you want to\nclear sample list?', -> STRID_AREUSURE
            'Unused', -> STRID_UNUSED
            'All', -> STRID_ALL
            'Cancel', -> STRID_CANCEL
            'request', -> STRID_REQUEST
            'Some existing active banks\ncould be overlapped!\nPlease select the course of action:', -> STRID_OVERORSKIP
            'Nothing to Undo', -> STRID_NOTTOUNDO
            'Nothing to Redo', -> STRID_NOTTOREDO
            'Overlap', -> STRID_OVERWRITE
            'Skip over', -> STRID_SKIPOVER
            'There are too many active banks\nin project being added!\n(\d of them will be cut off)', -> STRID_MERGEDONTFIT
            'Continue', -> STRID_CONTINUE
            'Current project is unsaved!\nAre U sure you want to lose it?', -> STRID_UNSAVED
            'Lose', -> STRID_LOSE
            'Back', -> STRID_BACK
            'question', -> STRID_QUESTION
            'Are U sure you want to exit?', -> STRID_SUREQUIT
            'Sure',     -> STRID_EXIT
            'Instrument not found:', -> STRID_INSTRNOTFOUND
            'Replace', -> STRID_REPLACE
            'Abort', -> STRID_ABORT
            'Skip', -> STRID_SKIP
            'trouble', -> STRID_TROUBLE
            'Unknown', -> STRID_UNKNOWN
            'Wrong file name\n or \nfile not found!', -> STRID_WRONGFILENAME
            'Not an Interchange File Format!\n(no "FORM" chunk)', -> STRID_NOTINTERCHANGEFF
            'Iffparse error or IFF file mangled!', -> STRID_IFFMANGLED
            'Error while writing to file!\n(BAD disk?)', -> STRID_WRITEERROR
            'Set range over more than one bank!', -> STRID_SETRANGEFIRST
            'Wrong CX_POPKEY argument!\n\q\s\q', -> STRID_POPKEYERROR
            'Bad project data!',  -> STRID_BADPROJECT
            'Can''t allocate AHI!\nCheck other programs that are\nusing audio!', -> STRID_AUDIOERROR
            'The file contains no common chunk!\n(is not an Audio IFF)', -> STRID_NOCOMMONERROR
            'The file contains no audio header!\n(is not an IFF 8SVX)', -> STRID_NOAHDRERROR
            'Unknown sound file type!', -> STRID_UNKNOWNSOUNDFILE
            'Trouble while reading file!\n(BAD disk or file?)', -> STRID_READERROR
            'Sample data not found!\n(no "SSND" chunk)', -> STRID_NOSSNDERROR
            'Sample data not found!\n(no "BODY" chunk)', -> STRID_NOBODYERROR
            'Couldn\at find required \qRIFF WAVE\q chunks', -> STRID_NOWAVECHUNKS
            'WAVE format category not supported', -> STRID_UNSUPPORTEDWAVE
            'Not enough memory!\n(or mem is fragmented)', -> STRID_MEMERROR
            'No \amathieeedoubbas.library\a!\n(check your LIBS: directory!)', -> STRID_MATHERROR
            'Required library: \a\s\a is missing!', -> STRID_LIBRARYERROR
            'Midi source: \a\s\a not found!', -> STRID_MIDISOURCEERR
            'Couldn\at open \agadtools.library\a!', -> STRID_GADTOOLSERR
            'Window doesn\at fit on current PubScreen!\n(Change to bigger screen size)', -> STRID_WINDOWERROR
            'No memory for gadgets or window!', -> STRID_GADMEMERROR
            'Preferences file corrupted!', -> STRID_PREFSERROR
            'Could not open \aiffparse.library\a!', -> STRID_IFFPARSEERROR
            'User BREAK \q^ C\q ', -> STRID_USERBREAK
            'No way!', -> STRID_NOWAY
            'ERROR!', -> STRID_ERROR
            'midiIn envelope window', -> STRID_VOLUMEENVELWIN
            'Bank:', -> STRID_BANKNUM
            'attack: ', -> STRID_EATTACK
            'decay: ',  -> STRID_EDECAY
            'sustain: ',-> STRID_ESUSTAIN
            'release: ', -> STRID_ERELEASE
            '100% of volume', -> STRID_OFVOLUME
            'midiIn monitor', -> STRID_MIDIMONITORWIN
            '_ctrl', -> STRID_MIDICONTROLLER
            'Volume', -> STRID_MIDIVOLUME
            'Pan', -> STRID_MIDIPAN
            'Pitchbend', -> STRID_MIDIPITCHBEND
            'N.A.', -> STRID_NOTAVAILABLE
            'project summary', -> STRID_PROJECTSUMMARY
            'Project name:', -> STRID_PROJECTNAME
            'Active banks:', -> STRID_ACTIVEBANKS
            'Samples in list:', -> STRID_SAMPLESINLIST
            'Samples in memory:', -> STRID_SAMPLESINMEMORY
            'total length:',-> STRID_TOTALLENGTH
            'bytes', -> STRID_BYTES
            'free memory', -> STRID_FREEMEMORY
            'Fast:', -> STRID_FAST
            'largest:', -> STRID_LARGEST
            'Chip:', -> STRID_CHIP
            'Data size:', -> STRID_DATASIZE
            'Undo memory left:', -> STRID_UNDOMEMLEFT
            'Run time:', -> STRID_RUNTIME
            'instruments', -> STRID_SET_SAMPLELIST
            'paste banks', -> STRID_SET_ALL
            'delete banks', -> STRID_SET_BANKS
            'exchange banks', -> STRID_SET_XCHGBANKS
            'delete instrument', -> STRID_SET_SAMPLEDELETE
            'instrument', -> STRID_SET_INSTR
            'midi', -> STRID_SET_MIDI
            'priority', -> STRID_SET_PRI
            'base', -> STRID_SET_BASE
            'fine', -> STRID_SET_FINE
            'loop', -> STRID_SET_SET_LOOP
            'duration', -> STRID_SET_SET_DUR
            'mono', -> STRID_SET_SET_MONO
            'sub aftertouch', -> STRID_SET_SET_ADDAFTERT
            'key range', -> STRID_SET_BOUNDS
            'volume', -> STRID_SET_VOLUME
            'velocity', -> STRID_SET_VELSENS
            'release', -> STRID_SET_RELEASE
            'panorama', -> STRID_SET_PANORAMA
            'autopan', -> STRID_SET_PANWIDE
            'pitch bender', -> STRID_SET_PITCHSENS
            'attack', -> STRID_SET_ATTACK
            'decay', -> STRID_SET_DECAY
            'sustain', -> STRID_SET_SUSTAINLEV
            'aftertouch', -> STRID_SET_AFTERSENS
            'offset', -> STRID_SET_FIRSTSKIP
            'midi volume', -> STRID_SET_MCTRLVOL
            'midi pan', -> STRID_SET_MCTRLPAN
            'mute group', -> STRID_SET_GROUP
            'portamento velocity', -> STRID_SET_MONOVSENS
            'portamento', -> STRID_SET_MONOSLIDE
            'Choose ahi mode', -> STRID_AHIREQUESTER
            'Undo', -> STRID_MAINUNDO
            '']

  IF id=STRID_MIDIIN THEN RETURN strings[id]
  IF (id < STRID_MAX) AND (id >= 0)
    ret:=IF localebase THEN GetCatalogStr(catalog,id,strings[id]) ELSE strings[id]
    IF (ret[1]=0) AND (gadchar <> -1)
      ret:=ret+2
    ELSEIF (gadchar = -1) AND (ret[1] <> 0) 
      ret:=0
    ELSEIF (gadchar <> 0) AND (gadchar <> -1)
      IF (i:=InStr(ret,'_',0)) = -1
        ^gadchar:=0
      ELSE
        i:=ret[i+1]
        i:=ToLower(i)
        ^gadchar:=i
      ENDIF
    ENDIF
  ELSE
    ret:=''
  ENDIF

ENDPROC ret
