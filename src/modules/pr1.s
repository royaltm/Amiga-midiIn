      bopt w2-
      MC68060

calcvolume:   ;<D0=volume(65536=100%), D1=panorama(0:131072) >D1,D2 l,r(256=100%)
        DIVU.L D1,D0
        RTS
        MULU.L D0,D1
        DIVU.L #65535,D1
        RTS
        DIVU.L  #65536,D0
        RTS

        BFEXTU  D0{0:D1},D1
        RTS
        LSR.L   #7,D1
        MOVE.W  #1024,D2
        SUB.W   D1,D2
        MOVE.L  #$100000,D3
        MULU    D1,D1
        MULU    D2,D2
        NEG.L   D1
        ADD.L   D3,D1
        NEG.L   D2
        ADD.L   D3,D2
        LSR.L   #8,D0
        MULU.L  D0,D1
        DIVU.L  D3,D1
        MULU.L  D0,D2
        DIVU.L  D3,D2
        RTS







divr:   MOVEQ   #32,D3      ;D0.D1=D0/D1
        BFFFO   D0{0:0},D4
        SUB.W   D4,D3
        LSL.L   D4,D0
        DIVUL.L D1,D2:D0
        MOVE.L  D0,D5
        LSR.L   D4,D5       ;xxx.
        MOVE.L  D0,D6       ;xxx.yyy
        MOVE.L  D2,D0
        TST.W   D3
        BLE.S   dkoniec
dloop:  BFFFO   D0{0:0},D4
        SUB.W   D4,D3
        BPL.S   dok
        ADD.W   D3,D4
dok:    LSL.L   D4,D0
        DIVUL.L D1,D2:D0
        LSL.L   D4,D6
        ADD.L   D0,D6
        MOVE.L  D2,D0
        TST.W   D3
        BGT.S   dloop
        MOVE.L  D5,D0
        MOVE.L  D6,D1
dkoniec: RTS
