      INTEGER,PARAMETER:: MAX_STOC   = 1024
      INTEGER,PARAMETER:: MAX_CADMAS = 1024
C
      INTEGER::NB_STOC,LB_STOC,IB_STOC(MAX_STOC)
      INTEGER::NB_CADMAS,LB_CADMAS,IB_CADMAS(MAX_CADMAS)
      INTEGER::NB_SC,ITAGSC
      COMMON /CADMAS1/NB_STOC,LB_STOC,IB_STOC,
     $                NB_CADMAS,LB_CADMAS,IB_CADMAS,NB_SC,ITAGSC
C
C
      INTEGER,PARAMETER::MAX_NIST = 2000
      INTEGER,PARAMETER::MAX_NJST = 2000
      INTEGER,PARAMETER::MAX_NKST = 500
      INTEGER,PARAMETER::MAX_CADBUF = 200000
C
      INTEGER::NIST,NJST,NKST
      INTEGER::IWCAD,IECAD,JSCAD,JNCAD,KBCAD,KTCAD
      INTEGER::IIOFF(2),JJOFF(2)
      INTEGER::IICAD(6,MAX_CADMAS),JJCAD(6,MAX_CADMAS)
      COMMON /CADMAS2/NIST,NJST,NKST,
     $                IWCAD,IECAD,JSCAD,JNCAD,KBCAD,KTCAD,
     $                IIOFF,JJOFF,IICAD,JJCAD
C
C
      REAL(8)::UWCAD(MAX_NJST,MAX_NKST,6),UECAD(MAX_NJST,MAX_NKST,6)
      REAL(8)::VSCAD(MAX_NIST,MAX_NKST,6),VNCAD(MAX_NIST,MAX_NKST,6)
      REAL(8)::CADBUF(MAX_CADBUF)
      COMMON /CADMAS3/UWCAD,UECAD,VSCAD,VNCAD,CADBUF
