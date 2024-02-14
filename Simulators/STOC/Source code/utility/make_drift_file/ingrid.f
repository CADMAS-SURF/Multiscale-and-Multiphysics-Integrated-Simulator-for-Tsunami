      SUBROUTINE INGRID
C======================================================================
C     格子データを読み込む
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'GRID.h'
C
      REAL(8)::RTMP(2),XORG,YORG
C
      INTEGER::I,IE,IEND,IS,J,M,NDAT1
      INTEGER::IX,IY
C
C
      CNUL(  1: 50)='                                                  '
      CNUL( 51:100)='                                                  '
      CNUL(101:132)='                                                  '
C
      MX=0
      MY=0
      MZ=0
      IX=0
      IY=0
      RTMP=0.D0
C
C ... Zの入力が省略された場合用のデフォルト値
      MZM=2
      ZGRID(1)=-1.D4
      ZGRID(2)=0.D0
C
      DO 300 M=1,100000
         CALL GET1(IS,IE,IEND)
         IF( IEND.EQ.1 ) GO TO 900
C
C     
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 400
C
         ELSE IF( CLINE(IS:IE) .EQ. 'X' ) THEN
            CALL MGETR(XGRID,MXM,NGRDSZ)
            IX=1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LONGITUDE' ) THEN
            CALL MGETR(XGRID,MXM,NGRDSZ)
            IX=2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'Y' ) THEN
            CALL MGETR(YGRID,MYM,NGRDSZ)
            IY=1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LATITUDE' ) THEN
            CALL MGETR(YGRID,MYM,NGRDSZ)
            IY=2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'Z' ) THEN
            CALL MGETR(ZGRID,MZM,NGRDSZ)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'ORIGIN' ) THEN
            CALL MGETR(RTMP,NDAT1,2)
C
         END IF
  300 CONTINUE
  400 CONTINUE
C
      IF( IX.NE.IY.OR.IX*IY.EQ.0 ) GOTO 910
      ICORDTYPE=IX
C
C     座標値をシフト(XORG=RTMP(1),YORG=RTMP(2))
C
      XORG = RTMP(1)
      YORG = RTMP(2)
      DO 500 I=1,MXM
        XGRID(I) = XGRID(I)+XORG
  500 CONTINUE
      DO 600 J=1,MYM
        YGRID(J) = YGRID(J)+YORG
  600 CONTINUE
C
C ... MX,MY,MZ,MXYZ,MXY,NXYZの値を設定
      MX   = MXM+1
      MY   = MYM+1
      MZ   = MZM+1
      NX   = MXM-1
      NY   = MYM-1
      NZ   = MZM-1
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INGRID',6573)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
  910 CONTINUE
      CALL ERRMSG('INGRID',6574)
      WRITE(LP,*) 'ONE OF (X,Y) OR (LONGITUDE,LATITUDE) ',
     $   'MUST BE SPECIFIED'
      CALL ABORT1('')
      END
