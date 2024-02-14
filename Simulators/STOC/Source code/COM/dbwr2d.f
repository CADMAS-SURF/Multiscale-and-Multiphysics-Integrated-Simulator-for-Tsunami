      SUBROUTINE DBWR2D(AAA,IDIR,IFIX,MX,MY,MZ,LPX)
C======================================================================
C     倍精度実数型3次元配列の2次元断面をリスト出力する
C
C         IDIR   : 出力する断面の方向
C                : = 1 のとき YZ断面
C                : = 2 のとき XZ断面
C                : = 3 のとき XY断面
C         IFIX   : 出力する断面の(法線方向)インデックス
C======================================================================
      IMPLICIT NONE
C
C     INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
C     INCLUDE 'TIME.h'
C
      INTEGER,PARAMETER::NCOL1=10
ccccc      INTEGER,PARAMETER::NCOL1=5
C
      INTEGER,INTENT(INOUT)::IDIR,IFIX,MX,MY,MZ,LPX
      REAL(8),INTENT(INOUT)::AAA(MX,MY,MZ)
C
      CHARACTER(32)::FORM1='(1X,2H#=,I4                    )'
      CHARACTER(32)::FORM2='(1X,1H#,5X,1H|,  2000(I11:)    )'
      CHARACTER(32)::FORM3='(1X,7H------+,   2000(A11:)    )'
      CHARACTER(32)::FORM4='(1X,2H#=,I3,2H |,2000(1PE11.4:))'
C
      INTEGER::J,JE,JEND,JS,K,KE,KS,N,NBLOCK,I
C
C
      IF( LPX.EQ.IFLLP ) THEN
         IF( MZ.EQ.1 ) THEN
            WRITE(LPX) MX-1,MY-1,1
            WRITE(LPX) ((REAL(AAA(I,J,1)),I=1,MX-1),J=1,MY-1)
         ELSE
            WRITE(LPX) MX-1,MY-1,MZ-1
            WRITE(LPX) (((REAL(AAA(I,J,K)),I=1,MX-1),J=1,MY-1),K=1,MZ-1)
         ENDIF
         RETURN
      ENDIF
C
      IF( IDIR .EQ. 1 ) THEN
         FORM1(7:7) = 'I'
         FORM2(7:7) = 'J'
         FORM4(7:7) = 'K'
         NBLOCK = ( MY-2 ) / NCOL1 + 1
         JEND   = MY
         KE     = MZ
      ELSE IF( IDIR .EQ. 2 ) THEN
         FORM1(7:7) = 'J'
         FORM2(7:7) = 'I'
         FORM4(7:7) = 'K'
         NBLOCK = ( MX-2 ) / NCOL1 + 1
         JEND   = MX
         KE     = MZ
      ELSE
         FORM1(7:7) = 'K'
         FORM2(7:7) = 'I'
         FORM4(7:7) = 'J'
         NBLOCK = ( MX-2 ) / NCOL1 + 1
         JEND   = MX
         KE     = MY
      END IF
C
C
      WRITE(LPX,FORM1) IFIX
C
      JS = 1
      KS = 1
      JE = MIN( JS + NCOL1 - 1, JEND)
C
      DO 100 N=1,NBLOCK
         WRITE(LPX,FORM2) (J,J=JS,JE)
         WRITE(LPX,FORM3) ('----------+',J=JS,JE)
         DO 200 K=KE,KS,-1
            FORM4(5:5) = '2'
            FORM4(8:8) = '='
            FORM4(11:11) = '3'
            IF(K.GT.999) THEN
               FORM4(5:5) = '1'
               FORM4(8:8) = ' '
               FORM4(11:11) = '4'
            ENDIF
            IF( IDIR .EQ. 1 ) WRITE(LPX,FORM4) K,(AAA(IFIX,J,K),J=JS,JE)
            IF( IDIR .EQ. 2 ) WRITE(LPX,FORM4) K,(AAA(J,IFIX,K),J=JS,JE)
            IF( IDIR .EQ. 3 ) WRITE(LPX,FORM4) K,(AAA(J,K,IFIX),J=JS,JE)
  200    CONTINUE
         JS = JS + NCOL1
         JE = MIN( JE + NCOL1, JEND)
  100 CONTINUE
C
      RETURN
      END
