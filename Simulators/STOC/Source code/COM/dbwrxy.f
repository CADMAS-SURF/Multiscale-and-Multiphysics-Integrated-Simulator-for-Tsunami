      SUBROUTINE DBWRXY(AAA)
C======================================================================
C     倍精度実数型2次元配列をリスト出力する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
C     INCLUDE 'TIME.h'
C
      INTEGER,PARAMETER::NCOL1=10
C
      REAL(8),INTENT(INOUT)::AAA(MX,MY)
C
      CHARACTER(32)::FORM1='(1X,2H#=,I4                    )'
      CHARACTER(32)::FORM2='(1X,1H#,5X,1H|,  1000(I11:)    )'
      CHARACTER(32)::FORM3='(1X,7H------+,   1000(A11:)    )'
      CHARACTER(32)::FORM4='(1X,2H#=,I3,2H |,1000(1PE11.4:))'
C
      INTEGER::J,JE,JEND,JS,K,KE,KS,N,NBLOCK
C
C
      FORM2(7:7) = 'I'
      FORM4(7:7) = 'J'
      NBLOCK = ( MX-2 ) / NCOL1 + 1
      JEND   = MX
      KE     = MY
C
      JS = 1
      KS = 1
      JE = MIN( JS + NCOL1 - 1, JEND)
C
      DO 100 N=1,NBLOCK
         WRITE(LP,FORM2) (J,J=JS,JE)
         WRITE(LP,FORM3) ('----------+',J=JS,JE)
         DO 200 K=KE,KS,-1
            FORM4(5:5) = '2'
            FORM4(8:8) = '='
            FORM4(11:11) = '3'
            IF(K.GT.999) THEN
               FORM4(5:5) = '1'
               FORM4(8:8) = ' '
               FORM4(11:11) = '4'
            ENDIF
            WRITE(LP,FORM4) K,(AAA(J,K),J=JS,JE)
  200    CONTINUE
         JS = JS + NCOL1
         JE = MIN( JE + NCOL1, JEND)
  100 CONTINUE
C
      RETURN
      END
