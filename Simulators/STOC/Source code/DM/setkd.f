      SUBROUTINE SETKD(HH,KK,NA)
C----------------------------------------
C     HHの高さを含む流体セルのインデックスKKを定める
C----------------------------------------
      USE M_GRID
C
      IMPLICIT NONE
C
      REAL(8),INTENT(IN)::HH
      INTEGER,INTENT(IN)::NA
      INTEGER,INTENT(OUT)::KK
C
      INTEGER::K
C
C
C
      IF( HH.LE.ZG(NA,0)  ) THEN
         KK = 1
      ELSEIF( HH.GT.ZG(NA,NK(NA)) ) THEN
         KK = NK(NA)
      ELSE
         DO K=1,NK(NA)
            IF( HH.GT.ZG(NA,K-1).AND.HH.LE.ZG(NA,K) ) EXIT
         ENDDO
         KK = K
      ENDIF
C
      RETURN
      END
