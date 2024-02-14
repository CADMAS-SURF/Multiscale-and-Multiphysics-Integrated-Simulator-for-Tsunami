      SUBROUTINE DSHIFT
C----------------------------------------
C     漂流物の基本変数の入れ換え(N+1→N)
C----------------------------------------
      USE M_DRIFT
C
      IMPLICIT NONE
C
      INTEGER::N
C
C
      DO N=1,ND
         XD(N) = XD1(N)
         YD(N) = YD1(N)
         TD(N) = TD1(N)
         UD(N) = UD1(N)
         VD(N) = VD1(N)
         OD(N) = OD1(N)
      ENDDO
C
      RETURN
      END
