      SUBROUTINE MODVERT(DLTX,DLTY,N)
C----------------------------------------
C     漂流物上の頂点座標と線分の両端座標値の
C     位置補正を行う
C----------------------------------------
      USE M_DRIFT
C
      IMPLICIT NONE
C
      REAL(8),INTENT(IN)::DLTX,DLTY
      INTEGER,INTENT(IN)::N
C
C
      XVERT(1,N) = XVERT(1,N) + DLTX
      XVERT(2,N) = XVERT(2,N) + DLTX
      XVERT(3,N) = XVERT(3,N) + DLTX
      XVERT(4,N) = XVERT(4,N) + DLTX
C
      YVERT(1,N) = YVERT(1,N) + DLTY
      YVERT(2,N) = YVERT(2,N) + DLTY
      YVERT(3,N) = YVERT(3,N) + DLTY
      YVERT(4,N) = YVERT(4,N) + DLTY
C
      XCOLLIS(1,N) = XCOLLIS(1,N) + DLTX
      XCOLLIS(2,N) = XCOLLIS(2,N) + DLTX
      YCOLLIS(1,N) = YCOLLIS(1,N) + DLTY
      YCOLLIS(2,N) = YCOLLIS(2,N) + DLTY
C
      RETURN
      END
