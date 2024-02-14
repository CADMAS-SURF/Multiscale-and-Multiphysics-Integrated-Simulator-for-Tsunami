      SUBROUTINE KOJI3D(STRESS,PSTRES,CMX,ITO)
C
C     STRESS : IN  : 応力
C                    STRESS(1) = σx
C                    STRESS(2) = σy
C                    STRESS(3) = σz
C                    STRESS(4) = τxy
C                    STRESS(5) = τyz
C                    STRESS(6) = τzx
C     PSTRES : OUT : 主応力
C                    PSTRES(1) = σ1
C                    PSTRES(2) = σ2
C                    PSTRES(3) = σ3
C     CMX    : OUT : 主軸
C                    CMX(:,1) = σ1の方向余弦
C                    CMX(:,2) = σ2の方向余弦
C                    CMX(:,3) = σ3の方向余弦
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      DIMENSION STRESS(6),PSTRES(3),CMX(3,3),STRMTX(3,3)
C
      SS=0.0D0
      DO 2000 I=1,6
 2000 SS=SS+STRESS(I)*STRESS(I)
      IF(SS.GT.1.0D-10)GO TO 4000
      DO 3100 I=1,3
      DO 3050 J=1,3
 3050 CMX(I,J)=0.0D0
      PSTRES(I)=0.0D0
 3100 CMX(I,I)=1.0D0
      GO TO 9000
C
 4000 STRMTX(1,1)=STRESS(1)
      STRMTX(1,2)=STRESS(4)
      STRMTX(1,3)=STRESS(6)
      STRMTX(2,1)=STRMTX(1,2)
      STRMTX(2,2)=STRESS(2)
      STRMTX(2,3)=STRESS(5)
      STRMTX(3,1)=STRMTX(1,3)
      STRMTX(3,2)=STRMTX(2,3)
      STRMTX(3,3)=STRESS(3)
C
      FACT=0.001
      CALL JACOBI(STRMTX,PSTRES,CMX,3,15,FACT ,0,3,K,ITO)
C
 9000 RETURN
      END
