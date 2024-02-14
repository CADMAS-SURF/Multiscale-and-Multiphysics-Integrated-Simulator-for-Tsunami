      SUBROUTINE DRAG2(FDX2,FDY2,MDZ2,COST,SINT,UFG,VFG,DH,K,N)
C----------------------------------------
C     抗力計算を行う(その2)
C----------------------------------------
      USE M_TIME
      USE M_GRID
      USE M_DRIFT
      USE M_FLUID
C
      IMPLICIT NONE
C
      REAL(8),INTENT(OUT)::FDX2,FDY2,MDZ2
C
      REAL(8),INTENT(IN)::COST,SINT
C
      REAL(8),INTENT(IN)::UFG(MXNK),VFG(MXNK)
      REAL(8),INTENT(IN)::DH
      INTEGER,INTENT(IN)::K,N
C
      REAL(8)::UG1,VG1,VELG1
      REAL(8)::COS1,SIN1,CDX2,CDY2,ML
C
C
      UG1 = (UFG(K)-UD(N))*COST + (VFG(K)-VD(N))*SINT
      VG1 =-(UFG(K)-UD(N))*SINT + (VFG(K)-VD(N))*COST
      VELG1 = SQRT(UG1**2+VG1**2)
C
      IF( VELG1.LE.1.0D-20 ) THEN   ! 相対速度がほぼ0のときは抗力0とする
         FDX2 = 0.0D0
         FDY2 = 0.0D0
         MDZ2 = 0.0D0
         RETURN
      ENDIF
C
C----------------------------------------
C     (1) 船首方向と流体速度方向の成す角を計算
C----------------------------------------
      COS1 = UG1/VELG1
      SIN1 = VG1/VELG1
C
C----------------------------------------
C     (2) FDX2の計算
C----------------------------------------
      CDX2 = 2.0D0*(COS1**2 + 1.2D0*ABS(SIN1*COS1))
      IF(UG1.EQ.0.0D0) THEN
         FDX2 = 0.0D0
      ELSE
         FDX2 = 0.5D0*RHO*CDX2*VELG1**2*SIGN(1.0D0,UG1)*AB(N)*DH
      ENDIF
C
C----------------------------------------
C     (3) FDY2の計算
C----------------------------------------
      CDY2 = 2.0D0*(SIN1**2 + 2.2D0*ABS(SIN1*COS1))
      IF(VG1.EQ.0.0D0) THEN
         FDY2 = 0.0D0
      ELSE
         FDY2 = 0.5D0*RHO*CDY2*VELG1**2*SIGN(1.0D0,VG1)*AL(N)*DH
      ENDIF
C
C----------------------------------------
C     (4) MDZ2の計算
C----------------------------------------
      ML = SIGN(1.0D0,COS1*SIN1)
     $   * 0.09D0*AL(N)*ABS(2.0D0*COS1*SIN1)**1.2D0
      MDZ2 = ML * SQRT(FDX2**2 + FDY2**2)
C
      RETURN
      END
