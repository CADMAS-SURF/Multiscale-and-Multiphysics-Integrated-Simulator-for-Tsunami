      SUBROUTINE VMASS(FX2F,FY2F,MZ2F,COST,SINT,
     $                 UFSM,UFSN,UFSB,UFPS,VFSM,VFSN,VFSB,VFPS,
     $                 UFSM1,UFSN1,UFSB1,UFPS1,VFSM1,VFSN1,VFSB1,VFPS1,
     $                 DH,K,N)
C----------------------------------------
C     付加質量の計算を行う
C     (ただし、漂流物の重心速度に関する項は含めない)
C----------------------------------------
      USE M_TIME
      USE M_GRID
      USE M_DRIFT
      USE M_FLUID
      USE M_MODEL
C
      IMPLICIT NONE
C
      REAL(8),INTENT(OUT)::FX2F,FY2F,MZ2F
C
      REAL(8),INTENT(IN)::COST,SINT
C
      REAL(8),INTENT(IN)::UFSM(MXNK,MXSIDE),UFSN(MXNK,MXSIDE)
      REAL(8),INTENT(IN)::UFSB(MXNK,MXSIDE),UFPS(MXNK,MXSIDE)
      REAL(8),INTENT(IN)::VFSM(MXNK,MXSIDE),VFSN(MXNK,MXSIDE)
      REAL(8),INTENT(IN)::VFSB(MXNK,MXSIDE),VFPS(MXNK,MXSIDE)
C
      REAL(8),INTENT(IN)::UFSM1(MXNK,MXSIDE),UFSN1(MXNK,MXSIDE)
      REAL(8),INTENT(IN)::UFSB1(MXNK,MXSIDE),UFPS1(MXNK,MXSIDE)
      REAL(8),INTENT(IN)::VFSM1(MXNK,MXSIDE),VFSN1(MXNK,MXSIDE)
      REAL(8),INTENT(IN)::VFSB1(MXNK,MXSIDE),VFPS1(MXNK,MXSIDE)
C
      REAL(8),INTENT(IN)::DH
      INTEGER,INTENT(IN)::K,N
C
C
      REAL(8)::WK1,WK2
      REAL(8)::DUFSM,DUFSN,DVFSB,DVFPS
      REAL(8)::DUFSMY,DUFSNY,DVFSBX,DVFPSX
      REAL(8)::DYY,DYY1,YY1,DXX,DXX1,XX1
      INTEGER::M
C
C
C----------------------------------------
C     (1) FX2Fの計算
C----------------------------------------
      DUFSM = 0.0D0
      DUFSN = 0.0D0
      DUFSMY = 0.0D0                   ! この2つはMZ2F用
      DUFSNY = 0.0D0
      DYY = AB(N)/DBLE(NDDIV(2,N)-1)   ! 側面分割時の一つ分の長さ
C
      DO M=1,NDDIV(2,N)
         DYY1 = DYY
         IF( M.EQ.1.OR.M.EQ.NDDIV(2,N) ) DYY1 = 0.5D0*DYY    ! 両端は半分の長さ
         YY1  = -0.5D0*AB(N) + DBLE(M-1)*DYY
C
         WK1 = ( (UFSM1(K,M)-UFSM(K,M))*COST
     $         + (VFSM1(K,M)-VFSM(K,M))*SINT )/DT*DYY1
         WK2 = ( (UFSN1(K,M)-UFSN(K,M))*COST
     $         + (VFSN1(K,M)-VFSN(K,M))*SINT )/DT*DYY1
         DUFSM = DUFSM + WK1
         DUFSN = DUFSN + WK2
         DUFSMY = DUFSMY + WK1*YY1                           ! この2つはMZ2F用
         DUFSNY = DUFSNY + WK2*YY1
      ENDDO
C
      FX2F = 0.5D0*RHO*CM*DH*AL(N)*(DUFSM+DUFSN)
C
C
C----------------------------------------
C     (2) FY1の計算
C----------------------------------------
      DVFPS = 0.0D0
      DVFSB = 0.0D0
      DVFPSX = 0.0D0                   ! この2つはMZ2F用
      DVFSBX = 0.0D0
      DXX = AL(N)/DBLE(NDDIV(1,N)-1)   ! 側面分割時の一つ分の長さ
C
      DO M=1,NDDIV(1,N)
         DXX1 = DXX
         IF( M.EQ.1.OR.M.EQ.NDDIV(1,N) ) DXX1 = 0.5D0*DXX    ! 両端は半分の長さ
         XX1  = -0.5D0*AL(N) + DBLE(M-1)*DXX
C
         WK1 = (-(UFPS1(K,M)-UFPS(K,M))*SINT
     $         + (VFPS1(K,M)-VFPS(K,M))*COST )/DT*DXX1
         WK2 = (-(UFSB1(K,M)-UFSB(K,M))*SINT
     $         + (VFSB1(K,M)-VFSB(K,M))*COST )/DT*DXX1
         DVFPS = DVFPS + WK1
         DVFSB = DVFSB + WK2
         DVFPSX = DVFPSX + WK1*XX1                           ! この2つはMZ2F用
         DVFSBX = DVFSBX + WK2*XX1
      ENDDO
C
      FY2F = 0.5D0*RHO*CM*DH*AB(N)*(DVFPS+DVFSB)
C
C
C----------------------------------------
C     (3) MZ2Fの計算
C----------------------------------------
      MZ2F =-0.5D0*RHO*CM*DH*AL(N)*(DUFSMY+DUFSNY)
     $      +0.5D0*RHO*CM*DH*AB(N)*(DVFPSX+DVFSBX)
C
      RETURN
      END
