      SUBROUTINE DRAG1(FDX1,FDY1,MDZ1,COST,SINT,
     $                 UFSM,UFSN,UFSB,UFPS,VFSM,VFSN,VFSB,VFPS,
     $                 DH,K,N)
C----------------------------------------
C     抗力計算を行う(その1)
C----------------------------------------
      USE M_TIME
      USE M_GRID
      USE M_DRIFT
      USE M_FLUID
      USE M_MODEL,ONLY:CDF1,CDF2
C
      IMPLICIT NONE
C
      REAL(8),INTENT(OUT)::FDX1,FDY1,MDZ1
C
      REAL(8),INTENT(IN)::COST,SINT
C
      REAL(8),INTENT(IN)::UFSM(MXNK,MXSIDE),UFSN(MXNK,MXSIDE)
      REAL(8),INTENT(IN)::UFSB(MXNK,MXSIDE),UFPS(MXNK,MXSIDE)
      REAL(8),INTENT(IN)::VFSM(MXNK,MXSIDE),VFSN(MXNK,MXSIDE)
      REAL(8),INTENT(IN)::VFSB(MXNK,MXSIDE),VFPS(MXNK,MXSIDE)
C
      REAL(8),INTENT(IN)::DH
      INTEGER,INTENT(IN)::K,N
C
C
      REAL(8)::UUD1,VVD1
      REAL(8)::UUFSM,UUFSN,VVFSB,VVFPS
      REAL(8)::URSM1,URSN1,VRSB1,VRPS1
      REAL(8)::WK1,WK2
      REAL(8)::CDX1SM,CDX1SN,CDY1PS,CDY1SB
      REAL(8)::CDX1SMUU,CDX1SNUU,CDY1PSVV,CDY1SBVV
      REAL(8)::CDX1SMUUY,CDX1SNUUY,CDY1PSVVX,CDY1SBVVX
      REAL(8)::DYY,DYY1,YY1,DXX,DXX1,XX1
      INTEGER::M
C
C
      UUD1 = UD(N)*COST + VD(N)*SINT
      VVD1 =-UD(N)*SINT + VD(N)*COST
C
C----------------------------------------
C     (1) FDX1の計算
C----------------------------------------
      CDX1SMUU = 0.0D0
      CDX1SNUU = 0.0D0
      CDX1SMUUY = 0.0D0                ! この2つはMDZ1用
      CDX1SNUUY = 0.0D0
      DYY = AB(N)/DBLE(NDDIV(2,N)-1)   ! 側面分割時の一つ分の長さ
C
      DO M=1,NDDIV(2,N)
         DYY1 = DYY
         IF( M.EQ.1.OR.M.EQ.NDDIV(2,N) ) DYY1 = 0.5D0*DYY   ! 両端は半分の長さ
         YY1  = -0.5D0*AB(N) + DBLE(M-1)*DYY
C
         UUFSM = UFSM(K,M)*COST + VFSM(K,M)*SINT
         UUFSN = UFSN(K,M)*COST + VFSN(K,M)*SINT
C
         URSM1 = UUFSM - UUD1 + OD(N)*YY1
         URSN1 = UUFSN - UUD1 + OD(N)*YY1
C
         IF( URSM1.LT.0.0D0 ) THEN
            CDX1SM = CDF1
         ELSE
            CDX1SM = CDF2
         ENDIF
C
         IF( URSN1.LT.0.0D0 ) THEN
            CDX1SN = CDF2
         ELSE
            CDX1SN = CDF1
         ENDIF
C
         WK1 = CDX1SM*URSM1*ABS(URSM1)*DYY1
         WK2 = CDX1SN*URSN1*ABS(URSN1)*DYY1
         CDX1SMUU = CDX1SMUU + WK1
         CDX1SNUU = CDX1SNUU + WK2
         CDX1SMUUY = CDX1SMUUY + WK1*YY1                     ! この2つはMDZ1用
         CDX1SNUUY = CDX1SNUUY + WK2*YY1
      ENDDO
C
      FDX1 = 0.5D0*RHO*DH*(CDX1SMUU+CDX1SNUU)
C
C----------------------------------------
C     (2) FDY1の計算
C----------------------------------------
      CDY1PSVV = 0.0D0
      CDY1SBVV = 0.0D0
      CDY1PSVVX = 0.0D0                ! この2つはMDZ1用
      CDY1SBVVX = 0.0D0
      DXX = AL(N)/DBLE(NDDIV(1,N)-1)   ! 側面分割時の一つ分の長さ
C
      DO M=1,NDDIV(1,N)
         DXX1 = DXX
         IF( M.EQ.1.OR.M.EQ.NDDIV(1,N) ) DXX1 = 0.5D0*DXX   ! 両端は半分の長さ
         XX1  = -0.5D0*AL(N) + DBLE(M-1)*DXX
C
         VVFPS =-UFPS(K,M)*SINT + VFPS(K,M)*COST
         VVFSB =-UFSB(K,M)*SINT + VFSB(K,M)*COST
C
         VRPS1 = VVFPS - VVD1 - OD(N)*XX1
         VRSB1 = VVFSB - VVD1 - OD(N)*XX1
C
         IF( VRPS1.LT.0.0D0 ) THEN
            CDY1PS = CDF1
         ELSE
            CDY1PS = CDF2
         ENDIF
C
         IF( VRSB1.LT.0.0D0 ) THEN
            CDY1SB = CDF2
         ELSE
            CDY1SB = CDF1
         ENDIF
C
         WK1 = CDY1PS*VRPS1*ABS(VRPS1)*DXX1
         WK2 = CDY1SB*VRSB1*ABS(VRSB1)*DXX1
         CDY1PSVV = CDY1PSVV + WK1
         CDY1SBVV = CDY1SBVV + WK2
         CDY1PSVVX = CDY1PSVVX + WK1*XX1                     ! この2つはMDZ1用
         CDY1SBVVX = CDY1SBVVX + WK2*XX1
      ENDDO
C
      FDY1 = 0.5D0*RHO*DH*(CDY1PSVV+CDY1SBVV)
C
C
C----------------------------------------
C     (3) MDZ1の計算
C----------------------------------------
      MDZ1 = 0.5D0*RHO*DH*(-CDX1SMUUY-CDX1SNUUY+CDY1PSVVX+CDY1SBVVX)
C
      RETURN
      END
