      SUBROUTINE DRAGWIN(FWX,FWY,MWZ,COST,SINT,
     $                   WXSM,WXSN,WXSB,WXPS,WYSM,WYSN,WYSB,WYPS,
     $                   DH,N)
C----------------------------------------
C     風応力計算を行う
C----------------------------------------
      USE M_DRIFT,ONLY:UD,VD,OD,AL,AB,NDDIV,MXSIDE
      USE M_WIND,ONLY:RHOA
      USE M_MODEL,ONLY:CDW1,CDW2
C
      IMPLICIT NONE
C
      REAL(8),INTENT(OUT)::FWX,FWY,MWZ
C
      REAL(8),INTENT(IN)::COST,SINT
C
      REAL(8),INTENT(IN)::WXSM(MXSIDE),WXSN(MXSIDE)
      REAL(8),INTENT(IN)::WXSB(MXSIDE),WXPS(MXSIDE)
      REAL(8),INTENT(IN)::WYSM(MXSIDE),WYSN(MXSIDE)
      REAL(8),INTENT(IN)::WYSB(MXSIDE),WYPS(MXSIDE)
C
      REAL(8),INTENT(IN)::DH
      INTEGER,INTENT(IN)::N
C
C
      REAL(8)::UUD1,VVD1
      REAL(8)::WWXSM,WWXSN,WWYSB,WWYPS
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
C     (1) FWXの計算
C----------------------------------------
      CDX1SMUU = 0.0D0
      CDX1SNUU = 0.0D0
      CDX1SMUUY = 0.0D0                ! この2つはMWZ用
      CDX1SNUUY = 0.0D0
      DYY = AB(N)/DBLE(NDDIV(2,N)-1)   ! 側面分割時の一つ分の長さ
C
      DO M=1,NDDIV(2,N)
         DYY1 = DYY
         IF( M.EQ.1.OR.M.EQ.NDDIV(2,N) ) DYY1 = 0.5D0*DYY   ! 両端は半分の長さ
         YY1  = -0.5D0*AB(N) + DBLE(M-1)*DYY
C
         WWXSM = WXSM(M)*COST + WYSM(M)*SINT
         WWXSN = WXSN(M)*COST + WYSN(M)*SINT
C
         URSM1 = WWXSM - UUD1 + OD(N)*YY1
         URSN1 = WWXSN - UUD1 + OD(N)*YY1
C
         IF( URSM1.LT.0.0D0 ) THEN
            CDX1SM = CDW1
         ELSE
            CDX1SM = CDW2
         ENDIF
C
         IF( URSN1.LT.0.0D0 ) THEN
            CDX1SN = CDW2
         ELSE
            CDX1SN = CDW1
         ENDIF
C
         WK1 = CDX1SM*URSM1*ABS(URSM1)*DYY1
         WK2 = CDX1SN*URSN1*ABS(URSN1)*DYY1
         CDX1SMUU = CDX1SMUU + WK1
         CDX1SNUU = CDX1SNUU + WK2
         CDX1SMUUY = CDX1SMUUY + WK1*YY1                     ! この2つはMWZ用
         CDX1SNUUY = CDX1SNUUY + WK2*YY1
      ENDDO
C
      FWX = 0.5D0*RHOA*DH*(CDX1SMUU+CDX1SNUU)
C
C----------------------------------------
C     (2) FWYの計算
C----------------------------------------
      CDY1PSVV = 0.0D0
      CDY1SBVV = 0.0D0
      CDY1PSVVX = 0.0D0                ! この2つはMWZ用
      CDY1SBVVX = 0.0D0
      DXX = AL(N)/DBLE(NDDIV(1,N)-1)   ! 側面分割時の一つ分の長さ
C
      DO M=1,NDDIV(1,N)
         DXX1 = DXX
         IF( M.EQ.1.OR.M.EQ.NDDIV(1,N) ) DXX1 = 0.5D0*DXX   ! 両端は半分の長さ
         XX1  = -0.5D0*AL(N) + DBLE(M-1)*DXX
C
         WWYPS =-WXPS(M)*SINT + WYPS(M)*COST
         WWYSB =-WXSB(M)*SINT + WYSB(M)*COST
C
         VRPS1 = WWYPS - VVD1 - OD(N)*XX1
         VRSB1 = WWYSB - VVD1 - OD(N)*XX1
C
         IF( VRPS1.LT.0.0D0 ) THEN
            CDY1PS = CDW1
         ELSE
            CDY1PS = CDW2
         ENDIF
C
         IF( VRSB1.LT.0.0D0 ) THEN
            CDY1SB = CDW2
         ELSE
            CDY1SB = CDW1
         ENDIF
C
         WK1 = CDY1PS*VRPS1*ABS(VRPS1)*DXX1
         WK2 = CDY1SB*VRSB1*ABS(VRSB1)*DXX1
         CDY1PSVV = CDY1PSVV + WK1
         CDY1SBVV = CDY1SBVV + WK2
         CDY1PSVVX = CDY1PSVVX + WK1*XX1                     ! この2つはMWZ用
         CDY1SBVVX = CDY1SBVVX + WK2*XX1
      ENDDO
C
      FWY = 0.5D0*RHOA*DH*(CDY1PSVV+CDY1SBVV)
C
C
C----------------------------------------
C     (3) MWZの計算
C----------------------------------------
      MWZ = 0.5D0*RHOA*DH*(-CDX1SMUUY-CDX1SNUUY+CDY1PSVVX+CDY1SBVVX)
C
      RETURN
      END
