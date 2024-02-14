      SUBROUTINE CLHUVWR(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GZ,GV0,GX0,GY0,
     $                   GZ0,XC,YC,ZC,YCOSP,HH,HDEP,
     $                   INDU,INDV,INDW,LLWALB,KF,KG)
C======================================================================
C     UU,VV値をMODIFY設定する
C
C     ISEAWL: =0 防潮堤部分の線流量計算で防潮堤を無視する
C           : =1 防潮堤部分の線流量計算で防潮堤を考慮する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'CADMAS.h'
      INCLUDE 'VVMAX.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(IN)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(IN)::FF(MX,MY,MZ)
      REAL(8),INTENT(IN)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(IN)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(IN)::GV0(MX,MY,MZ),GX0(MX,MY,MZ)
      REAL(8),INTENT(IN)::GY0(MX,MY,MZ),GZ0(MX,MY,MZ)
C
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOSP(MY)
      REAL(8),INTENT(IN)::HH(MX,MY),HDEP(MX,MY)
      INTEGER,INTENT(IN)::INDU(MX,MY,MZ),INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(IN)::LLWALB(3,MLWALB),KF(MX,MY),KG(MX,MY)
C
      INTEGER::I,J,K,N,KG1
      REAL(8)::FFI,FFJ,FF0,FF1,FF2,FFA,FFB,GV1,GX1,GY1,EPS0
      REAL(8),PARAMETER::EPSV=1.0D-2
C
C
      DO 200 K=2,MZM
      DO 200 J=2,MYM
      DO 200 I=1,MXM
C ...... 計算点及び流入出境界
         IF( INDU(I,J,K).GE.-1 ) THEN
            GX1 = 1.0D0-GX0(I,J,K)
            FFI = FF(I,J,K)*XC(7,I,J)+FF(I+1,J,K)*XC(8,I,J)
            FF0 = MAX(FFI        -GX1,0.0D0)
            FF1 = MAX(FF(I  ,J,K)-GX1,0.0D0)
            FF2 = MAX(FF(I+1,J,K)-GX1,0.0D0)
C
            EPS0=EPSV*ZC(6,K)
            IF(UU(I,J,K).GE.0.0D0) THEN
               UU(I,J,K)=HU(I,J,K)*GX0(I,J,K)/GX(I,J,K)
     $                  /MAX(PARAMF2*FF0+PARAMF*FF1,EPS0)
            ELSE
               UU(I,J,K)=HU(I,J,K)*GX0(I,J,K)/GX(I,J,K)
     $                  /MAX(PARAMF2*FF0+PARAMF*FF2,EPS0)
            ENDIF
c            IF(ABS(UU(I,J,K)).GT.VVMAX) UU(I,J,K)=SIGN(VVMAX,UU(I,J,K))
         END IF
  200 CONTINUE
C
C ... 防潮堤用の処理(X方向)
!CDIR NODEP
      DO 250 N=1,MLWALBX
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
C
         GX1 = 1.0D0-GX0(I,J,K)
         KG1=MAX(KG(I,J),KG(I+1,J))
         IF( ISEAWL.EQ.0 .AND. K.EQ.KG1 ) THEN
            GV1 = 1.0D0-GV0(I,J,K)*XC(7,I,J)-GV0(I+1,J,K)*XC(8,I,J)
            GX1 = MIN(GV1,GX1)
         END IF
C
         FFA = MAX(FF(I  ,J,K),GX1)
         FFB = MAX(FF(I+1,J,K),GX1)
         FF0 = FFA*XC(7,I,J)+FFB*XC(8,I,J)-GX1
         FF1 = FFA-GX1
         FF2 = FFB-GX1
C
         IF( ISEAWL.EQ.0 .AND. K.GT.KG1 ) THEN
            FF0=GX0(I,J,K)*(FF(I,J,K)*XC(7,I,J)+FF(I+1,J,K)*XC(8,I,J))
            FF1=GX0(I,J,K)*FF(I,J,K)
            FF2=GX0(I,J,K)*FF(I+1,J,K)
         ENDIF
C
         EPS0=EPSV*ZC(6,K)
         IF(UU(I,J,K).GE.0.0D0) THEN
            UU(I,J,K)=HU(I,J,K)*GX0(I,J,K)/GX(I,J,K)
     $               /MAX(PARAMF2*FF0+PARAMF*FF1,EPS0)
         ELSE
            UU(I,J,K)=HU(I,J,K)*GX0(I,J,K)/GX(I,J,K)
     $               /MAX(PARAMF2*FF0+PARAMF*FF2,EPS0)
         ENDIF
c         IF(ABS(UU(I,J,K)).GT.VVMAX) UU(I,J,K)=SIGN(VVMAX,UU(I,J,K))
  250 CONTINUE
C
C
      DO 300 K=2,MZM
      DO 300 J=1,MYM
      DO 300 I=2,MXM
C ...... 計算点及び流入出境界
         IF( INDV(I,J,K).GE.-1 ) THEN
            GY1 = 1.0D0-GY0(I,J,K)
            FFJ = FF(I,J,K)*YC(7,J)+FF(I,J+1,K)*YC(8,J)
            FF0 = MAX(FFJ        -GY1,0.0D0)
            FF1 = MAX(FF(I,J  ,K)-GY1,0.0D0)
            FF2 = MAX(FF(I,J+1,K)-GY1,0.0D0)
C
            EPS0=EPSV*ZC(6,K)
            IF(VV(I,J,K).GE.0.0D0) THEN
               VV(I,J,K)=HV(I,J,K)*GY0(I,J,K)/GY(I,J,K)/YCOSP(J)
     $                  /MAX(PARAMF2*FF0+PARAMF*FF1,EPS0)
            ELSE
               VV(I,J,K)=HV(I,J,K)*GY0(I,J,K)/GY(I,J,K)/YCOSP(J)
     $                  /MAX(PARAMF2*FF0+PARAMF*FF2,EPS0)
            ENDIF
c            IF(ABS(VV(I,J,K)).GT.VVMAX) VV(I,J,K)=SIGN(VVMAX,VV(I,J,K))
         END IF
  300 CONTINUE
C
C ... 防潮堤用の処理(Y方向)
!CDIR NODEP
      DO 350 N=MLWALBX+1,MLWALB
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
C
         GY1 = 1.0D0-GY0(I,J,K)
         KG1=MAX(KG(I,J),KG(I,J+1))
         IF( ISEAWL.EQ.0 .AND. K.EQ.KG1 ) THEN
            GV1 = 1.0D0-GV0(I,J,K)*YC(7,J)-GV0(I,J+1,K)*YC(8,J)
            GY1 = MIN(GV1,GY1)
         END IF
C
         FFA = MAX(FF(I,J  ,K),GY1)
         FFB = MAX(FF(I,J+1,K),GY1)
         FF0 = FFA*YC(7,J)+FFB*YC(8,J)-GY1
         FF1 = FFA-GY1
         FF2 = FFB-GY1
C
         IF( ISEAWL.EQ.0 .AND. K.GT.KG1 ) THEN
            FF0=GY0(I,J,K)*(FF(I,J,K)*YC(7,J)+FF(I,J+1,K)*YC(8,J))
            FF1=GY0(I,J,K)*FF(I,J,K)
            FF2=GY0(I,J,K)*FF(I,J+1,K)
         ENDIF
C
         EPS0=EPSV*ZC(6,K)
         IF(VV(I,J,K).GE.0.0D0) THEN
            VV(I,J,K)=HV(I,J,K)*GY0(I,J,K)/GY(I,J,K)/YCOSP(J)
     $               /MAX(PARAMF2*FF0+PARAMF*FF1,EPS0)
         ELSE
            VV(I,J,K)=HV(I,J,K)*GY0(I,J,K)/GY(I,J,K)/YCOSP(J)
     $               /MAX(PARAMF2*FF0+PARAMF*FF2,EPS0)
         ENDIF
c         IF(ABS(VV(I,J,K)).GT.VVMAX) VV(I,J,K)=SIGN(VVMAX,VV(I,J,K))
  350 CONTINUE
C
C ... UU,VV (for DOMAIN-DECOMP)
      CALL CP_DSR_DC2(MX,MY,MZ,1,1,UU)
      CALL CP_DSR_DC2(MX,MY,MZ,2,1,VV)
C
      RETURN
      END
