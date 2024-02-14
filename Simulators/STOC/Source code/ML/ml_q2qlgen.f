      SUBROUTINE Q2QLGEN(Q2,QL,RQ,RL,UU,VV,GV,RHOW,DKM,DKH,DKQL,GSGT,
     $                   Q3DB,HH,WX,WY,CD,Q2SRF,QLSRF,ZC,INDP,KF,KG)
C======================================================================
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'MYCNST.h'
C
      REAL(8),INTENT(INOUT)::Q2(MX,MY,MZ),QL(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::RQ(MX,MY,MZ),RL(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::DKM(MX,MY,MZ),DKH(MX,MY,MZ),DKQL(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GSGT(MX,MY,MZ),Q3DB(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),WX(MX,MY),WY(MX,MY),CD(MX,MY)
      REAL(8),INTENT(INOUT)::Q2SRF(MX,MY),QLSRF(MX,MY)
      REAL(8),INTENT(INOUT)::ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),KF(MX,MY),KG(MX,MY)
C
      REAL(8)::SQSL=0.2D0,CDMIN=1.0D-4,GAMMA=0.2D0
      REAL(8)::DUDZIK,DUDZMK,DUDZIM,DUDZMM,DUDZ
      REAL(8)::DVDZJK,DVDZMK,DVDZJM,DVDZMM,DVDZ
      REAL(8)::RHOWK,RHOWM,DRDZ
      REAL(8)::DRHO,DB1,BB1,TAU,CDD,AL0,AL1,AL2,ALB,ZZ0,ZZ1,ZZ2
      INTEGER::IDB=0
      INTEGER::I,J,K
C
      CALL ZERCLR(DKQL,MXYZ,0.0D0)
      CALL ZERCLR(GSGT,MXYZ,0.0D0)
      CALL ZERCLR(Q3DB,MXYZ,0.0D0)
      CALL ZERCLR(RQ  ,MXYZ,0.0D0)
      CALL ZERCLR(RL  ,MXYZ,0.0D0)
C
      DO 100 K=1,MZ
      DO 100 J=1,MY
      DO 100 I=1,MX
        IF(INDP(I,J,K).GT.0) THEN
          IF(K.LE.KF(I,J)) THEN
            RL(I,J,K) = 0.5D0*QL(I,J,K)/Q2(I,J,K)
            RQ(I,J,K) = SQRT(2.0D0*Q2(I,J,K))
          END IF
        END IF
 100  CONTINUE
C
      DRHO = 1.0D0/RHO
      DB1  = 1.0D0/RB1
      DO 200 K=1,MZ
      DO 200 J=1,MY
      DO 200 I=1,MX
        IF(INDP(I,J,K).GT.0) THEN
          IF(K.LE.KF(I,J)) THEN
            DUDZIK = (UU(I  ,J,K+1)-UU(I  ,J,K))*ZC(3,K  )    
            DUDZMK = (UU(I-1,J,K+1)-UU(I-1,J,K))*ZC(3,K  )    
            DUDZIM = (UU(I  ,J,K)-UU(I  ,J,K-1))*ZC(3,K-1)    
            DUDZMM = (UU(I-1,J,K)-UU(I-1,J,K-1))*ZC(3,K-1)
            DUDZ = DUDZIK**2+DUDZMK**2+DUDZIM**2+DUDZMM**2    
            DVDZJK = (VV(I,J  ,K+1)-VV(I,J  ,K))*ZC(3,K  )    
            DVDZMK = (VV(I,J-1,K+1)-VV(I,J-1,K))*ZC(3,K  )    
            DVDZJM = (VV(I,J  ,K)-VV(I,J  ,K-1))*ZC(3,K-1)    
            DVDZMM = (VV(I,J-1,K)-VV(I,J-1,K-1))*ZC(3,K-1)
            DVDZ = DVDZJK**2+DVDZMK**2+DVDZJM**2+DVDZMM**2
            RHOWK = RHOW(I,J,K  )*ZC(7,K  )+RHOW(I,J,K)*ZC(8,K-1)    
            IF(K.EQ.KF(I,J)) RHOWK=RHOW(I,J,K)
            RHOWM = RHOW(I,J,K-1)*ZC(7,K-1)+RHOW(I,J,K)*ZC(8,K-1)
            IF(K.EQ.KG(I,J)) RHOWM=RHOW(I,J,K)
            DRDZ = (RHOWK-RHOWM)*ZC(4,K)
            GSGT(I,J,K) = DKM(I,J,K)*0.25D0*(DUDZ+DVDZ)
     $                  - DRHO*GRAV*DKH(I,J,K)*DRDZ    
            Q3DB(I,J,K) = DB1*RQ(I,J,K)**3/RL(I,J,K)
            DKQL(I,J,K)=RL(I,J,K)*RQ(I,J,K)*SQSL
          END IF
        END IF
 200  CONTINUE
C
      BB1 = RB1**(2.0D0/3.0D0)
      DO 300 J=1,MY
      DO 300 I=1,MX
        IF(KF(I,J).LT.MZ) THEN
          CDD = CD(I,J)
          IF(WX(I,J)**2+WY(I,J)**2.NE.0.0D0.AND.CDD.EQ.0.0D0) THEN
            IF(CD(I,J-1).NE.0.0D0) CDD=CD(I,J-1)
            IF(CD(I-1,J).NE.0.0D0) CDD=CD(I-1,J) 
            IF(CD(I+1,J).NE.0.0D0) CDD=CD(I+1,J)
            IF(CD(I,J+1).NE.0.0D0) CDD=CD(I,J+1)
            IF(CDD.EQ.0.0D0) CDD=CDMIN
          END IF
          TAU = CDD*ADRHO*(WX(I,J)**2+WY(I,J)**2)
          Q2SRF(I,J) = BB1*TAU*0.5D0
C
          AL1 = 0.0D0
          AL2 = 0.0D0
          DO 310 K=KG(I,J),KF(I,J)
            ZZ1 = ZC(1,K-1)
            IF(K.EQ.KG(I,J)) ZZ1=ZC(1,K)-GV(I,J,K)*ZC(4,K)
            ZZ2 = ZC(1,K)
            IF(K.EQ.KF(I,J)) ZZ2=HH(I,J)
            AL1 = AL1+RQ(I,J,K)*(ZZ1**2-ZZ2**2)
            AL2 = AL2+RQ(I,J,K)*(ZZ2-ZZ1) 
  310     CONTINUE
          AL0 = GAMMA*AL1/AL2
          ZZ0 = HH(I,J)-ZC(1,KF(I,J)-1)
          ALB = AL0*RKAR*ZZ0/(AL0+ZZ0)
          QLSRF(I,J) = 2.0D0*Q2SRF(I,J)*ALB
        END IF
  300 CONTINUE    
C
      RETURN
      END
