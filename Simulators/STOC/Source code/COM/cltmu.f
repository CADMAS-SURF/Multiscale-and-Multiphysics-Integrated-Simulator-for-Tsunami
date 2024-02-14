      SUBROUTINE CLTMU(AK,EP,UU,VV,WW,UT,VT,WT,XC,YC,ZC,INDU,INDV,INDW,
     $                 INDP,KF,KP,KG,TMU)
C----------------------------------------------------------------------
C     乱流粘性係数の計算
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TURBR.h'
C
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::AK(MX,MY,MZ),EP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDW(MX,MY,MZ),INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
      REAL(8),INTENT(INOUT)::UT(MX,MY,MZ),VT(MX,MY,MZ),WT(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ)
C
      REAL(8)::CPWR=0.333333333333333D0
C
      REAL(8)::AKMAX,EPMAX,SXX,SXY,SYY,SYZ,SZX,SZZ,TMMAX
      REAL(8)::UTVM,UTVP,UTWM,UTWP,VTUM,VTUP,VTWM,VTWP
      REAL(8)::WTUM,WTUP,WTVM,WTVP
      INTEGER::I,IKMX,J,JKMX,K,KKMX
C
      CALL ZERCLR(TMU,MXYZ,0.0D0)
C
      IF(LTURB.EQ.1) THEN
C ... LESモデル
      CALL ZERCLR(UT,MXYZ,0.0D0)
      CALL ZERCLR(VT,MXYZ,0.0D0)
      CALL ZERCLR(WT,MXYZ,0.0D0)
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF(K.LE.KF(I,J).AND.INDP(I,J,K).GT.0) THEN
            UT(I,J,K) = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
            VT(I,J,K) = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
            WT(I,J,K) = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
         END IF
  100 CONTINUE
C
      DO 200 K=2,MZM
      DO 200 J=2,MYM
      DO 200 I=2,MXM
         IF(K.LE.KF(I,J).AND.INDP(I,J,K).GT.0) THEN
            IF(INDV(I,J,K).EQ.1) THEN
              UTVP=YC(8,J)*UT(I,J+1,K)+YC(7,J)*UT(I,J,K)
              WTVP=YC(8,J)*WT(I,J+1,K)+YC(7,J)*WT(I,J,K)
            ELSE IF(INDV(I,J,K).LE.-2.AND.MDWALV.EQ.1) THEN
              UTVP=0.0D0
              WTVP=0.0D0
            ELSE
              UTVP=UT(I,J,K)
              WTVP=WT(I,J,K)
            END IF
C
            IF(INDV(I,J-1,K).EQ.1) THEN
              UTVM=YC(8,J-1)*UT(I,J,K)+YC(7,J-1)*UT(I,J-1,K)
              WTVM=YC(8,J-1)*WT(I,J,K)+YC(7,J-1)*WT(I,J-1,K)
            ELSE IF(INDV(I,J-1,K).LE.-2.AND.MDWALV.EQ.1) THEN
              UTVM=0.0D0
              WTVM=0.0D0
            ELSE
              UTVM=UT(I,J,K)
              WTVM=WT(I,J,K)
            END IF
C
            IF(INDW(I,J,K).EQ.1) THEN
              UTWP=ZC(8,K)*UT(I,J,K+1)+ZC(7,K)*UT(I,J,K)
              VTWP=ZC(8,K)*VT(I,J,K+1)+ZC(7,K)*VT(I,J,K  )
            ELSE IF(INDW(I,J,K).LE.-2.AND.MOD(MDWALV,2).EQ.1) THEN
              UTWP=0.0D0
              VTWP=0.0D0
            ELSE
              UTWP=UT(I,J,K)
              VTWP=VT(I,J,K)
            END IF
C
            IF(INDW(I,J,K-1).EQ.1) THEN
              UTWM=ZC(8,K-1)*UT(I,J,K  )+ZC(7,K-1)*UT(I,J,K-1)
              VTWM=ZC(8,K-1)*VT(I,J,K  )+ZC(7,K-1)*VT(I,J,K-1)
            ELSE IF(INDW(I,J,K-1).LE.-2.AND.MOD(MDWALV,2).EQ.1) THEN
              UTWM=0.0D0
              VTWM=0.0D0
            ELSE
              UTWM=UT(I,J,K)
              VTWM=VT(I,J,K)
            END IF
C
            IF(INDU(I,J,K).EQ.1) THEN
              VTUP=XC(8,I  ,J)*VT(I+1,J,K)+XC(7,I  ,J)*VT(I  ,J,K)
              WTUP=XC(8,I  ,J)*WT(I+1,J,K)+XC(7,I  ,J)*WT(I  ,J,K)
            ELSE IF(INDU(I,J,K).LE.-2.AND.MDWALV.EQ.1) THEN
              VTUP=0.0D0
              WTUP=0.0D0
            ELSE
              VTUP=VT(I,J,K)
              WTUP=WT(I,J,K)
            END IF
C
            IF(INDU(I-1,J,K).EQ.1) THEN
              VTUM=XC(8,I-1,J)*VT(I  ,J,K)+XC(7,I-1,J)*VT(I-1,J,K)
              WTUM=XC(8,I-1,J)*WT(I  ,J,K)+XC(7,I-1,J)*WT(I-1,J,K)
            ELSE IF(INDU(I-1,J,K).LE.-2.AND.MDWALV.EQ.1) THEN
              VTUM=0.0D0
              WTUM=0.0D0
            ELSE
              VTUM=VT(I,J,K)
              WTUM=WT(I,J,K)
            END IF
C
            SXX=XC(6,I,J)*(UU(I,J,K)-UU(I-1,J,K))
            SYY=YC(6,J)*(VV(I,J,K)-VV(I,J-1,K))
            SZZ=ZC(6,K)*(WW(I,J,K)-WW(I,J,K-1))
            SXY=(YC(6,J)*(UTVP-UTVM)+XC(6,I,J)*(VTUP-VTUM))*0.5D0
            SYZ=(ZC(6,K)*(VTWP-VTWM)+YC(6,J)*(WTVP-WTVM))*0.5D0
            SZX=(XC(6,I,J)*(WTUP-WTUM)+ZC(6,K)*(UTWP-UTWM))*0.5D0
C
            TMU(I,J,K)=(CSMG*(XC(4,I,J)*YC(4,J)*ZC(4,K))**CPWR)**2
     1      *DSQRT(SXX**2+SYY**2+SZZ**2+2.0D0*(SXY**2+SYZ**2+SZX**2))
         END IF
  200 CONTINUE
C
      ELSE IF(LTURB.EQ.2) THEN
C ... K-εモデル
      TMMAX = - 1.0D30
      IKMX = 0
      JKMX = 0
      KKMX = 0
      DO 300 K=2,MZM
      DO 300 J=2,MYM
      DO 300 I=2,MXM
        IF(K.LE.KF(I,J).AND.INDP(I,J,K).GT.0) THEN
          IF(EP(I,J,K).GT.0.0D0) TMU(I,J,K)=CMU*AK(I,J,K)**2/EP(I,J,K)
          IF(TMU(I,J,K).GT.TMMAX) THEN
            IKMX = I
            JKMX = J
            KKMX = K
            AKMAX = AK(I,J,K)
            EPMAX = EP(I,J,K)
            TMMAX = TMU(I,J,K)
          END IF
        END IF
  300 CONTINUE
C
      IF(IKMX.NE.0) THEN
        WRITE(6,600) IKMX,JKMX,KKMX,TMMAX,AKMAX,EPMAX
  600   FORMAT('## TMU-MAX I,J,K   TMU,AK,EP=',3I5,1P,3E13.5)
      END IF
C
      END IF
C
      RETURN
      END
