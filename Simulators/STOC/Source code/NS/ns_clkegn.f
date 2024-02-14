      SUBROUTINE CLKEGN(UU,VV,WW,TMU,RHOW,UT,VT,WT,INDU,INDV,INDW,INDP,
     $                  XC,YC,ZC,KF,KP,KG,GS,GT)
C----------------------------------------------------------------------
C     乱流モデルの生成項の計算
C----------------------------------------------------------------------
C
C
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'PROPTY.h'
C
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UT(MX,MY,MZ),VT(MX,MY,MZ),WT(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDW(MX,MY,MZ),INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
      REAL(8),INTENT(INOUT)::GS(MX,MY,MZ),GT(MX,MY,MZ)
C
      REAL(8)::DRHO,ROWM,ROWP,SXX,SXY,SYY,SYZ,SZX,SZZ
      REAL(8)::UTVM,UTVP,UTWM,UTWP,VTUM,VTUP,VTWM,VTWP
      REAL(8)::WTUM,WTUP,WTVM,WTVP
      INTEGER::I,J,K
C
      CALL ZERCLR(GS,MXYZ,0.0D0)
      CALL ZERCLR(GT,MXYZ,0.0D0)
      CALL ZERCLR(UT,MXYZ,0.0D0)
      CALL ZERCLR(VT,MXYZ,0.0D0)
      CALL ZERCLR(WT,MXYZ,0.0D0)
C
      DO 100 K=2,MZM
      DO 110 J=2,MYM
      DO 120 I=2,MXM
        IF(INDP(I,J,K).NE.0) THEN
          UT(I,J,K) = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
          VT(I,J,K) = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
          WT(I,J,K) = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
        END IF
  120 CONTINUE
  110 CONTINUE
  100 CONTINUE
C
      CALL CP_DSR_DC2(MX,MY,MZ,0,1,UT)
      CALL CP_DSR_DC2(MX,MY,MZ,0,1,VT)
      CALL CP_DSR_DC2(MX,MY,MZ,0,1,WT)
C
c080606      DRHO = 1.0D0/RHO
      DRHO = GRAV/RHO
      DO 200 K=2,MZM
      DO 210 J=2,MYM
      DO 220 I=2,MXM
        IF(INDP(I,J,K).NE.0) THEN
          IF(INDV(I,J,K).EQ.1) THEN
            UTVP=YC(8,J)*UT(I,J+1,K)+YC(7,J)*UT(I,J,K)
            WTVP=YC(8,J)*WT(I,J+1,K)+YC(7,J)*WT(I,J,K)
          ELSE
            UTVP=UT(I,J,K)
            WTVP=WT(I,J,K)
          END IF
C
          IF(INDV(I,J-1,K).EQ.1) THEN
            UTVM=YC(8,J-1)*UT(I,J,K)+YC(7,J-1)*UT(I,J-1,K)
            WTVM=YC(8,J-1)*WT(I,J,K)+YC(7,J-1)*WT(I,J-1,K)
          ELSE
            UTVM=UT(I,J,K)
            WTVM=WT(I,J,K)
          END IF
C
          IF(INDW(I,J,K).EQ.1) THEN
            UTWP=ZC(8,K)*UT(I,J,K+1)+ZC(7,K)*UT(I,J,K)
            VTWP=ZC(8,K)*VT(I,J,K+1)+ZC(7,K)*VT(I,J,K  )
          ELSE
            UTWP=UT(I,J,K)
            VTWP=VT(I,J,K)
          END IF
C
          IF(INDW(I,J,K-1).EQ.1) THEN
            UTWM=ZC(8,K-1)*UT(I,J,K  )+ZC(7,K-1)*UT(I,J,K-1)
            VTWM=ZC(8,K-1)*VT(I,J,K  )+ZC(7,K-1)*VT(I,J,K-1)
          ELSE
            UTWM=UT(I,J,K)
            VTWM=VT(I,J,K)
          END IF
C
          IF(INDU(I,J,K).EQ.1) THEN
            VTUP=XC(8,I  ,J)*VT(I+1,J,K)+XC(7,I  ,J)*VT(I  ,J,K)
            WTUP=XC(8,I  ,J)*WT(I+1,J,K)+XC(7,I  ,J)*WT(I  ,J,K)
          ELSE
            VTUP=VT(I,J,K)
            WTUP=WT(I,J,K)
          END IF
C
          IF(INDU(I-1,J,K).EQ.1) THEN
            VTUM=XC(8,I-1,J)*VT(I  ,J,K)+XC(7,I-1,J)*VT(I-1,J,K)
            WTUM=XC(8,I-1,J)*WT(I  ,J,K)+XC(7,I-1,J)*WT(I-1,J,K)
          ELSE
            VTUM=VT(I,J,K)
            WTUM=WT(I,J,K)
          END IF
C
          SXX=XC(6,I,J)*(UU(I,J,K)-UU(I-1,J,K))
          SYY=YC(6,J)*(VV(I,J,K)-VV(I,J-1,K))
          SZZ=ZC(6,K)*(WW(I,J,K)-WW(I,J,K-1))
          SXY=(YC(6,J)*(UTVP-UTVM)+XC(6,I,J)*(VTUP-VTUM))
          SYZ=(ZC(6,K)*(VTWP-VTWM)+YC(6,J)*(WTVP-WTVM))
          SZX=(XC(6,I,J)*(WTUP-WTUM)+ZC(6,K)*(UTWP-UTWM))
C
          GS(I,J,K) = TMU(I,J,K)*(2.0D0*(SXX*SXX+SYY*SYY+SZZ*SZZ)
     $                           +SXY*SXY+SYZ*SYZ+SZX*SZX)
C
          IF(INDW(I,J,K).EQ.1) THEN
            ROWP=ZC(7,K)*RHOW(I,J,K)+ZC(8,K)*RHOW(I,J,K+1)
          ELSE
            ROWP=RHOW(I,J,K)
          END IF
          IF(INDW(I,J,K-1).EQ.1) THEN
            ROWM=ZC(7,K-1)*RHOW(I,J,K-1)+ZC(8,K-1)*RHOW(I,J,K  )
          ELSE
            ROWM=RHOW(I,J,K)
          END IF
C
          GT(I,J,K) = TMU(I,J,K)/SGT*(ROWP-ROWM)*ZC(6,K)*DRHO
        END IF
  220 CONTINUE
  210 CONTINUE
  200 CONTINUE
C
      RETURN
      END
