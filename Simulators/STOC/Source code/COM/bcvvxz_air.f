      SUBROUTINE BCVVXZ_AIR(VVX,VVZ,UUA,VVA,WWA,FFA,TMUA,VVBCAIR,
     $                      GVA,XCP,ZCA,INDPA,INDUA,INDWA)
C======================================================================
C     壁面、流速固定境界および自由流出境界の接線方向流速を設定する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(OUT)::VVX(MX,MY,MZA),VVZ(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::VVBCAIR(NXY,MZA,4)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XCP(8,MX,MY),ZCA(8,MZA)
C
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA),INDWA(MX,MY,MZA)
C
      REAL(8)::DD1,ENU,UU1,VEL1,VTAU,VV1,WW1,RNU
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,INB1
C
C
      CALL ZERCLR(VVX,MXY*MZA,0.0D0)
      CALL ZERCLR(VVZ,MXY*MZA,0.0D0)
C
C
C----------------------------------------------------------------------
C     (1) 自由流入出境界(0)またはスリップ壁を設定：上面
C----------------------------------------------------------------------
      K=MZMA
      DO J=2,MYM
      DO I=2,MXM
         IF(IBCAIRTOP.LE.-2) THEN
            VVZ(I,J,K) = 0.5D0*(VVA(I,J-1,K)+VVA(I,J,K))
         ELSE
            VVZ(I,J,K)=VSLIP
         ENDIF
      ENDDO
      ENDDO
C
C
C----------------------------------------------------------------------
C     (2) 速度固定境界またはスリップ壁を設定：西面、東面
C----------------------------------------------------------------------
      I=1
      DO K=2,MZMA
      DO J=2,MYM
         IF(IBCAIRWES.GE.0) THEN
            IF(UUA(I,J,K).GE.0.0D0) THEN
               VVX(I,J,K)=VVBCAIR(I,K,2)
            ELSE
               VVX(I,J,K)=0.5D0*(VVA(I+1,J-1,K)+VVA(I+1,J,K))
            ENDIF
         ELSE
            VVX(I,J,K)=VSLIP
         ENDIF
      ENDDO
      ENDDO
C
      I=MXM
      DO K=2,MZMA
      DO J=2,MYM
         IF(IBCAIREAS.GE.0) THEN
            IF(UUA(I,J,K).GE.0.0D0) THEN
               VVX(I,J,K)=VVBCAIR(I,K,3)
            ELSE
               VVX(I,J,K)=0.5D0*(VVA(I,J-1,K)+VVA(I,J,K))
            ENDIF
         ELSE
            VVX(I,J,K)=VSLIP
         ENDIF
      ENDDO
      ENDDO
C
C
C----------------------------------------------------------------------
C     (3) 壁面境界(LOG-LAW)
C----------------------------------------------------------------------
C ... 法線方向がZ方向の面
      DO J=2,MYM
      DO I=2,MXM
         DO K=MZMA-1,1,-1
            IF( INDWA(I,J,K).EQ.-2 ) THEN
               UU1 =0.5D0*(UUA(I-1,J,K+1)+UUA(I,J,K+1))
               VV1 =0.5D0*(VVA(I,J-1,K+1)+VVA(I,J,K+1))
               VEL1=SQRT(UU1*UU1+VV1*VV1)
               DD1 =0.5D0*(1.0D0-FFA(I,J,K+1))*ZCA(4,K+1)
               RNU =AMUAIR/RHOAIR
               CALL LOGLAW(VTAU,VEL1,DD1,RNU)
               ENU =TMUA(I,J,K+1)+RNU
               VVZ(I,J,K) = VV1 - VV1/MAX(VEL1,1.0D-20)
     $                    * VTAU*VTAU/ENU*DD1
               EXIT
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
C
C ... 法線方向がX方向の面
      DO J=2,MYM
      DO I=2,MXM-1
         DO K=2,MZMA
            IF( INDUA(I,J,K).EQ.-2 ) THEN
               IF( INDPA(I,J,K).GT.0 ) THEN
                  INB1=I
               ELSEIF( INDPA(I,J+1,K).GT.0 ) THEN
                  INB1=I+1
               ELSE
                  CYCLE
               ENDIF
C
               VV1 =0.5D0*(VVA(INB1,J-1,K)+VVA(INB1,J,K))
               WW1 =0.5D0*(WWA(INB1,J,K-1)+WWA(INB1,J,K))
               VEL1=SQRT(VV1*VV1+WW1*WW1)
               DD1 =0.5D0*XCP(4,I,J)
               RNU =AMUAIR/RHOAIR
               CALL LOGLAW(VTAU,VEL1,DD1,RNU)
               ENU =TMUA(INB1,J,K)+RNU
               VVX(I,J,K) = VV1 - VV1/MAX(VEL1,1.0D-20)
     $                    * VTAU*VTAU/ENU*DD1
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
      RETURN
      END
