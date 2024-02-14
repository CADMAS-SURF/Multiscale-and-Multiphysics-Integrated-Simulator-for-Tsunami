      SUBROUTINE BCWWXY_AIR(WWX,WWY,UUA,VVA,WWA,TMUA,
     $                      XC,YC,INDPA,INDUA,INDVA)
C======================================================================
C     壁面、流速固定境界および自由流出境界の接線方向流速を設定する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(OUT)::WWX(MX,MY,MZA),WWY(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY)
C
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA),INDVA(MX,MY,MZA)
C
      REAL(8)::DD1,ENU,UU1,VEL1,VTAU,VV1,WW1,RNU
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,INB1,JNB1
C
C
      CALL ZERCLR(WWX,MXY*MZA,0.0D0)
      CALL ZERCLR(WWY,MXY*MZA,0.0D0)
C
C----------------------------------------------------------------------
C     (1) 速度固定境界(=0)またはスリップ壁を設定：南面、西面、東面、北面
C----------------------------------------------------------------------
      J=1
      DO K=2,MZMA
      DO I=2,MXM
         IF(IBCAIRSOU.GE.0) THEN
            IF(VVA(I,J,K).GE.0.0D0) THEN
               WWY(I,J,K)=0.0D0
            ELSE
               WWY(I,J,K)=0.5D0*(WWA(I-1,J+1,K)+WWA(I,J+1,K))
            ENDIF
         ELSE
            WWY(I,J,K)=VSLIP
         ENDIF
      ENDDO
      ENDDO
C
      I=1
      DO K=2,MZMA
      DO J=2,MYM
         IF(IBCAIRWES.GE.0) THEN
            IF(UUA(I,J,K).GE.0.0D0) THEN
               WWX(I,J,K)=0.0D0
            ELSE
               WWX(I,J,K)=0.5D0*(WWA(I+1,J-1,K)+WWA(I+1,J,K))
            ENDIF
         ELSE
            WWX(I,J,K)=VSLIP
         ENDIF
      ENDDO
      ENDDO
C
      I=MXM
      DO K=2,MZMA
      DO J=2,MYM
         IF(IBCAIREAS.GE.0) THEN
            IF(UUA(I,J,K).GE.0.0D0) THEN
               WWX(I,J,K)=0.0D0
            ELSE
               WWX(I,J,K)=0.5D0*(WWA(I,J-1,K)+WWA(I,J,K))
            ENDIF
         ELSE
            WWX(I,J,K)=VSLIP
         ENDIF
      ENDDO
      ENDDO
C
      J=MYM
      DO K=2,MZMA
      DO I=2,MXM
         IF(IBCAIRNOR.GE.0) THEN
            IF(VVA(I,J,K).LE.0.0D0) THEN
               WWY(I,J,K)=0.0D0
            ELSE
               WWY(I,J,K)=0.5D0*(WWA(I-1,J,K)+WWA(I,J,K))
            ENDIF
         ELSE
            WWY(I,J,K)=VSLIP
         ENDIF
      ENDDO
      ENDDO
C
C
C----------------------------------------------------------------------
C     (2) 壁面境界(LOG-LAW)
C----------------------------------------------------------------------

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
               DD1 =0.5D0*XC(4,I,J)
               RNU =AMUAIR/RHOAIR
               CALL LOGLAW(VTAU,VEL1,DD1,RNU)
               ENU =TMUA(INB1,J,K)+RNU
               WWX(I,J,K) = WW1 - WW1/MAX(VEL1,1.0D-20)
     $                    * VTAU*VTAU/ENU*DD1
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
C ... 法線方向がY方向の面
      DO J=2,MYM-1
      DO I=2,MXM
         DO K=2,MZMA
            IF( INDVA(I,J,K).EQ.-2 ) THEN
               IF( INDPA(I,J,K).GT.0 ) THEN
                  JNB1=J
               ELSEIF( INDPA(I,J+1,K).GT.0 ) THEN
                  JNB1=J+1
               ELSE
                  CYCLE
               ENDIF
C
               UU1 =0.5D0*(UUA(I-1,JNB1,K)+UUA(I,JNB1,K))
               WW1 =0.5D0*(WWA(I,JNB1,K-1)+WWA(I,JNB1,K))
               VEL1=SQRT(UU1*UU1+WW1*WW1)
               DD1 =0.5D0*YC(4,J)
               RNU =AMUAIR/RHOAIR
               CALL LOGLAW(VTAU,VEL1,DD1,RNU)
               ENU =TMUA(I,JNB1,K)+RNU
               WWY(I,J,K) = WW1 - WW1/MAX(VEL1,1.0D-20)
     $                    * VTAU*VTAU/ENU*DD1
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
      RETURN
      END
