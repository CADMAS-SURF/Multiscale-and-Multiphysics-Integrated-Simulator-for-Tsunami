      SUBROUTINE BCUUYZ_AIR(UUY,UUZ,UUA,VVA,WWA,FFA,TMUA,UUBCAIR,
     $                      GVA,YC,ZCA,INDPA,INDVA,INDWA)
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
      REAL(8),INTENT(OUT)::UUY(MX,MY,MZA),UUZ(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::FFA(MX,MY,MZA),TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUBCAIR(NXY,MZA,4)
      REAL(8),INTENT(IN)::GVA(MX,MY,MZA)
      REAL(8),INTENT(IN)::YC(8,MY),ZCA(8,MZA)
C
      INTEGER,INTENT(IN)::INDPA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
C
      REAL(8)::DD1,ENU,UU1,VEL1,VTAU,VV1,WW1,RNU
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,J,K,JNB1
C
C
      CALL ZERCLR(UUY,MXY*MZA,0.0D0)
      CALL ZERCLR(UUZ,MXY*MZA,0.0D0)
C
C
C----------------------------------------------------------------------
C     (1) 自由流入出境界またはスリップ壁を設定：上面
C----------------------------------------------------------------------
      K=MZMA
      DO J=2,MYM
      DO I=2,MXM
         IF(IBCAIRTOP.LE.-2) THEN
            UUZ(I,J,K)=0.5D0*(UUA(I-1,J,K)+UUA(I,J,K))
         ELSE
            UUZ(I,J,K)=VSLIP
         ENDIF
      ENDDO
      ENDDO
C
C
C----------------------------------------------------------------------
C     (2) 速度固定境界またはスリップ壁を設定：南面、北面
C----------------------------------------------------------------------
      J=1
      DO K=2,MZMA
      DO I=2,MXM
         IF(IBCAIRSOU.GE.0) THEN
            IF(VVA(I,J,K).GE.0.0D0) THEN
               UUY(I,J,K)=UUBCAIR(I,K,1)
            ELSE
               UUY(I,J,K)=0.5D0*(UUA(I-1,J+1,K)+UUA(I,J+1,K))
            ENDIF
         ELSE
            UUY(I,J,K)=VSLIP
         ENDIF
      ENDDO
      ENDDO
C
      J=MYM
      DO K=2,MZMA
      DO I=2,MXM
         IF(IBCAIRNOR.GE.0) THEN
            IF(VVA(I,J,K).LE.0.0D0) THEN
               UUY(I,J,K)=UUBCAIR(I,K,4)
            ELSE
               UUY(I,J,K)=0.5D0*(UUA(I-1,J,K)+UUA(I,J,K))
            ENDIF
         ELSE
            UUY(I,J,K)=VSLIP
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
               UUZ(I,J,K) = UU1 - UU1/MAX(VEL1,1.0D-20)
     $                    * VTAU*VTAU/ENU*DD1
c               if(i.eq.3.and.k.eq.1)then
c                  write(16,*) 'i,k,velbc=',i,k,uuz(i,j,k),
c     $               uu1,vtau,enu,dd1
c               endif
               EXIT
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
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
               UUY(I,J,K) = UU1 - UU1/MAX(VEL1,1.0D-20)
     $                    * VTAU*VTAU/ENU*DD1
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
      RETURN
      END
