      SUBROUTINE CLBRKW(TMUBW,TIMBW,HH,HOLD,HDEP,UU,VV,HU,HV,XC,YC,ZC,
     $                  INDU,INDV,INDW,INDP,KF,KG)
C----------------------------------------------------------------------
C     砕波モデル用の粘性係数TMUBWを計算する
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(OUT)::TMUBW(MX,MY)
      REAL(8),INTENT(INOUT)::TIMBW(MX,MY)
      REAL(8),INTENT(IN) ::HH(MX,MY),HOLD(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(IN) ::UU(MX,MY,MZ),VV(MX,MY,MZ)
      REAL(8),INTENT(IN) ::HU(MX,MY,MZ),HV(MX,MY,MZ)
      REAL(8),INTENT(IN) ::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      INTEGER,INTENT(IN) ::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(IN) ::INDW(MX,MY,MZ),INDP(MX,MY,MZ)
      INTEGER,INTENT(IN) ::KF(MX,MY),KG(MX,MY)
C
      REAL(8),PARAMETER:: HHLIMIT=0.83D0  ! 孤立波の波速の計算に用いる波高水深比の上限
C
      REAL(8)::BB,CC,TMUBW0,DHDT,DHASTA,TASTA,TT0,DHDTF,DHDTL
      REAL(8)::DHDX,DHDY,HH1,HH2,XX1,XX2,YY1,YY2,DXX,DYY,DZZ
      REAL(8)::HHWES,HHEAS,HHSOU,HHNOR,HHXY
      REAL(8)::DHH,UVS,US,VS,DUDXX,DVDYY,DU1,DU2,DV1,DV2,DHH2
      REAL(8)::TMUWK
      INTEGER::I,J,K,L,N,II,JJ
C
C ... ローカル配列
      REAL(8):: UAVE(MX,MY),VAVE(MX,MY),CWAV(MX,MY)
C
C
      TMUBW = 0.0D0
C
C----------------------------------------------------------------------
C     Kennedyのモデル
C----------------------------------------------------------------------
      IF( LBRKW.EQ.1 ) THEN
         DO J=2,MYM
         DO I=2,MXM
            DHDT=ABS((HH(I,J)-HOLD(I,J))/DT)
            CC=SQRT(ABS(GRAV)*(HH(I,J)-HDEP(I,J)))
            DHDTF=DKENN3*CC
            DHDTL=DKENN2*CC
C
C ......... 砕波中
            IF( TIMBW(I,J).GE.0.0D0 ) THEN
               TASTA=DKENN1*CC/ABS(GRAV)
               TT0=TIME-TIMBW(I,J)
C
               IF( TT0.GE.TASTA ) THEN
                  DHASTA=DHDTF
               ELSE
                  DHASTA=(TT0/MAX(TASTA,1.0D-3))*DHDTF
     $                  +(1.0D0-TT0/MAX(TASTA,1.0D-3))*DHDTL
               ENDIF
C
               IF( DHDT.GE.2.0D0*DHASTA ) THEN
                  BB=1.0D0
               ELSEIF( DHDT.GT.DHASTA ) THEN
                  BB=DHDT/DHASTA-1.0D0
               ELSE
                  BB=0.0D0
C
C ............... 砕波停止
ccc                  if( TIMBW(I,J)>0.d0 )
ccc     $               write(318+nrank,*) 'off: ',TIME,i,j
                  TIMBW(I,J)=-9999.0D0
               ENDIF
C
               TMUBW(I,J)=BB*DKENNEDY**2*(HH(I,J)-HDEP(I,J))*DHDT
C
C
C ......... 砕波前(砕波条件チェック)
            ELSE
C              水面のある領域で、自分のセルに水があるときのみ計算
               IF( KF(I,J).LT.MZ.AND.
     $             HH(I,J)-HDEP(I,J).GE.EPSH*10.0D0 ) THEN
C
C ............... X方向の水面勾配
C                 左側の水位のチェック
                  IF( I.EQ.2 ) THEN
                     HH1=HH(I,J)
                     XX1=XC(2,I,J)
                  ELSE
                     IF( KF(I-1,J).LT.MZ.AND.
     $                   HH(I-1,J)-HDEP(I-1,J).GE.EPSH*10.0D0 ) THEN
                        HH1=HH(I-1,J)
                        XX1=XC(2,I-1,J)
                     ELSE
                        HH1=HH(I,J)
                        XX1=XC(2,I,J)
                     ENDIF
                  ENDIF
C
C                 右側の水位のチェック
                  IF( I.EQ.MXM ) THEN
                     HH2=HH(I,J)
                     XX2=XC(2,I,J)
                  ELSE
                     IF( KF(I+1,J).LT.MZ.AND.
     $                   HH(I+1,J)-HDEP(I+1,J).GE.EPSH*10.0D0 ) THEN
                        HH2=HH(I+1,J)
                        XX2=XC(2,I+1,J)
                     ELSE
                        HH2=HH(I,J)
                        XX2=XC(2,I,J)
                     ENDIF
                  ENDIF
CCC                  DHDX=(HH2-HH1)/MAX(XX2-XX1,1.0D-10)
                  DHDX=MAX((HH2-HH(I,J))*XC(5,I,J),
     $                     (HH(I,J)-HH1)*XC(5,I-1,J))
C
C ............... Y方向の水面勾配
C                 下側の水位のチェック
                  IF( J.EQ.2 ) THEN
                     HH1=HH(I,J)
                     YY1=YC(2,J)
                  ELSE
                     IF( KF(I,J-1).LT.MZ.AND.
     $                   HH(I,J-1)-HDEP(I,J-1).GE.EPSH*10.0D0 ) THEN
                        HH1=HH(I,J-1)
                        YY1=YC(2,J-1)
                     ELSE
                        HH1=HH(I,J)
                        YY1=YC(2,J)
                     ENDIF
                  ENDIF
C
C                 上側の水位のチェック
                  IF( J.EQ.MYM ) THEN
                     HH2=HH(I,J)
                     YY2=YC(2,J)
                  ELSE
                     IF( KF(I,J+1).LT.MZ.AND.
     $                   HH(I,J+1)-HDEP(I,J+1).GE.EPSH*10.0D0 ) THEN
                        HH2=HH(I,J+1)
                        YY2=YC(2,J+1)
                     ELSE
                        HH2=HH(I,J)
                        YY2=YC(2,J)
                     ENDIF
                  ENDIF
CCC                  DHDY=(HH2-HH1)/MAX(YY2-YY1,1.0D-10)
                  DHDY=MAX((HH2-HH(I,J))*YC(5,J),
     $                     (HH(I,J)-HH1)*YC(5,J-1))
C
                  IF( DHDX**2+DHDY**2 .GE. 0.6D0**2 .OR.
     $                DHDT .GE. DHDTL ) THEN
ccc                  write(318+nrank,*) 'on : ',TIME,i,j
                     TIMBW(I,J)=TIME
                  ENDIF
               ENDIF
            ENDIF
C
         ENDDO
         ENDDO
C
C----------------------------------------------------------------------
C     岩瀬らのモデル
C----------------------------------------------------------------------
      ELSEIF( LBRKW.EQ.2 ) THEN
C ...... 平均流速と波速の設定
         UAVE=0.0D0
         VAVE=0.0D0
         CWAV=0.0D0
         DO J=2,MYM
         DO I=2,MXM
            DHH=HH(I,J)-HDEP(I,J)
C           水面のある領域で、自分のセルに水があるときのみ計算
            IF( KF(I,J).LT.MZ.AND.DHH.GE.EPSH*10.0D0 ) THEN
               UAVE(I,J) = 0.5D0*(HU(I-1,J,MZ)+HU(I,J,MZ))/DHH
               VAVE(I,J) = 0.5D0*(HV(I,J-1,MZ)+HV(I,J,MZ))/DHH
               CWAV(I,J) = SQRT(ABS(GRAV)*DHH)*(1.0D0+0.5D0
     $                   *MIN(HH(I,J)/MAX(-HDEP(I,J),1.0D-4),HHLIMIT))
            ENDIF
         ENDDO
         ENDDO
C
C        UAVE,VAVEの通信(領域分割並列計算用)
         CALL CP_DSR_DC2(MX,MY,1,0,1,UAVE)
         CALL CP_DSR_DC2(MX,MY,1,0,1,VAVE)
C
C----------------------------------------
C        (a) 波峰の決定
C----------------------------------------
         DO J=2,MYM
         DO I=2,MXM
            DHH=HH(I,J)-HDEP(I,J)
C           水面のある領域で、自分のセルに水があるときのみ計算
            IF( KF(I,J).LT.MZ.AND.DHH.GE.EPSH*10.0D0 ) THEN
C ............ X方向の勾配
C              左側の水位のチェック
               IF( I.EQ.2 ) THEN
                  DU1=0.0D0
               ELSE
                  IF( KF(I-1,J).LT.MZ.AND.
     $                HH(I-1,J)-HDEP(I-1,J).GE.EPSH*10.0D0 ) THEN
                     DU1=(UAVE(I,J)-UAVE(I-1,J))*XC(5,I-1,J)
                  ELSE
                     DU1=0.0D0
                  ENDIF
               ENDIF
C
C              右側の水位のチェック
               IF( I.EQ.MXM ) THEN
                  DU2=0.0D0
               ELSE
                  IF( KF(I+1,J).LT.MZ.AND.
     $                HH(I+1,J)-HDEP(I+1,J).GE.EPSH*10.0D0 ) THEN
                     DU2=(UAVE(I+1,J)-UAVE(I,J))*XC(5,I,J)
                  ELSE
                     DU2=0.0D0
                  ENDIF
               ENDIF
C
C ............ Y方向の水面勾配
C              下側の水位のチェック
               IF( J.EQ.2 ) THEN
                  DV1=0.0D0
               ELSE
                  IF( KF(I,J-1).LT.MZ.AND.
     $                HH(I,J-1)-HDEP(I,J-1).GE.EPSH*10.0D0 ) THEN
                     DV1=(VAVE(I,J)-VAVE(I,J-1))*YC(5,J-1)
                  ELSE
                     DV1=0.0D0
                  ENDIF
               ENDIF
C
C              上側の水位のチェック
               IF( J.EQ.MYM ) THEN
                  DV2=0.0D0
               ELSE
                  IF( KF(I,J+1).LT.MZ.AND.
     $                HH(I,J+1)-HDEP(I,J+1).GE.EPSH*10.0D0 ) THEN
                     DV2=(VAVE(I,J+1)-VAVE(I,J))*YC(5,J)
                  ELSE
                     DV2=0.0D0
                  ENDIF
               ENDIF
C
               DUDXX=(DU2-DU1)*XC(6,I,J)
               DVDYY=(DV2-DV1)*YC(6,J)
               US=UAVE(I,J)-DHH**2/3.0D0*DUDXX
               VS=VAVE(I,J)-DHH**2/3.0D0*DVDYY
               UVS=SQRT(US**2+VS**2)
C
C ............ 波峰ON
               IF( UVS/CWAV(I,J).GT.0.59D0 ) THEN
ccc                  write(318+nrank,*) 'on : ',TIME,i,j
                  TIMBW(I,J)=TIME
C ............ 波峰OFF
               ELSEIF( UVS/CWAV(I,J).LE.0.55D0 ) THEN
ccc                  if( TIMBW(I,J)>0.d0 )
ccc     $               write(318+nrank,*) 'off: ',TIME,i,j
                  TIMBW(I,J)=-9999.0D0
               ELSE
C ............ 前回の状態を継続
               ENDIF
            ENDIF
         ENDDO
         ENDDO
C
C
C----------------------------------------
C        (b) 波谷の決定 と 動粘性係数の計算
C----------------------------------------
         DO J=2,MYM
         DO I=2,MXM
            IF( TIMBW(I,J).GE.0.0D0 ) THEN
C
C ............ 波谷の4方向検索(水位 西:HHWES,東:HHEAS,南:HHSOU,北:HHNOR)
C
C ............ 西方向へ波谷を検索
               JJ=J
               HHWES=HH(I,J)
               DO II=I-1,2,-1
C                 陸または非計算領域にぶつかったとき
                  IF( KF(II,JJ).EQ.MZ .OR.
     $               (HH(II,JJ)-HDEP(II,JJ)).LT.EPSH*10.0D0 ) THEN
                     HHWES=HH(II+1,JJ)
                     EXIT
                  ELSE
C                    波谷
                     IF( HH(II,JJ).GE.HH(II+1,JJ) ) THEN
                        HHWES=HH(II+1,JJ)
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
C
C ............ 東方向へ波谷を検索
               JJ=J
               HHEAS=HH(I,J)
               DO II=I+1,MXM
C                 陸または非計算領域にぶつかったとき
                  IF( KF(II,JJ).EQ.MZ .OR.
     $               (HH(II,JJ)-HDEP(II,JJ)).LT.EPSH*10.0D0 ) THEN
                     HHEAS=HH(II-1,JJ)
                     EXIT
                  ELSE
C                    波谷
                     IF( HH(II,JJ).GE.HH(II-1,JJ) ) THEN
                        HHEAS=HH(II-1,JJ)
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
C
C ............ 南方向へ波谷を検索
               II=I
               HHSOU=HH(I,J)
               DO JJ=J-1,2,-1
C                 陸または非計算領域にぶつかったとき
                  IF( KF(II,JJ).EQ.MZ .OR.
     $               (HH(II,JJ)-HDEP(II,JJ)).LT.EPSH*10.0D0 ) THEN
                     HHSOU=HH(II,JJ+1)
                     EXIT
                  ELSE
C                    波谷
                     IF( HH(II,JJ).GE.HH(II,JJ+1) ) THEN
                        HHSOU=HH(II,JJ+1)
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
C
C ............ 北方向へ波谷を検索
               II=I
               HHNOR=HH(I,J)
               DO JJ=J+1,MYM
C                 陸または非計算領域にぶつかったとき
                  IF( KF(II,JJ).EQ.MZ .OR.
     $               (HH(II,JJ)-HDEP(II,JJ)).LT.EPSH*10.0D0 ) THEN
                     HHNOR=HH(II,JJ-1)
                     EXIT
                  ELSE
C                    波谷
                     IF( HH(II,JJ).GE.HH(II,JJ-1) ) THEN
                        HHNOR=HH(II,JJ-1)
                        EXIT
                     ENDIF
                  ENDIF
               ENDDO
C
C ............ 波の進行方向側の波谷代表水位の決定(代表水位 HHXY)
               HH1=HHEAS
               IF( UAVE(I,J).LE.0.0D0 ) HH1=HHWES
               HH2=HHNOR
               IF( VAVE(I,J).LE.0.0D0 ) HH2=HHSOU
               HHXY=MIN(HH1,HH2)
C
C
C ............ 波峰の動粘性係数設定
               DHH =HH(I,J)-HDEP(I,J)
               DHH2=HH(I,J)-HHXY
               TMUWK=BETAIWA*SQRT(ABS(GRAV)*DHH)*DHH2
               TMUBW(I,J)=MAX(TMUBW(I,J),TMUWK)
C
C
C ............ 再度、波峰から波谷まで4方向検索(動粘性係数 TMUBW の設定)
C
C ............ 西方向へ波谷を検索
               JJ=J
               DO II=I-1,2,-1
C              陸または非計算領域にぶつかったとき
                  IF( KF(II,JJ).EQ.MZ .OR.
     $               (HH(II,JJ)-HDEP(II,JJ)).LT.EPSH*10.0D0 ) THEN
                     EXIT
                  ELSE
                     IF( HH(II,JJ).GE.HH(II+1,JJ) ) EXIT
C
                     DHH =HH(II,JJ)-HDEP(II,JJ)
                     DHH2=(HH(II,JJ)-HHWES)*(HH(I,J)-HHXY)
     $                   /(HH(I,J)-HHWES)
                     TMUWK=BETAIWA*SQRT(ABS(GRAV)*DHH)*DHH2
                     TMUBW(II,JJ)=MAX(TMUBW(II,JJ),TMUWK)
                  ENDIF
               ENDDO
C
C ............ 東方向へ波谷を検索
               JJ=J
               DO II=I+1,MXM
C                 陸または非計算領域にぶつかったとき
                  IF( KF(II,JJ).EQ.MZ .OR.
     $               (HH(II,JJ)-HDEP(II,JJ)).LT.EPSH*10.0D0 ) THEN
                     EXIT
                  ELSE
                     IF( HH(II,JJ).GE.HH(II-1,JJ) ) EXIT
C
                     DHH =HH(II,JJ)-HDEP(II,JJ)
                     DHH2=(HH(II,JJ)-HHEAS)*(HH(I,J)-HHXY)
     $                   /(HH(I,J)-HHEAS)
                     TMUWK=BETAIWA*SQRT(ABS(GRAV)*DHH)*DHH2
                     TMUBW(II,JJ)=MAX(TMUBW(II,JJ),TMUWK)
                  ENDIF
               ENDDO
C
C ............ 南方向へ波谷を検索
               II=I
               DO JJ=J-1,2,-1
C                 陸または非計算領域にぶつかったとき
                  IF( KF(II,JJ).EQ.MZ .OR.
     $               (HH(II,JJ)-HDEP(II,JJ)).LT.EPSH*10.0D0 ) THEN
                     EXIT
                  ELSE
                     IF( HH(II,JJ).GE.HH(II,JJ+1) ) EXIT
C
                     DHH =HH(II,JJ)-HDEP(II,JJ)
                     DHH2=(HH(II,JJ)-HHSOU)*(HH(I,J)-HHXY)
     $                   /(HH(I,J)-HHSOU)
                     TMUWK=BETAIWA*SQRT(ABS(GRAV)*DHH)*DHH2
                     TMUBW(II,JJ)=MAX(TMUBW(II,JJ),TMUWK)
                  ENDIF
               ENDDO
C
C ............ 北方向へ波谷を検索
               II=I
               DO JJ=J+1,MYM
C                 陸または非計算領域にぶつかったとき
                  IF( KF(II,JJ).EQ.MZ .OR.
     $               (HH(II,JJ)-HDEP(II,JJ)).LT.EPSH*10.0D0 ) THEN
                     EXIT
                  ELSE
                     IF( HH(II,JJ).GE.HH(II,JJ-1) ) EXIT
C
                     DHH =HH(II,JJ)-HDEP(II,JJ)
                     DHH2=(HH(II,JJ)-HHNOR)*(HH(I,J)-HHXY)
     $                   /(HH(I,J)-HHNOR)
                     TMUWK=BETAIWA*SQRT(ABS(GRAV)*DHH)*DHH2
                     TMUBW(II,JJ)=MAX(TMUBW(II,JJ),TMUWK)
                  ENDIF
               ENDDO
C
            ENDIF
         ENDDO
         ENDDO
C
      ENDIF
C
C
C ... 動粘性係数の上限を設定
      DZZ=0.0D0
      DO K=2,MZM
         DZZ=MAX(ZC(6,K)**2,DZZ)
      ENDDO
      IF(MZM.EQ.2) DZZ=0.0D0
C
      DO J=2,MYM
      DO I=2,MXM
         DXX=XC(6,I,J)**2
         DYY=YC(6,J)**2
         IF(MXM.EQ.2) DXX=0.0D0 
         IF(MYM.EQ.2) DYY=0.0D0
         TMUBW(I,J)=MIN(TMUBW(I,J),SAFEBRKW*0.5D0/DT/(DXX+DYY+DZZ))
      ENDDO
      ENDDO
C
      RETURN
      END
