      SUBROUTINE CP_MODDEP(INDP_NS,INDU_NS,INDV_NS,INDW_NS,
     1                     GV_NS,GX_NS,GY_NS,HDEP_NS,HDEP_ML,
     2                     XC_NS,YC_NS,ZC_NS,HH_NS,HH_ML,FF_NS,PP_NS,
     3                     PATM_NS,I_ML,J_ML,KF_NS,KF_ML,KP_NS,KG_NS,
     4                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     5                     NESXM,NESXP,NESYM,NESYP,
     6                     IEAS,IWES,JSOU,JNOR,IERR)
C-----------------------------------------------------------------------
C     オーバーラップ領域の水深を修正する
C       範囲:親領域の大きさで(オーバーラップ幅+1)の範囲
C       処理:(1)親子とも計算領域では子の水深=親の水深
C            (2)親は計算、子は非計算では子の領域を親の条件に合す
C            (3)親は非計算、子は計算では子の条件を優先
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'CONNEC.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),PARAMETER::GVMIN=1.0D-4
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      INTEGER,INTENT(INOUT)::NESXM,NESXP,NESYM,NESYP
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR
      INTEGER,INTENT(INOUT)::IERR
C
      INTEGER,INTENT(INOUT)::INDP_NS(MX_NS,MY_NS,MZ_NS)
      INTEGER,INTENT(INOUT)::INDU_NS(MX_NS,MY_NS,MZ_NS)
      INTEGER,INTENT(INOUT)::INDV_NS(MX_NS,MY_NS,MZ_NS)
      INTEGER,INTENT(INOUT)::INDW_NS(MX_NS,MY_NS,MZ_NS)
C
      REAL(8),INTENT(INOUT)::GV_NS(MX_NS,MY_NS,MZ_NS)
      REAL(8),INTENT(INOUT)::GX_NS(MX_NS,MY_NS,MZ_NS)
      REAL(8),INTENT(INOUT)::GY_NS(MX_NS,MY_NS,MZ_NS)
      REAL(8),INTENT(INOUT)::FF_NS(MX_NS,MY_NS,MZ_NS)
      REAL(8),INTENT(INOUT)::PP_NS(MX_NS,MY_NS,MZ_NS)
      REAL(8),INTENT(INOUT)::XC_NS(8,MX_NS),YC_NS(8,MY_NS)
      REAL(8),INTENT(INOUT)::ZC_NS(8,MZ_NS)
C
      INTEGER,INTENT(INOUT)::I_ML(2,MX_ML),J_ML(2,MY_ML)
      INTEGER,INTENT(INOUT)::KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      INTEGER,INTENT(INOUT)::KF_NS(MX_NS,MY_NS),KP_NS(MX_NS,MY_NS)
      INTEGER,INTENT(INOUT)::KG_NS(MX_NS,MY_NS)
C
      REAL(8),INTENT(INOUT)::HDEP_NS(MX_NS,MY_NS),HH_NS(MX_NS,MY_NS)
      REAL(8),INTENT(INOUT)::PATM_NS(MX_NS,MY_NS)
      REAL(8),INTENT(INOUT)::HDEP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::HH_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
C
      INTEGER::IDB=1,IOVR=0
      REAL(8)::DEPEPS=1.0D-3
C
      REAL(8)::DEPM,DEPP,GVM,GVP,HDEPO
      INTEGER::I,I1,I2,ICHK,II,IPARNT,J,J1,J2,JJ,K,KFO,KOUNT,IX,JX
      INTEGER::NESTXM,NESTXP,NESTYM,NESTYP
C
      IPARNT = IPECON(2,NRANK+1)
      IF(IPARNT.LT.0) GO TO 900
C
      NESTXM = NESXM-IOVR
      NESTXP = NESXP-IOVR
      NESTYM = NESYM-IOVR
      NESTYP = NESYP-IOVR
      KOUNT  = 0
      IERR  = 0
C
C ... 親の水深を子にコピー
C
      DO 100 JJ = JSOU,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 100
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 110 II = IWES,IWES+NESTXM
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 110
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
C
          DO 120 J = J1,J2
          DO 125 I = I1,I2
C
            HDEPO = HDEP_NS(I,J)
            KFO   = KF_NS(I,J)
            ICHK = 0
            IF(KF_ML(II,JJ).NE.MZ_ML.AND.KF_NS(I,J).NE.MZ_NS) ICHK=1
            IF(KF_ML(II,JJ).NE.MZ_ML.AND.KF_NS(I,J).EQ.MZ_NS) ICHK=2
            IF(KF_ML(II,JJ).EQ.MZ_ML.AND.KF_NS(I,J).NE.MZ_NS) ICHK=3
            IF(ICHK.NE.0) THEN
              KOUNT = KOUNT+1
              HDEP_NS(I,J) = HDEP_ML(II,JJ)
C
              IF((HDEP_ML(II,JJ).LT.ZC_NS(1,KG_NS(I,J)-1)).OR.
     &           (HDEP_ML(II,JJ).GE.ZC_NS(1,KG_NS(I,J)  ))) THEN
                IF(ICHK.EQ.1) HDEP_NS(I,J)=HDEPO
              END IF
C
              IF(LSTART.NE.0) GO TO 125
              IF(ICHK.EQ.1) THEN
                IF(HH_ML(II,JJ)-HDEP_ML(II,JJ).LT.GXB.OR.
     &             HH_NS(I ,J )-HDEPO         .LT.GXB) THEN
                   ICHK = 4
                END IF
              END IF
C
              CALL CP_SEAFLG(I,J,ICHK,II,JJ)
              IF(ICHK.LT.0) THEN
                IERR = 1
                GO TO 125
              END IF
C
              IF(ICHK.GE.2)THEN
                HH_NS(I,J) = HH_ML(II,JJ)
                KF_NS(I,J) = MZ_NS
                KG_NS(I,J) = MZ_NS
C
                DO 130 K = 2,MZ_NS-1
                  IF(KF_ML(II,JJ).EQ.MZ_ML) THEN
                    INDP_NS(I,J,K) = 0
                  ELSE IF(HDEP_NS(I,J).LE.ZC_NS(1,K-1)) THEN
                    INDP_NS(I,J,K) = 1
                    KG_NS(I,J) = MIN(KG_NS(I,J),K)
                  ELSE IF(HDEP_NS(I,J).GE.ZC_NS(1,K  )) THEN
                    INDP_NS(I,J,K) = 0
                  ELSE
                    INDP_NS(I,J,K) = 1
                    KG_NS(I,J) = MIN(KG_NS(I,J),K)
                  END IF
C
                  IF(KF_NS(I,J).EQ.MZ_NS) THEN
                    IF(INDP_NS(I,J,K).GT.0) THEN
                      IF(ZC_NS(1,K).LE.HH_NS(I,J)) THEN
                        FF_NS(I,J,K) = 1.0D0
                      ELSE IF(ZC_NS(1,K-1).GT.HH_NS(I,J)) THEN
                        FF_NS(I,J,K) = 0.0D0
                      ELSE
                        FF_NS(I,J,K) = (HH_NS(I,J)-ZC_NS(1,K-1))
     $                               * ZC_NS(6,K)
                        KF_NS(I,J) = K
                        KP_NS(I,J) = K
                      END IF
                    END IF
                  ELSE
                    FF_NS(I,J,K) = 0.0D0
                  END IF
  130           CONTINUE
C ............. 圧力の更新
                IF(KF_NS(I,J).NE.MZ_NS) THEN
                  DO 135 K=KF_NS(I,J),2,-1
                    IF(K.EQ.KF_NS(I,J)) THEN
                      PP_NS(I,J,K) = PATM_NS(I,J)
     1                             - (HH_NS(I,J)-ZC_NS(2,K))*RHO*GRAV
                    ELSE
                      PP_NS(I,J,K) = PP_NS(I,J,K+1)-RHO*GRAV*ZC_NS(3,K)
                    END IF
  135             CONTINUE
                END IF
C
              END IF
            END IF
C
cccccccccccc
      if(idb.ne.0) then
        if(ichk.ne.0.and.abs(hdep_ml(ii,jj)-hdepo).gt.gxb) then
c          write(6,11) ii,jj,hdep_ml(ii,jj),i,j,hdepo,hdep_ns(i,j)
 11       format(' #a moddep ii,jj,hdep_ml=',2i5,1p,d12.4,
     a           '     i,j,hdep_ns=',2i5,1p,2d12.4)
        end if
        if(ichk.eq.2.or.ichk.eq.3) then
          write(6,11) ii,jj,hdep_ml(ii,jj),i,j,hdepo,hdep_ns(i,j)
          if(kf_ml(ii,jj).ne.kfo)
     a    write(6,*) 'kf_ml,kf_ns=',kf_ml(ii,jj),kfo,kf_ns(i,j)
          write(6,*) 'hh_ml,hh_ns=',hh_ml(ii,jj),hh_ns(i,j)
        end if
      end if
cccccccccccc
  125     CONTINUE
  120     CONTINUE
C
  110   CONTINUE
  100 CONTINUE
C
      DO 150 JJ = JSOU,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 150
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 160 II = IEAS-NESTXP,IEAS
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 160
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
C
          DO 170 J = J1,J2
          DO 175 I = I1,I2
C
            HDEPO = HDEP_NS(I,J)
            KFO   = KF_NS(I,J)
            ICHK = 0
            IF(KF_ML(II,JJ).NE.MZ_ML.AND.KF_NS(I,J).NE.MZ_NS) ICHK=1
            IF(KF_ML(II,JJ).NE.MZ_ML.AND.KF_NS(I,J).EQ.MZ_NS) ICHK=2
            IF(KF_ML(II,JJ).EQ.MZ_ML.AND.KF_NS(I,J).NE.MZ_NS) ICHK=3
            IF(ICHK.NE.0) THEN
              KOUNT = KOUNT+1
              HDEP_NS(I,J) = HDEP_ML(II,JJ)
C
              IF((HDEP_ML(II,JJ).LT.ZC_NS(1,KG_NS(I,J)-1)).OR.
     &           (HDEP_ML(II,JJ).GE.ZC_NS(1,KG_NS(I,J)  ))) THEN
                IF(ICHK.EQ.1) HDEP_NS(I,J)=HDEPO
              END IF
C
              IF(LSTART.NE.0) GO TO 175
              IF(ICHK.EQ.1) THEN
                IF(HH_ML(II,JJ)-HDEP_ML(II,JJ).LT.GXB.OR.
     &             HH_NS(I ,J )-HDEPO         .LT.GXB) THEN
                   ICHK = 4
                END IF
              END IF
C
              CALL CP_SEAFLG(I,J,ICHK,II,JJ)
              IF(ICHK.LT.0) THEN
                IERR = 1
                GO TO 175
              END IF
C
              IF(ICHK.GE.2)THEN
                HH_NS(I,J) = HH_ML(II,JJ)
                KF_NS(I,J) = MZ_NS
                KG_NS(I,J) = MZ_NS
C
                DO 180 K = 2,MZ_NS-1
                  IF(KF_ML(II,JJ).EQ.MZ_ML) THEN
                    INDP_NS(I,J,K) = 0
                  ELSE IF(HDEP_NS(I,J).LE.ZC_NS(1,K-1)) THEN
                    INDP_NS(I,J,K) = 1
                    KG_NS(I,J) = MIN(KG_NS(I,J),K)
                  ELSE IF(HDEP_NS(I,J).GE.ZC_NS(1,K  )) THEN
                    INDP_NS(I,J,K) = 0
                  ELSE
                    INDP_NS(I,J,K) = 1
                    KG_NS(I,J) = MIN(KG_NS(I,J),K)
                  END IF
C
                  IF(KF_NS(I,J).EQ.MZ_NS) THEN
                    IF(INDP_NS(I,J,K).GT.0) THEN
                      IF(ZC_NS(1,K).LE.HH_NS(I,J)) THEN
                        FF_NS(I,J,K) = 1.0D0
                      ELSE IF(ZC_NS(1,K-1).GT.HH_NS(I,J)) THEN
                        FF_NS(I,J,K) = 0.0D0
                      ELSE
                        FF_NS(I,J,K) = (HH_NS(I,J)-ZC_NS(1,K-1))
     $                               * ZC_NS(6,K)
                        KF_NS(I,J) = K
                        KP_NS(I,J) = K
                      END IF
                    END IF
                  ELSE
                    FF_NS(I,J,K) = 0.0D0
                  END IF
  180           CONTINUE
C ............. 圧力の更新
                IF(KF_NS(I,J).NE.MZ_NS) THEN
                  DO 185 K=KF_NS(I,J),2,-1
                    IF(K.EQ.KF_NS(I,J)) THEN
                      PP_NS(I,J,K) = PATM_NS(I,J)
     1                             - (HH_NS(I,J)-ZC_NS(2,K))*RHO*GRAV
                    ELSE
                      PP_NS(I,J,K) = PP_NS(I,J,K+1)-RHO*GRAV*ZC_NS(3,K)
                    END IF
  185             CONTINUE
                END IF
C
              END IF
            END IF
C
cccccccccccc
      if(idb.ne.0) then
        if(ichk.ne.0.and.abs(hdep_ml(ii,jj)-hdepo).gt.gxb) then
c          write(6,12) ii,jj,hdep_ml(ii,jj),i,j,hdepo,hdep_ns(i,J)
 12       format(' #b moddep ii,jj,hdep_ml=',2i5,1p,d12.4,
     a           '     i,j,hdep_ns=',2i5,1p,2d12.4)
        end if
        if(ichk.eq.2.or.ichk.eq.3) then
          write(6,12) ii,jj,hdep_ml(ii,jj),i,j,hdepo,hdep_ns(i,J)
          if(kf_ml(ii,jj).ne.kfo)
     a    write(6,*) 'kf_ml,kf_ns=',kf_ml(ii,jj),kfo,kf_ns(i,j)
          write(6,*) 'hh_ml,hh_ns=',hh_ml(ii,jj),hh_ns(i,j)
        end if
      end if
cccccccccccc
  175     CONTINUE
  170     CONTINUE
C
  160   CONTINUE
  150 CONTINUE
C
      DO 200 JJ = JSOU,JSOU+NESTYM
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 200
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 210 II = IWES+NESTXM+1,IEAS-NESTXP-1
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 210
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
C
          DO 220 J = J1,J2
          DO 225 I = I1,I2
C
            HDEPO = HDEP_NS(I,J)
            KFO   = KF_NS(I,J)
            ICHK = 0
            IF(KF_ML(II,JJ).NE.MZ_ML.AND.KF_NS(I,J).NE.MZ_NS) ICHK=1
            IF(KF_ML(II,JJ).NE.MZ_ML.AND.KF_NS(I,J).EQ.MZ_NS) ICHK=2
            IF(KF_ML(II,JJ).EQ.MZ_ML.AND.KF_NS(I,J).NE.MZ_NS) ICHK=3
            IF(ICHK.NE.0) THEN
              KOUNT = KOUNT+1
              HDEP_NS(I,J) = HDEP_ML(II,JJ)
C
              IF((HDEP_ML(II,JJ).LT.ZC_NS(1,KG_NS(I,J)-1)).OR.
     &           (HDEP_ML(II,JJ).GE.ZC_NS(1,KG_NS(I,J)  ))) THEN
                IF(ICHK.EQ.1) HDEP_NS(I,J)=HDEPO
              END IF
C
              IF(LSTART.NE.0) GO TO 225
              IF(ICHK.EQ.1) THEN
                IF(HH_ML(II,JJ)-HDEP_ML(II,JJ).LT.GXB.OR.
     &             HH_NS(I ,J )-HDEPO         .LT.GXB) THEN
                   ICHK = 4
                END IF
              END IF
C
              CALL CP_SEAFLG(I,J,ICHK,II,JJ)
              IF(ICHK.LT.0) THEN
                IERR = 1
                GO TO 225
              END IF
C
              IF(ICHK.GE.2)THEN
                HH_NS(I,J) = HH_ML(II,JJ)
                KF_NS(I,J) = MZ_NS
                KG_NS(I,J) = MZ_NS
C
                DO 230 K = 2,MZ_NS-1
                  IF(KF_ML(II,JJ).EQ.MZ_ML) THEN
                    INDP_NS(I,J,K) = 0
                  ELSE IF(HDEP_NS(I,J).LE.ZC_NS(1,K-1)) THEN
                    INDP_NS(I,J,K) = 1
                    KG_NS(I,J) = MIN(KG_NS(I,J),K)
                  ELSE IF(HDEP_NS(I,J).GE.ZC_NS(1,K  )) THEN
                    INDP_NS(I,J,K) = 0
                  ELSE
                    INDP_NS(I,J,K) = 1
                    KG_NS(I,J) = MIN(KG_NS(I,J),K)
                  END IF
C
                  IF(KF_NS(I,J).EQ.MZ_NS) THEN
                    IF(INDP_NS(I,J,K).GT.0) THEN
                      IF(ZC_NS(1,K).LE.HH_NS(I,J)) THEN
                        FF_NS(I,J,K) = 1.0D0
                      ELSE IF(ZC_NS(1,K-1).GT.HH_NS(I,J)) THEN
                        FF_NS(I,J,K) = 0.0D0
                      ELSE
                        FF_NS(I,J,K) = (HH_NS(I,J)-ZC_NS(1,K-1))
     $                               * ZC_NS(6,K)
                        KF_NS(I,J) = K
                        KP_NS(I,J) = K
                      END IF
                    END IF
                  ELSE
                    FF_NS(I,J,K) = 0.0D0
                  END IF
  230           CONTINUE
C ............. 圧力の更新
                IF(KF_NS(I,J).NE.MZ_NS) THEN
                  DO 235 K=KF_NS(I,J),2,-1
                    IF(K.EQ.KF_NS(I,J)) THEN
                      PP_NS(I,J,K) = PATM_NS(I,J)
     1                             - (HH_NS(I,J)-ZC_NS(2,K))*RHO*GRAV
                    ELSE
                      PP_NS(I,J,K) = PP_NS(I,J,K+1)-RHO*GRAV*ZC_NS(3,K)
                    END IF
  235             CONTINUE
                END IF
C
              END IF
            END IF
C
cccccccccccc
      if(idb.ne.0) then
        if(ichk.ne.0.and.abs(hdep_ml(ii,jj)-hdepo).gt.gxb) then
c          write(6,13) ii,jj,hdep_ml(ii,jj),i,j,hdepo,hdep_ns(i,J)
 13       format(' #c moddep ii,jj,hdep_ml=',2i5,1p,d12.4,
     a           '     i,j,hdep_ns=',2i5,1p,2d12.4)
        end if
        if(ichk.eq.2.or.ichk.eq.3) then
          write(6,13) ii,jj,hdep_ml(ii,jj),i,j,hdepo,hdep_ns(i,J)
          if(kf_ml(ii,jj).ne.kfo)
     a    write(6,*) 'kf_ml,kf_ns=',kf_ml(ii,jj),kfo,kf_ns(i,j)
          write(6,*) 'hh_ml,hh_ns=',hh_ml(ii,jj),hh_ns(i,j)
        end if
      end if
cccccccccccc
  225     CONTINUE
  220     CONTINUE
C
  210   CONTINUE
  200 CONTINUE
C
      DO 250 JJ = JNOR-NESTYP,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 250
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 260 II = IWES+NESTXM+1,IEAS-NESTXP-1
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 260
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
C
          DO 270 J = J1,J2
          DO 275 I = I1,I2
C
            HDEPO = HDEP_NS(I,J)
            KFO   = KF_NS(I,J)
            ICHK = 0
            IF(KF_ML(II,JJ).NE.MZ_ML.AND.KF_NS(I,J).NE.MZ_NS) ICHK=1
            IF(KF_ML(II,JJ).NE.MZ_ML.AND.KF_NS(I,J).EQ.MZ_NS) ICHK=2
            IF(KF_ML(II,JJ).EQ.MZ_ML.AND.KF_NS(I,J).NE.MZ_NS) ICHK=3
            IF(ICHK.NE.0) THEN
              KOUNT = KOUNT+1
              HDEP_NS(I,J) = HDEP_ML(II,JJ)
C
              IF((HDEP_ML(II,JJ).LT.ZC_NS(1,KG_NS(I,J)-1)).OR.
     &           (HDEP_ML(II,JJ).GE.ZC_NS(1,KG_NS(I,J)  ))) THEN
                IF(ICHK.EQ.1) HDEP_NS(I,J)=HDEPO
              END IF
C
              IF(LSTART.NE.0) GO TO 275
              IF(ICHK.EQ.1) THEN
                IF(HH_ML(II,JJ)-HDEP_ML(II,JJ).LT.GXB.OR.
     &             HH_NS(I ,J )-HDEPO         .LT.GXB) THEN
                   ICHK = 4
                END IF
              END IF
C
              CALL CP_SEAFLG(I,J,ICHK,II,JJ)
              IF(ICHK.LT.0) THEN
                IERR = 1
                GO TO 275
              END IF
C
              IF(ICHK.GE.2)THEN
                HH_NS(I,J) = HH_ML(II,JJ)
                KF_NS(I,J) = MZ_NS
                KG_NS(I,J) = MZ_NS
C
                DO 280 K = 2,MZ_NS-1
                  IF(KF_ML(II,JJ).EQ.MZ_ML) THEN
                    INDP_NS(I,J,K) = 0
                  ELSE IF(HDEP_NS(I,J).LE.ZC_NS(1,K-1)) THEN
                    INDP_NS(I,J,K) = 1
                    KG_NS(I,J) = MIN(KG_NS(I,J),K)
                  ELSE IF(HDEP_NS(I,J).GE.ZC_NS(1,K  )) THEN
                    INDP_NS(I,J,K) = 0
                  ELSE
                    INDP_NS(I,J,K) = 1
                    KG_NS(I,J) = MIN(KG_NS(I,J),K)
                  END IF
C
                  IF(KF_NS(I,J).EQ.MZ_NS) THEN
                    IF(INDP_NS(I,J,K).GT.0) THEN
                      IF(ZC_NS(1,K).LE.HH_NS(I,J)) THEN
                        FF_NS(I,J,K) = 1.0D0
                      ELSE IF(ZC_NS(1,K-1).GT.HH_NS(I,J)) THEN
                        FF_NS(I,J,K) = 0.0D0
                      ELSE
                        FF_NS(I,J,K) = (HH_NS(I,J)-ZC_NS(1,K-1))
     $                               * ZC_NS(6,K)
                        KF_NS(I,J) = K
                        KP_NS(I,J) = K
                      END IF
                    END IF
                  ELSE
                    FF_NS(I,J,K) = 0.0D0
                  END IF
  280           CONTINUE
C ............. 圧力の更新
                IF(KF_NS(I,J).NE.MZ_NS) THEN
                  DO 285 K=KF_NS(I,J),2,-1
                    IF(K.EQ.KF_NS(I,J)) THEN
                      PP_NS(I,J,K) = PATM_NS(I,J)
     1                             - (HH_NS(I,J)-ZC_NS(2,K))*RHO*GRAV
                    ELSE
                      PP_NS(I,J,K) = PP_NS(I,J,K+1)-RHO*GRAV*ZC_NS(3,K)
                    END IF
  285             CONTINUE
                END IF
C
              END IF
            END IF
C
cccccccccccc
      if(idb.ne.0) then
        if(ichk.ne.0.and.abs(hdep_ml(ii,jj)-hdepo).gt.gxb) then
c          write(6,14) ii,jj,hdep_ml(ii,jj),i,j,hdepo,hdep_ns(i,J)
 14       format(' #d moddep ii,jj,hdep_ml=',2i5,1p,d12.4,
     a           '     i,j,hdep_ns=',2i5,1p,2d12.4)
        end if
        if(ichk.eq.2.or.ichk.eq.3) then
          write(6,14) ii,jj,hdep_ml(ii,jj),i,j,hdepo,hdep_ns(i,J)
          if(kf_ml(ii,jj).ne.kfo)
     a    write(6,*) 'kf_ml,kf_ns=',kf_ml(ii,jj),kfo,kf_ns(i,j)
          write(6,*) 'hh_ml,hh_ns=',hh_ml(ii,jj),hh_ns(i,j)
        end if
      end if
cccccccccccc
  275     CONTINUE
  270     CONTINUE
C
  260   CONTINUE
  250 CONTINUE
C
      WRITE(6,*) 'DEPTH MODIFIED POINT =',KOUNT
      IF(KOUNT.EQ.0) GO TO 900
C
C ... 流体比率(GV)を変更
C
      DO 300 JJ = JSOU,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 300
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 310 II = IWES,IWES+NESTXM
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 310
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DO 320 J = J1,J2
          DO 325 I = I1,I2
C
            DO 330 K = 2,MZ_NS-1
              IF(KF_NS(I,J).EQ.MZ_NS) THEN
                INDP_NS(I,J,K) = 0
                GV_NS(I,J,K) = 1.0D0
              ELSE IF(HDEP_NS(I,J).LT.ZC_NS(1,K-1)) THEN
                INDP_NS(I,J,K) = 1
                GV_NS(I,J,K) = 1.0D0
              ELSE IF(HDEP_NS(I,J).GT.ZC_NS(1,K  )) THEN
                INDP_NS(I,J,K) = 0
                GV_NS(I,J,K) = 1.0D0
              ELSE
                GV_NS(I,J,K) = (ZC_NS(1,K)-HDEP_NS(I,J))*ZC_NS(6,K)
                IF(GV_NS(I,J,K).LT.GVMIN) THEN
                  INDP_NS(I,J,K) = 0
                  GV_NS(I,J,K) = 1.0D0
                ELSE
                  INDP_NS(I,J,K) = 1
                  GV_NS(I,J,K) = MIN(GV_NS(I,J,K),1.0D0)
                END IF
              END IF
  330       CONTINUE
C
  325     CONTINUE
  320     CONTINUE
  310   CONTINUE
  300 CONTINUE
C
      DO 350 JJ = JSOU,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 350
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 360 II = IEAS-NESTXP,IEAS
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 360
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DO 370 J = J1,J2
          DO 375 I = I1,I2
C
            DO 380 K = 2,MZ_NS-1
              IF(KF_NS(I,J).EQ.MZ_NS) THEN
                INDP_NS(I,J,K) = 0
                GV_NS(I,J,K) = 1.0D0
              ELSE IF(HDEP_NS(I,J).LT.ZC_NS(1,K-1)) THEN
                INDP_NS(I,J,K) = 1
                GV_NS(I,J,K) = 1.0D0
              ELSE IF(HDEP_NS(I,J).GT.ZC_NS(1,K  )) THEN
                INDP_NS(I,J,K) = 0
                GV_NS(I,J,K) = 1.0D0
              ELSE
                GV_NS(I,J,K) = (ZC_NS(1,K)-HDEP_NS(I,J))*ZC_NS(6,K)
                IF(GV_NS(I,J,K).LT.GVMIN) THEN
                  INDP_NS(I,J,K) = 0
                  GV_NS(I,J,K) = 1.0D0
                ELSE
                  INDP_NS(I,J,K) = 1
                  GV_NS(I,J,K) = MIN(GV_NS(I,J,K),1.0D0)
                END IF
              END IF
  380       CONTINUE
C
  375     CONTINUE
  370     CONTINUE
  360   CONTINUE
  350 CONTINUE
C
      DO 400 JJ = JSOU,JSOU+NESTYM
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 400
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 410 II = IWES+NESTXM+1,IEAS-NESTXP-1
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 410
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DO 420 J = J1,J2
          DO 425 I = I1,I2
C
            DO 430 K = 2,MZ_NS-1
              IF(KF_NS(I,J).EQ.MZ_NS) THEN
                INDP_NS(I,J,K) = 0
                GV_NS(I,J,K) = 1.0D0
              ELSE IF(HDEP_NS(I,J).LT.ZC_NS(1,K-1)) THEN
                INDP_NS(I,J,K) = 1
                GV_NS(I,J,K) = 1.0D0
              ELSE IF(HDEP_NS(I,J).GT.ZC_NS(1,K  )) THEN
                INDP_NS(I,J,K) = 0
                GV_NS(I,J,K) = 1.0D0
              ELSE
                GV_NS(I,J,K) = (ZC_NS(1,K)-HDEP_NS(I,J))*ZC_NS(6,K)
                IF(GV_NS(I,J,K).LT.GVMIN) THEN
                  INDP_NS(I,J,K) = 0
                  GV_NS(I,J,K) = 1.0D0
                ELSE
                  INDP_NS(I,J,K) = 1
                  GV_NS(I,J,K) = MIN(GV_NS(I,J,K),1.0D0)
                END IF
              END IF
  430       CONTINUE
C
  425     CONTINUE
  420     CONTINUE
  410   CONTINUE
  400 CONTINUE
C
      DO 450 JJ = JNOR-NESTYP,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 450
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 460 II = IWES+NESTXM+1,IEAS-NESTXP-1
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 460
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DO 470 J = J1,J2
          DO 475 I = I1,I2
C
            DO 480 K = 2,MZ_NS-1
              IF(KF_NS(I,J).EQ.MZ_NS) THEN
                INDP_NS(I,J,K) = 0
                GV_NS(I,J,K) = 1.0D0
              ELSE IF(HDEP_NS(I,J).LT.ZC_NS(1,K-1)) THEN
                INDP_NS(I,J,K) = 1
                GV_NS(I,J,K) = 1.0D0
              ELSE IF(HDEP_NS(I,J).GT.ZC_NS(1,K  )) THEN
                INDP_NS(I,J,K) = 0
                GV_NS(I,J,K) = 1.0D0
              ELSE
                GV_NS(I,J,K) = (ZC_NS(1,K)-HDEP_NS(I,J))*ZC_NS(6,K)
                IF(GV_NS(I,J,K).LT.GVMIN) THEN
                  INDP_NS(I,J,K) = 0
                  GV_NS(I,J,K) = 1.0D0
                ELSE
                  INDP_NS(I,J,K) = 1
                  GV_NS(I,J,K) = MIN(GV_NS(I,J,K),1.0D0)
                END IF
              END IF
  480       CONTINUE
C
  475     CONTINUE
  470     CONTINUE
  460   CONTINUE
  450 CONTINUE
C
C ... 面透過率(GX)を変更
C
      DO 500 JJ = JSOU,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 500
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DEPM = HDEP_ML(IWES-1,JJ)
        DO 510 II = IWES,IWES+NESTXM
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 510
          I1 = I_ML(1,II-1)+1
          IF(II.EQ.IWES) I1=I_ML(1,II-1)
          I2 = I_ML(1,II)
          DO 520 J = J1,J2
          DO 525 I = I1,I2
C
            DO 530 K = 2,MZ_NS-1
              IX=MIN(I+1,MX_NS)
              IF(INDP_NS(I,J,K).EQ.0.OR.INDP_NS(IX,J,K).EQ.0) THEN
                GX_NS(I,J,K) = 1.0D0
              ELSE
                GX_NS(I,J,K) = GV_NS(I,J,K)*XC_NS(7,I)
     $                       + GV_NS(IX,J,K)*XC_NS(8,I)
              END IF
              IF(I.EQ.2) THEN
                IF(INDP_NS(I,J,K).EQ.0) THEN
                  GX_NS(I-1,J,K) = 1.0D0
                ELSE
                  IF(DEPM.LT.ZC_NS(1,K-1)) THEN
                    GVM = 1.0D0
                    GX_NS(I-1,J,K) = GVM*XC_NS(7,I-1)
     $                             + GV_NS(I,J,K)*XC_NS(8,I-1)
                  ELSE IF(DEPM.GT.ZC_NS(1,K)) THEN
                    GX_NS(I-1,J,K) = 1.0D0
                  ELSE
                    GVM = (ZC_NS(1,K)-DEPM)*ZC_NS(6,K)
                    GX_NS(I-1,J,K) = GVM*XC_NS(7,I-1)
     $                             + GV_NS(I,J,K)*XC_NS(8,I-1)
                  END IF
                END IF
              END IF
  530       CONTINUE
C
  525     CONTINUE
  520     CONTINUE
  510   CONTINUE
  500 CONTINUE
C
      DO 550 JJ = JSOU,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 550
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DEPP = HDEP_ML(IEAS+1,JJ)
        DO 560 II = IEAS-NESTXP,IEAS
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 560
          I1 = I_ML(1,II-1)+1
          IF(II.EQ.IEAS-NESTXP) I1=I_ML(1,II-1)
          I2 = I_ML(1,II)
          DO 570 J = J1,J2
          DO 575 I = I1,I2
C
            DO 580 K = 2,MZ_NS-1
              IF(I.EQ.MX_NS-1) THEN
                IF(INDP_NS(I,J,K).EQ.0) THEN
                  GX_NS(I,J,K) = 1.0D0
                ELSE
                  IF(DEPP.LT.ZC_NS(1,K-1)) THEN
                    GVP = 1.0D0
                    GX_NS(I,J,K) = GV_NS(I,J,K)*XC_NS(7,I)
     $                           + GVP*XC_NS(8,I)
                  ELSE IF(DEPP.GT.ZC_NS(1,K)) THEN
                    GX_NS(I,J,K) = 1.0D0
                  ELSE
                    GVP = (ZC_NS(1,K)-DEPP)*ZC_NS(6,K)
                    GX_NS(I,J,K) = GV_NS(I,J,K)*XC_NS(7,I)
     $                           + GVP*XC_NS(8,I)
                  END IF
                END IF
              ELSE
                IX=MIN(I+1,MX_NS)
                IF(INDP_NS(I,J,K).EQ.0.OR.INDP_NS(IX,J,K).EQ.0) THEN
                  GX_NS(I,J,K) = 1.0D0
                ELSE
                  GX_NS(I,J,K) = GV_NS(I,J,K)*XC_NS(7,I)
     $                         + GV_NS(IX,J,K)*XC_NS(8,I)
                END IF
              END IF
  580       CONTINUE
C
  575     CONTINUE
  570     CONTINUE
  560   CONTINUE
  550 CONTINUE
C
      DO 600 JJ = JSOU,JSOU+NESTYM
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 600
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 610 II = IWES+NESTXM+1,IEAS-NESTXP-1
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 610
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          IF(II.EQ.IEAS-NESTXP-1) I1=I_ML(1,II-1)-1
          DO 620 J = J1,J2
          DO 625 I = I1,I2
C
            DO 630 K = 2,MZ_NS-1
              IX=MIN(I+1,MX_NS)
              IF(INDP_NS(I,J,K).EQ.0.OR.INDP_NS(IX,J,K).EQ.0) THEN
                GX_NS(I,J,K) = 1.0D0
              ELSE
                GX_NS(I,J,K) = GV_NS(I,J,K)*XC_NS(7,I)
     $                       + GV_NS(IX,J,K)*XC_NS(8,I)
              END IF
  630       CONTINUE
C
  625     CONTINUE
  620     CONTINUE
  610   CONTINUE
  600 CONTINUE
C
      DO 650 JJ = JNOR-NESTYP,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 650
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 660 II = IWES+NESTXM+1,IEAS-NESTXP-1
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 660
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          IF(II.EQ.IEAS-NESTXP-1) I1=I_ML(1,II-1)-1
          DO 670 J = J1,J2
          DO 675 I = I1,I2
C
            DO 680 K = 2,MZ_NS-1
              IX=MIN(I+1,MX_NS)
              IF(INDP_NS(I,J,K).EQ.0.OR.INDP_NS(IX,J,K).EQ.0) THEN
                GX_NS(I,J,K) = 1.0D0
              ELSE
                GX_NS(I,J,K) = GV_NS(I,J,K)*XC_NS(7,I)
     $                       + GV_NS(IX,J,K)*XC_NS(8,I)
              END IF
  680       CONTINUE
C
  675     CONTINUE
  670     CONTINUE
  660   CONTINUE
  650 CONTINUE
C
C ... 面透過率(GY)を変更
C
      DO 700 JJ = JSOU,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 700
        J1 = J_ML(1,JJ-1)+1
        IF(JJ.EQ.JSOU) J1=J_ML(1,JJ-1)
        J2 = J_ML(1,JJ)
        DO 710 II = IWES,IWES+NESTXM
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 710
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DEPM = HDEP_ML(II,JSOU-1)
          DEPP = HDEP_ML(II,JNOR+1)
          DO 720 J = J1,J2
          DO 725 I = I1,I2
C
            DO 730 K = 2,MZ_NS-1
              IF(J.EQ.MY_NS-1) THEN
                IF(INDP_NS(I,J,K).EQ.0) THEN
                  GY_NS(I,J,K) = 1.0D0
                ELSE
                  IF(DEPP.LT.ZC_NS(1,K-1)) THEN
                    GVP = 1.0D0
                    GY_NS(I,J,K) = GV_NS(I,J,K)*YC_NS(7,J)
     $                           + GVP*YC_NS(8,J)
                  ELSE IF(DEPP.GT.ZC_NS(1,K)) THEN
                    GY_NS(I,J,K) = 1.0D0
                  ELSE
                    GVP = (ZC_NS(1,K)-DEPP)*ZC_NS(6,K)
                    GY_NS(I,J,K) = GV_NS(I,J,K)*YC_NS(7,J)
     $                           + GVP*YC_NS(8,J)
                  END IF
                END IF
              ELSE
                JX=MIN(J+1,MY_NS)
                IF(INDP_NS(I,J,K).EQ.0.OR.INDP_NS(I,JX,K).EQ.0) THEN
                  GY_NS(I,J,K) = 1.0D0
                ELSE
                  GY_NS(I,J,K) = GV_NS(I,J,K)*YC_NS(7,J)
     $                         + GV_NS(I,JX,K)*YC_NS(8,J)
                END IF
              END IF
              IF(J.EQ.2) THEN
                IF(INDP_NS(I,J,K).EQ.0) THEN
                  GY_NS(I,J-1,K) = 1.0D0
                ELSE
                  IF(DEPM.LT.ZC_NS(1,K-1)) THEN
                    GVM = 1.0D0
                    GY_NS(I,J-1,K) = GVM*YC_NS(7,J-1)
     $                             + GV_NS(I,J,K)*YC_NS(8,J-1)
                  ELSE IF(DEPM.GT.ZC_NS(1,K)) THEN
                    GY_NS(I,J-1,K) = 1.0D0
                  ELSE
                    GVM = (ZC_NS(1,K)-DEPM)*ZC_NS(6,K)
                    GY_NS(I,J-1,K) = GVM*YC_NS(7,J-1)
     $                             + GV_NS(I,J,K)*YC_NS(8,J-1)
                  END IF
                END IF
              END IF
  730       CONTINUE
C
  725     CONTINUE
  720     CONTINUE
  710   CONTINUE
  700 CONTINUE
C
      DO 750 JJ = JSOU,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 750
        J1 = J_ML(1,JJ-1)+1
        IF(JJ.EQ.JSOU) J1=J_ML(1,JJ-1)
        J2 = J_ML(1,JJ)
        DO 760 II = IEAS-NESTXP,IEAS
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 760
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DEPM = HDEP_ML(II,JSOU-1)
          DEPP = HDEP_ML(II,JNOR+1)
          DO 770 J = J1,J2
          DO 775 I = I1,I2
C
            DO 780 K = 2,MZ_NS-1
              IF(J.EQ.MY_NS-1) THEN
                IF(INDP_NS(I,J,K).EQ.0) THEN
                  GY_NS(I,J,K) = 1.0D0
                ELSE
                  IF(DEPP.LT.ZC_NS(1,K-1)) THEN
                    GVP = 1.0D0
                    GY_NS(I,J,K) = GV_NS(I,J,K)*YC_NS(7,J)
     $                           + GVP*YC_NS(8,J)
                  ELSE IF(DEPP.GT.ZC_NS(1,K)) THEN
                    GY_NS(I,J,K) = 1.0D0
                  ELSE
                    GVP = (ZC_NS(1,K)-DEPP)*ZC_NS(6,K)
                    GY_NS(I,J,K) = GV_NS(I,J,K)*YC_NS(7,J)
     $                           + GVP*YC_NS(8,J)
                  END IF
                END IF
              ELSE
                JX=MIN(J+1,MY_NS)
                IF(INDP_NS(I,J,K).EQ.0.OR.INDP_NS(I,JX,K).EQ.0) THEN
                  GY_NS(I,J,K) = 1.0D0
                ELSE
                  GY_NS(I,J,K) = GV_NS(I,J,K)*YC_NS(7,J)
     $                         + GV_NS(I,JX,K)*YC_NS(8,J)
                END IF
              END IF
              IF(J.EQ.2) THEN
                IF(INDP_NS(I,J,K).EQ.0) THEN
                  GY_NS(I,J-1,K) = 1.0D0
                ELSE
                  IF(DEPM.LT.ZC_NS(1,K-1)) THEN
                    GVM = 1.0D0
                    GY_NS(I,J-1,K) = GVM*YC_NS(7,J-1)
     $                             + GV_NS(I,J,K)*YC_NS(8,J-1)
                  ELSE IF(DEPM.GT.ZC_NS(1,K)) THEN
                    GY_NS(I,J-1,K) = 1.0D0
                  ELSE
                    GVM = (ZC_NS(1,K)-DEPM)*ZC_NS(6,K)
                    GY_NS(I,J-1,K) = GVM*YC_NS(7,J-1)
     $                             + GV_NS(I,J,K)*YC_NS(8,J-1)
                  END IF
                END IF
              END IF
  780       CONTINUE
C
  775     CONTINUE
  770     CONTINUE
  760   CONTINUE
  750 CONTINUE
C
      DO 800 JJ = JSOU,JSOU+NESTYM
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 800
        J1 = J_ML(1,JJ-1)+1
        IF(JJ.EQ.JSOU) J1=J_ML(1,JJ-1)
        J2 = J_ML(1,JJ)
        DO 810 II = IWES+NESTXM+1,IEAS-NESTXP-1
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 810
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DEPM = HDEP_ML(II,JSOU-1)
          DO 820 J = J1,J2
          DO 825 I = I1,I2
C
            DO 830 K = 2,MZ_NS-1
              JX=MIN(J+1,MY_NS)
              IF(INDP_NS(I,J,K).EQ.0.OR.INDP_NS(I,JX,K).EQ.0) THEN
                GY_NS(I,J,K) = 1.0D0
              ELSE
                GY_NS(I,J,K) = GV_NS(I,J,K)*YC_NS(7,J)
     $                       + GV_NS(I,JX,K)*YC_NS(8,J)
              END IF
              IF(J.EQ.2) THEN
                IF(INDP_NS(I,J,K).EQ.0) THEN
                  GY_NS(I,J-1,K) = 1.0D0
                ELSE
                  IF(DEPM.LT.ZC_NS(1,K-1)) THEN
                    GVM = 1.0D0
                    GY_NS(I,J-1,K) = GVM*YC_NS(7,J-1)
     $                             + GV_NS(I,J,K)*YC_NS(8,J-1)
                  ELSE IF(DEPM.GT.ZC_NS(1,K)) THEN
                    GY_NS(I,J-1,K) = 1.0D0
                  ELSE
                    GVM = (ZC_NS(1,K)-DEPM)*ZC_NS(6,K)
                    GY_NS(I,J-1,K) = GVM*YC_NS(7,J-1)
     $                             + GV_NS(I,J,K)*YC_NS(8,J-1)
                  END IF
                END IF
              END IF
  830       CONTINUE
C
  825     CONTINUE
  820     CONTINUE
  810   CONTINUE
  800 CONTINUE
C
      DO 850 JJ = JNOR-NESTYP,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 850
        J1 = J_ML(1,JJ-1)+1
        IF(JJ.EQ.JNOR-NESTYP) J1=J_ML(1,JJ-1)
        J2 = J_ML(1,JJ)
        DO 860 II = IWES+NESTXM+1,IEAS-NESTXP-1
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 860
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DEPP = HDEP_ML(II,JNOR+1)
          DO 870 J = J1,J2
          DO 875 I = I1,I2
C
            DO 880 K = 2,MZ_NS-1
              IF(J.EQ.MY_NS-1) THEN
                IF(INDP_NS(I,J,K).EQ.0) THEN
                  GY_NS(I,J,K) = 1.0D0
                ELSE
                  IF(DEPP.LT.ZC_NS(1,K-1)) THEN
                    GVP = 1.0D0
                    GY_NS(I,J,K) = GV_NS(I,J,K)*YC_NS(7,J)
     $                           + GVP*YC_NS(8,J)
                  ELSE IF(DEPP.GT.ZC_NS(1,K)) THEN
                    GY_NS(I,J,K) = 1.0D0
                  ELSE
                    GVP = (ZC_NS(1,K)-DEPP)*ZC_NS(6,K)
                    GY_NS(I,J,K) = GV_NS(I,J,K)*YC_NS(7,J)
     $                           + GVP*YC_NS(8,J)
                  END IF
                END IF
              ELSE
                JX=MIN(J+1,MY_NS)
                IF(INDP_NS(I,J,K).EQ.0.OR.INDP_NS(I,JX,K).EQ.0) THEN
                  GY_NS(I,J,K) = 1.0D0
                ELSE
                  GY_NS(I,J,K) = GV_NS(I,J,K)*YC_NS(7,J)
     $                         + GV_NS(I,JX,K)*YC_NS(8,J)
                END IF
              END IF
  880       CONTINUE
C
  875     CONTINUE
  870     CONTINUE
  860   CONTINUE
  850 CONTINUE
C
C
      DO 1500 JJ = JSOU,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 1500
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DEPM = HDEP_ML(IWES-1,JJ)
        DO 1510 II = IWES,IWES+NESTXM
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 1510
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DO 1520 J = J1,J2
          DO 1525 I = I1,I2
            DO 1530 K = 2,MZ_NS-1
              IF(I.LE.MX_NS-1)THEN
              IF(INDP_NS(I,J,K).EQ.1.AND.INDP_NS(I+1,J,K).EQ.1)
     $        INDU_NS(I,J,K) = 1
              ENDIF
              IF(J.LE.MY_NS-1)THEN
              IF(INDP_NS(I,J,K).EQ.1.AND.INDP_NS(I,J+1,K).EQ.1)
     $        INDV_NS(I,J,K) = 1
              ENDIF
 1530       CONTINUE
 1525     CONTINUE
 1520     CONTINUE
 1510   CONTINUE
 1500 CONTINUE
C
      DO 1550 JJ = JSOU,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 1550
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DEPP = HDEP_ML(IEAS+1,JJ)
        DO 1560 II = IEAS-NESTXP,IEAS
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 1560
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DO 1570 J = J1,J2
          DO 1575 I = I1,I2
            DO 1580 K = 2,MZ_NS-1
              IF(I.GE.2)THEN
              IF(INDP_NS(I-1,J,K).EQ.1.AND.INDP_NS(I,J,K).EQ.1)
     $        INDU_NS(I-1,J,K) = 1
              ENDIF
              IF(J.LE.MY_NS-1)THEN
              IF(INDP_NS(I,J,K).EQ.1.AND.INDP_NS(I,J+1,K).EQ.1)
     $        INDV_NS(I,J,K) = 1
              ENDIF
 1580       CONTINUE
 1575     CONTINUE
 1570     CONTINUE
 1560   CONTINUE
 1550 CONTINUE
C
      DO 1600 JJ = JSOU,JSOU+NESTYM
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 1600
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 1610 II = IWES+NESTXM+1,IEAS-NESTXP-1
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 1610
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DO 1620 J = J1,J2
          DO 1625 I = I1,I2
            DO 1630 K = 2,MZ_NS-1
              IF(I.LE.MX_NS-1)THEN
              IF(INDP_NS(I,J,K).EQ.1.AND.INDP_NS(I+1,J,K).EQ.1)
     $        INDU_NS(I,J,K) = 1
              ENDIF
              IF(J.LE.MY_NS-1)THEN
              IF(INDP_NS(I,J,K).EQ.1.AND.INDP_NS(I,J+1,K).EQ.1)
     $        INDV_NS(I,J,K) = 1
              ENDIF
 1630       CONTINUE
 1625     CONTINUE
 1620     CONTINUE
 1610   CONTINUE
 1600 CONTINUE
C
      DO 1650 JJ = JNOR-NESTYP,JNOR
        IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 1650
        J1 = J_ML(1,JJ-1)+1
        J2 = J_ML(1,JJ)
        DO 1660 II = IWES+NESTXM+1,IEAS-NESTXP-1
          IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 1660
          I1 = I_ML(1,II-1)+1
          I2 = I_ML(1,II)
          DO 1670 J = J1,J2
          DO 1675 I = I1,I2
            DO 1680 K = 2,MZ_NS-1
              IF(I.LE.MX_NS-1)THEN
              IF(INDP_NS(I,J,K).EQ.1.AND.INDP_NS(I+1,J,K).EQ.1)
     $        INDU_NS(I,J,K) = 1
              ENDIF
              IF(J.GE.2)THEN
              IF(INDP_NS(I,J-1,K).EQ.1.AND.INDP_NS(I,J,K).EQ.1)
     $        INDV_NS(I,J-1,K) = 1
              ENDIF
 1680       CONTINUE
 1675     CONTINUE
 1670     CONTINUE
 1660   CONTINUE
 1650 CONTINUE
C
C
C
  900 CONTINUE
C
      DO K=2,MZ_NS-1
      DO J=2,MY_NS-1
      DO I=1,MX_NS-1
         IF( INDP_NS(I,J,K).EQ.0 .AND. INDP_NS(I+1,J,K).EQ.0 ) THEN
            INDU_NS(I,J,K) = -4
         ELSE IF( INDP_NS(I,J,K).EQ.0 .OR. INDP_NS(I+1,J,K).EQ.0 ) THEN
            IF( INDU_NS(I,J,K).EQ.1 ) INDU_NS(I,J,K) = -2
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C
      DO K=2,MZ_NS-1
      DO J=1,MY_NS-1
      DO I=2,MX_NS-1
         IF( INDP_NS(I,J,K).EQ.0 .AND. INDP_NS(I,J+1,K).EQ.0 ) THEN
            INDV_NS(I,J,K) = -4
         ELSE IF( INDP_NS(I,J,K).EQ.0 .OR. INDP_NS(I,J+1,K).EQ.0 ) THEN
            IF( INDV_NS(I,J,K).EQ.1 ) INDV_NS(I,J,K) = -2
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C
      DO K=1,MZ_NS-1
      DO J=2,MY_NS-1
      DO I=2,MX_NS-1
         IF( INDP_NS(I,J,K).EQ.0 .AND. INDP_NS(I,J,K+1).EQ.0 ) THEN
            INDW_NS(I,J,K) = -4
         ELSE IF( INDP_NS(I,J,K).EQ.0 .OR. INDP_NS(I,J,K+1).EQ.0 ) THEN
            IF( INDW_NS(I,J,K).EQ.1 ) INDW_NS(I,J,K) = -2
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C
      RETURN
      END
