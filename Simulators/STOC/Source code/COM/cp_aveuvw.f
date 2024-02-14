      SUBROUTINE CP_AVEUVW(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                     INDU_NS,INDV_NS,INDW_NS,INDP_NS,
     2                     XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     3                     GX_ML,GY_ML,GZ_ML,GX_NS,GY_NS,GZ_NS,GV_NS,
     4                     I_ML,J_ML,K_ML,KF_ML,KG_ML,KF_NS,KG_NS,
     5                     UU_NS,VV_NS,WW_NS,TT_NS,CC_NS,HH_NS,HDEP_NS,
     6                     UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                     CSD_NS,ZBD_NS,CSD_ML,ZBD_ML,
     $                     X1_NS,X2_NS,X1_ML,X2_ML,
     7                     I1,I2,J1,J2,
     8                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     9                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
C-----------------------------------------------------------------------
C     NSの平均流速をMLのエリアに直接セットする
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'TIMEI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'VVMAX.h'
C
      INTEGER,INTENT(INOUT)::I1,I2,J1,J2
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR,KBOT,KTOP
C
      INTEGER,INTENT(INOUT)::
     $   INDU_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDV_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDW_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDU_NS(MX_NS,MY_NS,MZ_NS),INDV_NS(MX_NS,MY_NS,MZ_NS),
     $   INDW_NS(MX_NS,MY_NS,MZ_NS),INDP_NS(MX_NS,MY_NS,MZ_NS)
C
      REAL(8),INTENT(INOUT)::
     $   XC_ML(8,MX_ML),YC_ML(8,MY_ML),ZC_ML(8,MZ_ML),
     $   XC_NS(8,MX_NS),YC_NS(8,MY_NS),ZC_NS(8,MZ_NS)
      REAL(8),INTENT(INOUT)::
     $   GX_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GY_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GZ_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GX_NS(MX_NS,MY_NS,MZ_NS),GY_NS(MX_NS,MY_NS,MZ_NS),
     $   GZ_NS(MX_NS,MY_NS,MZ_NS),GV_NS(MX_NS,MY_NS,MZ_NS)
C
      INTEGER,INTENT(INOUT)::
     $   I_ML(2,MX_ML),J_ML(2,MY_ML),K_ML(2,MZ_ML)
      INTEGER,INTENT(INOUT)::
     $   KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   KG_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   KF_NS(MX_NS,MY_NS),KG_NS(MX_NS,MY_NS)
C
      REAL(8),INTENT(INOUT)::
     $   UU_NS(MX_NS,MY_NS,MZ_NS),VV_NS(MX_NS,MY_NS,MZ_NS),
     $   WW_NS(MX_NS,MY_NS,MZ_NS),
     $   TT_NS(MX_NS,MY_NS,MZ_NS),CC_NS(MX_NS,MY_NS,MZ_NS),
     $   CSD_NS(MX_NS,MY_NS,MZ_NS),ZBD_NS(MX_NS,MY_NS),
     $   X1_NS(MX_NS,MY_NS,MZ_NS),X2_NS(MX_NS,MY_NS,MZ_NS),
     $   HH_NS(MX_NS,MY_NS),HDEP_NS(MX_NS,MY_NS)
      REAL(8),INTENT(INOUT)::
     $   UU_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   VV_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   WW_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   TT_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   CC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   CSD_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   ZBD_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   X1_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   X2_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   HH_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   HDEP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
C
      INTEGER::IDB=0
C
      REAL(8)::CSUM,DEP,DEPKG2,DEPX,DEPY,HH1,HZ1,HZ2,HZK
      REAL(8)::SS,SSML,SSUM,TSUM,USUM,VOL,VSUM,WSUM
      REAL(8)::X1SUM,X2SUM
      INTEGER::I0,II,II1,II2,IXX,J0,JJ,JJ1,JJ2
      INTEGER::K,K1,K2,KF1,KF2,KG1,KG2,KK,KK1,KK2
C
      IXX = 0
C
      I0 = I1
      IF(I1.NE.I2) I0=I2
      II = I_ML(1,I0)
      J0 = J1
      IF(J1.NE.J2) J0=J2
      JJ = J_ML(1,J0)
C
CCCC      IF(I1.NE.I2.AND.J1.NE.J2) GO TO 350
C
C     平均流速(U)を計算
C
      KG1 = MIN(KG_ML(I0,J1),KG_ML(I0+1,J1))
      KF1 = MIN(MAX(KF_ML(I0,J1),KF_ML(I0+1,J1)),MZ_ML-1)
cc      write(*,*) 'uuu kg1,kf1,i0,j1=',kg1,kf1,i0,j1
      IF(KF1.NE.MZ_ML) THEN
        JJ1 = J_ML(1,J1-1)+1
        JJ2 = J_ML(1,J1)
C
        DO 100 K=KG1,KF1-1
          IF(INDU_ML(I0,J1,K).LE.-2) GO TO 100
          K1 = K_ML(1,K-1)+1
          K2 = K_ML(1,K)
C
          USUM = 0.0D0
          SSUM = 0.0D0
          DO 110 JJ=JJ1,JJ2
            DO 120 KK=K1,K2
              IF(INDU_NS(II,JJ,KK).GE.-1) THEN
                SS = GX_NS(II,JJ,KK)*YC_NS(4,JJ)*ZC_NS(4,KK)
                SSUM = SSUM+SS
                USUM = USUM+SS*UU_NS(II,JJ,KK)
              END IF
  120       CONTINUE
  110     CONTINUE
C
          SSML = GX_ML(I0,J1,K)*YC_ML(4,J1)*ZC_ML(4,K)
          UU_ML(I0,J1,K) = USUM/SSML
cc       if(idb.ne.0.and.k.eq.11) write(*,1) i0,j1,k,uu_ml(i0,j1,k)
 1     format('aveuvw(U) i,j,k,UU_ML=',3i5,1pd25.15)
  100   CONTINUE
C
        DO 150 K=KF1,KF1
cc      write(*,*) 'i0,j1,k,indu=',i0,j1,k,indu_ml(i0,j1,k)
          IF(INDU_ML(I0,J1,K).LE.-2) GO TO 150
          K1 = K_ML(1,K-1)+1
          K2 = K_ML(1,K)
C
          USUM = 0.0D0
          SSUM = 0.0D0
          DO 160 JJ=JJ1,JJ2
            HH1 = HH_NS(II,JJ)*XC_NS(7,II)+HH_NS(II+1,JJ)*XC_NS(8,II)
            DEP = HDEP_NS(II  ,JJ)*XC_NS(7,II)
     $          + HDEP_NS(II+1,JJ)*XC_NS(8,II)
            KF2 = MAX(KF_NS(II,JJ),KF_NS(II+1,JJ))
            KG2 = MAX(KG_NS(II,JJ),KG_NS(II+1,JJ))
            DO 170 KK=K1,K2
cc       write(*,*) 'u ii,jj,kk,indu=',ii,jj,kk,indu_ns(ii,jj,kk)
              IF(KK.GT.KF2) GO TO 170
              IF(INDU_NS(II,JJ,KK).GE.-1) THEN
                HZ1 = MIN(ZC_NS(1,KK),HH1)
                HZ2 = MAX(ZC_NS(1,KK-1),DEP)
                DEPX = ZC_NS(1,KK)-GX_NS(II,JJ,KK)*ZC_NS(4,KK)
                IF(DEPX.GT.DEP) HZ2=MAX(ZC_NS(1,KK-1),DEPX)
                SS  = YC_NS(4,JJ)*(HZ1-HZ2)
                IF(HZ1-HZ2.LT.GXB) SS=0.0D0
                IF(KK.NE.KG2) SS=SS*GX_NS(II,JJ,KK)
                SSUM = SSUM+SS
                USUM = USUM+SS*UU_NS(II,JJ,KK)
cc       write(*,4) ss,ssum,usum,hz,uu_ns(ii,jj,kk)
 4     format('ss,ssum,usum,hz,uu_ns=',1p,5d13.5)
              END IF
  170       CONTINUE
  160     CONTINUE
C
          HZ1 = HH_ML(I0,J1)*XC_ML(7,I0)+HH_ML(I0+1,J1)*XC_ML(8,I0)
          DEP = HDEP_ML(I0,J1)*XC_ML(7,I0)+HDEP_ML(I0+1,J1)*XC_ML(8,I0)
          DEPX = ZC_ML(1,K)-GX_ML(I0,J1,K)*ZC_ML(4,K)
          IF(DEPX.GT.DEP) DEP=DEPX
          IF(IXX.EQ.0) THEN
            HZ2 = MAX(ZC_ML(1,K-1),DEP)
          ELSE
            DEPKG2 = MAX(HDEP_NS(II,JJ),HDEP_NS(II+1,JJ))
            HZK = MAX(ZC_NS(1,KG2-1),DEPKG2)
            HZ2 = MAX(ZC_ML(1,K-1),DEP,HZK)
          END IF
          SSML  = YC_ML(4,J1)*(HZ1-HZ2)
          IF(K.NE.KG1) SSML=SSML*GX_ML(I0,J1,K)
cc       write(*,*) 'uu_ml hz,ssml=',hz,ssml
          IF(SSML.GE.GXB*YC_ML(4,J1)) THEN
            UU_ML(I0,J1,K) = USUM/SSML
            IF(ABS(UU_ML(I0,J1,K)).GT.VVMAX) THEN
      write(6,*) 'istep=',istep
      write(6,1) i0,j1,k,uu_ml(i0,j1,k)
              UU_ML(I0,J1,K) = SIGN(VVMAX,UU_ML(I0,J1,K))
            END IF
       if(idb.ne.0) write(*,1) i0,j1,k,uu_ml(i0,j1,k)
       if(idb.ne.0.and.k.eq.11) write(*,1) i0,j1,k,uu_ml(i0,j1,k)
          ELSE
            UU_ML(I0,J1,K) = 0.0D0
          END IF
  150   CONTINUE
      END IF
C
C     平均流速(V)を計算
C
      JJ  = J_ML(1,J0)
      KG1 = MIN(KG_ML(I1,J0),KG_ML(I1,J0+1))
      KF1 = MIN(MAX(KF_ML(I1,J0),KF_ML(I1,J0+1)),MZ_ML-1)
cc      write(*,*) 'vvv kg1,kf1,i1,j0=',kg1,kf1,i1,j0
      IF(KF1.NE.MZ_ML) THEN
        II1 = I_ML(1,I1-1)+1
        II2 = I_ML(1,I1)
C
        DO 200 K=KG1,KF1-1
          IF(INDV_ML(I1,J0,K).LE.-2) GO TO 200
          K1 = K_ML(1,K-1)+1
          K2 = K_ML(1,K)
C
          VSUM = 0.0D0
          SSUM = 0.0D0
          DO 210 II=II1,II2
            DO 220 KK=K1,K2
              IF(INDV_NS(II,JJ,KK).GE.-1) THEN
                SS = GY_NS(II,JJ,KK)*XC_NS(4,II)*ZC_NS(4,KK)
                SSUM = SSUM+SS
                VSUM = VSUM+SS*VV_NS(II,JJ,KK)
              END IF
  220       CONTINUE
  210     CONTINUE
C
          SSML = GY_ML(I1,J0,K)*XC_ML(4,I1)*ZC_ML(4,K)
          VV_ML(I1,J0,K) = VSUM/SSML
cc       if(idb.ne.0.and.k.eq.11) write(*,2) i1,j0,k,vv_ml(i1,j0,k)
 2     format('aveuvw(V) i,j,k,VV_ML=',3i5,1pd15.5)
  200   CONTINUE
C
        DO 250 K=KF1,KF1
cc      write(*,*) 'i1,j0,k,indv=',i1,j0,k,indv_ml(i1,j0,k)
          IF(INDV_ML(I1,J0,K).LE.-2) GO TO 250
          K1 = K_ML(1,K-1)+1
          K2 = K_ML(1,K)
C
          VSUM = 0.0D0
          SSUM = 0.0D0
          DO 260 II=II1,II2
            HH1 = HH_NS(II,JJ)*YC_NS(7,JJ)+HH_NS(II,JJ+1)*YC_NS(8,JJ)
            DEP = HDEP_NS(II,JJ  )*YC_NS(7,JJ)
     $          + HDEP_NS(II,JJ+1)*YC_NS(8,JJ)
            KF2 = MAX(KF_NS(II,JJ),KF_NS(II,JJ+1))
            KG2 = MAX(KG_NS(II,JJ),KG_NS(II,JJ+1))
            DO 270 KK=K1,K2
cc      write(*,*) 'v ii,jj,kk,indv=',ii,jj,kk,indv_ns(ii,jj,kk)
              IF(KK.GT.KF2) GO TO 270
              IF(INDV_NS(II,JJ,KK).GE.-1) THEN
                HZ1 = MIN(ZC_NS(1,KK),HH1)
                HZ2 = MAX(ZC_NS(1,KK-1),DEP)
                DEPY = ZC_NS(1,KK)-GY_NS(II,JJ,KK)*ZC_NS(4,KK)
                IF(DEPY.GT.DEP) HZ2=MAX(ZC_NS(1,KK-1),DEPY)
                SS  = XC_NS(4,II)*(HZ1-HZ2)
                IF(HZ1-HZ2.LT.GXB) SS=0.0D0
                IF(KK.NE.KG2) SS=SS*GY_NS(II,JJ,KK)
                SSUM = SSUM+SS
                VSUM = VSUM+SS*VV_NS(II,JJ,KK)
cc       write(*,5) ss,ssum,vsum,hz,vv_ns(ii,jj,kk)
 5     format('ss,ssum,usum,hz,vv_ns=',1p,5d13.5)
              END IF
  270       CONTINUE
  260     CONTINUE
C
          HZ1 = HH_ML(I1,J0)*YC_ML(7,J0)+HH_ML(I1,J0+1)*YC_ML(8,J0)
          DEP = HDEP_ML(I1,J0)*YC_ML(7,J0)+HDEP_ML(I1,J0+1)*YC_ML(8,J0)
          DEPY = ZC_ML(1,K)-GY_ML(I1,J0,K)*ZC_ML(4,K)
          IF(DEPY.GT.DEP) DEP=DEPY
          IF(IXX.EQ.0) THEN
            HZ2 = MAX(ZC_ML(1,K-1),DEP)
          ELSE
            DEPKG2 = MAX(HDEP_NS(II,JJ),HDEP_NS(II,JJ+1))
            HZK = MAX(ZC_NS(1,KG2-1),DEPKG2)
            HZ2 = MAX(ZC_ML(1,K-1),DEP,HZK)
          END IF
          SSML = XC_ML(4,I1)*(HZ1-HZ2)
          IF(K.NE.KG1) SSML=SSML*GY_ML(I1,J0,K)
cc       write(*,*) 'vv_ml hz,ssml=',hz,ssml
          IF(SSML.GE.GXB*XC_ML(4,I1)) THEN
            VV_ML(I1,J0,K) = VSUM/SSML
            IF(ABS(VV_ML(I1,J0,K)).GT.VVMAX) THEN
      write(6,*) 'istep=',istep
      write(6,2) i1,j0,k,vv_ml(i1,j0,k)
              VV_ML(I1,J0,K) = SIGN(VVMAX,VV_ML(I1,J0,K))
            END IF
       if(idb.ne.0) write(*,2) i1,j0,k,vv_ml(i1,j0,k)
       if(idb.ne.0.and.k.eq.11) write(*,2) i1,j0,k,vv_ml(i1,j0,k)
          ELSE
            VV_ML(I1,J0,K) = 0.0D0
          END IF
  250   CONTINUE
      END IF
C
C     平均流速(W)を計算
C
  350 CONTINUE
      IF(KF_ML(I1,J1).NE.MZ_ML) THEN
        II1 = I_ML(1,I1-1)+1
        II2 = I_ML(1,I1)
        JJ1 = J_ML(1,J1-1)+1
        JJ2 = J_ML(1,J1)
        KG1 = KG_ML(I1,J1)
        KF1 = KF_ML(I1,J1)
        DO 300 K=KG1,KF1
cc       write(*,*) 'i1,j1,k,indw=',i1,j1,k,indw_ml(i1,j1,k)
          IF(INDW_ML(I1,J1,K).LE.-2) GO TO 300
C
          KK = K_ML(1,K)
          WSUM = 0.0D0
          SSUM = 0.0D0
          DO 310 JJ=JJ1,JJ2
            DO 320 II=II1,II2
cc       write(*,*) 'ww ii,jj,kk,k=',ii,jj,kk,k
             IF(KF_NS(II,JJ).EQ.MZ_NS) GO TO 320
C
              IF(INDW_NS(II,JJ,KK).GE.-1) THEN
                SS = GZ_NS(II,JJ,KK)*XC_NS(4,II)*YC_NS(4,JJ)
                SSUM = SSUM+SS
                WSUM = WSUM+SS*WW_NS(II,JJ,KK)
cc       write(*,6) ss,ssum,wsum,ww_ns(ii,jj,kk)
 6     format('ss,ssum,usum,ww_ns=',1p,4d13.5)
              END IF
  320       CONTINUE
  310     CONTINUE
C
          SSML = GZ_ML(I1,J1,K)*XC_ML(4,I1)*YC_ML(4,J1)
          WW_ML(I1,J1,K) = WSUM/SSML
c////////////////////////////////
c      W-LIMIT by HONDA (081021)
c      START
c////////////////////////////////
            IF(ABS(WW_ML(I1,J1,K)).GT.VVMAX) THEN
              WW_ML(I1,J1,K) = SIGN(VVMAX,WW_ML(I1,J1,K))
            END IF
c////////////////////////////////
c      W-LIMIT by HONDA (081021)
c      END
c////////////////////////////////
cc       write(*,*) 'ww_ml ssml=',ssml
       if(idb.ne.0) write(*,3) i1,j1,k,ww_ml(i1,j1,k)
 3     format('aveuvw(W) i,j,k,WW_ML=',3i5,1pd12.5)
  300   CONTINUE
      END IF
C
C     平均温度(T)を計算
C
      IF(LTEMP.EQ.1.AND.KF_ML(I1,J1).NE.MZ_ML) THEN
        II1 = I_ML(1,I1-1)+1
        II2 = I_ML(1,I1)
        JJ1 = J_ML(1,J1-1)+1
        JJ2 = J_ML(1,J1)
        KG1 = KG_ML(I1,J1)
        KF1 = KF_ML(I1,J1)
        DO 400 K=KG1,KF1
          IF(INDP_ML(I1,J1,K).EQ.0) GO TO 400
C
          KK1 = K_ML(1,K-1)+1
          KK2 = K_ML(1,K)
          TSUM = 0.0D0
          VSUM = 0.0D0
          DO 410 JJ=JJ1,JJ2
            DO 420 II=II1,II2
              IF(KF_NS(II,JJ).EQ.MZ_NS) GO TO 420
              K1 = MAX(KK1,KG_NS(II,JJ))
              K2 = MIN(KK2,KF_NS(II,JJ))
C
              SS = XC_NS(4,II)*YC_NS(4,JJ)
              DO 430 KK=K1,K2
                IF(INDP_NS(II,JJ,KK).EQ.1) THEN
                  HZ1 = MAX(HDEP_NS(II,JJ),ZC_NS(1,KK-1))
                  HZ2 = MIN(HH_NS(II,JJ),ZC_NS(1,KK))
                  VOL = GV_NS(II,JJ,KK)*SS*(HZ2-HZ1)
                  IF(KK.EQ.KG_NS(II,JJ)) THEN
                    VOL = SS*(HZ2-HZ1)
                  END IF
                  VSUM = VSUM+VOL
                  TSUM = TSUM+VOL*TT_NS(II,JJ,KK)
                END IF
  430         CONTINUE
  420       CONTINUE
  410     CONTINUE
C
          IF(VSUM.NE.0.0D0) THEN
            TT_ML(I1,J1,K) = TSUM/VSUM
          END IF
c       write(*,7) i1,j1,k,tt_ml(i1,j1,k)
 7     format('aveuvw(T) i,j,k,CC_ML=',3i5,1pd12.5)
  400   CONTINUE
      END IF
C
C     平均濃度(C)を計算
C
      IF(LCONC.EQ.1.AND.KF_ML(I1,J1).NE.MZ_ML) THEN
        II1 = I_ML(1,I1-1)+1
        II2 = I_ML(1,I1)
        JJ1 = J_ML(1,J1-1)+1
        JJ2 = J_ML(1,J1)
        KG1 = KG_ML(I1,J1)
        KF1 = KF_ML(I1,J1)
        DO 500 K=KG1,KF1
          IF(INDP_ML(I1,J1,K).EQ.0) GO TO 500
C
          KK1 = K_ML(1,K-1)+1
          KK2 = K_ML(1,K)
          CSUM = 0.0D0
          VSUM = 0.0D0
          DO 510 JJ=JJ1,JJ2
            DO 520 II=II1,II2
              IF(KF_NS(II,JJ).EQ.MZ_NS) GO TO 520
              K1 = MAX(KK1,KG_NS(II,JJ))
              K2 = MIN(KK2,KF_NS(II,JJ))
C
              SS = XC_NS(4,II)*YC_NS(4,JJ)
              DO 530 KK=K1,K2
                IF(INDP_NS(II,JJ,KK).EQ.1) THEN
                  HZ1 = MAX(HDEP_NS(II,JJ),ZC_NS(1,KK-1))
                  HZ2 = MIN(HH_NS(II,JJ),ZC_NS(1,KK))
                  VOL = GV_NS(II,JJ,KK)*SS*(HZ2-HZ1)
                  IF(KK.EQ.KG_NS(II,JJ)) THEN
                    VOL = SS*(HZ2-HZ1)
                  END IF
                  VSUM = VSUM+VOL
                  CSUM = CSUM+VOL*CC_NS(II,JJ,KK)
                END IF
  530         CONTINUE
  520       CONTINUE
  510     CONTINUE
C
          IF(VSUM.NE.0.0D0) THEN
            CC_ML(I1,J1,K) = CSUM/VSUM
          END IF
c       write(*,8) i1,j1,k,cc_ml(i1,j1,k)
 8     format('aveuvw(C) i,j,k,CC_ML=',3i5,1pd12.5)
  500   CONTINUE
      END IF
C
C     乱流量(X1,X2)を計算
C
      IF(LTURB.EQ.3.AND.KF_ML(I1,J1).NE.MZ_ML) THEN
        II1 = I_ML(1,I1-1)+1
        II2 = I_ML(1,I1)
        JJ1 = J_ML(1,J1-1)+1
        JJ2 = J_ML(1,J1)
        KG1 = KG_ML(I1,J1)
        KF1 = KF_ML(I1,J1)
        DO 600 K=KG1,KF1
          IF(INDP_ML(I1,J1,K).EQ.0) GO TO 600
C
          KK1 = K_ML(1,K-1)+1
          KK2 = K_ML(1,K)
          X1SUM = 0.0D0
          X2SUM = 0.0D0
          VSUM = 0.0D0
          DO 610 JJ=JJ1,JJ2
            DO 620 II=II1,II2
              IF(KF_NS(II,JJ).EQ.MZ_NS) GO TO 620
              K1 = MAX(KK1,KG_NS(II,JJ))
              K2 = MIN(KK2,KF_NS(II,JJ))
C
              SS = XC_NS(4,II)*YC_NS(4,JJ)
              DO 630 KK=K1,K2
                IF(INDP_NS(II,JJ,KK).EQ.1) THEN
                  HZ1 = MAX(HDEP_NS(II,JJ),ZC_NS(1,KK-1))
                  HZ2 = MIN(HH_NS(II,JJ),ZC_NS(1,KK))
                  VOL = GV_NS(II,JJ,KK)*SS*(HZ2-HZ1)
                  IF(KK.EQ.KG_NS(II,JJ)) THEN
                    VOL = SS*(HZ2-HZ1)
                  END IF
                  VSUM = VSUM+VOL
                  X1SUM = X1SUM+VOL*X1_NS(II,JJ,KK)
                  X2SUM = X2SUM+VOL*X2_NS(II,JJ,KK)
                END IF
  630         CONTINUE
  620       CONTINUE
  610     CONTINUE
C
          IF(VSUM.NE.0.0D0) THEN
            X1_ML(I1,J1,K) = X1SUM/VSUM
            X2_ML(I1,J1,K) = X2SUM/VSUM
          END IF
c       write(*,9) i1,j1,k,x1_ml(i1,j1,k),x2_ml(i1,j1,k)
 9     format('aveuvw(x) i,j,k,x1_ml,x2_ml=',3i5,1p,2d12.5)
  600   CONTINUE
      END IF
C
C     乱流量(X1)を計算
C
      IF(LTURB.EQ.3.AND.KF_ML(I1,J1).NE.MZ_ML) THEN
        II1 = I_ML(1,I1-1)+1
        II2 = I_ML(1,I1)
        JJ1 = J_ML(1,J1-1)+1
        JJ2 = J_ML(1,J1)
        KG1 = KG_ML(I1,J1)
        KF1 = KF_ML(I1,J1)
        DO 700 K=KG1,KF1
          IF(INDP_ML(I1,J1,K).EQ.0) GO TO 700
C
          KK1 = K_ML(1,K-1)+1
          KK2 = K_ML(1,K)
          X1SUM = 0.0D0
          VSUM = 0.0D0
          DO 710 JJ=JJ1,JJ2
            DO 720 II=II1,II2
              IF(KF_NS(II,JJ).EQ.MZ_NS) GO TO 720
              K1 = MAX(KK1,KG_NS(II,JJ))
              K2 = MIN(KK2,KF_NS(II,JJ))
C
              SS = XC_NS(4,II)*YC_NS(4,JJ)
              DO 730 KK=K1,K2
                IF(INDP_NS(II,JJ,KK).EQ.1) THEN
                  HZ1 = MAX(HDEP_NS(II,JJ),ZC_NS(1,KK-1))
                  HZ2 = MIN(HH_NS(II,JJ),ZC_NS(1,KK))
                  VOL = GV_NS(II,JJ,KK)*SS*(HZ2-HZ1)
                  IF(KK.EQ.KG_NS(II,JJ)) THEN
                    VOL = SS*(HZ2-HZ1) 
                 END IF
                  VSUM = VSUM+VOL
                  X1SUM = X1SUM+VOL*X1_NS(II,JJ,KK)
                END IF
  730         CONTINUE
  720       CONTINUE
  710     CONTINUE
C
          IF(VSUM.NE.0.0D0) THEN
            X1_ML(I1,J1,K) = X1SUM/VSUM
          END IF
c       write(*,9) i1,j1,k,x1_ml(i1,j1,k)
  700   CONTINUE
      END IF
C
C     平均浮遊砂濃度(CSD)／平均掃流砂厚さ(ZBD)を計算
C
      IF(LSEDI.EQ.1.AND.KF_ML(I1,J1).NE.MZ_ML) THEN
        II1 = I_ML(1,I1-1)+1
        II2 = I_ML(1,I1)
        JJ1 = J_ML(1,J1-1)+1
        JJ2 = J_ML(1,J1)
        KG1 = KG_ML(I1,J1)
        KF1 = KF_ML(I1,J1)
        DO 800 K=KG1,KF1
          IF(INDP_ML(I1,J1,K).EQ.0) GO TO 800
C
          KK1 = K_ML(1,K-1)+1
          KK2 = K_ML(1,K)
          CSUM = 0.0D0
          VSUM = 0.0D0
          DO 810 JJ=JJ1,JJ2
            DO 820 II=II1,II2
              IF(KF_NS(II,JJ).EQ.MZ_NS) GO TO 820
              K1 = MAX(KK1,KG_NS(II,JJ))
              K2 = MIN(KK2,KF_NS(II,JJ))
C
              SS = XC_NS(4,II)*YC_NS(4,JJ)
              DO 830 KK=K1,K2
                IF(INDP_NS(II,JJ,KK).EQ.1) THEN
                  HZ1 = MAX(HDEP_NS(II,JJ),ZC_NS(1,KK-1))
                  HZ2 = MIN(HH_NS(II,JJ),ZC_NS(1,KK))
                  VOL = GV_NS(II,JJ,KK)*SS*(HZ2-HZ1)
                  IF(KK.EQ.KG_NS(II,JJ)) THEN
                    VOL = SS*(HZ2-HZ1)
                  END IF
                  VSUM = VSUM+VOL
                  CSUM = CSUM+VOL*CSD_NS(II,JJ,KK)
                END IF
  830         CONTINUE
  820       CONTINUE
  810     CONTINUE
C
          IF(VSUM.NE.0.0D0) THEN
            CSD_ML(I1,J1,K) = CSUM/VSUM
          END IF
  800   CONTINUE
C
        CSUM = 0.0D0
        VSUM = 0.0D0
        DO 840 JJ=JJ1,JJ2
        DO 840 II=II1,II2
           IF(KF_NS(II,JJ).EQ.MZ_NS) CYCLE
           SS = XC_NS(4,II)*YC_NS(4,JJ)
           VSUM = VSUM+SS
           CSUM = CSUM+SS*ZBD_NS(II,JJ)
  840   CONTINUE
C
        IF(VSUM.NE.0.0D0) THEN
           ZBD_ML(I1,J1) = CSUM/VSUM
        END IF
C
      END IF
C
      RETURN
      END
