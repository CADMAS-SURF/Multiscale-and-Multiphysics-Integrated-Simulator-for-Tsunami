      SUBROUTINE MODHUV(HU,HV,UU,VV,FF,GX,GY,GV,XC,YC,ZC,YCOSP,
     $                  INDU,INDV,LLWALB,KF)
C======================================================================
C     HU,HVを設定する
C     ISEAWL: =0 防潮堤部分の線流量計算で防潮堤を無視する
C           : =1 防潮堤部分の線流量計算で防潮堤を考慮する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'OBSTR.h'
C
      REAL(8),INTENT(OUT)::HU(MX,MY,MZ),HV(MX,MY,MZ)
      REAL(8),INTENT(IN) ::UU(MX,MY,MZ),VV(MX,MY,MZ),FF(MX,MY,MZ)
      REAL(8),INTENT(IN) ::GX(MX,MY,MZ),GY(MX,MY,MZ),GV(MX,MY,MZ)
      REAL(8),INTENT(IN) ::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOSP(MY)
C
      INTEGER,INTENT(IN) ::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(IN) ::LLWALB(3,MLWALB),KF(MX,MY)
C
      REAL(8),ALLOCATABLE :: GXMD(:,:,:),GYMD(:,:,:)
      REAL(8),ALLOCATABLE :: UAVE(:,:),VAVE(:,:)
      INTEGER,ALLOCATABLE :: INDX(:,:,:),INDY(:,:,:),INDT(:,:,:)
      INTEGER,SAVE :: IMIN,IMAX,JMIN,JMAX,KMIN,KMAX,IFLAG=0
      INTEGER::I,J,K,N,IS,IE,JS,JE,KS,KE,NN,IERR
      REAL(8)::FFI,FFJ,FF0,FF1,FF2,FFA,FFB,HU1,HV1,GV1,GX1,GY1
      REAL(8)::UT,VT,GXM,GXI,GYM,GYJ,SS,SU,SV
C
      IF( IFLAG.EQ.0 ) THEN
        IMIN = MX
        IMAX = 0
        JMIN = MY
        JMAX = 0
        KMIN = MZ
        KMAX = 0
C
        DO 100 N=1,NPORS
          IS = IPORS(1,N)
          IE = IPORS(2,N)
          JS = IPORS(3,N)
          JE = IPORS(4,N)
          KS = IPORS(5,N)
          KE = IPORS(6,N)
          IF(IPORS(7,N).EQ.0) GO TO 100          
          IMIN = MIN(IS,IMIN)
          IMAX = MAX(IE,IMAX)
          JMIN = MIN(JS,JMIN)
          JMAX = MAX(JE,JMAX)
          KMIN = MIN(KS,KMIN)
          KMAX = MAX(KE,KMAX)
  100   CONTINUE
        IFLAG = 1
      END IF
C
      ALLOCATE (GXMD(IMIN-1:IMAX,JMIN:JMAX,KMIN:KMAX),STAT=IERR)
      ALLOCATE (GYMD(IMIN:IMAX,JMIN-1:JMAX,KMIN:KMAX),STAT=IERR)
      ALLOCATE (UAVE(IMIN-1:IMAX,JMIN:JMAX),STAT=IERR)
      ALLOCATE (VAVE(IMIN:IMAX,JMIN-1:JMAX),STAT=IERR)
      ALLOCATE (INDX(MX,MY,MZ),STAT=IERR)
      ALLOCATE (INDY(MX,MY,MZ),STAT=IERR)
      ALLOCATE (INDT(IMIN:IMAX,JMIN:JMAX,KMIN:KMAX)  ,STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('MODHUV',7110)
         WRITE(LP,*) 'CANNOT ALLOCATE GXMD,...'
         CALL ABORT1('')
      END IF
      GXMD = 0.0D0
      GYMD = 0.0D0
      UAVE = 0.0D0
      VAVE = 0.0D0
      INDX = 0
      INDY = 0
      INDT = 0
C
      DO 110 N=1,NPORS
        IS = IPORS(1,N)
        IE = IPORS(2,N)
        JS = IPORS(3,N)
        JE = IPORS(4,N)
        KS = IPORS(5,N)
        KE = IPORS(6,N)
        NN = IPORS(7,N)
        IF(NN.EQ.0) GO TO 110          
C
        DO 120 K=KS,KE
        DO 120 J=JS,JE
          IF(INDU(IS-1,J,K).GE.0) INDX(IS-1,J,K)=N
          DO 125 I=IS,IE-1
            IF(INDU(I,J,K).GE.0) INDX(I,J,K)=N
  125     CONTINUE
          IF(INDU(IE,J,K).GE.0) INDX(IE,J,K)=N
  120   CONTINUE
C
        DO 130 K=KS,KE
        DO 130 I=IS,IE
          IF(INDV(I,JS-1,K).GE.0) INDY(I,JS-1,K)=N
          DO 135 J=JS,JE-1
            IF(INDV(I,J,K).GE.0) INDY(I,J,K)=N
  135     CONTINUE
          IF(INDV(I,JE,K).GE.0) INDY(I,JE,K)=N
  130   CONTINUE
C
        DO 140 K=KS,KE
        DO 140 J=JS,JE
        DO 140 I=IS,IE
          INDT(I,J,K) = N
  140   CONTINUE
  110 CONTINUE
C
C ... 平均流速UAVE、VAVEを計算する
      DO 150 J=JMIN,JMAX
      DO 150 I=IMIN-1,IMAX
        SS = 0.0D0
        SU = 0.0D0
        DO 155 K=KMIN,KMAX
          IF(INDU(I,J,K).GE.-1) THEN
            IF(UU(I,J,K).NE.0.0) THEN
              SS = SS+HU(I,J,K)/UU(I,J,K)*ZC(4,K)
              SU = SU+HU(I,J,K)*ZC(4,K)
            END IF
          END IF
  155   CONTINUE
        IF(SS.NE.0.0D0) UAVE(I,J)=SU/SS
  150 CONTINUE
C
      DO 160 J=JMIN-1,JMAX
      DO 160 I=IMIN,IMAX
        SS = 0.0D0
        SV = 0.0D0
        DO 165 K=KMIN,KMAX
          IF(INDV(I,J,K).GE.-1) THEN
            IF(VV(I,J,K).NE.0.0) THEN
              SS = SS+HV(I,J,K)/VV(I,J,K)*ZC(4,K)
              SV = SV+HV(I,J,K)*ZC(4,K)
            END IF
          END IF
  165   CONTINUE
        IF(SS.NE.0.0D0) VAVE(I,J)=SV/SS
  160 CONTINUE
C
C ...
      DO 200 K=KMIN,KMAX
      DO 200 J=JMIN,JMAX
      DO 200 I=IMIN,IMAX
        IF(INDT(I,J,K).GT.0) THEN
C          IF(INDU(I-1,J,K).GT.-2.OR.INDU(I,J,K).GT.-2) THEN
            N  = INDT(I,J,K)
            NN = IPORS(7,N)
            UT = (UAVE(I-1,J)+UAVE(I,J))*0.5D0
            VT = (VAVE(I,J-1)+VAVE(I,J))*0.5D0
            CALL CALGXY(GXM,GXI,GYM,GYJ,UT,VT,I,J,N)
            IF(GXMD(I-1,J,K).EQ.0.0D0) THEN
              GXMD(I-1,J,K) = GXM
            ELSE
              GXMD(I-1,J,K) = (GXMD(I-1,J,K)+GXM)*0.5D0
            END IF
            GXMD(I,J,K) = GXI
            IF(GYMD(I,J-1,K).EQ.0.0D0) THEN
              GYMD(I,J-1,K) = GYM
            ELSE
              GYMD(I,J-1,K) = (GYMD(I,J-1,K)+GYM)*0.5D0
            END IF
            GYMD(I,J,K) = GYJ
C          END IF
        END IF
  200 CONTINUE    
C
      DO 210 K=KMIN  ,KMAX
      DO 210 J=JMIN-1,JMAX
      DO 210 I=IMIN-1,IMAX
        IF(J.GE.JMIN) THEN
          IF(INDX(I,J,K).LE.0) GXMD(I,J,K)=GX(I,J,K)
        END IF
        IF(I.GE.IMIN) THEN
          IF(INDY(I,J,K).LE.0) GYMD(I,J,K)=GY(I,J,K)
        END IF
  210 CONTINUE    
C    
      CALL ZERCLR(HU,MXYZ,0.0D0)
      CALL ZERCLR(HV,MXYZ,0.0D0)
      DO 300 K=2,MZM
      DO 300 J=2,MYM
      DO 300 I=1,MXM
C ...... 計算点及び流入出境界
         IF( INDU(I,J,K).GE.-1 ) THEN
            GX1 = 1.0D0-GX(I,J,K)
            FFI = FF(I,J,K)*XC(7,I,J)+FF(I+1,J,K)*XC(8,I,J)
            FF0 = MAX(FFI        -GX1,0.0D0)
            FF1 = MAX(FF(I  ,J,K)-GX1,0.0D0)
            FF2 = MAX(FF(I+1,J,K)-GX1,0.0D0)
C
            HU(I,J,K) = PARAMF2*FF0*UU(I,J,K)
     $                + PARAMF*(FF1*MAX(UU(I,J,K),0.0D0)
     $                +         FF2*MIN(UU(I,J,K),0.0D0))
         END IF
C
C ...... 線流量を合計しておく(ワークエリア)
         IF(INDX(I,J,K).EQ.0) THEN
            HU(I,J,MZ) = HU(I,J,MZ)+HU(I,J,K)*ZC(4,K)
         END IF
  300 CONTINUE
C
C ... 浮上型防波堤領域の流量を修正する
      DO 310 K=KMIN,KMAX
      DO 310 J=JMIN,JMAX
      DO 310 I=IMIN-1,IMAX
C ...... 計算点及び流入出境界
         IF(INDX(I,J,K).GT.0) THEN
            IF(K.LT.KF(I,J).AND.K.LT.KF(I+1,J)) THEN
               IF(INDU(I,J,K-1).GT.0) THEN
                  GX1 = GX(I,J,K)+GXMD(I,J,K)-1.0D0
               ELSE
                  GX1 = GX(I,J,K)*GXMD(I,J,K)
               END IF
               HU(I,J,K) = GX1*UU(I,J,K)
               HU(I,J,MZ) = HU(I,J,MZ)+HU(I,J,K)*ZC(4,K)
            ELSE IF(K.LE.KF(I,J).OR.K.LE.KF(I+1,J)) THEN
               IF(INDU(I,J,K-1).GT.0) THEN
                  GX1 = GX(I,J,K)+GXMD(I,J,K)-1.0D0
                  FFI = FF(I,J,K)*XC(7,I,J)+FF(I+1,J,K)*XC(8,I,J)
                  FF0 = FFI*GX1
                  FF1 = FF(I  ,J,K)*GX1
                  FF2 = FF(I+1,J,K)*GX1
                  HU(I,J,K) = PARAMF2*FF0*UU(I,J,K)
     $                      + PARAMF*(FF1*MAX(UU(I,J,K),0.0D0)
     $                      +         FF2*MIN(UU(I,J,K),0.0D0))
               ELSE
                  GX1 = 1.0D0-GX(I,J,K)
                  FFI = FF(I,J,K)*XC(7,I,J)+FF(I+1,J,K)*XC(8,I,J)
                  FF0 = MAX(FFI        -GX1,0.0D0)*GXMD(I,J,K)
                  FF1 = MAX(FF(I  ,J,K)-GX1,0.0D0)*GXMD(I,J,K)
                  FF2 = MAX(FF(I+1,J,K)-GX1,0.0D0)*GXMD(I,J,K)
                  HU(I,J,K) = PARAMF2*FF0*UU(I,J,K)
     $                      + PARAMF*(FF1*MAX(UU(I,J,K),0.0D0)
     $                      +         FF2*MIN(UU(I,J,K),0.0D0))
               END IF
               HU(I,J,MZ) = HU(I,J,MZ)+HU(I,J,K)*ZC(4,K)
            END IF
         END IF
C
  310 CONTINUE
C
C ... 防潮堤用の処理(X方向)
!CDIR NODEP
      DO 350 N=1,MLWALBX
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
C
         GX1 = 1.0D0-GX(I,J,K)
         IF( ISEAWL.EQ.0 ) THEN
            GV1 = 1.0D0-GV(I,J,K)*XC(7,I,J)-GV(I+1,J,K)*XC(8,I,J)
            GX1 = MIN(GV1,GX1)
         END IF
C
         FFA = MAX(FF(I  ,J,K),GX1)
         FFB = MAX(FF(I+1,J,K),GX1)
         FF0 = FFA*XC(7,I,J)+FFB*XC(8,I,J)-GX1
         FF1 = FFA-GX1
         FF2 = FFB-GX1
C
         HU1 = HU(I,J,K)
         HU(I,J,K) = PARAMF2*FF0*UU(I,J,K)
     $             + PARAMF*(FF1*MAX(UU(I,J,K),0.0D0)
     $             +         FF2*MIN(UU(I,J,K),0.0D0))
         HU(I,J,MZ) = HU(I,J,MZ) + (HU(I,J,K)-HU1)*ZC(4,K)
  350 CONTINUE
C
      DO 400 K=2,MZM
      DO 400 J=1,MYM
      DO 400 I=2,MXM
C ...... 計算点及び流入出境界
         IF( INDV(I,J,K).GE.-1 ) THEN
            GY1 = 1.0D0-GY(I,J,K)
            FFJ = FF(I,J,K)*YC(7,J)+FF(I,J+1,K)*YC(8,J)
            FF0 = MAX(FFJ        -GY1,0.0D0)
            FF1 = MAX(FF(I,J  ,K)-GY1,0.0D0)
            FF2 = MAX(FF(I,J+1,K)-GY1,0.0D0)
C
            HV(I,J,K) = PARAMF2*FF0*VV(I,J,K)
     $                + PARAMF*(FF1*MAX(VV(I,J,K),0.0D0)
     $                +         FF2*MIN(VV(I,J,K),0.0D0))
            HV(I,J,K) = HV(I,J,K)*YCOSP(J)
         END IF
C
C ...... 線流量を合計しておく(ワークエリア)
         IF(INDY(I,J,K).EQ.0) THEN
            HV(I,J,MZ) = HV(I,J,MZ)+HV(I,J,K)*ZC(4,K)
         END IF
  400 CONTINUE
C
C ... 浮上型防波堤領域の流量を修正する
      DO 410 K=KMIN,KMAX
      DO 410 J=JMIN-1,JMAX
      DO 410 I=IMIN,IMAX
C ...... 計算点及び流入出境界
         IF( INDY(I,J,K).GT.0 ) THEN
            IF(K.LT.KF(I,J).AND.K.LT.KF(I,J+1)) THEN
               IF(INDV(I,J,K-1).GT.0) THEN
                  GY1 = GY(I,J,K)+GYMD(I,J,K)-1.0D0
               ELSE
                  GY1 = GY(I,J,K)*GYMD(I,J,K)
               END IF
               HV(I,J,K) = GY1*VV(I,J,K)*YCOSP(J)
               HV(I,J,MZ) = HV(I,J,MZ)+HV(I,J,K)*ZC(4,K)
            ELSE IF(K.LE.KF(I,J).OR.K.LE.KF(I,J+1)) THEN
               IF(INDV(I,J,K-1).GT.0) THEN
                  GY1 = GY(I,J,K)+GYMD(I,J,K)-1.0D0
                  FFJ = FF(I,J,K)*YC(7,J)+FF(I,J+1,K)*YC(8,J)
                  FF0 = FFJ*GY1
                  FF1 = FF(I,J  ,K)*GY1
                  FF2 = FF(I,J+1,K)*GY1
                  HV(I,J,K) = PARAMF2*FF0*VV(I,J,K)
     $                      + PARAMF*(FF1*MAX(VV(I,J,K),0.0D0)
     $                      +         FF2*MIN(VV(I,J,K),0.0D0))
                  HV(I,J,K) = HV(I,J,K)*YCOSP(J)
               ELSE
                  GY1 = 1.0D0-GY(I,J,K)
                  FFJ = FF(I,J,K)*YC(7,J)+FF(I,J+1,K)*YC(8,J)
                  FF0 = MAX(FFJ        -GY1,0.0D0)*GYMD(I,J,K)
                  FF1 = MAX(FF(I,J  ,K)-GY1,0.0D0)*GYMD(I,J,K)
                  FF2 = MAX(FF(I,J+1,K)-GY1,0.0D0)*GYMD(I,J,K)
                  HV(I,J,K) = PARAMF2*FF0*VV(I,J,K)
     $                      + PARAMF*(FF1*MAX(VV(I,J,K),0.0D0)
     $                      +         FF2*MIN(VV(I,J,K),0.0D0))
                  HV(I,J,K) = HV(I,J,K)*YCOSP(J)
               END IF
               HV(I,J,MZ) = HV(I,J,MZ)+HV(I,J,K)*ZC(4,K)
            END IF
         END IF
C
  410 CONTINUE
C
C ... 防潮堤用の処理(Y方向)
!CDIR NODEP
      DO 450 N=MLWALBX+1,MLWALB
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
C
         GY1 = 1.0D0-GY(I,J,K)
         IF( ISEAWL.EQ.0 ) THEN
            GV1 = 1.0D0-GV(I,J,K)*YC(7,J)-GV(I,J+1,K)*YC(8,J)
            GY1 = MIN(GV1,GY1)
         END IF
C
         FFA = MAX(FF(I,J  ,K),GY1)
         FFB = MAX(FF(I,J+1,K),GY1)
         FF0 = FFA*YC(7,J)+FFB*YC(8,J)-GY1
         FF1 = FFA-GY1
         FF2 = FFB-GY1
C
         HV1 = HV(I,J,K)
         HV(I,J,K) = PARAMF2*FF0*VV(I,J,K)
     $             + PARAMF*(FF1*MAX(VV(I,J,K),0.0D0)
     $             +         FF2*MIN(VV(I,J,K),0.0D0))
         HV(I,J,K) = HV(I,J,K)*YCOSP(J)
         HV(I,J,MZ) = HV(I,J,MZ) + (HV(I,J,K)-HV1)*ZC(4,K)
  450 CONTINUE
C
      DEALLOCATE (GXMD,GYMD,UAVE,VAVE,INDX,INDY,INDT)
      RETURN
      END
