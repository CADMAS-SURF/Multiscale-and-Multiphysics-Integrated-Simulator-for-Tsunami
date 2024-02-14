      SUBROUTINE CLHUVWD(HU,HV,HW,UU,VV,WW,FF,GV,GX,GY,GV0,GX0,GY0,GZ0,
     $                   GXD,GYD,GZD,CMD,XC,YC,ZC,YCOSP,HH,HDEP,HHOFL,
     $                   INDU,INDV,INDW,LLWALB,LLOFL,KF,KG,IFLAG)
C======================================================================
C     HU,HV,HW値を設定する
C     IFLAG = 0: HU,HV,HW
C     IFLAG = 1: HU,HV
C     IFLAG = 2: HW
C
C     ISEAWL: =0 防潮堤部分の線流量計算で防潮堤を無視する
C           : =1 防潮堤部分の線流量計算で防潮堤を考慮する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'CADMAS.h'
      INCLUDE 'VVMAX.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(OUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(IN)::FF(MX,MY,MZ)
      REAL(8),INTENT(IN)::GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(IN)::GV0(MX,MY,MZ),GX0(MX,MY,MZ)
      REAL(8),INTENT(IN)::GY0(MX,MY,MZ),GZ0(MX,MY,MZ)
      REAL(8),INTENT(IN)::GXD(MX,MY,MZ),GYD(MX,MY,MZ)
      REAL(8),INTENT(IN)::GZD(MX,MY,MZ),CMD(MX,MY,MZ)
C
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOSP(MY)
      REAL(8),INTENT(IN)::HH(MX,MY),HDEP(MX,MY),HHOFL(MLOFL)
      INTEGER,INTENT(IN)::INDU(MX,MY,MZ),INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(IN)::LLWALB(3,MLWALB),KF(MX,MY),KG(MX,MY)
      INTEGER,INTENT(IN)::LLOFL(3,MLOFL)
      INTEGER,INTENT(IN)::IFLAG
C
      INTEGER::I,J,K,N,IX,IH1,IH2,JH1,JH2,IQ,KG1
      REAL(8)::FFI,FFJ,FF0,FF1,FF2,FFA,FFB,HU1,HV1,GV1,GX1,GY1
      REAL(8):: H1,H2,QQ,QSUM,HX,SGN,UU1,VV1,AUP
C
C
      IF( IFLAG.LE.1 ) THEN
C
      IF( LFOBS.NE.0 ) THEN
C ... 浮上型防波堤がある場合のhu,hvの計算
        CALL MODHUV(HU,HV,UU,VV,FF,GX,GY,GV,XC,YC,ZC,YCOSP,
     $              INDU,INDV,LLWALB,KF)
      ELSE
C ... 浮上型防波堤がない場合のhu,hvの計算
      CALL ZERCLR(HU,MXYZ,0.0D0)
      CALL ZERCLR(HV,MXYZ,0.0D0)
      DO 200 K=2,MZM
      DO 200 J=2,MYM
      DO 200 I=1,MXM
C ...... 計算点及び流入出境界
         IF( INDU(I,J,K).GE.-1 ) THEN
            GX1 = 1.0D0-GX0(I,J,K)
            FFI = FF(I,J,K)*XC(7,I,J)+FF(I+1,J,K)*XC(8,I,J)
            FF0 = MAX(FFI        -GX1,0.0D0)
            FF1 = MAX(FF(I  ,J,K)-GX1,0.0D0)
            FF2 = MAX(FF(I+1,J,K)-GX1,0.0D0)
C
            HU(I,J,K) = PARAMF2*FF0*UU(I,J,K)
     $                + PARAMF*(FF1*MAX(UU(I,J,K),0.0D0)
     $                +         FF2*MIN(UU(I,J,K),0.0D0))
            HU(I,J,K) = HU(I,J,K)
     $                *(GXD(I,J,K)+(1.0D0-GXD(I,J,K))*CMD(I,J,K))
         END IF
C
C ...... 線流量を合計しておく(ワークエリア)
CCC         HU(I,J,MZ) = HU(I,J,MZ) + HU(I,J,K)*ZC(4,K)
  200 CONTINUE
C
      IF( NB_SC.GT.0 ) THEN
         DO K=1,NKST
         DO J=1,NJST-JJOFF(1)-JJOFF(2)
            IF(IWCAD.GT.0)
     $         HU(IWCAD  ,J+JSCAD-1+JJOFF(1),K+KBCAD-1) = UWCAD(J,K,4)
            IF(IECAD.GT.0)
     $         HU(IECAD-1,J+JSCAD-1+JJOFF(1),K+KBCAD-1) = UECAD(J,K,4)
         ENDDO
         ENDDO
      ENDIF
C
C ... 防潮堤用の処理(X方向)
!CDIR NODEP
      DO 250 N=1,MLWALBX
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
C
         GX1 = 1.0D0-GX0(I,J,K)
         KG1=MAX(KG(I,J),KG(I+1,J))
         IF( ISEAWL.EQ.0 .AND. K.EQ.KG1 ) THEN
CCC         IF( ISEAWL.EQ.0 ) THEN
            GV1 = 1.0D0-GV0(I,J,K)*XC(7,I,J)-GV0(I+1,J,K)*XC(8,I,J)
            GX1 = MIN(GV1,GX1)
         END IF
C
         FFA = MAX(FF(I  ,J,K),GX1)
         FFB = MAX(FF(I+1,J,K),GX1)
         FF0 = FFA*XC(7,I,J)+FFB*XC(8,I,J)-GX1
         FF1 = FFA-GX1
         FF2 = FFB-GX1
C
         IF( ISEAWL.EQ.0 .AND. K.GT.KG1 ) THEN
            FF0=GX0(I,J,K)*(FF(I,J,K)*XC(7,I,J)+FF(I+1,J,K)*XC(8,I,J))
            FF1=GX0(I,J,K)*FF(I,J,K)
            FF2=GX0(I,J,K)*FF(I+1,J,K)
         ENDIF
C
         HU(I,J,K) = PARAMF2*FF0*UU(I,J,K)
     $             + PARAMF*(FF1*MAX(UU(I,J,K),0.0D0)
     $             +         FF2*MIN(UU(I,J,K),0.0D0))
         HU(I,J,K) = HU(I,J,K)
     $             *(GXD(I,J,K)+(1.0D0-GXD(I,J,K))*CMD(I,J,K))
  250 CONTINUE
C
C ... 越流用の処理(X方向)
!CDIR NODEP
      DO N=1,MLOFLX
         I = LLOFL(1,N)
         J = LLOFL(2,N)
         IX = LLOFL(3,N)
         HX = HHOFL(N)
C
         QSUM  = 0.0D0
         DO K=2,MZM
            IF( INDU(I,J,K).GE.-1 )
     $         QSUM = QSUM + HU(I,J,K)*ZC(4,K)
         ENDDO
C
         IH1=I
         IH2=I+1
         SGN=1.0D0 ! 右向き
         IF( HH(I,J).LT.HH(I+1,J) ) THEN
            IH1=I+1
            IH2=I
            SGN=-1.0D0 ! 左向き
         ENDIF
         H1 = MAX(HH(IH1,J)-HX,0.0D0)
         H2 = MAX(HH(IH2,J)-HX,0.0D0)
C
         IQ=0
         IF( IX.EQ.1 ) THEN
            IF(H2.LE.H1*0.66667D0) THEN
               QQ=0.35D0*H1*SQRT(2.0D0*ABS(GRAV)*H1)
            ELSE
               QQ=0.91D0*H2*SQRT(2.0D0*ABS(GRAV)*(H1-H2))
            ENDIF
            IF( IHONMA.EQ.1 .OR. (IHONMA.EQ.2.AND.
     $         (SGN.GT.0.0D0.AND.QSUM.GT.QQ).OR.
     $         (SGN.LT.0.0D0.AND.-QSUM.GT.QQ) ) )
     $         IQ=1
C
         ELSEIF( IX.EQ.2 ) THEN
            IF( SGN.GT.0.0D0.AND.IAIDA.GT.0.AND.H2.GT.HAIDA ) THEN
               QQ=0.6D0*H2*SQRT(ABS(GRAV)*(H1-H2))
               IQ=1
            ELSEIF( SGN.LT.0.0D0.AND.IBKSTP.GT.0 ) THEN
               QQ=0.544D0*(H1-H2)*SQRT(ABS(GRAV)*(H1-H2))
               IQ=1
            ENDIF
C
         ELSEIF( IX.EQ.3 ) THEN
            IF( SGN.LT.0.0D0.AND.IAIDA.GT.0.AND.H2.GT.HAIDA ) THEN
               QQ=0.6D0*H2*SQRT(ABS(GRAV)*(H1-H2))
               IQ=1
            ELSEIF( SGN.GT.0.0D0.AND.IBKSTP.GT.0 ) THEN
               QQ=0.544D0*(H1-H2)*SQRT(ABS(GRAV)*(H1-H2))
               IQ=1
            ENDIF
         ENDIF
C
         IF( IQ.EQ.1 ) THEN
            UU1=MIN(QQ/MAX(H1,EPSH),VVMAX)
            DO K=2,MZM
               IF( INDU(I,J,K).GE.-1 ) THEN
                  GX1 = 1.0D0-GX0(I,J,K)
                  HU(I,J,K)=SGN*MAX(FF(IH1,J,K)-GX1,0.0D0)*UU1
                  UU(I,J,K)=SGN*UU1
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C
      DO 210 K=2,MZM
      DO 210 J=2,MYM
      DO 210 I=1,MXM
C ...... 流量制限
         IF( INDU(I,J,K).GT.0 ) THEN
         IF(HU(I,J,K).GT.0.0D0) THEN
            AUP=(HH(I  ,J)-HDEP(I  ,J))*XC(4,I  ,J)
            HU(I,J,K)=MIN(HU(I,J,K),AUP/(ZC(4,K)*DT))
         ELSE
            AUP=(HH(I+1,J)-HDEP(I+1,J))*XC(4,I+1,J)
            HU(I,J,K)=MAX(HU(I,J,K),-AUP/(ZC(4,K)*DT))              
         ENDIF
         ENDIF
C
C ...... 線流量を合計しておく(ワークエリア)
         HU(I,J,MZ) = HU(I,J,MZ) + HU(I,J,K)*ZC(4,K)
  210 CONTINUE
C
C
      DO 300 K=2,MZM
      DO 300 J=1,MYM
      DO 300 I=2,MXM
C ...... 計算点及び流入出境界
         IF( INDV(I,J,K).GE.-1 ) THEN
            GY1 = 1.0D0-GY0(I,J,K)
            FFJ = FF(I,J,K)*YC(7,J)+FF(I,J+1,K)*YC(8,J)
            FF0 = MAX(FFJ        -GY1,0.0D0)
            FF1 = MAX(FF(I,J  ,K)-GY1,0.0D0)
            FF2 = MAX(FF(I,J+1,K)-GY1,0.0D0)
C
            HV(I,J,K) = PARAMF2*FF0*VV(I,J,K)
     $                + PARAMF*(FF1*MAX(VV(I,J,K),0.0D0)
     $                +         FF2*MIN(VV(I,J,K),0.0D0))
            HV(I,J,K) = HV(I,J,K)
     $                *(GYD(I,J,K)+(1.0D0-GYD(I,J,K))*CMD(I,J,K))
     $                *YCOSP(J)
         END IF
C
C ...... 線流量を合計しておく(ワークエリア)
CCC         HV(I,J,MZ) = HV(I,J,MZ) + HV(I,J,K)*ZC(4,K)
  300 CONTINUE
C
      IF( NB_SC.GT.0 ) THEN
         DO K=1,NKST
         DO I=1,NIST-IIOFF(1)-IIOFF(2)
            IF(JSCAD.GT.0)
     $         HV(I+IWCAD-1+IIOFF(1),JSCAD  ,K+KBCAD-1) = VSCAD(I,K,4)
            IF(JNCAD.GT.0)
     $         HV(I+IWCAD-1+IIOFF(1),JNCAD-1,K+KBCAD-1) = VNCAD(I,K,4)
         ENDDO
         ENDDO
      ENDIF
C
C ... 防潮堤用の処理(Y方向)
!CDIR NODEP
      DO 350 N=MLWALBX+1,MLWALB
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
C
         GY1 = 1.0D0-GY0(I,J,K)
         KG1=MAX(KG(I,J),KG(I,J+1))
         IF( ISEAWL.EQ.0 .AND. K.EQ.KG1 ) THEN
CCC         IF( ISEAWL.EQ.0 ) THEN
            GV1 = 1.0D0-GV0(I,J,K)*YC(7,J)-GV0(I,J+1,K)*YC(8,J)
            GY1 = MIN(GV1,GY1)
         END IF
C
         FFA = MAX(FF(I,J  ,K),GY1)
         FFB = MAX(FF(I,J+1,K),GY1)
         FF0 = FFA*YC(7,J)+FFB*YC(8,J)-GY1
         FF1 = FFA-GY1
         FF2 = FFB-GY1
C
         IF( ISEAWL.EQ.0 .AND. K.GT.KG1 ) THEN
            FF0=GY0(I,J,K)*(FF(I,J,K)*YC(7,J)+FF(I,J+1,K)*YC(8,J))
            FF1=GY0(I,J,K)*FF(I,J,K)
            FF2=GY0(I,J,K)*FF(I,J+1,K)
         ENDIF
C
         HV(I,J,K) = PARAMF2*FF0*VV(I,J,K)
     $             + PARAMF*(FF1*MAX(VV(I,J,K),0.0D0)
     $             +         FF2*MIN(VV(I,J,K),0.0D0))
         HV(I,J,K) = HV(I,J,K)
     $             *(GYD(I,J,K)+(1.0D0-GYD(I,J,K))*CMD(I,J,K))
     $             *YCOSP(J)
  350 CONTINUE
C
C ... 越流用の処理(Y方向)
!CDIR NODEP
      DO N=MLOFLX+1,MLOFL
         I = LLOFL(1,N)
         J = LLOFL(2,N)
         IX = LLOFL(3,N)
         HX = HHOFL(N)
C
         QSUM  = 0.0D0
         DO K=2,MZM
            IF( INDV(I,J,K).GE.-1 )
     $         QSUM = QSUM + HV(I,J,K)*ZC(4,K)
         ENDDO
C
         JH1=J
         JH2=J+1
         SGN=1.0D0 ! 右向き
         IF( HH(I,J).LT.HH(I,J+1) ) THEN
            JH1=J+1
            JH2=J
            SGN=-1.0D0 ! 左向き
         ENDIF
         H1 = MAX(HH(I,JH1)-HX,0.0D0)
         H2 = MAX(HH(I,JH2)-HX,0.0D0)
C
         IQ=0
         IF( IX.EQ.1 ) THEN
            IF(H2.LE.H1*0.66667D0) THEN
               QQ=0.35D0*H1*SQRT(2.0D0*ABS(GRAV)*H1)
            ELSE
               QQ=0.91D0*H2*SQRT(2.0D0*ABS(GRAV)*(H1-H2))
            ENDIF
            IF( IHONMA.EQ.1 .OR. (IHONMA.EQ.2.AND.
     $         (SGN.GT.0.0D0.AND.QSUM.GT.QQ).OR.
     $         (SGN.LT.0.0D0.AND.-QSUM.GT.QQ) ) )
     $         IQ=1
C
         ELSEIF( IX.EQ.2 ) THEN
            IF( SGN.GT.0.0D0.AND.IAIDA.GT.0.AND.H2.GT.HAIDA ) THEN
               QQ=0.6D0*H2*SQRT(ABS(GRAV)*(H1-H2))
               IQ=1
            ELSEIF( SGN.LT.0.0D0.AND.IBKSTP.GT.0 ) THEN
               QQ=0.544D0*(H1-H2)*SQRT(ABS(GRAV)*(H1-H2))
               IQ=1
            ENDIF
C
         ELSEIF( IX.EQ.3 ) THEN
            IF( SGN.LT.0.0D0.AND.IAIDA.GT.0.AND.H2.GT.HAIDA ) THEN
               QQ=0.6D0*H2*SQRT(ABS(GRAV)*(H1-H2))
               IQ=1
            ELSEIF( SGN.GT.0.0D0.AND.IBKSTP.GT.0 ) THEN
               QQ=0.544D0*(H1-H2)*SQRT(ABS(GRAV)*(H1-H2))
               IQ=1
            ENDIF
         ENDIF
C
         IF( IQ.EQ.1 ) THEN
            VV1=MIN(QQ/MAX(H1,EPSH),VVMAX)
            DO K=2,MZM
               IF( INDV(I,J,K).GE.-1 ) THEN
                  GY1 = 1.0D0-GY0(I,J,K)
                  HV(I,J,K)=SGN*MAX(FF(I,JH1,K)-GY1,0.0D0)*VV1
                  VV(I,J,K)=SGN*VV1
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C
      DO 310 K=2,MZM
      DO 310 J=1,MYM
      DO 310 I=2,MXM
C ...... 流量制限
         IF( INDV(I,J,K).GT.0 ) THEN
         IF(HV(I,J,K).GT.0.0D0) THEN
            AUP=(HH(I,J  )-HDEP(I,J  ))*YC(4,J  )
            HV(I,J,K)=MIN(HV(I,J,K),AUP/(ZC(4,K)*DT))
         ELSE
            AUP=(HH(I,J+1)-HDEP(I,J+1))*YC(4,J+1)
            HV(I,J,K)=MAX(HV(I,J,K),-AUP/(ZC(4,K)*DT))              
         ENDIF
         ENDIF
C
C ...... 線流量を合計しておく(ワークエリア)
         HV(I,J,MZ) = HV(I,J,MZ) + HV(I,J,K)*ZC(4,K)
  310 CONTINUE
C
      END IF
C
C ... HU,HV (for DOMAIN-DECOMP)
      CALL CP_DSR_DC2(MX,MY,MZ,1,1,HU)
      CALL CP_DSR_DC2(MX,MY,MZ,2,1,HV)
C
      END IF
C
C
      IF( IFLAG.EQ.0.OR.IFLAG.EQ.2 ) THEN
      CALL ZERCLR(HW,MXYZ,0.0D0)
C
      DO 400 K=1,MZM
      DO 400 J=2,MYM
      DO 400 I=2,MXM
C ...... 計算点及び流入出境界
         IF( INDW(I,J,K).GE.-1 .AND. K.LT.KF(I,J) )
     $      HW(I,J,K) = GZ0(I,J,K)*WW(I,J,K)
     $      *(GZD(I,J,K)+(1.0D0-GZD(I,J,K))*CMD(I,J,K))
  400 CONTINUE
C
C ... HW (for DOMAIN-DECOMP)
      CALL CP_DSR_DC2(MX,MY,MZ,3,1,HW)
C
      END IF
C
      RETURN
      END
