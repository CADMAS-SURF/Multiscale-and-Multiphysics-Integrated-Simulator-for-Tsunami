      SUBROUTINE CP_BCHHML(XC_NS,YC_NS,I_ML,J_ML,KF_ML,KF_NS,HH_NS,
     1                     HH_ML,HDEP_NS,HDEP_ML,
     2                     IS,IE,JS,JE,NESTXM,NESTXP,NESTYM,NESTYP,
     3                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     4                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
C-----------------------------------------------------------------------
C     NSの内側境界値(潮位)をML用のエリアに平均する
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'CONNEC.h'
C
      INTEGER,INTENT(INOUT)::IS,IE,JS,JE,NESTXM,NESTXP,NESTYM,NESTYP
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR,KBOT,KTOP
C
      REAL(8),INTENT(INOUT)::XC_NS(8,MX_NS),YC_NS(8,MY_NS)
      INTEGER,INTENT(INOUT)::I_ML(2,MX_ML),J_ML(2,MY_ML)
      INTEGER,INTENT(INOUT)::KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      INTEGER,INTENT(INOUT)::KF_NS(MX_NS,MY_NS)
      REAL(8),INTENT(INOUT)::HH_NS(MX_NS,MY_NS),HDEP_NS(MX_NS,MY_NS)
      REAL(8),INTENT(INOUT)::HH_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::HDEP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
C
      INTEGER::IHH_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      INTEGER,SAVE::IDB=0
C
      REAL(8)::HSUM,SS,SSUM
      INTEGER::I,I1,I2,IEE,II,ISS,J,J1,J2,JEE,JJ,JSS
C
      NPNTML = 0
      CALL ZERCLI(IHH_ML,(IEAS-IWES+3)*(JNOR-JSOU+3),0)
C
      if(idb.ne.0) then
        write(*,*) 'bchhml is,ie,js,je=',is,ie,js,je
        write(*,*) 'nest(xm,xp,ym,yp)=',nestxm,nestxp,nestym,nestyp
      end if
C
C     潮位HH_ML(I,J):(J=JS,JS+NESTYM),(I=IS,IE-NESTXM)
C
CCC      JEE = JS+NESTYM-1
      JEE = JS+NESTYM
      IF(JS.EQ.JE) JEE=JS
      IF(IPECON(4,NRANK+1).GE.0) JEE=JS-1
      DO 150 J = JS,JEE
        J1 = J_ML(1,J-1)+1
        J2 = J_ML(1,J)
        DO 100 I = IS,IE
          IF(JS.EQ.JE) THEN
            IF(I.GT.IS+NESTXM.AND.I.LT.IE-NESTXP) GO TO 100
          END IF
C
          IF(KF_ML(I,J).EQ.MZ_ML) GO TO 100
C
          I1 = I_ML(2,I-1)+1
          I2 = I_ML(2,I)
          SSUM = 0.0D0
          HSUM = 0.0D0
          DO 110 JJ = J1,J2
            DO 120 II = I1,I2
              IF(KF_NS(II,JJ).NE.MZ_NS.AND.
     $           HH_NS(II,JJ)-HDEP_NS(II,JJ).GE.GXB) THEN
                SS = XC_NS(4,II)*YC_NS(4,JJ)
                SSUM = SSUM+SS
                HSUM = HSUM+SS*HH_NS(II,JJ)
              END IF
  120       CONTINUE
  110     CONTINUE
C
          IF(SSUM.GT.0.0D0) THEN
            HH_ML(I,J) = HSUM/SSUM
            IF(HH_ML(I,J)-HDEP_ML(I,J).LT.EPSH) THEN
              HH_ML(I,J) = HDEP_ML(I,J)+EPSH
            END IF
            if(idb.ne.0) write(*,*) 'i,j,h_ml',i,j,hh_ml(i,j)
          ELSE
            HH_ML(I,J) = HDEP_ML(I,J)+EPSH
          END IF
          IHH_ML(I,J) = 1
C
  100   CONTINUE
  150 CONTINUE
C
C     <JE.NE.JS>
C     潮位HH_ML(I,J):(J=JE-NESTYP,JE),(I=IS,IE-NESTXP)
C
      IF(JE.NE.JS) THEN
CCC        JSS = JE-NESTYP+1
        JSS = JE-NESTYP
        IF(JSS.LE.JS+NESTYM) JSS=JS+NESTYM+1
        IF(IPECON(7,NRANK+1).GE.0) JSS=JE+1
        DO 250 J = JSS,JE
          J1 = J_ML(1,J-1)+1
          J2 = J_ML(1,J)
          DO 200 I = IS,IE
          IF(KF_ML(I,J).EQ.MZ_ML) GO TO 200
C
            I1 = I_ML(2,I-1)+1
            I2 = I_ML(2,I)
            SSUM = 0.0D0
            HSUM = 0.0D0
            DO 210 JJ = J1,J2
              DO 220 II = I1,I2
                IF(KF_NS(II,JJ).NE.MZ_NS.AND.
     $             HH_NS(II,JJ)-HDEP_NS(II,JJ).GE.GXB) THEN
                  SS = XC_NS(4,II)*YC_NS(4,JJ)
                  SSUM = SSUM+SS
                  HSUM = HSUM+SS*HH_NS(II,JJ)
                END IF
  220         CONTINUE
  210       CONTINUE
C
            IF(SSUM.GT.0.0D0) THEN
              HH_ML(I,J) = HSUM/SSUM
              IF(HH_ML(I,J)-HDEP_ML(I,J).LT.EPSH) THEN
                HH_ML(I,J) = HDEP_ML(I,J)+EPSH
              END IF
              if(idb.ne.0) write(*,*) 'i,j,hh_ml',i,j,hh_ml(i,j)
            ELSE
              HH_ML(I,J) = HDEP_ML(I,J)+EPSH
            END IF
            IHH_ML(I,J) = 1
C
  200     CONTINUE
  250   CONTINUE
      END IF
C
C     <JE.GT.JS+1>
C     潮位HH_ML(I,J):(I=IS),(J=JS+1,JE-1)
C
      IF(IPECON(4,NRANK+1).LT.0.AND.IPECON(7,NRANK+1).LT.0) THEN
        IF(JE.GT.JS+1.AND.JS+NESTYM+1.LE.JE-NESTYP-1) THEN
          JSS = JS+NESTYM+1
          JEE = JE-NESTYP-1
        ELSE
          JSS = JE
          JEE = JS
        END IF
      ELSE
        JSS = JS
        JEE = JE
      END IF
C
          IEE = IS+NESTXM
          IF(IS.EQ.IE) IEE=IE
          IF(IPECON(5,NRANK+1).GE.0) IEE=IS-1
          DO 350 I = IS,IEE
            I1 = I_ML(2,I-1)+1
            I2 = I_ML(2,I)
            DO 300 J = JSS,JEE
              IF(KF_ML(I,J).EQ.MZ_ML) GO TO 300
              IF(IHH_ML(I,J).EQ.1) GO TO 300
C
              J1 = J_ML(2,J-1)+1
              J2 = J_ML(2,J)
              SSUM = 0.0D0
              HSUM = 0.0D0
              DO 310 JJ = J1,J2
                DO 320 II = I1,I2
                  IF(KF_NS(II,JJ).NE.MZ_NS.AND.
     $               HH_NS(II,JJ)-HDEP_NS(II,JJ).GE.GXB) THEN
                    SS = XC_NS(4,II)*YC_NS(4,JJ)
                    SSUM = SSUM+SS
                    HSUM = HSUM+SS*HH_NS(II,JJ)
                  END IF
  320           CONTINUE
  310         CONTINUE
C
              IF(SSUM.GT.0.0D0) THEN
                HH_ML(I,J) = HSUM/SSUM
                IF(HH_ML(I,J)-HDEP_ML(I,J).LT.EPSH) THEN
                  HH_ML(I,J) = HDEP_ML(I,J)+EPSH
                END IF
                if(idb.ne.0) write(*,*) 'i,j,h_ml',i,j,hh_ml(i,j)
              ELSE
                HH_ML(I,J) = HDEP_ML(I,J)+EPSH
              END IF
C
  300       CONTINUE
  350     CONTINUE
C
C     <JE.GT.JS+1>,<IE.NE.IS>
C     潮位HH_ML(I,J):(I=IS),(J=JS+1,JE-1)
C
          IF(IE.GT.IS) THEN
            ISS = IE-NESTXP
            IF(IPECON(6,NRANK+1).GE.0) ISS=IE+1
            DO 450 I = ISS,IE
              I1 = I_ML(2,I-1)+1
              I2 = I_ML(2,I)
              DO 400 J = JSS,JEE
                IF(KF_ML(I,J).EQ.MZ_ML) GO TO 400
                IF(IHH_ML(I,J).EQ.1) GO TO 400
C
                J1 = J_ML(2,J-1)+1
                J2 = J_ML(2,J)
                SSUM = 0.0D0
                HSUM = 0.0D0
                DO 410 JJ = J1,J2
                  DO 420 II = I1,I2
                    IF(KF_NS(II,JJ).NE.MZ_NS.AND.
     $                 HH_NS(II,JJ)-HDEP_NS(II,JJ).GE.GXB) THEN
                      SS = XC_NS(4,II)*YC_NS(4,JJ)
                      SSUM = SSUM+SS
                      HSUM = HSUM+SS*HH_NS(II,JJ)
                    END IF
  420             CONTINUE
  410           CONTINUE
C
                IF(SSUM.GT.0.0D0) THEN
                  HH_ML(I,J) = HSUM/SSUM
                  IF(HH_ML(I,J)-HDEP_ML(I,J).LT.EPSH) THEN
                    HH_ML(I,J) = HDEP_ML(I,J)+EPSH
                  END IF
                  if(idb.ne.0) write(*,*) 'i,j,h_ml',i,j,hh_ml(i,j)
                ELSE
                  HH_ML(I,J) = HDEP_ML(I,J)+EPSH
                END IF
C
  400         CONTINUE
  450       CONTINUE
C
          END IF
C
      RETURN
      END
