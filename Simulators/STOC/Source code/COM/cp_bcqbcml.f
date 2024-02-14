      SUBROUTINE CP_BCQBCML(IEAS,IWES,JSOU,JNOR,
     $                      IS,IE,JS,JE,NESTXM,NESTXP,NESTYM,NESTYP,
     $                      MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     $                      I_ML,J_ML,KF_ML,KF_NS,
     $                      XC_NS,YC_NS,QBXC_NS,QBYC_NS,QBXC_ML,QBYC_ML)
C----------------------------------------------------------------------
C     セル中心の掃流砂量をMLエリアのセルに直接セットする
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'TIMEI.h'
      INCLUDE 'CONNEC.h'
C
      INTEGER,INTENT(IN)::IEAS,IWES,JSOU,JNOR
      INTEGER,INTENT(IN)::IS,IE,JS,JE,NESTXM,NESTXP,NESTYM,NESTYP
      INTEGER,INTENT(IN)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      INTEGER,INTENT(IN)::I_ML(2,MX_ML),J_ML(2,MY_ML)
      INTEGER,INTENT(IN)::KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    KF_NS(MX_NS,MY_NS)
      REAL(8),INTENT(IN)::XC_NS(8,MX_NS),YC_NS(8,MY_NS)
      REAL(8),INTENT(IN)::QBXC_NS(MX_NS,MY_NS),QBYC_NS(MX_NS,MY_NS)
      REAL(8),INTENT(INOUT)::QBXC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                       QBYC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
C
      INTEGER::I,I1,I2,J,J1,J2,IEM,ISKIP
C
      if(istep.eq.1) then
        write(*,*) '# CP_BCQBML is,ie,js,je=',is,ie,js,je
        write(*,*) '    nest(xm,xp,ym,yp)=',nestxm,nestxp,nestym,nestyp
      end if
C
C     <IS.NE.IE>
C
      IF(IS.NE.IE) THEN
C
C     下側境界の処理(J=JS,JE-1:I=IS,IE-1)
C
      IF(JS.NE.JE) THEN
        DO 100 J = JS,JE
          IF(J.LT.JS+NESTYM.AND.IPECON(4,NRANK+1).LT.0) GO TO 100
          IF(J.GT.JE-NESTYP) GO TO 100
          ISKIP = 0
          IF(J.EQ.JS+NESTYM.AND.IPECON(4,NRANK+1).GE.0) ISKIP=1
          J1 = J
          J2 = J-1
          DO 150 I = IS,IE
            IF(ISKIP.EQ.1.AND.I.NE.IS+NESTXM) GO TO 150
            IF(ISKIP.EQ.1.AND.I.EQ.IS+NESTXM.AND.
     &                IPECON(5,NRANK+1).GE.0) GO TO 150
            IF(I.LT.IS+NESTXM.AND.IPECON(5,NRANK+1).LT.0) GO TO 150
            IF(J.GT.JS+NESTYM.AND.I.GT.IS+NESTXM) GO TO 150
            IF(I.GT.IE-NESTXP) GO TO 150
            IF(I.EQ.IS+NESTXM.AND.
     &         J.GT.JS+NESTYM.AND.IPECON(5,NRANK+1).GE.0) GO TO 150
            I1 = I
            I2 = I-1
            CALL CP_AVEQBC(IEAS,IWES,JSOU,JNOR,I1,I2,J1,J2,
     $                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     $                     I_ML,J_ML,KF_ML,KF_NS,
     $                     XC_NS,YC_NS,QBXC_NS,QBYC_NS,QBXC_ML,QBYC_ML)
  150     CONTINUE
  100   CONTINUE
      END IF
C
C     <IS.NE.IE>,<JS.NE.JE>
C     上側境界の処理(J=JE,I=IS,IE-1)
C
      IF(IPECON(7,NRANK+1).LT.0) THEN
        J  = JE-NESTYP
        IF(JS.EQ.JE) J=JE
        J1 = J
        J2 = J
        IEM = IE-1
        IF(IPECON(6,NRANK+1).GE.0) IEM=IE
        DO 200 I = IS,IEM
          IF(I.LT.IS+NESTXM) GO TO 200
          IF(JS.EQ.JE.AND.I.GT.IS+NESTXM) GO TO 200
          IF(I.GE.IE-NESTXP+1.AND.
     &       IPECON(6,NRANK+1).LT.0) GO TO 200
          I1 = I
          I2 = I-1
          CALL CP_AVEQBC(IEAS,IWES,JSOU,JNOR,I1,I2,J1,J2,
     $                   MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     $                   I_ML,J_ML,KF_ML,KF_NS,
     $                   XC_NS,YC_NS,QBXC_NS,QBYC_NS,QBXC_ML,QBYC_ML)
  200     CONTINUE
        END IF
C
C     <IS.NE.IE>
C     右側境界の処理(I=IE,J=JS,JE)
C
      IF(IPECON(6,NRANK+1).LT.0) THEN
        I  = IE-NESTXP
        I1 = I
        I2 = I
        DO 300 J = JS,JE
          IF(JS.NE.JE) THEN
            IF(J.LT.JS+NESTYM) GO TO 300
            IF(J.GT.JE-NESTYP) GO TO 300
          END IF
          J1 = J
          J2 = J
          IF(JS.NE.JE.AND.J.EQ.JE-NESTYP+1) J2=J-1
          CALL CP_AVEQBC(IEAS,IWES,JSOU,JNOR,I1,I2,J1,J2,
     $                   MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     $                   I_ML,J_ML,KF_ML,KF_NS,
     $                   XC_NS,YC_NS,QBXC_NS,QBYC_NS,QBXC_ML,QBYC_ML)
  300   CONTINUE
      END IF
C
      ELSE
C
C     <IS.EQ.IE>
C     左(=右)側境界の処理(I=IS,J=JS,JE)
C
      I1 = IS
      I2 = IS
      DO 400 J = JS,JE
        IF((J.NE.JS+NESTYM).AND.(J.NE.JE-NESTYP)) GO TO 400
        J1 = J
        J2 = J
        IF(J.EQ.JS+NESTYM) J2=J-1
        CALL CP_AVEQBC(IEAS,IWES,JSOU,JNOR,I1,I2,J1,J2,
     $                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     $                 I_ML,J_ML,KF_ML,KF_NS,
     $                 XC_NS,YC_NS,QBXC_NS,QBYC_NS,QBXC_ML,QBYC_ML)
  400 CONTINUE
C
      END IF
C
      RETURN
      END
