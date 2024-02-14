      SUBROUTINE CP_MODZBD(ZBED_NS,ZBED_ML,
     $                     I_ML,J_ML,MX_ML,MY_ML,MX_NS,MY_NS,
     $                     NESXM,NESXP,NESYM,NESYP,
     $                     IEAS,IWES,JSOU,JNOR)
C-----------------------------------------------------------------------
C     オーバーラップ領域のZBEDを修正する
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'CONNEC.h'
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MX_NS,MY_NS
      INTEGER,INTENT(INOUT)::NESXM,NESXP,NESYM,NESYP
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR
C
      INTEGER,INTENT(INOUT)::I_ML(2,MX_ML),J_ML(2,MY_ML)
C
      REAL(8),INTENT(INOUT)::ZBED_NS(MX_NS,MY_NS)
      REAL(8),INTENT(INOUT)::ZBED_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
C
      INTEGER::I,I1,I2,II,J,J1,J2,JJ
      INTEGER::NESTXM,NESTXP,NESTYM,NESTYP
      INTEGER::IOVR=0
C
C
      NESTXM = NESXM-IOVR
      NESTXP = NESXP-IOVR
      NESTYM = NESYM-IOVR
      NESTYP = NESYP-IOVR
C
C ... 親のZBEDを子にコピー
C
!!!!!!!!!!!!!!!!!!WEST!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if(mx_ns.gt.3)then
      DO 100 JJ = JSOU,JNOR
         IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 100
         J1 = J_ML(1,JJ-1)+1
         J2 = J_ML(1,JJ)
c         DO 110 II = IWES,IWES+NESTXM
         DO 110 II = IWES,IWES
            IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 110
            I1 = I_ML(1,II-1)+1
            I2 = I_ML(1,II)
C
            DO J = J1,J2
            DO I = I1,I2
               ZBED_NS(I,J) = ZBED_ML(II,JJ)
            ENDDO
            ENDDO
C
  110    CONTINUE
  100 CONTINUE
      endif
C
!!!!!!!!!!!!!!!!!!EAST!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if(mx_ns.gt.3)then
      DO 150 JJ = JSOU,JNOR
         IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 150
         J1 = J_ML(1,JJ-1)+1
         J2 = J_ML(1,JJ)
c         DO 160 II = IEAS-NESTXP,IEAS
         DO 160 II = IEAS,IEAS
            IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 160
            I1 = I_ML(1,II-1)+1
            I2 = I_ML(1,II)
C
            DO J = J1,J2
            DO I = I1,I2
               ZBED_NS(I,J) = ZBED_ML(II,JJ)
            ENDDO
            ENDDO
C
  160    CONTINUE
  150 CONTINUE
      endif
C
!!!!!!!!!!!!!!!!!!SOUTH!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if(my_ns.gt.3)then
c      DO 200 JJ = JSOU,JSOU+NESTYM
      DO 200 JJ = JSOU,JSOU
         IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 200
         J1 = J_ML(1,JJ-1)+1
         J2 = J_ML(1,JJ)
c         DO 210 II = IWES+NESTXM+1,IEAS-NESTXP-1
         DO 210 II = IWES,IEAS
            IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 210
            I1 = I_ML(1,II-1)+1
            I2 = I_ML(1,II)
C
            DO J = J1,J2
            DO I = I1,I2
               ZBED_NS(I,J) = ZBED_ML(II,JJ)
            ENDDO
            ENDDO
C
  210    CONTINUE
  200 CONTINUE
      endif
C
!!!!!!!!!!!!!!!!!!NORTH!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if(my_ns.gt.3)then
c      DO 250 JJ = JNOR-NESTYP,JNOR
      DO 250 JJ = JNOR,JNOR
         IF(J_ML(1,JJ-1).LE.0.OR.J_ML(1,JJ).LE.0) GO TO 250
         J1 = J_ML(1,JJ-1)+1
         J2 = J_ML(1,JJ)
c         DO 260 II = IWES+NESTXM+1,IEAS-NESTXP-1
         DO 260 II = IWES,IEAS
            IF(I_ML(1,II-1).LE.0.OR.I_ML(1,II).LE.0) GO TO 260
            I1 = I_ML(1,II-1)+1
            I2 = I_ML(1,II)
C
            DO J = J1,J2
            DO I = I1,I2
               ZBED_NS(I,J) = ZBED_ML(II,JJ)
            ENDDO
            ENDDO
C
  260    CONTINUE
  250 CONTINUE
      endif
C
      RETURN
      END
