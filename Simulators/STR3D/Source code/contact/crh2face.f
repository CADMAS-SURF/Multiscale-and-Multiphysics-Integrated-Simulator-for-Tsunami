      SUBROUTINE CRH2FACE(CRH,NS,J2,J3,IELC,IFCQ,POS)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,4),POS(3,*),IELC(3),RL(3),E(3),CRH(*)
C----&------------------------------------------------------------------
      DO 110 J=1,3
        CALL SHIFT1(XYZ(1,J),POS(1,IELC(J)),3)
  110 CONTINUE
      CALL SHIFT1(XYZ(1,4),POS(1,NS),3)
C
      CALL AREACD(RL,E,H,XYZ)
C
      DO 200 J=1,3
        IF( J .NE. J2 .AND. J .NE. J3 ) THEN
          J1=J
          GOTO 210
        ENDIF
  200 CONTINUE
C
  210 E0=E(J1)
C
      CRH(1)=-E(J2)/E0
      CRH(2)=-E(J3)/E0
C
      IF( IFCQ .EQ. 0 ) THEN
C
        IP=2
        DO 130 K1=1,3
          DO 131 K2=1,3
            IP=IP+1
            CRH(IP)=RL(K1)*E(K2)/E0
  131     CONTINUE
  130   CONTINUE
C
      ELSE
C
        IP=2
        DO 140 K1=1,2
          DO 141 K2=1,3
            IP=IP+1
            CRH(IP)=(RL(K1)+.25D0*RL(3))*E(K2)/E0
  141     CONTINUE
  140   CONTINUE
C
        DO 150 K1=3,4
          DO 151 K2=1,3
            IP=IP+1
            CRH(IP)=.25D0*RL(3)*E(K2)/E0
  151     CONTINUE
  150   CONTINUE
C
      ENDIF
C
      END
