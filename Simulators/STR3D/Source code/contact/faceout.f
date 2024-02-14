      SUBROUTINE FACEOUT(ISLV,RSLV,POSSO,RLN,IELC,IEDG,IELA,POSO,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IELC(3),POSO(3,*),RLO(3),RLN(3),RLX(3),ISLV(2),RSLV(3)
     &         ,IEDG(6,*),IELA(3),N(3),POSSO(3)
      DATA TOL1 / 1.D-10 /
      DATA TOL2 / 1.D-6 /
C----&------------------------------------------------------------------
      CALL RLCORR(RLO,POSSO,IELC,POSO)
C
      N(1) = IELC(1)
      N(2) = IELC(2)
      N(3) = IELC(3)
C
      DO I1 = 1, 3
C
        SELECT CASE(I1)
        CASE(1)
          I2 = 2
          I3 = 3
        CASE(2)
          I2 = 3
          I3 = 1
        CASE(3)
          I2 = 1
          I3 = 2
        END SELECT
C
        DRL = RLO(I1) - RLN(I1)
        IF( DABS(DRL) < TOL1 ) CYCLE
C
        S = RLO(I1)/DRL
        RLX(I2) = ( 1. - S )*RLO(I2) + S*RLN(I2)
        RLX(I3) = 1. - RLX(I2)
C
        IF( ( S > 0.D0 .AND. S < 1.D0 ) .AND.
     &      ( RLX(I2) >= 0.D0 .AND. RLX(I2) <= 1.D0 ) ) THEN
C
          IEDGE = IELA(I1)
          IG1 = IEDG(1,IEDGE)
          IG2 = IEDG(2,IEDGE)
C
          IF( IG1 == N(I2) ) THEN
            T = RLX(I3)
          ELSE
            T = RLX(I2)
          ENDIF
C
          IF( T < TOL2 ) THEN
            ISLV(1) = 11
            ISLV(2) = IG1
          ELSEIF( T > 1.-TOL2 ) THEN
            ISLV(1) = 11
            ISLV(2) = IG2
          ELSE
            ISLV(1) = 12
            ISLV(2) = IEDGE
            RSLV(1) = T
          ENDIF
C
          RETURN
C
        ENDIF
C
      ENDDO
C
      WRITE(ITO,'(/X,A)') 'STOP IN SUB. FACEOUT!'
      CALL ERRSTP(90,ITO)
C
      END