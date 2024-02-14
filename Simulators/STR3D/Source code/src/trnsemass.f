      SUBROUTINE TRNSEMASS(EMASS,EMASR,ND,NDF,LUMP)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EMASS(*),EMASR(ND,ND)
C-----------------------------------------------------------------------
      N1=ND*NDF
      N2=N1*(N1+1)/2
C
      CALL CLEAR1(EMASS,N2)
C
      IF( NDF .EQ. 3 ) THEN
C
        IF( LUMP .EQ. 0 ) THEN
          IP=1
          DO 200 I=1,N1
            II=(I-1)/3+1
            EMASS(IP)=EMASR(II,1)
            IP=IP+(N1-I+1)
  200     CONTINUE
        ELSE
          IP=0
          DO 100 I=1,N1
            DO 110 J=I,N1
              IP=IP+1
              IF( MOD(I,3) .EQ. MOD(J,3) ) THEN
                II=(I-1)/3+1
                JJ=(J-1)/3+1
                EMASS(IP)=EMASR(II,JJ)
              ENDIF
  110       CONTINUE
  100     CONTINUE
        ENDIF
C
      ELSEIF( NDF .EQ. 6 ) THEN
C
        IF( LUMP .EQ. 0 ) THEN
          IP=1
          DO 400 I=1,N1
            IF( MOD(I,6) .GE. 1 .AND. MOD(I,6) .LE. 3 ) THEN
              II=(I-1)/6+1
              EMASS(IP)=EMASR(II,1)
            ENDIF
            IP=IP+(N1-I+1)
  400     CONTINUE
        ELSE
          IP=0
          DO 300 I=1,N1
            DO 310 J=I,N1
              IP=IP+1
              IF( ( MOD(I,6) .GE. 1 .AND. MOD(I,6) .LE. 3 ) .AND.
     &            ( MOD(J,6) .GE. 1 .AND. MOD(J,6) .LE. 3 ) .AND.
     &            MOD(I,6) .EQ. MOD(J,6) ) THEN
                II=(I-1)/6+1
                JJ=(J-1)/6+1
                EMASS(IP)=EMASR(II,JJ)
              ENDIF
  310       CONTINUE
  300     CONTINUE
        ENDIF
C
      ENDIF
C
      RETURN
      END
