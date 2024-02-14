      SUBROUTINE SF_ELEM_RANGE(ISKIP,IS,IE,JS,JE,KS,KE,XX,YY,ZZ,EPS,N,NP
     &                        ,GRID)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK),NP(N)
     &         ,GRID(3,*),XMIN(3),XMAX(3)
!-----------------------------------------------------------------------
      XMIN(:) =  1.D20
      XMAX(:) = -1.D20

      DO I = 1, N
        DO J = 1, 3
          XMIN(J) = DMIN1( XMIN(J), GRID(J,NP(I)) )
          XMAX(J) = DMAX1( XMAX(J), GRID(J,NP(I)) )
        ENDDO
      ENDDO

      ISKIP = 1
 
      DO I = MYIS + 1, MYIE + 1
        IF( XX(1,I) - EPS  >= XMIN(1) ) THEN
          IS = I - 1
          ISKIP = 0
          EXIT
        ENDIF
      ENDDO

      IF( ISKIP == 1 ) RETURN

      IF( MYMIE == 1 ) THEN
        IP = 1
      ELSE
        IP = 0
      ENDIF

      ISKIP = 1

      DO I = MYIE + IP, MYIS, -1
        IF( XX(1,I) - EPS < XMAX(1) ) THEN
          IE = I
          ISKIP = 0
          EXIT
        ENDIF
      ENDDO

      IF( ISKIP == 1 ) RETURN

      ISKIP = 1
 
      DO J = MYJS + 1, MYJE + 1
        IF( YY(1,J) - EPS  >= XMIN(2) ) THEN
          JS = J - 1
          ISKIP = 0
          EXIT
        ENDIF
      ENDDO

      IF( ISKIP == 1 ) RETURN

      IF( MYMJE == 1 ) THEN
        JP = 1
      ELSE
        JP = 0
      ENDIF

      ISKIP = 1

      DO J = MYJE + JP, MYJS, -1
        IF( YY(1,J) - EPS < XMAX(2) ) THEN
          JE = J
          ISKIP = 0
          EXIT
        ENDIF
      ENDDO

      IF( ISKIP == 1 ) RETURN

      ISKIP = 1
 
      DO K = 3, NUMK
        IF( ZZ(1,K) - EPS  >= XMIN(3) ) THEN
          KS = K - 1
          ISKIP = 0
          EXIT
        ENDIF
      ENDDO

      IF( ISKIP == 1 ) RETURN

      ISKIP = 1

      DO K = NUMK, 2, -1
        IF( ZZ(1,K) - EPS < XMAX(3) ) THEN
          KE = K
          ISKIP = 0
          EXIT
        ENDIF
      ENDDO

      END