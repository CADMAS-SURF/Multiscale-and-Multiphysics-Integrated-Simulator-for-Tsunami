      SUBROUTINE FRICSET(U0,RL0,IFRIC,NINDC,ISLV,INDC,IEDG,IELC,UG,POS
     &                  ,ISTEP,ITER2,IDYN)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U0(3,4,NINDC),RL0(3,NINDC),ISLV(2,NINDC),INDC(NINDC)
     &         ,IEDG(6,*),IELC(3,*),UG(6,*),POS(3,*),IFRIC(10,NINDC)
C-----------------------------------------------------------------------
      DO I = 1, NINDC
C
        IF( IFRIC(2,I) == 3 ) IFRIC(2,I) = 1
        IFRIC(7,I) = 1
C
        IF( IFRIC(1,I) == 1 .OR.
     &      ( IDYN == 1 .AND. IFRIC(1,I) == 2 .AND. ITER2 == 1 .AND.
     &        ( IFRIC(3,I) == 0 .OR. ISTEP == 1 ) ) ) THEN
C
          NS = INDC(I)
          IST = ISLV(1,I)
          MA  = ISLV(2,I)
C
          IF( IST == 2 ) THEN
            CALL FRICEDGE(U0(1,1,I),RL0(1,I),NS,IEDG(1,MA),UG,POS)
          ELSEIF( IST == 3 ) THEN
            CALL FRICFACE(U0(1,1,I),RL0(1,I),NS,IELC(1,MA),UG,POS)
          ENDIF
C
          IFRIC(2,I) = 1
          IFRIC(9,I) = 1
C
        ENDIF
C
      ENDDO
C
      END
