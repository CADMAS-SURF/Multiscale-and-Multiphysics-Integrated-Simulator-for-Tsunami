      SUBROUTINE FRICCHK(IFRIC,FRIC,NBDY,NINDC,ICBD,INDA,IEDA,ISLV
     &                  ,ISLVO,FRTB,EDML,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ISLVO(2,NINDC),ISLV(2,NINDC),IFRIC(10,NINDC)
     &         ,FRIC(10,NINDC),INDA(NBDY),IEDA(NBDY),FRTB(3,NBDY,NBDY)
     &         ,ICBD(NBDY),EDML(NBDY)
C----&------------------------------------------------------------------
      IFRIC(1,:) = 0
      IFRIC(8,:) = 0
C
      DO IBDY = 1, NBDY
        IF( IBDY == 1 ) THEN
          IS = 1
        ELSE
          IS = INDA(IBDY-1) + 1
        ENDIF
        IE = INDA(IBDY)
        DO 10 I = IS, IE
          IF( ISLV(1,I) == 2 ) THEN
            IF( ISLVO(1,I) == 2 ) THEN
              IFRIC(1,I) = 2
            ELSE
              IFRIC(1,I) = 1
            ENDIF
          ELSEIF( ISLV(1,I) == 3 ) THEN
            IF( ISLVO(1,I) == 3 ) THEN
              IFRIC(1,I) = 2
            ELSE
              IFRIC(1,I) = 1
            ENDIF
          ELSEIF( ISLV(1,I) == 12 .OR. ISLV(1,I) == 13 ) THEN
            IFRIC(1,I) = 1
          ENDIF
          IF( IFRIC(1,I) == 0 ) GOTO 10
          MA = ISLV(2,I)
          DO JBDY = 1, NBDY
            IF( ISLV(1,I) == 2 .OR. ISLV(1,I) == 12 ) THEN
              MAE = IEDA(JBDY)
            ELSEIF( ISLV(1,I) == 3 .OR. ISLV(1,I) == 13 ) THEN
              MAE = ICBD(JBDY)
            ENDIF
            IF( MA <= MAE ) THEN
              IFRIC(5,I) = IBDY
              IFRIC(6,I) = JBDY
              FRIC(1,I) = FRTB(1,IBDY,JBDY)
              IF( FRIC(1,I) == 0.D0 ) IFRIC(1,I) = 0
              IF( IFRIC(1,I) > 0 .AND. ISLVO(1,I) == 0 ) IFRIC(8,I) = 1
              FRIC(2,I) = EDML(JBDY) * 1.D-2 * FRTB(2,IBDY,JBDY)
              FRIC(4,I) = FRTB(3,IBDY,JBDY)
              GOTO 10
            ENDIF
          ENDDO
          WRITE(ITO,'(/X,A)') 'STOP IN SUB. FRICCHK!'
          CALL ERRSTP(90,ITO)
   10   CONTINUE
      ENDDO
C
      END