      SUBROUTINE STICKCHK(IFRIC,ISTK,NBDY,NINDC,ICBD,INDA,ISLV,ISLVO
     &                   ,FRIC,EPS,ITER2,MAXITER2,ICONV2,IDYN,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ISTK(NBDY,NBDY,2),INDA(NBDY),ICBD(NBDY),ISLV(2,NINDC)
     &         ,ISLVO(2,NINDC),IFRIC(10,NINDC),FRIC(10,NINDC)

      IF( IDYN == 1 .AND. ( ICONV2 == 1 .OR. ITER2 == MAXITER2 ) ) THEN

        NMD1 = 0

        DO I = 1, NINDC
          IF( ISLV(1,I) == 3 .AND. IFRIC(3,I) == 0 .AND.
     &        IFRIC(2,I) /= 2 ) NMD1 = NMD1 + 1
        ENDDO

        IF( NMD1 > 0 ) WRITE(ITO,'(/A,I5)')
     &    '   * FRICTION MODEL CHANGED : STICK-SLIP ---> BILINEAR',NMD1

      ENDIF

      SELECT CASE( IDYN )
      CASE( 1 )

        ISTK(:,:,1) = 0

        DO I = 1, NINDC
          IF( IFRIC(3,I) == 1 .AND. IFRIC(2,I) /= 2 ) THEN
            IBDY = IFRIC(5,I)
            JBDY = IFRIC(6,I)
            ISTK(IBDY,JBDY,1) = 1
          ENDIF
        ENDDO

        IF( ITER2 == 1 ) THEN
          IFRIC(4,:) = 0
          DO I = 1, NINDC
            IF( ISLVO(1,I) == 3 ) IFRIC(4,I) = 1
          ENDDO
        ENDIF

        DO I = 1, NINDC
          IF( IFRIC(4,I) == 1 .AND. ISLV(1,I) /= 3 ) IFRIC(4,I) = 0
        ENDDO

        IF( ICONV2 == 1 .OR. ITER2 == MAXITER2 ) THEN

          DO IBDY = 1, NBDY
            CALL ADDSET4(IS,IE,INDA,IBDY)
            DO JBDY = 1, NBDY
              IF( ISTK(IBDY,JBDY,2) == 1 ) CYCLE
              DO I = IS, IE
                IF( IFRIC(4,I) == 1 ) THEN
                  MA = ISLV(2,I)
                  CALL ADDSET4(JS,JE,ICBD,JBDY)
                  IF( MA >= JS .AND. MA <= JE ) THEN
                    IF( FRIC(5,I) <= EPS ) THEN
                      ISTK(IBDY,JBDY,1) = 1
                    ELSE
                      ISTK(IBDY,JBDY,1) = 0
                      GOTO 10
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
   10       ENDDO
          ENDDO

          ISTK(:,:,2) = ISTK(:,:,1)

        ENDIF

        IFRIC(3,:) = 0

        DO I = 1, NINDC
          IF( ISLV(1,I) == 3 .OR. ISLV(1,I) == 13 ) THEN
            IBDY = IFRIC(5,I)
            JBDY = IFRIC(6,I)
            IFRIC(3,I) = ISTK(IBDY,JBDY,1)
          ENDIF
        ENDDO

      CASE( 0 )

        IFRIC(3,:) = 0

        IF( ICONV2 == 1 .OR. ITER2 == MAXITER2 ) THEN

          ISTK(:,:,2) = 1

          DO I = 1, NINDC
            IF( ISLV(1,I) == 3 ) IFRIC(3,I) = 1
          ENDDO

        ENDIF

      END SELECT

      END