      SUBROUTINE CGADM(STF,ESTF,IDCR,NCR,IDSK,IDCG,INDOF,ISLV,ITO)

      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  STF(*),ESTF(*),IDCR(2,NCR),IDSK(*),INDOF(6,*),IDCG(*)

      IC = 0
      DO I = 1, NCR
        L1 = INDOF( IDCR(2,I), IDCR(1,I) )
        IF( L1 > 0 ) THEN
          DO J = I, NCR
            IC = IC + 1
            L2 = INDOF( IDCR(2,J), IDCR(1,J) )
            IF( L2 <= 0 ) CYCLE
            SELECT CASE( ISLV )
            CASE( 1 )
              IF( L1 >= L2 ) THEN
                M1 = L1
                M2 = L2
              ELSE
                M1 = L2
                M2 = L1
              ENDIF
            CASE( 2:4 )
              IF( L1 <= L2 ) THEN
                M1 = L1
                M2 = L2
              ELSE
                M1 = L2
                M2 = L1
              ENDIF
            CASE( 11: )
              M1 = L1
              M2 = L2
            END SELECT
            DO K = IDSK(M1), IDSK(M1+1) - 1
              IF( IDCG(K) == M2 ) THEN
                STF(K) = STF(K) + ESTF(IC)
                GOTO 10
              ENDIF
            ENDDO
            GOTO 90
   10     ENDDO
        ELSE
          IC = IC + NCR - I + 1
        ENDIF
      ENDDO

      IF( ISLV <= 4 ) RETURN

      IC = 0
      DO I = 1, NCR
        L1 = INDOF( IDCR(2,I), IDCR(1,I) )
        IF( L1 > 0 ) THEN
          IC = IC + 1
          DO J = I + 1, NCR
            IC = IC + 1
            L2 = INDOF( IDCR(2,J), IDCR(1,J) )
            IF( L2 <= 0 ) CYCLE
            M1 = L2
            M2 = L1
            DO K = IDSK(M1), IDSK(M1+1) - 1
              IF( IDCG(K) == M2 ) THEN
                STF(K) = STF(K) + ESTF(IC)
                GOTO 20
              ENDIF
            ENDDO
            GOTO 90
   20     ENDDO
        ELSE
          IC = IC + NCR - I + 1
        ENDIF
      ENDDO

      RETURN

   90 WRITE(ITO,*) 'NO ADRESS FOR [K], PROGRAM STOPPED.'
      CALL ERRSTP(90,ITO)

      END
