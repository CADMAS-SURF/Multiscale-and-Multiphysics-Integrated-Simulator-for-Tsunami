      SUBROUTINE CGADMG_S(STF,ELHM,KN,ND,IDSK,IDCG,INDOP,ISLV,ITO)

      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  STF(*),ELHM(*),KN(ND),IDSK(*),INDOP(*),IDCG(*)

      IC = 0
      DO I = 1, ND
        L1 = INDOP( KN(I) )
        IF( L1 > 0 ) THEN
          DO J = I, ND
            IC = IC + 1
            L2 = INDOP( KN(J) )
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
            END SELECT
            DO K = IDSK(M1), IDSK(M1+1) - 1
              IF( IDCG(K) == M2 ) THEN
                STF(K) = STF(K) + ELHM(IC)
                GOTO 10
              ENDIF
            ENDDO
            GOTO 90
   10     ENDDO
        ELSE
          IC = IC + ND - I + 1
        ENDIF
      ENDDO

      RETURN

   90 WRITE(ITO,*) 'NO ADRESS FOR [K], PROGRAM STOPPED.'
      CALL ERRSTP(90,ITO)

      END
