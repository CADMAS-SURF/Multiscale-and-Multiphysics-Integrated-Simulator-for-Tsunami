      SUBROUTINE SPCDRHVG_S(RHV,ELHM,KN,ND,INDOP,PG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION PG(*),ELHM(*),KN(ND),INDOP(*),RHV(*)

      DO J = 1, ND
        JJ = INDOP( KN(J) )
        IF( JJ == -1 ) THEN
          DO I = 1, ND
            II = INDOP( KN(I) )
            IF( II > 0 ) THEN
              IF( J >= I ) THEN
                IP = ( 2*ND - I + 2 ) * ( I - 1 ) / 2 + J - I + 1
              ELSE
                IP = ( 2*ND - J + 2 ) * ( J - 1 ) / 2 + I - J + 1
              ENDIF
              RHV(II) = RHV(II) - ELHM(IP) * PG( KN(J) )
            ENDIF
          ENDDO
        ENDIF
      ENDDO

      END
