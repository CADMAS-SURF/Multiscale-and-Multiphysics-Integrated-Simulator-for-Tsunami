      SUBROUTINE SF_TETDIV(NT,P,N,NP,XX)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION P(3,4,24),NP(N),XX(3,*),IP(4,24),XG(3,6)
!-----------------------------------------------------------------------
      SELECT CASE( N )
      CASE( 4, 10 )

        NT = 1

        IP(:,1) = NP(1:4)

      CASE( 6, 15 )

        NT = 11

        CALL SF_MKTEPN(IP,NP)

        CALL SF_GSFPN(XG,XX,NP)

      CASE( 8, 20 )

        NT = 24

        CALL SF_MKTEHX(IP,NP)

        CALL SF_GSFHX(XG,XX,NP)

      END SELECT

      DO I = 1, NT
        DO J = 1, 4
          IG = IP(J,I)
          IF( IG > 0 ) THEN
            P(:,J,I) = XX(:,IG)
          ELSEIF( IG < 0 ) THEN
            P(:,J,I) = XG(:,-IG)
          ELSE
            CALL VF_A2ERR('SF_TETDIV','P.G ERROR.')
          ENDIF
        ENDDO
      ENDDO

      END