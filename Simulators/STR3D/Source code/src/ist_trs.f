      SUBROUTINE IST_TRS(IST,SY,EPSY,S,GR,E,ST,IYLD,HD,EPS)

      IMPLICIT REAL*8(A-H,O-Z)

      IF( IST == 1 ) RETURN

      IF( ST > 0. ) THEN
        IF( S > ST ) THEN
          IST = 1
          GR = 0.
          S = 0.
          RETURN
        ENDIF
      ENDIF

      IF( IYLD > 0 ) THEN
        SELECT CASE( IST )
        CASE( 0 )
          IF( S > SY ) THEN
            IST = 2
            GR = E * HD / ( E + HD )
            S = SY + GR * ( EPS - EPSY )
            EPSY = EPS
            SY = S
          ENDIF
        CASE( 2 )
          IF( S >= SY ) THEN
            EPSY = EPS
            SY = S
          ELSE
            IST = 0
            GR = E
            S = SY + GR * ( EPS - EPSY )
          ENDIF
        END SELECT
      ENDIF

      END
