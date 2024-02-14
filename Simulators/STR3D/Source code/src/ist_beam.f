      SUBROUTINE IST_BEAM(IST,SY,S,D,E,ANU,ST,IYLD,HD)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IST(9),SY(9),S(3,9),D(6,9)

      DO J = 1, 9
        SELECT CASE( IST(J) )
        CASE( 0 )
          CALL BEAM0(IST(J),SY(J),S(1,J),D(1,J),E,ANU,ST,IYLD,HD)
        CASE( 2 )
          CALL BEAM2(IST(J),SY(J),S(1,J),D(1,J),E,ANU,ST)
        END SELECT
      ENDDO

      END