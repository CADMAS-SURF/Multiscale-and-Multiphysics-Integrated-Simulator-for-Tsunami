      SUBROUTINE SF_VOLCHK(IFIX0,IELM,IGNO,GRID,POS,XX,YY)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION IFIX0(NNOD0),IELM(24,NELM),NP(20),XG(3),GRID(3,NNOD)
     &         ,POS(3,NNOD),XX(MAXG1,NUMI),YY(MAXG1,NUMJ),IGNO(NNOD)
     &         ,P0(3,4,24),P(3,4,24)

      INTEGER, POINTER :: IFIX(:)

      ALLOCATE( IFIX(NNOD) )

      IFIX(:) = 0

      DO I = 1, NELM

        LTYP = IELM(3,I)
        N = IELM(4,I)
        NP(1:N) = IELM(5:4+N,I)

        IF( LTYP /= 2 ) CYCLE

        CALL SF_MEAN4(XG,POS,3,NP,N)

        IF( XG(1) >= XX(1,MYIS) .AND. XG(1) < XX(1,MYIE+1) .AND.
     &      XG(2) >= YY(1,MYJS) .AND. XG(2) < YY(1,MYJE+1) ) THEN

          CALL SF_TETDIV(NT,P0,N,NP,GRID)
          CALL SF_TETDIV(NT,P,N,NP,POS)

          DO J = 1, NT

            CALL SF_TETVOL(P0(1,1,J),V0)
            CALL SF_TETVOL(P(1,1,J),V)

            IF( V/V0 < .2D0 ) THEN
              IFIX( NP(1:N) ) = 1
              EXIT
            ENDIF

          ENDDO

        ENDIF

      ENDDO

      IF( IPART == 0 ) THEN
        CALL SF_MPI_REDUCE_I(IFIX,IFIX0,NNOD,0)
      ELSEIF( IPART == 1 ) THEN
        CALL SF_REDUCE_I(IFIX0,NNOD0,IFIX,IGNO,NNOD)
      ENDIF

      DEALLOCATE( IFIX )

      END
