      SUBROUTINE SF_GPRES(PRES0,PP,IPGRID,IGNO,POS,ZZ,NF)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION PRES0(NNOD0),PP(NUMI,NUMJ,NUMK),IPGRID(2,NNOD)
     &         ,POS(3,NNOD),ZZ(MAXG1,NUMK),NF(NUMI,NUMJ,NUMK),IGNO(NNOD)

      REAL(8), POINTER :: PRES(:)

      DATA G0 / 9.8D3 /

      ALLOCATE( PRES(NNOD) )

      PRES(:) = 0.

      DO IG = 1, NNOD

        IJK = IPGRID(1,IG)

        IF( IJK <= 0 ) CYCLE

        CALL SF_IJK(I,J,K,IJK)

        Z = POS(3,IG)
        Z0 = ZZ(1,K) + .5D0 * ZZ(2,K)

        IF( Z >= Z0 ) THEN
          IF( NF(I,J,K+1) >= 0 ) THEN
            INTP = 1
          ELSEIF( NF(I,J,K-1) >= 0 ) THEN
            INTP = 2
          ELSE
            INTP = 3
          ENDIF
        ELSEIF( Z < Z0 ) THEN
          IF( NF(I,J,K-1) >= 0 ) THEN
            INTP = 2
          ELSEIF( NF(I,J,K+1) >= 0 ) THEN
            INTP = 1
          ELSE
            INTP = 3
          ENDIF
        ENDIF

        SELECT CASE( INTP )
        CASE( 1 )
          DZ = Z - Z0
          CZ = ZZ(3,K+1)
          PRES(IG) = ( DZ * PP(I,J,K+1)  + (CZ - DZ) * PP(I,J,K) ) / CZ
        CASE( 2 )
          DZ = Z0 - Z
          CZ = ZZ(3,K)
          PRES(IG) = ( DZ * PP(I,J,K-1)  + (CZ - DZ) * PP(I,J,K) ) / CZ
        CASE( 3 )
          PRES(IG) = PP(I,J,K) + ( Z0 - Z ) * G0
        END SELECT

      ENDDO

      IF( IPART == 0 ) THEN
        CALL SF_MPI_REDUCE_D(PRES,PRES0,NNOD,0)
      ELSEIF( IPART == 1 ) THEN
        CALL SF_REDUCE_D(PRES0,NNOD0,PRES,IGNO,NNOD)
      ENDIF

      DEALLOCATE( PRES )

      END
