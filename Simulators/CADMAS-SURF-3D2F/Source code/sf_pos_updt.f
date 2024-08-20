      SUBROUTINE SF_POS_UPDT(POS,SPC,GRDL,IGFC,IELM,POSN,XX,YY,ZZ)

      USE SF_TYPE

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'SF_STRUCT.h'

      TYPE(SPACE) :: SPC(NELM)

      DIMENSION POS(3,NNOD),POSN(3,NNOD),IGFC(NNOD),GRDL(NNOD)
     &         ,IELM(24,NELM),XX(MAXG1,NUMI),YY(MAXG1,NUMJ)
     &         ,ZZ(MAXG1,NUMK),XMIN(MAXG1),YMIN(MAXG1),ZMIN(MAXG1)

      INTEGER, POINTER :: IRND(:), IFIX(:)

      ALLOCATE( IRND(NNOD) )

      IRND(:) = 0

      IF( ISTM == 1 ) THEN
        DO I = 1, NELM
          IF( IELM(3,I) == 2 ) THEN
            N = IELM(4,I)
            IRND( IELM(5:4+N,I) ) = 1
          ENDIF
        ENDDO
      ENDIF

      XMIN = MINVAL(XX,DIM=2)
      YMIN = MINVAL(YY,DIM=2)
      ZMIN = MINVAL(ZZ,DIM=2)

      DMIN = DMIN1(XMIN(2),YMIN(2),ZMIN(2))

      CALL SF_MPI_ALLREDUCE_D(DMIN,D_MIN,1,2)

      ALLOCATE( IFIX(NNOD) )

      IFIX(:) = 0

      DO I = 1, NNOD

        IF( IGFC(I) == 1 ) THEN
          TOL = D_MIN * 1.D-3
        ELSEIF( IRND(I) == 1 ) THEN
          TOL = GRDL(I) * 1.D-2
        ELSE
          TOL = GRDL(I) * 1.D-1
        ENDIF

        CALL SF_LENGTH(DP,POSN(1,I),POS(1,I),3)

        IF( DP < TOL ) THEN
          IFIX(I) = 1
        ELSE
          POS(:,I) = POSN(:,I)
        ENDIF

      ENDDO

      DO I = 1, NELM

        SPC(I)%IFIX = 1

        N = IELM(4,I)

        DO J = 1, N
          IF( IFIX( IELM(4+J,I) ) == 0 ) THEN
            SPC(I)%IFIX = 0
            EXIT
          ENDIF
        ENDDO

      ENDDO

      DEALLOCATE( IRND )
      DEALLOCATE( IFIX )

      END
