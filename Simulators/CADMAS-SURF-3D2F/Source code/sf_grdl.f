      SUBROUTINE SF_GRDL(GRDL,GRID,NP,NNP)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,8),GRID(3,*),NP(NNP),IP(2,12),XG(3),EL(12)
     &         ,GRDL(*)

      DATA IP / 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8, 8,5,
     &          1,5, 2,6, 3,7, 4,8 /

      X(:,1:NNP) = GRID(:,NP(1:NNP))

      SELECT CASE( NNP )
      CASE( 4 )

        XG(:) = ( X(:,1) + X(:,2) + X(:,3) + X(:,4) ) / 4.D0

        DO I = 1, 4
          CALL SF_LENGTH(EL(I),X(1,I),XG,3)
        ENDDO

        EL(5:12) = 1.D20

      CASE( 6 )

        DO I = 1, 3
          CALL SF_LENGTH(EL(I),X(1,I),X(1,I+3),3)
          EL(I) = EL(I) * .5D0
        ENDDO

        XG(:) = ( X(:,1) + X(:,2) + X(:,3) ) / 3.D0

        DO I = 1, 3
          CALL SF_LENGTH(EL(I+3),X(1,I),XG,3)
        ENDDO

        XG(:) = ( X(:,4) + X(:,5) + X(:,6) ) / 3.D0

        DO I = 1, 3
          CALL SF_LENGTH(EL(I+6),X(1,I+3),XG,3)
        ENDDO

        EL(10:12) = 1.D20

      CASE( 8 )

        DO I = 1, 12
          CALL SF_LENGTH(EL(I),X(1,IP(1,I)),X(1,IP(2,I)),3)
          EL(I) = EL(I) * .5D0
        ENDDO

      END SELECT

      EL_MIN = MINVAL( EL )

      DO I = 1, NNP
        GRDL(NP(I)) = DMIN1( GRDL(NP(I)), EL_MIN )
      ENDDO

      END
