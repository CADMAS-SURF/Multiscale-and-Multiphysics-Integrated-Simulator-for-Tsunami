      SUBROUTINE SF_RFACE0()

      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION E(3)

      IF( MYRANK > 0 ) RETURN
      
      CR1 = -DSIN( 85.D0 / 180.D0 * PI )
      CR2 =  DSIN( 15.D0 / 180.D0 * PI )

      ALLOCATE( IRFACE(NPFC0) )

      IRFACE(:) = 0

      DO I = 1, NPFC0
        IE = IPFACE0(3,I)
        IF( IELM(3,IE) == 2 ) THEN
          CALL SF_NRMVEC(E,IPFACE0(5,I),IPFACE0(4,I),GRID)
          IF( E(3) < CR1 ) THEN
            IRFACE(I) = 1
          ELSEIF( E(3) > CR2 ) THEN
            IRFACE(I) = 2
          ENDIF
        ENDIF
      ENDDO

      ALLOCATE( IRGRID0(NNOD0) )

      IRGRID0(:) = 0

      DO I = 1, NELM
        IF( IELM(3,I) == 2 ) THEN
          N = IELM(4,I)
          IRGRID0( IELM(5:4+N,I) ) = -1
        ENDIF
      ENDDO

      DO I = 1, NPFC0
        IF( IRFACE(I) == 2 ) THEN
          N = IPFACE0(4,I)
          IRGRID0( IPFACE0(5:4+N,I) ) = 1
        ENDIF
      ENDDO

      DO I = 1, NPFC0
        IF( IRFACE(I) == 1 ) THEN
          N = IPFACE0(4,I)
          IRGRID0( IPFACE0(5:4+N,I) ) = 0
        ENDIF
      ENDDO

      ALLOCATE( DELZ0(NNOD0) )
      ALLOCATE( IFIX0(NNOD0) )

      END