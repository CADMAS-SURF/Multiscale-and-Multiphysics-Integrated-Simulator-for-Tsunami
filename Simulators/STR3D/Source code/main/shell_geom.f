      SUBROUTINE SHELL_GEOM( NG, IG, NE, IE, IP, IPART, JPART, INDG,
     &                       GRID, IELM, NM, NNOD, NSHL, NUG, LG, IFG )
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8 ETYPE(4)
      DIMENSION NE(4), NUG(NNOD), IELM(NM,NSHL), IE(NSHL,4), IG(NNOD),
     &          LG(NNOD), INDG(NNOD), GRID(3,NNOD), KN(8), IP(NSHL)
C
      DATA ETYPE / 'tria3   ', 'quad4   ', 'tria6   ', 'quad8   ' /
C
      NE(:) = 0
      IE(:,:) = 0
      NUG(:) = 0
C
      DO I = 1, NSHL
C
        IF( IP(I) /= IPART ) CYCLE
C
        ND = IELM(3,I)
        IF( ND == 9 ) ND = 8
C
        SELECT CASE( ND )
        CASE( 3 )
          ITYP = 1
        CASE( 4 )
          ITYP = 2
        CASE( 6 )
          ITYP = 3
        CASE( 8 )
          ITYP = 4
        END SELECT
C
        NE(ITYP) = NE(ITYP) + 1
        IE( NE(ITYP), ITYP ) = I
        NUG( IELM(8:7+ND,I) ) = 1
C
      ENDDO
C
      NG = 0
      IG(:) = 0
C
      DO I = 1, NNOD
        IF( NUG(I) == 1 ) THEN
          NG = NG + 1
          IG(NG) = I
          LG(I) = NG
        ENDIF
      ENDDO
C
      WRITE(IFG,'(A4)') 'part'
C
      WRITE(IFG,'(I10)') IPART
C
      WRITE(IFG,'(A,X,I0)') 'shell elements', JPART
C
      WRITE(IFG,'(A)') 'coordinates'
C
      WRITE(IFG,'(I10)') NG
C
      DO I = 1, NG
        WRITE(IFG,'(I10)') INDG(IG(I))
      ENDDO
C
      DO J = 1, 3
        DO I = 1, NG
          WRITE(IFG,'(1PE12.5)') GRID(J,IG(I))
        ENDDO
      ENDDO
C
      DO ITYP = 1, 4
C
        IF( NE(ITYP) == 0 ) CYCLE
C
        WRITE(IFG,'(A)') ETYPE(ITYP)
C
        WRITE(IFG,'(I10)') NE(ITYP)
C
        DO I = 1, NE(ITYP)
          IENO = IE(I,ITYP)
          WRITE(IFG,'(I10)') IELM(1,IENO)
        ENDDO
C
        DO I = 1, NE(ITYP)
C
          IENO = IE(I,ITYP)
C
          ND = IELM(3,IENO)
          IF( ND == 9 ) ND = 8

          KN(1:ND) = IELM(8:7+ND,IENO)
C
          WRITE(IFG,'(20I10)') LG( KN(1:ND) )
C
        ENDDO
C
      ENDDO
C
      END