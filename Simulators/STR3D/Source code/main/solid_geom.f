      SUBROUTINE SOLID_GEOM( NG, IG, NE, IE, IP, IPART, JPART, IMAT,
     &                       INDG, GRID, IELM, NM, NNOD, NSOL, NUG, LG,
     &                       IFG )
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8 ETYPE(6)
      CHARACTER*14 MATL(2)
      DIMENSION NE(6), NUG(NNOD), IELM(NM,NSOL), IE(NSOL,6), IG(NNOD),
     &          LG(NNOD), INDG(NNOD), GRID(3,NNOD), KN(20), IWK(4),
     &          IP(NSOL)
C
      DATA ETYPE / 'tetra4  ', 'penta6  ', 'hexa8   ', 'tetra10 ',
     &             'penta15 ', 'hexa20  ' /
      DATA MATL / 'solid elements', 'soil elements ' /
C
      NE(:) = 0
      IE(:,:) = 0
      NUG(:) = 0
C
      DO I = 1, NSOL
C
        IF( IP(I) /= IPART ) CYCLE
C
        ND = IELM(3,I)
C
        SELECT CASE( ND )
        CASE( 4 )
          ITYP = 1
        CASE( 6 )
          ITYP = 2
        CASE( 8 )
          ITYP = 3
        CASE( 10 )
          ITYP = 4
        CASE( 15 )
          ITYP = 5
        CASE( 20 )
          ITYP = 6
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
      WRITE(IFG,'(A,X,I0)') MATL(IMAT), JPART
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
      DO ITYP = 1, 6
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
          KN(1:ND) = IELM(8:7+ND,IENO)
C
          IF( ND == 15 ) THEN
            IWK(1:3) = KN(10:12)
            KN(10:12) = KN(13:15)
            KN(13:15) = IWK(1:3)
          ELSEIF( ND == 20 ) THEN
            IWK(:) = KN(13:16)
            KN(13:16) = KN(17:20)
            KN(17:20) = IWK(:)
          ENDIF
C
          WRITE(IFG,'(20I10)') LG( KN(1:ND) )
C
        ENDDO
C
      ENDDO
C
      END