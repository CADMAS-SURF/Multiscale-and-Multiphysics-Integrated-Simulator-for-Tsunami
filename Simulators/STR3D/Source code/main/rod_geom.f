      SUBROUTINE ROD_GEOM( NG, IG, NE, IE, IP, IPART, JPART, INDG, GRID,
     &                     IELM, NM, NNOD, NROD, NUG, LG, IFG )
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION NUG(NNOD), IELM(NM,NROD), IE(NROD), IG(NNOD), LG(NNOD),
     &          INDG(NNOD), GRID(3,NNOD), KN(2), IP(NROD)
C
      NE = 0
      IE(:) = 0
      NUG(:) = 0
C
      DO I = 1, NROD
        IF( IP(I) /= IPART) CYCLE
        NE = NE + 1
        IE(NE) = I
        NUG( IELM(8:9,I) ) = 1
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
      WRITE(IFG,'(A,X,I0)') 'rod elements', JPART
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
      WRITE(IFG,'(A)') 'bar2'
C
      WRITE(IFG,'(I10)') NE
C
      DO I = 1, NE
        IENO = IE(I)
        WRITE(IFG,'(I10)') IELM(1,IENO)
      ENDDO
C
      DO I = 1, NE
        IENO = IE(I)
        KN(1:2) = IELM(8:9,IENO)
        WRITE(IFG,'(2I10)') LG( KN(1:2) )
      ENDDO
C
      END