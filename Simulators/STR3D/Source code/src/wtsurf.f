      SUBROUTINE WTSURF( AFC, NMAT, NPFC, N_PART, NE_ENSW, IE_ENSW, IFL)
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*8 ETYPE(4)
      DIMENSION NE_ENSW(4,NMAT), IE_ENSW(NPFC,4,NMAT), AFC(NPFC)
C
      DATA ETYPE / 'tria3   ', 'quad4   ', 'tria6   ', 'quad8   ' /
C
      WRITE(IFL,'(A)') 'effective surface area'
C
      DO IP = 1, NMAT
C
        WRITE(IFL,'(A)') 'part'
        WRITE(IFL,'(I10)') N_PART + IP
C
        DO ITYP = 1, 4
C
          IF( NE_ENSW(ITYP,IP) == 0 ) CYCLE
C
          WRITE(IFL,'(A)') ETYPE(ITYP)
C
          DO I = 1, NE_ENSW(ITYP,IP)
            IENO = IE_ENSW(I,ITYP,IP)
            WRITE(IFL,'(1PE12.5)') AFC( IENO )
          ENDDO
C
        ENDDO
C
      ENDDO
C
      END
