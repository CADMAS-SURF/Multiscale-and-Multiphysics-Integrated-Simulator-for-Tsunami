      SUBROUTINE WTSOIL( VELG, NNOD, IPS, IPE, NG_ENS, IG_ENS, IFL )
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION VELG(3,NNOD), NG_ENS(*), IG_ENS(NNOD,*)
C
      WRITE(IFL,'(A)') 'pore water flux'
C
      DO IP = IPS, IPE
C
        WRITE(IFL,'(A)') 'part'
        WRITE(IFL,'(I10)') IP
        WRITE(IFL,'(A)') 'coordinates'
C
        DO J = 1, 3
          DO I = 1, NG_ENS(IP)
            IG = IG_ENS(I,IP)
            WRITE(IFL,'(1PE12.4)') VELG(J,IG)
          ENDDO
        ENDDO
C
      ENDDO
C
      END
