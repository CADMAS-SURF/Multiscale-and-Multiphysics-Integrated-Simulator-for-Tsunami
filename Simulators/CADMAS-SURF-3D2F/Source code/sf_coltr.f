      SUBROUTINE SF_COLTR(IBING,IP,JP,NP)

      DIMENSION IP(3),JP(NP)

      DO I = 1, 3
        DO J = 1, NP
          IF( IP(I) == JP(J) ) GOTO 10
        ENDDO
        IBING = 0
        RETURN
   10 ENDDO

      IBING = 1

      END
