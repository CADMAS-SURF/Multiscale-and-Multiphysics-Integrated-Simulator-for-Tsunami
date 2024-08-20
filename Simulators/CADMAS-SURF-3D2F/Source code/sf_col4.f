      SUBROUTINE SF_COL4(IBING,IP,JP)

      DIMENSION IP(4),JP(4),JM(4)
      DATA JM / 4, 1, 2, 3 /
!-----------------------------------------------------------------------
      IBING = 0

      DO J1 = 1, 4
        IF( IP(1) == JP(J1) ) THEN
          J2 = JM(J1)
          IF( IP(2) == JP(J2) ) THEN
            J3 = JM(J2)
            IF( IP(3) == JP(J3) ) THEN
              IBING = 1
            ENDIF
          ENDIF
          EXIT
        ENDIF
      ENDDO

      END