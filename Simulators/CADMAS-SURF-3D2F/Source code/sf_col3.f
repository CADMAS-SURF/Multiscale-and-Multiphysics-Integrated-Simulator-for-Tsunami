      SUBROUTINE SF_COL3(IBING,IP,JP)

      DIMENSION IP(3),JP(3),JM(3)
      DATA JM / 3, 1, 2 /
!-----------------------------------------------------------------------
      IBING = 0

      DO J1 = 1, 3
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