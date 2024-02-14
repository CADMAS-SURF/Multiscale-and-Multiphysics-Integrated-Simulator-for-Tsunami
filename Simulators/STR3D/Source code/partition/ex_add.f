      SUBROUTINE EX_ADD(IADW,IAD,NI,NO,NJ)

      DIMENSION IADW(NI),IAD(NI),NO(NJ)

      J = 1

      DO I = 1, NI
        IE = IAD(I)
        IADW(I) = J - 1
        JS = J
        DO J = JS, NJ
          JP = NO(J)
          IF( JP <= IE ) THEN
            IADW(I) = J
          ELSEIF( JP > IE ) THEN
            EXIT
          ENDIF
        ENDDO
      ENDDO

      END
