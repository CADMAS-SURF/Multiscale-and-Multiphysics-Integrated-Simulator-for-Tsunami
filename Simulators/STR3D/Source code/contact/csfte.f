      SUBROUTINE CSFTE(ICSF,JG,NP,ITO)

      DIMENSION II(3,4),ICSF(6),IG(3),NP(4),JG(3)
      DATA II / 2,4,3, 3,4,1, 4,2,1, 1,2,3 /

      ICSF(2) = 3

      DO I = 1, 4

        IG(:) = NP( II(:,I) )

        DO J = 1, 3

          DO K = 1, 3
            IF( JG(J) == IG(K) ) GOTO 10
          ENDDO

          GOTO 20

   10   ENDDO

        ICSF(1) = I
        ICSF(3:5) = IG(:)
        RETURN

   20 ENDDO

      WRITE(ITO,*) 'STOP IN SUB. CSFTE.'
      CALL ERRSTP(90,ITO)

      END
