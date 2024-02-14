      SUBROUTINE CSFHX(ICSF,JG,NP,ITO)

      DIMENSION II(4,6),ICSF(6),IG(4),NP(8),JG(3)
      DATA II / 1,2,3,4, 1,5,6,2, 2,6,7,3, 3,7,8,4, 4,8,5,1, 5,8,7,6 /

      ICSF(2) = 4

      DO I = 1, 6

        IG(:) = NP( II(:,I) )

        DO J = 1, 3

          DO K = 1, 4
            IF( JG(J) == IG(K) ) GOTO 10
          ENDDO

          GOTO 20

   10   ENDDO

        ICSF(1) = I
        ICSF(3:6) = IG(:)
        RETURN

   20 ENDDO

      WRITE(ITO,*) 'STOP IN SUB. CSFHX.'
      CALL ERRSTP(90,ITO)

      END
