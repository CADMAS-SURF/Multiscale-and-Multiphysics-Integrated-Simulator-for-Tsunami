      SUBROUTINE CSFPN(ICSF,JG,NP,ITO)

      DIMENSION II(4,5),ICSF(6),IG(4),NP(6),JG(3)
      DATA II / 1,2,3,0, 4,6,5,0, 1,4,5,2, 2,5,6,3, 3,6,4,1 /

      DO I = 1, 5

        SELECT CASE( I )
        CASE( 1:2 )
          N = 3
        CASE( 3:5 )
          N = 4
        END SELECT

        IG(1:N) = NP( II(1:N,I) )

        DO J = 1, 3

          DO K = 1, N
            IF( JG(J) == IG(K) ) GOTO 10
          ENDDO

          GOTO 20

   10   ENDDO

        ICSF(1) = I
        ICSF(2) = N
        ICSF(3:2+N) = IG(1:N)
        RETURN

   20 ENDDO

      WRITE(ITO,*) 'STOP IN SUB. CSFPN.'
      CALL ERRSTP(90,ITO)

      END
