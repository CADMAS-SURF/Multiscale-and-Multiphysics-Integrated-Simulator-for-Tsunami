      SUBROUTINE CIRCHK(ISLV,ISLVG,NNOD,NINDC,INDC,IEDG,IELC,IELQ,IFCQ
     &                 ,IEDQ,IVRQ)

      DIMENSION ISLVG(3,NNOD),ISLV(2,NINDC),INDC(NINDC),IEDG(6,*)
     &         ,IELC(3,*),IELQ(4,*),IVRQ(*),IEDQ(*),IFCQ(*),JP(4)
     &         ,IP(4096),IP0(4096)

      ISLVG(:,:) = 0

      DO I = 1, NINDC
        IF( ISLV(1,I) == 0 ) CYCLE
        IG = INDC(I)
        ISLVG(1:2,IG) = ISLV(:,I)
        ISLVG(3,IG) = I
      ENDDO

      DO I = 1, NINDC

        IG = INDC(I)

        N = 1
        IP(1) = IG

        DO

          N0 = 0

          DO J = 1, N

            JG = IP(J)

            IST = ISLVG(1,JG)
            MA = ISLVG(2,JG)

            IF( IST == 0 ) CYCLE

            CALL MSTNOD(M,JP,IST,MA,IELC,IEDG,IELQ,IFCQ,IEDQ,IVRQ)

            DO K = 1, M
              IF( JP(K) == IG ) THEN
                ISLVG(1:2,JG) = 0
                ISLV(:,ISLVG(3,JG)) = 0
                GOTO 10
              ENDIF
            ENDDO

            IP0(N0+1:N0+M) = JP(1:M)
            N0 = N0 + M

   10     ENDDO

          IF( N0 == 0 ) EXIT

          N = N0

          IP(1:N) = IP0(1:N0)
          
        ENDDO

      ENDDO

      END
