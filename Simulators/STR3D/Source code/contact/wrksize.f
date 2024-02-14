      SUBROUTINE WRKSIZE(NSIZ1,NSIZ2,NSIZ3,ISLVG,NS,NM,NNOD,NINDC,INDC
     &                  ,IEDG,IELC,IELQ,IFCQ,IEDQ,IVRQ,ISLV)

      DIMENSION ISLVG(2,NNOD),ISLV(2,NINDC),INDC(NINDC),IEDG(6,*)
     &         ,IELC(3,*),IELQ(4,*),IVRQ(*),IEDQ(*),IFCQ(*),JP(4)
     &         ,IP(4096),IP0(4096),NS(NINDC),NM(NINDC)

      ISLVG(:,:) = 0

      ISLVG(:,INDC(:)) = ISLV(:,:)

      NS(:) = 0
      NM(:) = 0

      DO I = 1, NINDC

        N = 1

        IP(1) = INDC(I)

        DO

          N0 = 0

          DO J = 1, N

            JG = IP(J)

            IST = ISLVG(1,JG)
            MA = ISLVG(2,JG)

            IF( IST == 0 ) CYCLE

            NS(I) = NS(I) + 1

            CALL MSTNOD(M,JP,IST,MA,IELC,IEDG,IELQ,IFCQ,IEDQ,IVRQ)

            IP0(N0+1:N0+M) = JP(1:M)
            N0 = N0 + M

          ENDDO

          IF( N0 == 0 ) EXIT

          N = N0

          IP(1:N) = IP0(1:N0)

          IF( NM(I) == 0 ) NM(I) = 1

          NM(I) = NM(I) + N
          
        ENDDO

      ENDDO

      NSIZ1 = SUM(NS) * 9
      NSIZ2 = SUM(NM) * 9
      NSIZ3 = MAXVAL(NM) * 3 * 24

      END
