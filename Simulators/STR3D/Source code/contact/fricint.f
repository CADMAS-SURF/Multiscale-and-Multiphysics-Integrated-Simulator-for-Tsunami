      SUBROUTINE FRICINT(IFRIC,NNODI,NINDC,ISLV,INDC,IELC,IEDG,IELQ,IFCQ
     &                  ,IEDQ)

      DIMENSION IFRIC(10,NINDC),ISLV(2,NINDC),INDC(NINDC),IELC(3,*)
     &         ,IEDG(6,*),IELQ(4,*),IFCQ(*),IEDQ(*),IG(4)

      DO I = 1, NINDC

        IF( IFRIC(1,I) == 0 ) CYCLE

        IF( INDC(I) <= NNODI ) CYCLE

        IST = ISLV(1,I)
        MST = ISLV(2,I)

        SELECT CASE( IST )
        CASE( 2, 12 )
          IF( IEDQ(MST) > 0 ) THEN
            N = 4
            IG(:) = IELQ(:,IEDQ(MST))
          ELSE
            N = 2
            IG(1:2) = IEDG(1:2,MST)
          ENDIF
        CASE( 3, 13 )
          IF( IFCQ(MST) > 0 ) THEN
            N = 4
            IG(:) = IELQ(:,IFCQ(MST))
          ELSE
            N = 3
            IG(1:3) = IELC(:,MST)
          ENDIF
        END SELECT

        DO J = 1, N
          IF( IG(J) <= NNODI ) GOTO 10
        ENDDO

        IFRIC(1,I) = 0

   10 ENDDO

      END
