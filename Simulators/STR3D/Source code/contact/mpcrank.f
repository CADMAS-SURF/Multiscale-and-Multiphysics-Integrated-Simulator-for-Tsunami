      SUBROUTINE MPCRANK(NRANK,IRANK,ICORR,NNOD,NINDC,INDC,IEDG,IELC
     &                  ,IELQ,IFCQ,IEDQ,IVRQ,ISLV)

      DIMENSION IRANK(NINDC),ICORR(NNOD),ISLV(2,NINDC),INDC(NINDC)
     &         ,IVRQ(*),IEDQ(*),IFCQ(*),IELQ(4,*),IELC(3,*),IEDG(6,*)
     &         ,IG(4)

      IRANK(:) = 0

      ICORR(:) = 1

      DO I = 1, NINDC
        IF( ISLV(1,I) > 0 ) ICORR( INDC(I) ) = 0
      ENDDO

      NRANK = 0

      DO

        NRANK = NRANK + 1

        DO I = 1, NINDC

          IST = ISLV(1,I)
          MA = ISLV(2,I)

          IF( IST > 0 .AND. IRANK(I) == 0 ) THEN

            SELECT CASE( IST )
            CASE( 1, 11 )
              IF( IVRQ(MA) > 0 ) THEN
                N = 4
                IG(:) = IELQ(:,IVRQ(MA))
              ELSE
                N = 1
                IG(1) = MA
              ENDIF
            CASE( 2, 12, 14 )
              IF( IEDQ(MA) > 0 ) THEN
                N = 4
                IG(:) = IELQ(:,IEDQ(MA))
              ELSE
                N = 2
                IG(1:2) = IEDG(1:2,MA)
              ENDIF
            CASE( 3, 13, 15 )
              IF( IFCQ(MA) > 0 ) THEN
                N = 4
                IG(:) = IELQ(:,IFCQ(MA))
              ELSE
                N = 3
                IG(1:3) = IELC(:,MA)
              ENDIF
            END SELECT

            DO J = 1, N
              IF( ICORR( IG(J) ) == 0 ) GOTO 10
            ENDDO

            IRANK(I) = NRANK

          ENDIF

   10   ENDDO

        NCORR = 0

        DO I = 1, NINDC
          IF( ISLV(1,I) == 0 ) CYCLE
          IF( IRANK(I) == NRANK ) THEN
            ICORR( INDC(I) ) = 1
          ELSEIF( IRANK(I) == 0 ) THEN
            NCORR = NCORR + 1
          ENDIF
        ENDDO

        IF( NCORR == 0 ) EXIT

      ENDDO

      END
