      SUBROUTINE NPFLOW(FTI,FLI,FLO,FCP,VELG,MGP,VELE,KK,NNOD,RR,GRID
     &                 ,IELM,NM,AMAT,UG1,UG2,UG3,PG1,PG2,PG3,DT1,DT2
     &                 ,IDYN,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FLI(NNOD),FLO(NNOD),FCP(3,NNOD),VELG(3,MGP,*),VELE(3,*)
     &         ,GRID(3,NNOD),UG1(6,NNOD),UG2(6,NNOD),UG3(6,NNOD)
     &         ,PG1(NNOD),PG2(NNOD),PG3(NNOD),AMAT(33,*),GRAV(3)
     &         ,IELM(NM,*),KK(*),RR(*),FTI(6,NNOD)
      DATA GRAV / 0.D0, 0.D0, -9.8D0 /

      NELM = KK(12)
      NELMC = KK(29)
      NELMX = KK(32)

      FLI(:) = 0.
      FLO(:) = 0.
      FCP(:,:) = 0.

      DO I = 1, NELM + NELMC + NELMX

        IF( I > NELM .AND. I <= NELM + NELMC ) CYCLE

        ITYP = IELM(2,I)
        ND   = IELM(3,I)
        IMAT = IELM(4,I)

        IF( ITYP == 6 )
     &    CALL  GNPFLW(FLI,FLO,FCP,VELG(1,1,I),VELE(1,I),GRID,UG1,UG2
     &                ,UG3,PG1,PG2,PG3,IELM(8,I),ND,AMAT(1,IMAT),GRAV
     &                ,DT1,DT2,RR(7),RR(4),IDYN,ITO)

      ENDDO

      FTI(1:3,:) = FTI(1:3,:) - FCP(:,:)

      END
