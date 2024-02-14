      SUBROUTINE NPFORCE_S(FTI,FCK,FCD,FCM,FCMD,FCP,EPS,S,MGP,KK,NNOD
     &                    ,NESTF,RR,GRID,IELM,NM,AMAT,RODA,BARD,BVEC,D
     &                    ,UG,VG,PPND,DT1,DT2,ITER,ITER2,IDYN,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),RR(*),FCK(6,NNOD,3),FCD(6,NNOD,4),FCM(6,NNOD,3)
     &         ,FCMD(6,NNOD,3),IELM(NM,*),FC(60),EPS(6*MGP,*),S(6*MGP,*)
     &         ,D(21*MGP,*),GRID(3,NNOD),UG(6,NNOD),AMAT(33,*)
     &         ,EMASS(NESTF),FTI(6,NNOD),VG(12,*),RODA(*),BARD(6,*)
     &         ,BVEC(3,*),FCP(3,NNOD,2),PPND(NNOD)

      NELM = KK(12)
      NELMC = KK(29)
      NELMX = KK(32)

      BETA = RR(4)
      W4 = RR(5)

      DT12 = .5D0 * ( DT1 + DT2 )

      FCK(:,:,3) = 0.
      FCD(:,:,4) = 0.
      FCM(:,:,3) = 0.
      FCMD(:,:,3) = 0.

      FCP(:,:,2) = 0.

      DO I = 1, NELM + NELMC + NELMX

        IF( I > NELM .AND. I <= NELM + NELMC ) CYCLE

        ITYP = IELM(2,I)
        IF( ITYP == 1 .OR. ITYP == 4 ) THEN
          NDF = 6
        ELSE
          NDF = 3
        ENDIF
        NNP = IELM(3,I)
        IMAT = IELM(4,I)
        I5 = IELM(5,I)
        I7 = IELM(7,I)

        CALL ENPFRC(FC,EPS(1,I),S(1,I),GRID,UG,VG(1,I7),ITYP,IELM(8,I)
     &             ,NNP,RODA(I5),BARD(1,I5),BVEC(1,I7),D(1,I),KK(2),ITO)

        CALL ENPFADD(FCK(1,1,3),FC,NDF,NNP,IELM(8,I))

        GE = AMAT(4,IMAT) / W4

        CALL ENPFADDG(FCD(1,1,4),GE,FC,NDF,NNP,IELM(8,I))

        CALL EMASSMTX(EMASS,GRID,ITYP,NDF,NNP,IELM(6,I),IELM(8,I)
     &               ,AMAT(1,IMAT),RODA(I5),BARD(1,I5),KK(30),ITO)

        CALL ENPFADDM(FCM(1,1,3),FCMD(1,1,3),EMASS,UG,IELM(8,I),NDF,NNP
     &               ,AMAT(5,IMAT))

        IF( ITYP == 2 .AND. I5 == 1 ) 
     &    CALL EFRCP(FCP(1,1,2),GRID,PPND,IELM(8,I),NNP,ITO)

      ENDDO

      IF( ITER == 1 .AND. ITER2 == 1 ) FCD(:,:,3) = FCD(:,:,4)

      IF( IDYN == 1 ) THEN
        FTI(:,:) = ( FCM(:,:,3)/DT2 - ( 1.D0/DT2 + 1.D0/DT1 )*FCM(:,:,2)
     &             + FCM(:,:,1)/DT1 ) / DT12
     &           + ( FCMD(:,:,3) - FCMD(:,:,1) ) / DT12 * .5D0
     &           + ( ( FCD(:,:,4) - FCD(:,:,3) ) / DT2 
     &             + ( FCD(:,:,2) - FCD(:,:,1) ) / DT1 ) * .5D0
     &           + ( BETA * FCK(:,:,3) + ( 1.D0 - 2.D0*BETA )*FCK(:,:,2)
     &             + BETA * FCK(:,:,1) )
      ELSEIF( IDYN == 0 ) THEN
        FTI(:,:) = FCK(:,:,3)
      ENDIF

      FTI(1:3,:) = FTI(1:3,:) - FCP(:,:,1) - FCP(:,:,2)

      END