      SUBROUTINE SF_BC_RESET(INDB,INDBK,INDBE,INDBT,INDBC,BCU,BCV,BCW
     &                      ,BCP,BCF,BCVI,BCK,BCE,BCT,BCTI,BCC,BCCI
     &                      ,GGX,GGY,GGZ,GLX,GLY,GLZ,NF,INDX,INDY,INDZ
     &                      ,INDX0,INDY0,INDZ0,INDB0,INDBK0,INDBE0
     &                      ,INDBT0,INDBC0,BCU0,BCV0,BCW0,BCP0,BCF0
     &                      ,BCVI0,BCK0,BCE0,BCT0,BCTI0,BCC0,BCCI0,XX,YY
     &                      ,ZZ,IELM,POS,DVEL)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION INDX(NUMI,NUMJ,NUMK),INDY(NUMI,NUMJ,NUMK)
     &         ,INDZ(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)
     &         ,INDBK(MAXBK1,NUMB),INDBE(MAXBE1,NUMB),INDBT(NUMB)
     &         ,INDBC(NUMB,LEQC),BCU(NUMB,3),BCV(NUMB,3),BCW(NUMB,3)
     &         ,BCP(NUMB,3),BCF(NUMB),BCVI(NUMB),BCK(NUMB,3)
     &         ,BCE(NUMB,3),BCT(NUMB),BCTI(2,NUMB),BCC(NUMB,LEQC)
     &         ,BCCI(2,NUMB,LEQC)
     &         ,INDX0(NUMI,NUMJ,NUMK),INDY0(NUMI,NUMJ,NUMK)
     &         ,INDZ0(NUMI,NUMJ,NUMK),INDB0(MAXB1,NUMB0)
     &         ,INDBK0(MAXBK1,NUMB0),INDBE0(MAXBE1,NUMB0),INDBT0(NUMB0)
     &         ,INDBC0(NUMB0,LEQC),BCU0(NUMB0,3),BCV0(NUMB0,3)
     &         ,BCW0(NUMB0,3),BCP0(NUMB0,3),BCF0(NUMB0),BCVI0(NUMB0)
     &         ,BCK0(NUMB0,3),BCE0(NUMB0,3),BCT0(NUMB0),BCTI0(2,NUMB0)
     &         ,BCC0(NUMB0,LEQC),BCCI0(2,NUMB0,LEQC)
     &         ,IM(3),JM(3),KM(3),NF(NUMI,NUMJ,NUMK)
     &         ,IELM(24,NELM),POS(3,NNOD),XX(MAXG1,NUMI)
     &         ,YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK),DVEL(3,NELM)
     &         ,GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
     &         ,GGZ(NUMI,NUMJ,NUMK),GLX(NUMI,NUMJ,NUMK)
     &         ,GLY(NUMI,NUMJ,NUMK),GLZ(NUMI,NUMJ,NUMK)

      REAL(8), POINTER :: GELM(:,:)

      DATA IM / 0, 1, 1 /
      DATA JM / 1, 0, 1 /
      DATA KM / 1, 1, 0 /
!-----------------------------------------------------------------------
      IF( ICPL == 2 .AND. IGEO == 1 ) THEN
        ALLOCATE( GELM(3,NELM), STAT=IERR )
        IF( IERR /= 0 ) CALL VF_A2ERR('SF_BC_RESET','CAN NOT ALLOC.')
        CALL SF_CENT_ELEM(GELM,NELM,IELM,POS)
      ENDIF

      DO ID = 1, 3
        DO K = 2, NUMK - KM(ID)
          DO J = 2, NUMJ - JM(ID)
            DO I = 2, NUMI - IM(ID)

              SELECT CASE( ID )
              CASE( 1 )
                NB  = INDX(I,J,K)
                NB0 = INDX0(I,J,K)
              CASE( 2 )
                NB  = INDY(I,J,K)
                NB0 = INDY0(I,J,K)
              CASE( 3 )
                NB  = INDZ(I,J,K)
                NB0 = INDZ0(I,J,K)
              END SELECT

              IF( NB <= 0 ) CYCLE

              IF( NB0 > 0 ) THEN

                INDB(:,NB)  = INDB0(:,NB0)
                BCU(NB,2:3) = BCU0(NB0,2:3)
                BCV(NB,2:3) = BCV0(NB0,2:3)
                BCW(NB,2:3) = BCW0(NB0,2:3)
                BCP(NB,2:3) = BCP0(NB0,2:3)
                BCF(NB)     = BCF0(NB0)
                BCVI(NB)    = BCVI0(NB0)

                IF( LEQK /= 0 ) THEN
                  INDBK(:,NB) = INDBK0(:,NB0)
                  INDBE(:,NB) = INDBE0(:,NB0)
                  BCK(NB,2:3) = BCK0(NB0,2:3)
                  BCE(NB,2:3) = BCE0(NB0,2:3)
                ENDIF

                IF( LEQT /= 0 ) THEN
                  INDBT(NB)  = INDBT0(NB0)
                  BCT(NB)    = BCT0(NB0)
                  BCTI(:,NB) = BCTI0(:,NB0)
                ENDIF

                IF( LEQC > 0 ) THEN
                  INDBC(NB,:)  = INDBC0(NB0,:)
                  BCC(NB,:)    = BCC0(NB0,:)
                  BCCI(:,NB,:) = BCCI0(:,NB0,:)
                ENDIF

              ELSE

                INDB(1,NB) = I + NUMI*(J-1) + NUMI*NUMJ*(K-1)

                IF( NF(I,J,K) >= 0 ) THEN
                  INDB(2,NB) = 1 + 2*(ID-1)
                ELSE
                  INDB(2,NB) = 2 + 2*(ID-1)
                ENDIF

                INDB(3,NB)  = 1
                INDB(4,NB)  = 2
                INDB(5,NB)  = 1
                BCVI(NB)    = 0.

!                IF( ICPL == 2 .AND. IGEO == 1 ) THEN
                IF( ICPL == -1 .AND. IGEO == 1 ) THEN
                  CALL SF_BND_GELEM(IEB,ID,I,J,K,XX,YY,ZZ,GELM,IELM)
                  IF( IEB > 0 ) THEN
                    INDB(3,NB) = 3
                    INDB(5,NB) = 3
                    BCU(NB,2:3) = DVEL(1,IEB)
                    BCV(NB,2:3) = DVEL(2,IEB)
                    BCW(NB,2:3) = DVEL(3,IEB)
                    SELECT CASE( ID )
                    CASE( 1 )
                      GGX(I,J,K) = 1.D0
                      GLX(I,J,K) = 1.D0
                    CASE( 2 )
                      GGY(I,J,K) = 1.D0
                      GLY(I,J,K) = 1.D0
                    CASE( 3 )
                      GGZ(I,J,K) = 1.D0
                      GLZ(I,J,K) = 1.D0
                    END SELECT
                  ENDIF
                ENDIF

                IF( LEQK /= 0 ) THEN
                  INDBK(:,NB) = 2
                  INDBE(:,NB) = 2
                ENDIF

                IF( LEQT /= 0 ) THEN
                  INDBT(NB)  = 2
                  BCTI(:,NB) = 0.
                ENDIF

                IF( LEQC > 0 ) THEN
                  INDBC(NB,:)  = 2
                  BCCI(:,NB,:) = 0.
                ENDIF
                
              ENDIF

            ENDDO
          ENDDO
        ENDDO
      ENDDO

      IF( ICPL == 2 .AND. IGEO == 1 ) THEN
        DEALLOCATE( GELM, STAT=IERR )
        IF( IERR /= 0 ) CALL VF_A2ERR('SF_BC_RESET','CAN NOT DEALLOC.')
      ENDIF

      END