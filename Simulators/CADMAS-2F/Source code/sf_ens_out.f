      SUBROUTINE SF_ENS_OUT(XX,YY,ZZ,UU,VV,WW,PP,FF,GGV,GGX,GGY,GGZ,NF
     &                     ,INDX,INDY,INDZ)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
     &         ,UU(NUMI,NUMJ,NUMK),VV(NUMI,NUMJ,NUMK),WW(NUMI,NUMJ,NUMK)
     &         ,NF(NUMI,NUMJ,NUMK),PP(NUMI,NUMJ,NUMK)
     &         ,INDX(NUMI,NUMJ,NUMK),INDY(NUMI,NUMJ,NUMK)
     &         ,INDZ(NUMI,NUMJ,NUMK),GGX(NUMI,NUMJ,NUMK)
     &         ,GGY(NUMI,NUMJ,NUMK),GGZ(NUMI,NUMJ,NUMK)
     &         ,FF(NUMI,NUMJ,NUMK),GGV(NUMI,NUMJ,NUMK)

      CHARACTER*1 IP
      CHARACTER*4 NO
      CHARACTER*18 FLN

      REAL(8), POINTER :: VEL(:,:,:,:)

      DATA IC / 0 /
!-----------------------------------------------------------------------
      IF( NPROCS == 1 ) THEN
        FLN = 'cadmas_out/data.'
        N = 16
      ELSE
        WRITE(IP,'(I1)') MYRANK
        FLN = 'cadmas_'//IP//'_out/data.'
        N = 18
      ENDIF

      IC = IC + 1
      WRITE(NO,'(I4.4)') IC

      OPEN(40, FILE = FLN(1:N)//'GGV_'//NO)
      OPEN(41, FILE = FLN(1:N)//'FF_'//NO)
      OPEN(42, FILE = FLN(1:N)//'NF_'//NO)
      OPEN(43, FILE = FLN(1:N)//'PP_'//NO)
      OPEN(44, FILE = FLN(1:N)//'VEL_'//NO)

      WRITE(40,'(A)') 'GGV'

      WRITE(40,'(A)') 'part'
      WRITE(40,'(I10)') 1
      WRITE(40,'(A)') 'block'

      WRITE(40,'(1PE12.5)') GGV(MYIS:MYIE,MYJS:MYJE,2:NUMK-1)

      WRITE(41,'(A)') 'FF'

      WRITE(41,'(A)') 'part'
      WRITE(41,'(I10)') 1
      WRITE(41,'(A)') 'block'

      WRITE(41,'(1PE12.5)') FF(MYIS:MYIE,MYJS:MYJE,2:NUMK-1)

      WRITE(42,'(A)') 'NF'

      WRITE(42,'(A)') 'part'
      WRITE(42,'(I10)') 1
      WRITE(42,'(A)') 'block'

      WRITE(42,'(1PE12.5)') DBLE( NF(MYIS:MYIE,MYJS:MYJE,2:NUMK-1) )

      WRITE(43,'(A)') 'PP'

      WRITE(43,'(A)') 'part'
      WRITE(43,'(I10)') 1
      WRITE(43,'(A)') 'block'

      WRITE(43,'(1PE12.5)') PP(MYIS:MYIE,MYJS:MYJE,2:NUMK-1)

      ALLOCATE( VEL(3,NUMI,NUMJ,NUMK) )

      VEL = 0.

      DO K = 2, NUMK
        DO J = MYJS, MYJE + 1
          DO I = MYIS, MYIE + 1

            F = 0.
            A = 0.

            DO KK = K-1, K
              DO JJ = J-1, J
                IF( INDX(I,JJ,KK) >= 0 ) THEN
                  F = F + UU(I,JJ,KK) * YY(2,JJ)*ZZ(2,KK) * GGX(I,JJ,KK)
                  A = A + YY(2,JJ) * ZZ(2,KK)
                ENDIF
              ENDDO
            ENDDO

            IF( A > 0. ) VEL(1,I,J,K) = F / A

            F = 0.
            A = 0.

            DO KK = K-1, K
              DO II = I-1, I
                IF( INDY(II,J,KK) >= 0 ) THEN
                  F = F + VV(II,J,KK) * XX(2,II)*ZZ(2,KK) * GGY(II,J,KK)
                  A = A + XX(2,II) * ZZ(2,KK)
                ENDIF
              ENDDO
            ENDDO

            IF( A > 0. ) VEL(2,I,J,K) = F / A

            F = 0.
            A = 0.

            DO JJ = J-1, J
              DO II = I-1, I
                IF( INDZ(II,JJ,K) >= 0 ) THEN
                  F = F + WW(II,JJ,K) * XX(2,II)*YY(2,JJ) * GGZ(II,JJ,K)
                  A = A + XX(2,II) * YY(2,JJ)
                ENDIF
              ENDDO
            ENDDO

            IF( A > 0. ) VEL(3,I,J,K) = F / A

          ENDDO
        ENDDO
      ENDDO

      WRITE(44,'(A)') 'VEL'

      WRITE(44,'(A)') 'part'
      WRITE(44,'(I10)') 1
      WRITE(44,'(A)') 'block'

      DO I = 1, 3
        WRITE(44,'(1PE12.5)') VEL(I,MYIS:MYIE+1,MYJS:MYJE+1,2:NUMK)
      ENDDO

      DEALLOCATE( VEL )

      CLOSE(42)
      CLOSE(43)
      CLOSE(44)

      IF( NPROCS == 1 ) THEN
        OPEN(40, FILE = 'cadmas.case')
      ELSE
        WRITE(IP,'(I1)') MYRANK
        OPEN(40, FILE = 'cadmas_'//IP//'.case')
      ENDIF

      WRITE(40,'(A)') 'FORMAT'

      WRITE(40,'(A)') 'type: ensight gold'

      WRITE(40,'(A)') 'GEOMETRY'

      WRITE(40,'(2A)') 'model: ', FLN(1:N)//'geom'

      WRITE(40,'(A)') 'VARIABLE'

      WRITE(40,'(2A)') 'scalar per element: GGV ',
     &                 FLN(1:N)//'GGV_****'

      WRITE(40,'(2A)') 'scalar per element: FF ',
     &                 FLN(1:N)//'FF_****'

      WRITE(40,'(2A)') 'scalar per element: NF ',
     &                 FLN(1:N)//'NF_****'

      WRITE(40,'(2A)') 'scalar per element: PP ',
     &                 FLN(1:N)//'PP_****'

      WRITE(40,'(2A)') 'vector per node: VEL ',
     &                 FLN(1:N)//'VEL_****'

      WRITE(40,'(A)') 'TIME'

      WRITE(40,'(A)') 'time set: 1'

      WRITE(40,'(A,I4)') 'number of steps: ', IC

      WRITE(40,'(A)') 'filename start number: 0001'

      WRITE(40,'(A)') 'filename increment: 1'

      WRITE(40,'(A)') 'time values:'

      DO I = 1, IC
        WRITE(40,'(F8.1)') DBLE(I)
      ENDDO

      CLOSE(40)

      END
