      SUBROUTINE OUTNOD( DT1, DT2, UG1, UG2, UG3, UG, VEL, ACC, RFCO,
     &                   PG3, NNOD, NNODI, N_PART, NP_ENS, NG_ENS,
     &                   IG_ENS, IGEO, IDYN, IFL )
C
      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION UG1(6,NNOD), UG2(6,NNOD), UG3(6,NNOD), UG(6,NNOD),
     &          RFCO(6,NNOD), PG3(NNOD), VEL(3,NNOD), ACC(3,NNOD),
     &          NP_ENS(6), NG_ENS(N_PART), IG_ENS(NNOD,N_PART), IFL(*)
C
      IF( IDYN == 1 ) THEN
C
        VEL(:,:) = ( UG3(1:3,:) - UG1(1:3,:) ) / ( DT1 + DT2 )
C
        ACC(:,:) = ( ( UG3(1:3,:) - UG2(1:3,:) ) / DT2 
     &               - ( UG2(1:3,:) - UG1(1:3,:) ) / DT1 ) 
     &             / ( DT1 + DT2 ) * 2.D0
C
      ELSEIF( IDYN == 0 ) THEN
C
        VEL(:,:) = 0.
C
        ACC(:,:) = 0.
C
      ENDIF
C
      IF( MYRANK == 0 ) THEN
C
        WRITE(IFL(20),'(A)') 'displacement'
C
        DO IP = 1, N_PART
C
          WRITE(IFL(20),'(A)') 'part'
          WRITE(IFL(20),'(I10)') IP
          WRITE(IFL(20),'(A)') 'coordinates'
C
          DO J = 1, 3
            DO I = 1, NG_ENS(IP)
              IG = IG_ENS(I,IP)
              WRITE(IFL(20),'(1PE12.4)') UG(J,IG)
            ENDDO
          ENDDO
C
        ENDDO
C
        WRITE(IFL(21),'(A)') 'velocity'
C
        DO IP = 1, N_PART
C
          WRITE(IFL(21),'(A)') 'part'
          WRITE(IFL(21),'(I10)') IP
          WRITE(IFL(21),'(A)') 'coordinates'
C
          DO J = 1, 3
            DO I = 1, NG_ENS(IP)
              IG = IG_ENS(I,IP)
              WRITE(IFL(21),'(1PE12.4)') VEL(J,IG)
            ENDDO
          ENDDO
C
        ENDDO
C
        WRITE(IFL(22),'(A)') 'acceleration'
C
        DO IP = 1, N_PART
C
          WRITE(IFL(22),'(A)') 'part'
          WRITE(IFL(22),'(I10)') IP
          WRITE(IFL(22),'(A)') 'coordinates'
C
          DO J = 1, 3
            DO I = 1, NG_ENS(IP)
              IG = IG_ENS(I,IP)
              WRITE(IFL(22),'(1PE12.4)') ACC(J,IG)
            ENDDO
          ENDDO
C
        ENDDO
C
        WRITE(IFL(23),'(A)') 'reactive force'
C
        DO IP = 1, N_PART
C
          WRITE(IFL(23),'(A)') 'part'
          WRITE(IFL(23),'(I10)') IP
          WRITE(IFL(23),'(A)') 'coordinates'
C
          DO J = 1, 3
            DO I = 1, NG_ENS(IP)
              IG = IG_ENS(I,IP)
              WRITE(IFL(23),'(1PE12.4)') RFCO(J,IG)
            ENDDO
          ENDDO
C
        ENDDO
C
        IF( IGEO > 0 ) THEN
C
          WRITE(IFL(24),'(A)') 'pore water pressure'
C
          IPS = NP_ENS(2) + NP_ENS(3) + NP_ENS(4) + 1
          IPE = NP_ENS(2) + NP_ENS(3) + NP_ENS(4) + NP_ENS(6)
C
          DO IP = IPS, IPE
C
            WRITE(IFL(24),'(A)') 'part'
            WRITE(IFL(24),'(I10)') IP
            WRITE(IFL(24),'(A)') 'coordinates'
C
            DO I = 1, NG_ENS(IP)
              IG = IG_ENS(I,IP)
              WRITE(IFL(24),'(1PE12.4)') PG3(IG)
            ENDDO
C
          ENDDO
C
        ENDIF
C
      ELSE
C
        CALL M_MPI_SEND_D(UG,6*NNODI,0)
        CALL M_MPI_SEND_D(VEL,3*NNODI,0)
        CALL M_MPI_SEND_D(ACC,3*NNODI,0)
        CALL M_MPI_SEND_D(RFCO,6*NNODI,0)
        IF( IGEO > 0 ) CALL M_MPI_SEND_D(PG3,NNODI,0)
C
      ENDIF
C
      END
