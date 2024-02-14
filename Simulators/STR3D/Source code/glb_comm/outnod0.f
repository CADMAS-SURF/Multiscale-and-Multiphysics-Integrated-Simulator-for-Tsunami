      SUBROUTINE OUTNOD0( NNOD, IGEO, IFL )
C
      USE M_VAL
C
      DIMENSION IFL(*)
      REAL(8),POINTER :: UG(:,:),VEL(:,:),ACC(:,:)
C
      ALLOCATE( UG(6,NNOD) )
      ALLOCATE( VEL(3,NNOD) )
      ALLOCATE( ACC(3,NNOD) )
      ALLOCATE( RFCO(6,NNOD) )
      IF( IGEO > 0 ) ALLOCATE( PG3(NNOD) )
C
      CALL GATHER_NODAL_D(UG,6)
      CALL GATHER_NODAL_D(VEL,3)
      CALL GATHER_NODAL_D(ACC,3)
      CALL GATHER_NODAL_D(RFCO,6)
      IF( IGEO > 0 ) CALL GATHER_NODAL_D(PG3,1)
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
      DEALLOCATE( UG )
      DEALLOCATE( VEL )
      DEALLOCATE( ACC )
      DEALLOCATE( RFCO )
      IF( IGEO > 0 ) DEALLOCATE( PG3 )
C
      END
