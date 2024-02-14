      SUBROUTINE BCTSET(ICTB,FRTB,I_BCT,KK,NICRG,ICPA,NCPA,ICPR,NCPR,CPR
     &                 ,ICRG,ITO)

      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),ICTB(NICRG,NICRG),FRTB(3,NICRG,NICRG),ICPR(2,*)
     &         ,NCPR(2,*),CPR(2,*),ICRG(2,NICRG),ICPA(2,*),NCPA(*)

      NICPA = KK(88)
      NICPR = KK(90)

      IADD = 0
      IS = 1
      IE = 1

      DO I = 1, NICPA
        IF( ICPA(1,I) == I_BCT ) THEN
          IADD = 1
          CALL ADDSET3(IS,IE,ICPA,I)
          EXIT
        ENDIF
      ENDDO

      ICTB(:,:) = 0

      FRTB(:,:,:) = 0.

      DO I = IS, IE

        SELECT CASE( IADD )
        CASE( 1 )
          ID = NCPA(I)
        CASE( 0 )
          ID = I_BCT
        END SELECT

        DO J = 1, NICPR
          IF( ICPR(1,J) == ID ) THEN
            CALL ADDSET3(KS,KE,ICPR,J)
            DO K = KS, KE
              ISLAVE = NCPR(1,K)
              MASTER = NCPR(2,K)
              ICTB(ISLAVE,MASTER) = 1
              FRTB(1,ISLAVE,MASTER) = CPR(1,J)
              FRTB(2,ISLAVE,MASTER) = 1.D0
              FRTB(3,ISLAVE,MASTER) = CPR(2,J)
            ENDDO
            EXIT
          ENDIF
        ENDDO

      ENDDO

      DO I = 1, NICRG
        DO J = 1, NICRG
          IF( FRTB(1,I,J) > 0.D0 ) THEN
            KK(96) = 2
            EXIT
          ENDIF
        ENDDO
      ENDDO

      DO I = 1, NICRG
        DO J = I + 1, NICRG
          IF( ICTB(I,J) == 1 .AND. ICTB(J,I) == 1 ) THEN
            WRITE(ITO,'(A,2I3)') 'BIDIRECTIONAL MASTER-SLAVE :'
     &                          ,ICRG(1,I),ICRG(1,J)
            CALL ERRSTP(52,ITO)
          ENDIF
        ENDDO
      ENDDO

      IF( MYRANK == 1 ) THEN
        CALL M_MPI_SEND_I(20,1,0)  ! SEND IOP=20 TO GLB_COMM
        CALL M_MPI_SEND_I(ICTB,NICRG*NICRG,0)
        CALL M_MPI_SEND_D(FRTB,3*NICRG*NICRG,0)
        CALL M_MPI_SEND_I(KK(96),1,0)
      ENDIF

      END
