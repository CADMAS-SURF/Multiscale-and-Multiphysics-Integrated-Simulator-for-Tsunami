      SUBROUTINE ISLV_UPDT(ISLV,ISLVP,RSLV,NNOD,NINDC,NP,NN_EXT,NN_EXTC
     &                    ,NOD,MN,NODG,MG,NODP,IEG,MIEG,IEDGP,IEC,MIEC
     &                    ,IELCP,INDC,IEDG,IELC,IELQ,IFCQ,IEDQ,IVRQ)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION ISLV(2,NINDC),ISLVP(NINDC),RSLV(3,NINDC),NOD(MN,NP)
     &         ,NN_EXT(NP),NN_EXTC(NP),NODG(MG,NP),NODP(2,*)
     &         ,IEG(MIEG,NP),IEDGP(2,*),IEC(MIEC,NP),IELCP(2,*)
     &         ,INDC(NINDC),IELC(3,*),IEDG(6,*),IELQ(4,*),IVRQ(*)
     &         ,IEDQ(*),IFCQ(*)

      INTEGER, POINTER :: ISLVW(:,:),ISLVPW(:),ISLVG(:,:)
      REAL(8), POINTER :: RSLVW(:,:)

      ALLOCATE( ISLVW(2,NINDC) )
      ALLOCATE( ISLVPW(NINDC) )
      ALLOCATE( RSLVW(3,NINDC) )

      ISLV(:,:) = 0
      ISLVP(:) = 0
      RSLV(:,:) = 0.

      DO IP = 1, NP

        CALL M_MPI_RECV_I(ISLVW,2*NINDC,IP)
        CALL M_MPI_RECV_I(ISLVPW,NINDC,IP)
        CALL M_MPI_RECV_D(RSLVW,3*NINDC,IP)

        DO I = 1, NINDC
          IF( ISLVW(1,I) > 0 ) THEN
            ISLV(:,I) = ISLVW(:,I)
            ISLVP(I) = ISLVPW(I)
            RSLV(:,I) = RSLVW(:,I)
          ENDIF
        ENDDO

      ENDDO

      DEALLOCATE( ISLVW )
      DEALLOCATE( ISLVPW )
      DEALLOCATE( RSLVW )

      DO I = 1, NINDC

        IST = ISLV(1,I)
        MA  = ISLV(2,I)
        IP  = ISLVP(I)

        SELECT CASE( IST )
        CASE( 1, 11 )
          IF( MA <= NN_EXT(IP) + NN_EXTC(IP) ) THEN
            MAG = NOD(MA,IP)
          ELSE
            MA = MA - ( NN_EXT(IP) + NN_EXTC(IP) )
            MAG = NODG(MA,IP)
          ENDIF
          ISLVP(I)  = NODP(1,MAG)
!         ISLV(2,I) = NODP(2,MAG)
          ISLV(2,I) = MAG
        CASE( 2, 12 )
          MAG = IEG(MA,IP)
          ISLVP(I)  = IEDGP(1,MAG)
!         ISLV(2,I) = IEDGP(2,MAG)
          ISLV(2,I) = MAG
        CASE( 3, 13 )
          MAG = IEC(MA,IP)
          ISLVP(I)  = IELCP(1,MAG)
!         ISLV(2,I) = IELCP(2,MAG)
          ISLV(2,I) = MAG
        END SELECT

      ENDDO

      ALLOCATE( ISLVG(3,NNOD) )

      CALL CIRCHK(ISLV,ISLVG,NNOD,NINDC,INDC,IEDG,IELC,IELQ,IFCQ,IEDQ
     &           ,IVRQ)

      DEALLOCATE( ISLVG )

      DO I = 1, NINDC

        IST = ISLV(1,I)
        MAG = ISLV(2,I)

        SELECT CASE( IST )
        CASE( 1, 11 )
          ISLV(2,I) = NODP(2,MAG)
        CASE( 2, 12 )
          ISLV(2,I) = IEDGP(2,MAG)
        CASE( 3, 13 )
          ISLV(2,I) = IELCP(2,MAG)
        END SELECT

      ENDDO

      CALL M_MPI_BCAST_I(ISLV,2*NINDC)
      CALL M_MPI_BCAST_I(ISLVP,NINDC)
      CALL M_MPI_BCAST_D(RSLV,3*NINDC)

      END
