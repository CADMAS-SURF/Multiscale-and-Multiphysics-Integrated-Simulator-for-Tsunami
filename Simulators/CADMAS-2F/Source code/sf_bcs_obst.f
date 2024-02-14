      SUBROUTINE SF_BCS_OBST()

      USE VF_A2ARRAY
      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION RNG(2,3)

      INTEGER, POINTER :: ISKIP(:),IBING(:),IGNR(:),IELMW(:,:),INDGW(:)
      INTEGER, POINTER :: IGFCW(:),IPFACEW(:,:)
      REAL(8), POINTER :: GRIDW(:,:),GRDLW(:),PORW(:)

      CALL VF_P1BCSI(IPART,1,0)

      IF( IPART == 0 ) THEN

        CALL VF_P1BCSI(NNOD,1,0)
        CALL VF_P1BCSI(NELM,1,0)

        IF( MYRANK > 0 ) THEN
          ALLOCATE( INDG(NNOD) )
          ALLOCATE( GRID(3,NNOD) )
          ALLOCATE( IGFC(NNOD) )
          ALLOCATE( GRDL(NNOD) )
          ALLOCATE( IELM(24,NELM) )
          ALLOCATE( POR(NELM) )
        ENDIF

        CALL VF_P1BCSI(INDG,NNOD,0)
        CALL VF_P1BCSD(GRID,3*NNOD,0)
        CALL VF_P1BCSI(IGFC,NNOD,0)
        CALL VF_P1BCSD(GRDL,NNOD,0)
        CALL VF_P1BCSI(IELM,24*NELM,0)
        CALL VF_P1BCSD(POR,NELM,0)

        CALL VF_P1BCSI(NPFC,1,0)

        IF( MYRANK > 0 ) ALLOCATE( IPFACE(12,NPFC) )

        CALL VF_P1BCSI(IPFACE,12*NPFC,0)

      ELSEIF( IPART == 1 ) THEN

        IF( MYRANK == 0 ) THEN

          ! 各プロセスの範囲を受信
          ! 各プロセスの範囲にある要素，節点，面データを *W に作成 -> 送信

          ALLOCATE( ISKIP( MAX0(NELM,NPFC) ) )
          ALLOCATE( IBING(NNOD) )
          ALLOCATE( IGNR(NNOD) )

          DO IP = 1, NPROCS

            ! IP < NPROCS : RANK = IP のプロセスが対象
            ! IP = NPROCS : RANK = 0 のプロセス自身が対象

            IF( IP < NPROCS ) THEN
              CALL SF_MPI_RECV_D(RNG,4,IP)
            ELSE
              RNG(1,1) = XX(1,1)
              RNG(2,1) = XX(1,NUMI)
              RNG(1,2) = YY(1,1)
              RNG(2,2) = YY(1,NUMJ)
            ENDIF

            IE = 0

            DO I = 1, NELM
              CALL SF_ELEM_SKIP(ISKIP(I),RNG(1,1),RNG(2,1),RNG(1,2)
     &                         ,RNG(2,2),IELM(4,I),IELM(5,I),GRID)
              IF( ISKIP(I) == 0 ) IE = IE + 1
            ENDDO

            NELMW = IE

            ALLOCATE( IELMW(24,NELMW) )
            ALLOCATE( PORW(NELMW) )
            ALLOCATE( IENO(NELMW) )

            IE = 0

            IBING(:) = 0

            DO I = 1, NELM
              IF( ISKIP(I) == 1 ) CYCLE
              IE = IE + 1
              IELMW(:,IE) = IELM(:,I)
              PORW(IE) = POR(I)
              IENO(IE) = I
              N = IELM(4,I)
              IBING( IELM(5:4+N,I) ) = 1
            ENDDO

            IG = 0

            DO I = 1, NNOD
              IF( IBING(I) == 1 ) IG = IG + 1
            ENDDO

            NNODW = IG

            ALLOCATE( INDGW(NNODW) )
            ALLOCATE( GRIDW(3,NNODW) )
            ALLOCATE( IGFCW(NNODW) )
            ALLOCATE( GRDLW(NNODW) )
            ALLOCATE( IGNO(NNODW) )

            IG = 0

            DO I = 1, NNOD
              IF( IBING(I) == 0 ) CYCLE
              IG = IG + 1
              INDGW(IG) = INDG(I)
              GRIDW(:,IG) = GRID(:,I)
              IGFCW(IG) = IGFC(I)
              GRDLW(IG) = GRDL(I)
              IGNO(IG) = I
              IGNR(I) = IG
            ENDDO

            DO I = 1, NELMW
              N = IELMW(4,I)
              IELMW(5:4+N,I) = IGNR( IELMW(5:4+N,I) )
            ENDDO

            IF( IP < NPROCS ) THEN
              CALL SF_MPI_RECV_D(RNG,6,IP)
            ELSE
              RNG(1,1) = XX(1,MYIS)
              RNG(2,1) = XX(1,MYIE+1)
              RNG(1,2) = YY(1,MYJS)
              RNG(2,2) = YY(1,MYJE+1)
              RNG(1,3) = ZZ(1,2)
              RNG(2,3) = ZZ(1,NUMK)
            ENDIF

            IPFC = 0

            DO I = 1, NPFC
              CALL SF_PFC_SKIP(ISKIP(I),RNG,IPFACE(4,I),IPFACE(5,I)
     &                        ,GRID)
              IF( ISKIP(I) == 0 ) IPFC = IPFC + 1
            ENDDO

            NPFCW = IPFC

            ALLOCATE( IPFACEW(12,NPFCW) )
            ALLOCATE( IPFNO(NPFCW) )

            IPFC = 0

            DO I = 1, NPFC
              IF( ISKIP(I) == 1 ) CYCLE
              IPFC = IPFC + 1
              IPFACEW(:,IPFC) = IPFACE(:,I)
              IPFNO(IPFC) = I
            ENDDO

            DO I = 1, NPFCW
              N = IPFACEW(4,I)
              IPFACEW(5:4+N,I) = IGNR( IPFACEW(5:4+N,I) )
            ENDDO

            IF( IP < NPROCS ) THEN  ! RANK = IP のプロセスにデータ送信

              CALL SF_MPI_SEND_I(NNODW,1,IP)
              CALL SF_MPI_SEND_I(NELMW,1,IP)

              CALL SF_MPI_SEND_I(INDGW,NNODW,IP)
              CALL SF_MPI_SEND_D(GRIDW,3*NNODW,IP)
              CALL SF_MPI_SEND_I(IGFCW,NNODW,IP)
              CALL SF_MPI_SEND_D(GRDLW,NNODW,IP)
              CALL SF_MPI_SEND_I(IELMW,24*NELMW,IP)
              CALL SF_MPI_SEND_D(PORW,NELMW,IP)

              DEALLOCATE( INDGW )
              DEALLOCATE( GRIDW )
              DEALLOCATE( IGFCW )
              DEALLOCATE( GRDLW )
              DEALLOCATE( IELMW )
              DEALLOCATE( PORW )

              CALL SF_MPI_SEND_I(IGNO,NNODW,IP)
              CALL SF_MPI_SEND_I(IENO,NELMW,IP)

              DEALLOCATE( IGNO )
              DEALLOCATE( IENO )

              CALL SF_MPI_SEND_I(NPFCW,1,IP)

              CALL SF_MPI_SEND_I(IPFACEW,12*NPFCW,IP)

              DEALLOCATE( IPFACEW )

              CALL SF_MPI_SEND_I(IPFNO,NPFCW,IP)

              DEALLOCATE( IPFNO )

            ELSE  ! RANK = 0 のプロセス自身にデータセット

              NNOD = NNODW
              NELM = NELMW

              DEALLOCATE( INDG )
              DEALLOCATE( GRID )
              DEALLOCATE( IGFC )
              DEALLOCATE( GRDL )
              DEALLOCATE( IELM )
              DEALLOCATE( POR )

              ALLOCATE( INDG(NNOD) )
              ALLOCATE( GRID(3,NNOD) )
              ALLOCATE( IGFC(NNOD) )
              ALLOCATE( GRDL(NNOD) )
              ALLOCATE( IELM(24,NELM) )
              ALLOCATE( POR(NELM) )

              INDG(:) = INDGW(:)
              GRID(:,:) = GRIDW(:,:)
              IGFC(:) = IGFCW(:)
              GRDL(:) = GRDLW(:)
              IELM(:,:) = IELMW(:,:)
              POR(:) = PORW(:)

              DEALLOCATE( INDGW )
              DEALLOCATE( GRIDW )
              DEALLOCATE( IGFCW )
              DEALLOCATE( GRDLW )
              DEALLOCATE( IELMW )
              DEALLOCATE( PORW )

              NPFC = NPFCW

              DEALLOCATE( IPFACE )

              ALLOCATE( IPFACE(12,NPFC) )

              IPFACE(:,:) = IPFACEW(:,:)

              DEALLOCATE( IPFACEW )

            ENDIF

          ENDDO

          DEALLOCATE( ISKIP )
          DEALLOCATE( IBING )
          DEALLOCATE( IGNR )

        ELSEIF( MYRANK > 0 ) THEN

          ! 各プロセスの範囲を RANK = 0 のプロセスに送信
          ! 各プロセスの範囲にある要素，節点，面データを RANK = 0 のプロセスから受信

          RNG(1,1) = XX(1,1)
          RNG(2,1) = XX(1,NUMI)
          RNG(1,2) = YY(1,1)
          RNG(2,2) = YY(1,NUMJ)

          CALL SF_MPI_SEND_D(RNG,4,0)

          RNG(1,1) = XX(1,MYIS)
          RNG(2,1) = XX(1,MYIE+1)
          RNG(1,2) = YY(1,MYJS)
          RNG(2,2) = YY(1,MYJE+1)
          RNG(1,3) = ZZ(1,2)
          RNG(2,3) = ZZ(1,NUMK)

          CALL SF_MPI_SEND_D(RNG,6,0)

          CALL SF_MPI_RECV_I(NNOD,1,0)
          CALL SF_MPI_RECV_I(NELM,1,0)

          ALLOCATE( INDG(NNOD) )
          ALLOCATE( GRID(3,NNOD) )
          ALLOCATE( IGFC(NNOD) )
          ALLOCATE( GRDL(NNOD) )
          ALLOCATE( IELM(24,NELM) )
          ALLOCATE( POR(NELM) )

          CALL SF_MPI_RECV_I(INDG,NNOD,0)
          CALL SF_MPI_RECV_D(GRID,3*NNOD,0)
          CALL SF_MPI_RECV_I(IGFC,NNOD,0)
          CALL SF_MPI_RECV_D(GRDL,NNOD,0)
          CALL SF_MPI_RECV_I(IELM,24*NELM,0)
          CALL SF_MPI_RECV_D(POR,NELM,0)

          ALLOCATE( IGNO(NNOD) )
          ALLOCATE( IENO(NELM) )

          CALL SF_MPI_RECV_I(IGNO,NNOD,0)
          CALL SF_MPI_RECV_I(IENO,NELM,0)

          CALL SF_MPI_RECV_I(NPFC,1,0)

          ALLOCATE( IPFACE(12,NPFC) )

          CALL SF_MPI_RECV_I(IPFACE,12*NPFC,0)

          ALLOCATE( IPFNO(NPFC) )

          CALL SF_MPI_RECV_I(IPFNO,NPFC,0)

        ENDIF

      ENDIF

      CALL VF_P1BCSI(IGEO,1,0)

      CALL VF_P1BCSD(PLOWER2,1,0)
      CALL VF_P1BCSD(GMIN,1,0)
      CALL VF_P1BCSD(FMIN,1,0)

      ALLOCATE( POS(3,NNOD) )
      ALLOCATE( POS1(3,NNOD) )
      ALLOCATE( POS2(3,NNOD) )
      ALLOCATE( DVEL(3,NELM) )
      ALLOCATE( DVEL1(3,NELM) )
      ALLOCATE( DVEL2(3,NELM) )

      POS(:,:) = GRID(:,:)
      DVEL(:,:) = 0.

      ALLOCATE( SPC(NELM) )

      SPC(:)%IFIX = 0

      ALLOCATE( IPGRID(2,NNOD) )

      IF( ISTM == 1 ) ALLOCATE( IRGRID(NNOD) )

      END
