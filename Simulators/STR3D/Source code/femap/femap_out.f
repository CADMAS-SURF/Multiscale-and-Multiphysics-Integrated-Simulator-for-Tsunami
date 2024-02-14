      SUBROUTINE FEMAP_OUT(KK,INDG,GRID,IELM,DT1,DT2,UG1,UG2,UG3,DZ,UG
     &                    ,VEL,ACC,RFCO,PPND,PG3,EPSG,SIGG,VELG,VELE
     &                    ,ISTEP,TIME,IDYN,ITO,IFMP)

      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),INDG(*),UG1(6,*),UG2(6,*),UG3(6,*),DZ(*),UG(6,*)
     &         ,VEL(3,*),ACC(3,*),RFCO(6,*),PG3(*),GRID(3,*),IELM(*)
     &         ,EPSG(*),SIGG(*),VELG(*),VELE(*),PPND(*)
      DATA IOUT / 0 /

      IF( MYRANK == 1 ) CALL M_MPI_SEND_I(5,1,0)  ! SEND IOP=5 TO GLB_COMM

      IOUT = IOUT + 1

      IF( MYRANK == 0 ) CALL BLK_S(450,IFMP)

      CALL FMP_OUS(IOUT,ISTEP,TIME,IFMP)

      IF( MYRANK == 0 ) CALL BLK_E(IFMP)

      IF( MYRANK == 0 ) CALL BLK_S(1051,IFMP)

      CALL FMP_NODAL(IOUT,DT1,DT2,INDG,UG1,UG2,UG3,DZ,UG,VEL,ACC,RFCO
     &              ,PPND,PG3,KK(8),KK(26),KK(81),KK(25),IDYN,IFMP)

      CALL FMP_SOL(IOUT,KK,INDG,GRID,IELM,KK(37),EPSG,SIGG,KK(36),PG3
     &            ,ITO,IFMP)

      CALL FMP_ROD(IOUT,KK(10),KK(14),IELM,KK(37),SIGG,KK(36),IFMP)

      CALL FMP_BAR(IOUT,KK(10)+KK(14),KK(16),IELM,KK(37),SIGG,KK(36)
     &            ,IFMP)

      IF( KK(25) > 0 ) 
     &  CALL FMP_SOIL(IOUT,KK,INDG,IELM,KK(37),VELG,KK(36),VELE,IFMP)

      IF( MYRANK == 0 ) CALL BLK_E(IFMP)

      END
