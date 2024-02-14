      SUBROUTINE OIL_COMM_PHYS(HDEP,HH,UU,VV,WX,WY,KF,WRK,MX,MY,MZ)
C
      use mod_comm,only: comm_mlicds_oil
      IMPLICIT NONE
C
      INCLUDE 'CONNEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'OIL.h'
      INCLUDE 'mpif.h'
C
      INTEGER,INTENT(IN) :: MX,MY,MZ
      REAL(8),INTENT(IN) :: HDEP(MX,MY),HH(MX,MY)
      REAL(8),INTENT(IN) :: UU(MX,MY,MZ)
      REAL(8),INTENT(IN) :: VV(MX,MY,MZ)
      REAL(8),INTENT(IN) :: WX(MX,MY)
      REAL(8),INTENT(IN) :: WY(MX,MY)
      INTEGER,INTENT(IN) :: KF(MX,MY)

      REAL(8),INTENT(OUT) :: WRK(MX,MY,2)
C
      CHARACTER(LEN=128) :: CFILE
      INTEGER :: ITAG
      INTEGER :: IREQ(4)
      INTEGER :: ISTAT(MPI_STATUS_SIZE,4)
      INTEGER :: IERR
      INTEGER :: I,J,K
C
      LOGICAL,SAVE:: AT_FIRST=.TRUE.
C
C-----------------------------------------------------------------------
C
C 表面速度の設定
C
      WRK(:,:,:) = 0.0D0
      DO J=2,MY-1
        DO I=1,MX-1
!          WRK(I  ,J,1) = UU(I  ,J,MAX(KF(I-1,J),KF(I,J)))
          K=MAX(KF(I+1,J),KF(I,J))
          IF( K<MZ ) WRK(I+1,J,1) = UU(I+1,J,K)
        END DO
      END DO
      DO J=1,MY-1
        DO I=2,MX-1
!          WRK(I,J  ,2) = VV(I,J  ,MAX(KF(I,J-1),KF(I,J)))
          K=MAX(KF(I,J+1),KF(I,J))
          IF( K<MZ ) WRK(I,J+1,2) = VV(I,J+1,K)
        END DO
      END DO
C
C ONLINE接続時の通信
C
      IF(ICAL_OIL.NE.0) THEN
        ITAG = 4*NRANK
        CALL MPI_ISEND(WRK(:,:,1),MX*MY,MPI_DOUBLE_PRECISION
     &                ,NP_OIL,ITAG+1,comm_mlicds_oil,IREQ(1),IERR)
        CALL MPI_ISEND(WRK(:,:,2),MX*MY,MPI_DOUBLE_PRECISION
     &                ,NP_OIL,ITAG+2,comm_mlicds_oil,IREQ(2),IERR)
        CALL MPI_ISEND(WX,MX*MY,MPI_DOUBLE_PRECISION
     &                ,NP_OIL,ITAG+3,comm_mlicds_oil,IREQ(3),IERR)
        CALL MPI_ISEND(WY,MX*MY,MPI_DOUBLE_PRECISION
     &                ,NP_OIL,ITAG+4,comm_mlicds_oil,IREQ(4),IERR)
        CALL MPI_WAITALL(4,IREQ,ISTAT,IERR)
      END IF
C
C OFFLINE接続用のファイルの出力
C
      IF(ROILF(1).LE.ROILF(2)) THEN
        IF(TIME-DT-ROILF(1).LE.1.0D-7*DT .AND.
     &     ROILF(1)-TIME.LE.1.0D-7*DT) THEN
!
          call flnam('.uv ')
          WRITE(CFILE,'(A,I8.8)')
     &      TRIM(CFLNM)//'_',INT(ROILF(1))
          OPEN(LOILF,FILE='oil/'//TRIM(CFILE),FORM='UNFORMATTED')
          WRITE(LOILF) MX,MY
          WRITE(LOILF) REAL(WRK(:,:,1))
          WRITE(LOILF) REAL(WRK(:,:,2))
          IF( OIL_WIN.GT.0 ) THEN
             WRITE(LOILF) REAL(WX)
             WRITE(LOILF) REAL(WY)
          ENDIF
          CLOSE(LOILF)
C
          write(16,*) 'OIL OUT AT TIME=',TIME,INT(ROILF(1)),ROILF(1)
          call flnam('.zz ')
          WRITE(CFILE,'(A,I8.8)')
     &      TRIM(CFLNM)//'_',INT(ROILF(1))
          OPEN(LOILF,FILE='oil/'//TRIM(CFILE),FORM='UNFORMATTED')
          WRITE(LOILF) MX,MY
          WRITE(LOILF) REAL(HH(:,:))
          CLOSE(LOILF)
C
          IF( AT_FIRST ) THEN
             call flnam('.dep')
             OPEN(LOILF,FILE='oil/'//TRIM(CFLNM),FORM='UNFORMATTED')
             WRITE(LOILF) MX,MY
             WRITE(LOILF) REAL(HDEP(:,:))
             CLOSE(LOILF)
C
             AT_FIRST=.FALSE.
          ENDIF
C
          ROILF(1) = ROILF(1) + ROILF(3)
        END IF
      END IF
C
      RETURN
      END
