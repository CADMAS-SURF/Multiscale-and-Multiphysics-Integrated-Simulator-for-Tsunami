C=======================================================================
      SUBROUTINE COM_DRIFT1 (XC,YC,ZC)
C=======================================================================
C
C     領域情報・計算条件の通信
C
C-----------------------------------------------------------------------
C
      use mod_comm,only: comm_mlicds_dm
      IMPLICIT NONE
C
      INCLUDE 'CONNEC.h'
      INCLUDE 'mpif.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'DRIFT.h'
C
C
      INTEGER::NP,ITAG
      INTEGER::IERR
      INTEGER::ISTAT(MPI_STATUS_SIZE)
      INTEGER::IREQ1
C
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
C
      INTEGER::I,J,K
      INTEGER::IWK,JWK,KWK
      REAL(8)::AWK,BWK
      REAL(8),ALLOCATABLE::ZWK0(:)
      REAL(8),ALLOCATABLE::XWK1(:),YWK1(:),ZWK1(:)
      REAL(8),ALLOCATABLE::XWK2(:),YWK2(:),ZWK2(:)
C
C
C======================================================================
C
C
         IWK = MX-2
         JWK = MY-2
         KWK = MZ-2
C
         ALLOCATE(ZWK0(KWK))
         ALLOCATE(XWK1(IWK+1))
         ALLOCATE(YWK1(JWK+1))
         ALLOCATE(ZWK1(KWK+1))
         ALLOCATE(XWK2(IWK))
         ALLOCATE(YWK2(JWK))
         ALLOCATE(ZWK2(KWK))
C
         AWK = XC(4,1+1,1)
         BWK = YC(4,1+1)
         DO K=1,KWK
            ZWK0(K) = ZC(4,K+1)
         ENDDO
         DO I=1,IWK+1
            XWK1(I) = XC(1,I,1)
         ENDDO
         DO J=1,JWK+1
            YWK1(J) = YC(1,J)
         ENDDO
         DO K=1,KWK+1
            ZWK1(K) = ZC(1,K)
         ENDDO
         DO I=1,IWK
            XWK2(I) = XC(2,I+1,1)
         ENDDO
         DO J=1,JWK
            YWK2(J) = YC(2,J+1)
         ENDDO
         DO K=1,KWK
            ZWK2(K) = ZC(2,K+1)
         ENDDO
C
CCC         IF( .NOT.NOCALDM ) THEN
         ITAG=120
         CALL MPI_ISEND(AWK,1,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  DX の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         ITAG=130
         CALL MPI_ISEND(BWK,1,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  DY の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         ITAG=140
         CALL MPI_ISEND(ZWK0,KWK,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  DZ(NK) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         ITAG=150
         CALL MPI_ISEND(XWK1,IWK+1,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  XG(0:NI) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         ITAG=160
         CALL MPI_ISEND(YWK1,JWK+1,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  YG(0:NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         ITAG=170
         CALL MPI_ISEND(ZWK1,KWK+1,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  ZG(0:NK) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         ITAG=180
         CALL MPI_ISEND(XWK2,IWK,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  XC(1:NI) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         ITAG=190
         CALL MPI_ISEND(YWK2,JWK,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  YC(1:NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         ITAG=200
         CALL MPI_ISEND(ZWK2,KWK,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  ZC(1:NK) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
CCC         ENDIF
C
         DEALLOCATE(ZWK0)
         DEALLOCATE(XWK1)
         DEALLOCATE(YWK1)
         DEALLOCATE(ZWK1)
         DEALLOCATE(XWK2)
         DEALLOCATE(YWK2)
         DEALLOCATE(ZWK2)
C
C
      IF ( NB_SD_MAIN.EQ.1 ) THEN
         AWK = RHO
CCC         IF( .NOT.NOCALDM ) THEN
         ITAG=210
         CALL MPI_ISEND(AWK,1,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  RHO の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
CCC         ENDIF
      ENDIF
C
C
C----------------------------------------------------------------------
      RETURN
      END
