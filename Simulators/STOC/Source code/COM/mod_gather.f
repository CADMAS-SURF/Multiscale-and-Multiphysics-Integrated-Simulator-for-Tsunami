      MODULE MOD_GATHER
C----------------------------------------------------------------------
C     自動領域分割時に、hstファイルとendファイルの出力を
C     一箇所にまとめるためのモジュール
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      include 'mpif.h'
C
C
C ... 時系列出力用
      INTEGER,ALLOCATABLE :: NHCELLALL(:)   ! 各部分領域で出力するポイントの数
      INTEGER,ALLOCATABLE :: NHDISP(:)      ! 部分領域からデータをMPI_GATHERVで集めるためのDISPLACEMENT変数
      INTEGER,ALLOCATABLE :: LHORDER(:)     ! 部分領域から集めたデータを入力の順番に並べ替えるためのリスト
      REAL(8),ALLOCATABLE :: RHBUFF(:)      ! 部分領域から物理量を集めるためのバッファ
C
C ... endファイル出力用
      REAL(8),ALLOCATABLE :: RV2DG(:,:)     ! 部分領域から物理量を集めたデータを格納するための配列
      REAL(8),ALLOCATABLE :: RVBUFF(:)      ! 部分領域から物理量を集めるための通信用バッファ
      INTEGER,ALLOCATABLE :: NVBUFF(:)      ! 部分領域から物理量を集めるための通信用バッファのサイズ
      CHARACTER(12) :: FORM610='(1P,10E10.3)'
C
C ... 作業用一時配列
      INTEGER,ALLOCATABLE :: LWRK(:)
C
C
C-----------------------------------------------------------------------
      CONTAINS
C-----------------------------------------------------------------------
      SUBROUTINE GATHERH_PRE(NHCELL,NHCELLSUM,LHCELL,IHCELL,
     $                       MYPROC,NPROC,MYIS,MYJS,COMM,IFLHS,
     $                       CHIST,MHIST)
C-----------------------------------------------------------------------
C     時系列出力用の MPI_GATHERV 通信に用いる整数型配列の作成 及び
C     データの並べ替えリストの作成を行う
C-----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NHCELL,MYPROC,NPROC,MYIS,MYJS,COMM,IFLHS
      INTEGER,INTENT(IN) :: LHCELL(NHCELL)
      INTEGER,INTENT(INOUT) :: IHCELL(3,NHCELL)
      INTEGER,INTENT(OUT) :: NHCELLSUM
      INTEGER,INTENT(IN) :: MHIST
      CHARACTER(8),INTENT(IN) :: CHIST(MHIST)
      INTEGER :: L,M,N,LHMAX,IERR,I,J
      CHARACTER(80):: FORM1
C
C ... NHCELLSUMを設定
      CALL MPI_REDUCE(NHCELL,NHCELLSUM,1,MPI_INTEGER,
     $                MPI_SUM,0,COMM,IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('GATHERH_PRE',7070)
         WRITE(16,*) 'ERROR: MPI_REDUCE: NHCELLSUM'
         CALL ABORT1('')
      ENDIF
      IF(MYPROC.NE.1) THEN
         NHCELLSUM=NHCELL
      ENDIF
C
C ... 配列のALLOCATE
      IF(MYPROC.EQ.1) THEN
         ALLOCATE(NHCELLALL(NPROC),NHDISP(NPROC),
     $            LHORDER(NHCELLSUM),LWRK(3*NHCELLSUM),
     $            RHBUFF(NHCELLSUM),STAT=IERR)
         IF(IERR.NE.0)THEN
            CALL ERRMSG('GATHERH_PRE',7071)
            WRITE(16,*) 'ERROR: CANNOT ALLOCATE NHCELLALL'
            CALL ABORT1('')
         ENDIF
C
         NHCELLALL(:)=0
         NHDISP(:)=0
         LHORDER(:)=0
         LWRK(:)=0
         RHBUFF(:)=0.0D0
      ELSE
         ALLOCATE(NHCELLALL(1),NHDISP(1),
     $            LHORDER(1),LWRK(1),
     $            RHBUFF(1),STAT=IERR)
         IF(IERR.NE.0)THEN
            CALL ERRMSG('GATHERH_PRE',7072)
            WRITE(16,*) 'ERROR: CANNOT ALLOCATE NHCELLALL'
            CALL ABORT1('')
         ENDIF
      ENDIF
C
      CALL MPI_GATHER(NHCELL,1,MPI_INTEGER, 
     $                NHCELLALL,1,MPI_INTEGER,
     $                0,COMM,IERR) 
C
C ... NHCELLALLとNHDISPを設定(LHCELL集め用)
      IF( MYPROC.EQ.1 ) THEN
         L=0
         DO M=1,NPROC
            NHDISP(M)=L
            L=L+NHCELLALL(M)
         ENDDO
      ENDIF
C
      CALL MPI_GATHERV(LHCELL,NHCELL,MPI_INTEGER, 
     $                 LWRK,NHCELLALL,NHDISP,MPI_INTEGER,
     $                 0,COMM,IERR) 
C
C ... LHCELLの値順に並べ替えるためのポインタ変数LHORDERを設定
      IF( MYPROC.EQ.1 ) THEN
         DO L=NHCELLSUM,1,-1
            N=0
            LHMAX=0
            DO M=1,NHCELLSUM
               IF(LWRK(M).GT.LHMAX) THEN
                  N=M
                  LHMAX=LWRK(M)
               ENDIF
            ENDDO
C
            LHORDER(L)=N
            LWRK(N)=0       ! Clear
         ENDDO
C
         DO L=1,NHCELLSUM
            WRITE(6,*) 'LHORDER(',L,')=',LHORDER(L)
         ENDDO
      ENDIF
C
C ... NHCELLALLとNHDISPを更新(IHCELL集め用)
      IF( MYPROC.EQ.1 ) THEN
         L=0
         DO M=1,NPROC
            NHDISP(M)=L
            NHCELLALL(M)=NHCELLALL(M)*3
            L=L+NHCELLALL(M)
         ENDDO
      ENDIF
C
C ... CHANGE TO GLOBAL INDEX
      DO L=1,NHCELL
         IHCELL(1,L)=IHCELL(1,L)+MYIS-2
         IHCELL(2,L)=IHCELL(2,L)+MYJS-2
      ENDDO
C
      CALL MPI_GATHERV(IHCELL,3*NHCELL,MPI_INTEGER, 
     $                 LWRK,NHCELLALL,NHDISP,MPI_INTEGER,
     $                 0,COMM,IERR) 
C
C ... CHANGE TO LOCAL INDEX
      DO L=1,NHCELL
         IHCELL(1,L)=IHCELL(1,L)-MYIS+2
         IHCELL(2,L)=IHCELL(2,L)-MYJS+2
      ENDDO
C
C ... NHCELLALLとNHDISPを更新(物理量集め用)
      IF( MYPROC.EQ.1 ) THEN
         L=0
         DO M=1,NPROC
            NHDISP(M)=L
            NHCELLALL(M)=NHCELLALL(M)/3
            L=L+NHCELLALL(M)
         ENDDO
C
C ...... Write header part
         WRITE(IFLHS,600) NHCELLSUM*MHIST+5
  600    FORMAT('# START IMPORT AT ROW',I5,/,
     $          '# COLUMN: VARIABLE     I     J     K')
         I=0
         DO M=1,MHIST
         DO N=1,NHCELLSUM
            I=I+1
            WRITE(IFLHS,601) I,CHIST(M),
     $            (LWRK(3*(LHORDER(N)-1)+J),J=1,3)
  601       FORMAT('#',1X,I6,':',1X,A8,3I6)
         ENDDO
         ENDDO
         FORM1='(''#'',/,''#        TIME'',        )'
         IF(I.GT.0) WRITE(FORM1(24:31),'(I5,A3)') I,'I13'
         WRITE(IFLHS,FORM1) (J,J=1,I)
      ENDIF
C
      DEALLOCATE(LWRK)
C
      RETURN
      END SUBROUTINE GATHERH_PRE
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GATHERH(OUTWRK,NHCELL,COMM)
C-----------------------------------------------------------------------
C     時系列出力するデータを各PEから集める
C-----------------------------------------------------------------------
C
      REAL(8),INTENT(IN) :: OUTWRK(NHCELL)
      INTEGER,INTENT(IN) :: NHCELL,COMM
      INTEGER :: IERR
C
C
      CALL MPI_GATHERV(OUTWRK,NHCELL,MPI_DOUBLE_PRECISION, 
     $                 RHBUFF,NHCELLALL,NHDISP,MPI_DOUBLE_PRECISION,
     $                 0,COMM,IERR) 
C
      RETURN
      END SUBROUTINE GATHERH
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GATHERV_PRE(MX,MY,MXG,MYG,MYPROC,NPROC,COMM,IRTN)
C-----------------------------------------------------------------------
C     endファイル出力のために配列の割当を行う
C-----------------------------------------------------------------------
C
      INTEGER,INTENT(IN) :: MX,MY,MXG,MYG,MYPROC,NPROC,COMM
      INTEGER,INTENT(OUT) :: IRTN
      INTEGER :: NSIZE,IERR
C
C
C     通信バッファサイズ配列NVBUFFを作成する
C
      IF( MYPROC.EQ.1 ) THEN
         ALLOCATE(NVBUFF(NPROC),STAT=IERR)
      ELSE
         ALLOCATE(NVBUFF(1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0)THEN
         CALL ERRMSG('GATHERV_PRE',7073)
         WRITE(16,*) 'ERROR: CANNOT ALLOCATE NVBUFF'
         CALL ABORT1('')
      ENDIF
C
      NSIZE=MX*MY
      CALL MPI_GATHER(NSIZE,1,MPI_INTEGER, 
     $                NVBUFF,1,MPI_INTEGER,
     $                0,COMM,IERR) 
      IF(IERR.NE.0) THEN
         CALL ERRMSG('GATHERV_PRE',7074)
         WRITE(16,*) 'ERROR: MPI_GATHER: NVBUFF'
         CALL ABORT1('')
      ENDIF
C
C     通信用配列の割当を行う
C
      IF( MYPROC.EQ.1 ) THEN
         NSIZE=MAXVAL(NVBUFF(:))
         ALLOCATE(RV2DG(MXG,MYG),STAT=IERR)
         ALLOCATE(RVBUFF(NSIZE),STAT=IERR)
      ELSE
         ALLOCATE(RV2DG(1,1),STAT=IERR)
         ALLOCATE(RVBUFF(1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
         CALL ERRMSG('GATHERV_PRE',7075)
         WRITE(16,*) 'ERROR: CANNOT ALLOCATE RV2DG'
         CALL ABORT1('')
      ENDIF
C
      RV2DG(:,:)=0.0D0
      RVBUFF(:)=0.0D0
      IRTN=1
C
      RETURN
      END SUBROUTINE GATHERV_PRE
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GATHERV(PHYS,MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,COMM,
     $                   IFLEN)
C-----------------------------------------------------------------------
C     endファイル出力のために部分領域のデータを一つの配列に集めて出力する
C-----------------------------------------------------------------------
C
      REAL(8),INTENT(IN) :: PHYS(MX,MY)
      INTEGER,INTENT(IN) :: INDCOM(6,NPROC)
      INTEGER,INTENT(IN) :: MX,MY,MXG,MYG,MYPROC,NPROC,COMM,IFLEN
      INTEGER :: I,J,N,NSIZE,ITAG,IREQ,IERR
      INTEGER :: IS,IE,JS,JE,MX1,MY1
      INTEGER :: ISTAT(MPI_STATUS_SIZE)
      INTEGER,SAVE :: ITAG0=1000
C
C
      IF(MYPROC.EQ.1)THEN
         RV2DG(:,:)=1.d99
         DO N=1,NPROC
            IF(N.GT.1)THEN
               ITAG=ITAG0+N
               CALL MPI_IRECV(RVBUFF,NVBUFF(N),MPI_DOUBLE_PRECISION,N-1,
     $                        ITAG,COMM,IREQ,IERR)
               CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
               IF(IERR.NE.0) THEN
                  CALL ERRMSG('GATHERV',7076)
                  WRITE(16,*) 'ERROR: MPI_IRECV'
                  CALL ABORT1('')
               ENDIF
            ENDIF
C
            IS=INDCOM(1,N)
            IE=INDCOM(2,N)
            JS=INDCOM(3,N)
            JE=INDCOM(4,N)
            MX1=IE-IS+3
            MY1=JE-JS+3
            IF(N.EQ.1)THEN
               CALL SETRV2DG(PHYS  ,MX1,MY1,MXG,MYG,IS,IE,JS,JE)
            ELSE
               CALL SETRV2DG(RVBUFF,MX1,MY1,MXG,MYG,IS,IE,JS,JE)
            ENDIF
         ENDDO
C
      ELSE
         NSIZE=MX*MY
         ITAG=ITAG0+MYPROC
         CALL MPI_ISEND(PHYS,NSIZE,MPI_DOUBLE_PRECISION,0,
     $                  ITAG,COMM,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)         
C
         IF(IERR.NE.0) THEN
            CALL ERRMSG('GATHERV',7077)
            WRITE(16,*) 'ERROR: MPI_ISEND'
            CALL ABORT1('')
         ENDIF
      ENDIF
C
      ITAG0=ITAG0+100
      IF(ITAG0.GT.10000) ITAG0=1000
C
      IF(MYPROC.EQ.1) then
         if( FORM610.eq.'(     10I10)' ) then
         WRITE(IFLEN,FORM610)((nint(RV2DG(I,J)),I=1,MXG),J=1,MYG)
         else
         WRITE(IFLEN,FORM610)((RV2DG(I,J),I=1,MXG),J=1,MYG)
         endif
      endif
C
      RETURN
      END SUBROUTINE GATHERV
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GATHERVI(IPHYS,WRK,MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,
     $                    COMM,IFLEN)
C-----------------------------------------------------------------------
C     endファイル出力のために部分領域のデータを一つの配列に集めて出力する
C     (整数型変数用)
C-----------------------------------------------------------------------
C
      INTEGER,INTENT(IN) :: IPHYS(MX,MY)
      REAL(8),INTENT(OUT) :: WRK(MX,MY)
      INTEGER,INTENT(IN) :: INDCOM(6,NPROC)
      INTEGER,INTENT(IN) :: MX,MY,MXG,MYG,MYPROC,NPROC,COMM,IFLEN
      INTEGER :: I,J
      CHARACTER(12) :: FORM610BACK
C
C
      DO J=1,MY
      DO I=1,MX
         WRK(I,J)=REAL(IPHYS(I,J))
      ENDDO
      ENDDO
C
      FORM610BACK=FORM610
      FORM610='(     10I10)'
      CALL GATHERV(WRK,MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,COMM,IFLEN) 
      FORM610=FORM610BACK
C
      RETURN
      END SUBROUTINE GATHERVI
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SETRV2DG(PHYS,MX1,MY1,MXG,MYG,IS,IE,JS,JE)
C-----------------------------------------------------------------------
C     RV2DG配列にデータを格納する
C-----------------------------------------------------------------------
C
      REAL(8),INTENT(IN) :: PHYS(MX1,MY1)
      INTEGER,INTENT(IN) :: MX1,MY1,MXG,MYG,IS,IE,JS,JE
      INTEGER :: I,J,IG,JG
      INTEGER :: I1,J1,I2,J2
C
C
      I1=2
      J1=2
      I2=MX1-1
      J2=MY1-1
      IF(IS.EQ.2) I1=1
      IF(JS.EQ.2) J1=1
      IF(IE.EQ.MXG-1) I2=MX1
      IF(JE.EQ.MYG-1) J2=MY1
C
      DO J=J1,J2
      DO I=I1,I2
         IG=I+IS-2
         JG=J+JS-2
         RV2DG(IG,JG)=PHYS(I,J)
      ENDDO
      ENDDO
C
      RETURN
      END SUBROUTINE SETRV2DG
C-----------------------------------------------------------------------
C
      END MODULE MOD_GATHER
