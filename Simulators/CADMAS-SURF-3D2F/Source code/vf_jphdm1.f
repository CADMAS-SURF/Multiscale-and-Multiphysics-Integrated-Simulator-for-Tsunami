      SUBROUTINE VF_JPHDM1(XX,YY,ZZ)

CD=== 概要 ===========================================================

CDT   VF_JPHDM1:HiDEMとのインターフェイス(1):連成解析計算領域の確認

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_comm,only: comm_2fc_dem
      use mod_dem

      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'mpif.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI) : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ) : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)

CD    -- 主要制御変数 --
CD    IPRNT : I*4 : 空隙率の時間方向のデータ数
CD                  =0:空隙率ファイルを読み込まない
CD                  =1:時間依存データでは無い
CD                  >1:時間依存データ
CD    IPRNB : I*4 : 時間依存型空隙率の空間ブロックの最大数
CD    IPRNP : I*4 : 時間依存型空隙率の設定セル数

      integer,allocatable :: ncadm_all(:,:,:) ! (3,0:nsize_dem-1,0:nprocs-1)
                                              ! ncadm_sub を gather したもの
      real(8),allocatable :: buff1(:)

      INTEGER :: STATUS(MPI_STATUS_SIZE)

C==== 実行 ===========================================================

CD    -- HiDEMのランク --
CCCCC      IHIDM=0
C
C     comm_2fc_demにおいて、どのPEが2fcで、どのPEがdemかの情報を共有する
C
      allocate(nrank_2fc_dem(0:nprwld-1))
      allocate(nrank_dem(0:nprwld-1))  ! nsize_demの大きさで十分だけど
                                       ! 2 pass させるほどのこともないのでこの大きさで

      call mpi_allgather(-(myrank+1),   1, MPI_INTEGER,
     $                   nrank_2fc_dem, 1, MPI_INTEGER,
     $                   comm_2fc_dem, ierr)

      nsize_dem = 0
      do lpe=0,nprwld-1
        if(nrank_2fc_dem(lpe)==0+1) IHIDM = lpe  ! HiDEM でのrank 0 のPE
        if(nrank_2fc_dem(lpe)>0) then
          nrank_dem(nsize_dem) = lpe
          nsize_dem = nsize_dem + 1
        end if
      end do

      ALLOCATE(xhidem_sub(6,0:nsize_dem-1))
      ALLOCATE(jcadm_sub(6,0:nsize_dem-1))
      ALLOCATE(ncadm_sub(3,0:nsize_dem-1))

CD    -- 制御変数の設定 --
      IPRNT=2

CD    -- HiDEM側から連成解析計算領域データをもらう --
      if(myrank==0) then
        call MPI_RECV(xhidem_sub,6*nsize_dem,MPI_DOUBLE_PRECISION,
     &                IHIDM, 0, comm_2fc_dem, STATUS, IERR)
      end if
      CALL MPI_BCAST(xhidem_sub,6*nsize_dem,MPI_DOUBLE_PRECISION,0,
     &               MGCOMM,IERR)

CD    -- 連成解析計算領域の確認 --
      jcadm(1:3) =  2**30
      jcadm(4:6) = -2**30

      XS_CADMAS = XX(1,MYIS  )
      XE_CADMAS = XX(1,MYIE+1)
      YS_CADMAS = YY(1,MYJS  )
      YE_CADMAS = YY(1,MYJE+1)
      ZS_CADMAS = ZZ(1,2     )
      ZE_CADMAS = ZZ(1,NUMK  )

      do lpe=0,nsize_dem-1
C
C DEM のlpe番目のPEの担当範囲
C
        XS_DEM = xhidem_sub(1,lpe)
        YS_DEM = xhidem_sub(2,lpe)
        ZS_DEM = xhidem_sub(3,lpe)
        XE_DEM = xhidem_sub(4,lpe)
        YE_DEM = xhidem_sub(5,lpe)
        ZE_DEM = xhidem_sub(6,lpe)
C
C 2FC で自分が担当する範囲とDEMの範囲が重なっていない
C
        IF(XE_DEM < XS_CADMAS .OR. XS_DEM > XE_CADMAS .OR.
     &     YE_DEM < YS_CADMAS .OR. YS_DEM > YE_CADMAS .OR.
     &     ZE_DEM < ZS_CADMAS .OR. ZS_DEM > ZE_CADMAS) THEN
          jcadm_sub(:,lpe) = 0
C
C 2FC で自分が担当する範囲とDEMの範囲がちょっとでも重なっている
C
        ELSE
          jcadm_sub(1,lpe) = MYIS
          DO 110 I=MYIS,MYIE+1
            IF(XS_DEM >= XX(1,I)) jcadm_sub(1,lpe)=I
            IF(XE_DEM >= XX(1,I)) THEN
              jcadm_sub(4,lpe)=I-1
            ELSE
              EXIT
            END IF
 110      CONTINUE

          jcadm_sub(2,lpe) = MYJS
          DO 120 J=MYJS,MYJE+1
            IF(YS_DEM >= YY(1,J)) jcadm_sub(2,lpe)=J
            IF(YE_DEM >= YY(1,J)) THEN
              jcadm_sub(5,lpe)=J-1
            ELSE
              EXIT
            END IF
 120      CONTINUE

          jcadm_sub(3,lpe) = 2
          DO 130 K=2,NUMK
            IF(ZS_DEM >= ZZ(1,K)) jcadm_sub(3,lpe)=K
            IF(ZE_DEM >= ZZ(1,K)) THEN
              jcadm_sub(6,lpe)=K-1
            ELSE
              EXIT
            END IF
 130      CONTINUE

          jcadm(1) = min(jcadm(1), jcadm_sub(1,lpe))
          jcadm(2) = min(jcadm(2), jcadm_sub(2,lpe))
          jcadm(3) = min(jcadm(3), jcadm_sub(3,lpe))
          jcadm(4) = max(jcadm(4), jcadm_sub(4,lpe))
          jcadm(5) = max(jcadm(5), jcadm_sub(5,lpe))
          jcadm(6) = max(jcadm(6), jcadm_sub(6,lpe))

        END IF

CD    -- 連成計算領域のCADMAS-SURF側での分割数を求める
        IF (jcadm_sub(4,lpe)*jcadm_sub(1,lpe).NE.0) THEN
          ncadm_sub(1,lpe)=jcadm_sub(4,lpe) - jcadm_sub(1,lpe) + 1
        ELSE
          ncadm_sub(1,lpe)=0
        ENDIF
        IF (jcadm_sub(5,lpe)*jcadm_sub(2,lpe).NE.0) THEN
          ncadm_sub(2,lpe)=jcadm_sub(5,lpe) - jcadm_sub(2,lpe) + 1
        ELSE
          ncadm_sub(2,lpe)=0
        ENDIF
        IF (jcadm_sub(6,lpe)*jcadm_sub(3,lpe).NE.0) THEN
          ncadm_sub(3,lpe)=jcadm_sub(6,lpe) - jcadm_sub(3,lpe) + 1
        ELSE
          ncadm_sub(3,lpe)=0
        ENDIF

      end do

CD    -- DEM のPE との通信マトリックスの作成とDEM側への送信
      ALLOCATE(ncadm_all(3,0:nsize_dem-1,0:nprocs-1))
      CALL MPI_GATHER(ncadm_sub,3*nsize_dem,MPI_INTEGER
     &               ,ncadm_all,3*nsize_dem,MPI_INTEGER
     &               ,0,MGCOMM,IERR)

      IF(MYRANK==0) THEN
        CALL MPI_SEND(ncadm_all,3*nsize_dem*nprocs,MPI_INTEGER
     &               ,ihidm,0,comm_2fc_dem,IERR)
      END IF

      DEALLOCATE(ncadm_all)

CD    -- 時間依存型空隙率の設定セル数 --
      IPRNB = 1
C
C local --> global
      IP = MYGIS - 1
      JP = MYGJS - 1
      jcadm(1) = jcadm(1) + IP
      jcadm(2) = jcadm(2) + JP
      jcadm(4) = jcadm(4) + IP
      jcadm(5) = jcadm(5) + JP
      CALL MPI_ALLREDUCE(JCADM(1),IPRARA(1,IPRNB),3
     &                  ,MPI_INTEGER,MPI_MIN,MGCOMM,IERR)
      CALL MPI_ALLREDUCE(JCADM(4),IPRARA(4,IPRNB),3
     &                  ,MPI_INTEGER,MPI_MAX,MGCOMM,IERR)
C global --> local
      jcadm(1) = jcadm(1) - IP
      jcadm(2) = jcadm(2) - JP
      jcadm(4) = jcadm(4) - IP
      jcadm(5) = jcadm(5) - JP
C
C     この大きさはglobalな大きさであり、これで配列がallocateされるが、
C       PE毎にlocalな大きさでallocateする方がいいけど。。。
      ncadm(1) = IPRARA(4,IPRNB) - IPRARA(1,IPRNB) + 1
      ncadm(2) = IPRARA(5,IPRNB) - IPRARA(2,IPRNB) + 1
      ncadm(3) = IPRARA(6,IPRNB) - IPRARA(3,IPRNB) + 1
      IPRNP = ncadm(1)*ncadm(2)*ncadm(3)

CD    -- 連成計算領域のCADMAS-SURF側での分割数と体積多孔率の下限値を送る
      do lpe=0,nsize_dem-1
        nx = ncadm_sub(1,lpe)
        ny = ncadm_sub(2,lpe)
        nz = ncadm_sub(3,lpe)
        if(nx*ny*nz/=0) then
          allocate(buff1(3+nx+ny+nz))
          buff1(1      :1+nx      )
     &      = XX(1,jcadm_sub(1,lpe):jcadm_sub(4,lpe)+1)
          buff1(2+nx   :2+nx+ny   )
     &      = YY(1,jcadm_sub(2,lpe):jcadm_sub(5,lpe)+1)
          buff1(3+nx+ny:3+nx+ny+nz)
     &      = ZZ(1,jcadm_sub(3,lpe):jcadm_sub(6,lpe)+1)
          call mpi_send(buff1,3+nx+ny+nz,MPI_REAL8
     &                 ,nrank_dem(lpe),myrwld,comm_2fc_dem,ierr)
          deallocate(buff1)
        end if
      end do

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
