      MODULE MOD_APARA
C----------------------------------------------------------------------
C     領域分割による並列計算の機能拡張を行うためのモジュール
C     親側M分割、子側N分割でのネスティング時の並列計算に対応
C
C     (なお、親側1分割、子側N分割でのネスティング時の
C      並列計算に関する変数はVF_APARAI.h)
C
C     含まれるサブルーチン
C
C     MAKE_GGV_LIST: 
C----------------------------------------------------------------------
      IMPLICIT NONE
      private
! subroutines
      public:: MAKE_GGV_LIST,DELETE_GGV_LIST,SET_GGV_F,SET_GGV_UVW
      public:: MAKE_P2C_LIST,DELETE_P2C_LIST,SET_P2C
      public:: MAKE_C2P_LIST,DELETE_C2P_LIST,SET_C2P
! variables
      public:: NSEND,NRECV,NCOUNTSSUM,NCOUNTRSUM,RBUF
C
C****************************************
C     地形データの送信用(make_ggv_list,delete_ggv_list,set_ggv_f,set_ggv_uvwで使用)
C****************************************
      INTEGER            :: NCOUNTF      ! 送信するデータの数(セル中心定義)
      INTEGER            :: NCOUNTU      ! 送信するデータの数(X方向セル境界定義)
      INTEGER            :: NCOUNTV      ! 送信するデータの数(Y方向セル境界定義)
      INTEGER            :: NCOUNTW      ! 送信するデータの数(Z方向セル境界定義)
      INTEGER            :: NCOUNTFSUM   ! NCOUNTFのMGCOM内のプロセスの合計
      INTEGER            :: NCOUNTUSUM   ! NCOUNTUのMGCOM内のプロセスの合計
      INTEGER            :: NCOUNTVSUM   ! NCOUNTVのMGCOM内のプロセスの合計
      INTEGER            :: NCOUNTWSUM   ! NCOUNTWのMGCOM内のプロセスの合計
C
      INTEGER,ALLOCATABLE:: IPOSF(:,:)   ! 送信するデータの位置(セル中心定義)
      INTEGER,ALLOCATABLE:: IPOSU(:,:)   ! 送信するデータの位置(X方向セル境界定義)
      INTEGER,ALLOCATABLE:: IPOSV(:,:)   ! 送信するデータの位置(Y方向セル境界定義)
      INTEGER,ALLOCATABLE:: IPOSW(:,:)   ! 送信するデータの位置(Z方向セル境界定義)
C                                        ! 第一要素は1:I, 2:J, 3:K,
C                                        ! 第二要素はデータ番号
      INTEGER,ALLOCATABLE:: JPOSF(:)     ! 送信するデータの全体領域における番号(セル中心定義)
      INTEGER,ALLOCATABLE:: JPOSU(:)     ! 送信するデータの全体領域における番号(X方向セル境界定義)
      INTEGER,ALLOCATABLE:: JPOSV(:)     ! 送信するデータの全体領域における番号(Y方向セル境界定義)
      INTEGER,ALLOCATABLE:: JPOSW(:)     ! 送信するデータの全体領域における番号(Z方向セル境界定義)
C
C ... 0番PEのみ
      INTEGER,ALLOCATABLE:: NCOUNTFALL(:)! MGCOM内のプロセスのNCOUNTFの集合
      INTEGER,ALLOCATABLE:: NCOUNTUALL(:)! MGCOM内のプロセスのNCOUNTUの集合
      INTEGER,ALLOCATABLE:: NCOUNTVALL(:)! MGCOM内のプロセスのNCOUNTVの集合
      INTEGER,ALLOCATABLE:: NCOUNTWALL(:)! MGCOM内のプロセスのNCOUNTWの集合
      INTEGER,ALLOCATABLE:: NDISPFALL(:) ! GATHER時の各PEからのデータの格納位置
      INTEGER,ALLOCATABLE:: NDISPUALL(:) ! GATHER時の各PEからのデータの格納位置
      INTEGER,ALLOCATABLE:: NDISPVALL(:) ! GATHER時の各PEからのデータの格納位置
      INTEGER,ALLOCATABLE:: NDISPWALL(:) ! GATHER時の各PEからのデータの格納位置
      INTEGER,ALLOCATABLE:: JPOSFALL(:)  ! MGCOM内のプロセスのJPOSFの集合
      INTEGER,ALLOCATABLE:: JPOSUALL(:)  ! MGCOM内のプロセスのJPOSUの集合
      INTEGER,ALLOCATABLE:: JPOSVALL(:)  ! MGCOM内のプロセスのJPOSVの集合
      INTEGER,ALLOCATABLE:: JPOSWALL(:)  ! MGCOM内のプロセスのJPOSWの集合
C
C****************************************
C     境界面の流量送信用(make_p2c_list,delete_p2c_list,set_p2cで使用)
C****************************************
      INTEGER            :: NCOUNTS      ! 送信するデータの数
      INTEGER            :: NCOUNTSSUM   ! NCOUNTSのMGCOM内のプロセスの合計
      INTEGER,ALLOCATABLE:: JPOSS(:)     ! 送信するデータの全体領域における番号
      INTEGER,ALLOCATABLE:: NSEND(:)     ! 送信するデータの数(子供1つ毎)
C
C ... 0番PEのみ
      INTEGER,ALLOCATABLE:: NCOUNTSALL(:)! MGCOM内のプロセスのNCOUNTSの集合
      INTEGER,ALLOCATABLE:: NDISPSALL(:) ! GATHER時の各PEからのデータの格納位置
      INTEGER,ALLOCATABLE:: JPOSSALL(:)  ! MGCOM内のプロセスのJPOSSの集合
C
C****************************************
C     境界面の流量受信用(make_c2p_list,delete_c2p_list,set_c2pで使用)
C****************************************
      INTEGER            :: NCOUNTR      ! 受信するデータの数
      INTEGER            :: NCOUNTRSUM   ! NCOUNTRのMGCOM内のプロセスの合計
      INTEGER,ALLOCATABLE:: JPOSR(:)     ! 受信するデータの全体領域における番号
      INTEGER,ALLOCATABLE:: NRECV(:)     ! 受信するデータの数(子供1つ毎)
C
C ... 0番PEのみ
      INTEGER,ALLOCATABLE:: NCOUNTRALL(:)! MGCOM内のプロセスのNCOUNTRの集合
      INTEGER,ALLOCATABLE:: NDISPRALL(:) ! GATHER時の各PEからのデータの格納位置
      INTEGER,ALLOCATABLE:: JPOSRALL(:)  ! MGCOM内のプロセスのJPOSRの集合
C
      DOUBLE PRECISION,ALLOCATABLE:: RBUF(:) ! 境界面の流量を送受信するときの一時格納用途バッファ(送信、受信で共用)
      DOUBLE PRECISION,ALLOCATABLE:: RBUFWRK(:) ! 境界面の流量を送受信するときの一時格納用途バッファ(送信、受信で共用)
C
C
      CONTAINS
C
C
      SUBROUTINE MAKE_GGV_LIST(MGCPOS,MGCNUM,MYRANK,NPROCS,
     $                         ISG,JSG,IEG,JEG)
C----------------------------------------
C     親から子への地形データの通信に用いるリストを作成する
C----------------------------------------
      INTEGER,INTENT(IN):: MGCPOS(6,MGCNUM),MGCNUM
      INTEGER,INTENT(IN):: MYRANK,NPROCS
      INTEGER,INTENT(IN):: ISG,IEG,JSG,JEG
C
      INTEGER:: I,J,K,N,NN,IERR
      INTEGER:: IS,IE,JS,JE,KS,KE
      INTEGER:: NCOUNTF2,NCOUNTU2,NCOUNTV2,NCOUNTW2
C
C
      NCOUNTF = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
C
         DO K=KS,KE
         DO J=JS,JE
         DO I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTF = NCOUNTF + 2
               ENDIF
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDDO

      NCOUNTU = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
C
         DO K=KS,KE
         DO J=JS,JE
         DO I=IS,IE+1
            IF(I.LE.IS+1 .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTU = NCOUNTU + 1
               ENDIF
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDDO
C
      NCOUNTV = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
C
         DO K=KS,KE
         DO J=JS,JE+1
         DO I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS+1 .OR. J.GE.JE) THEN
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTV = NCOUNTV + 1
               ENDIF
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDDO
C
      NCOUNTW = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
C
         DO K=KS,KE+1
         DO J=JS,JE
         DO I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTW = NCOUNTW + 1
               ENDIF
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDDO
C
      ALLOCATE(IPOSF(3,max(NCOUNTF,1)),IPOSU(3,max(NCOUNTU,1)),
     $         IPOSV(3,max(NCOUNTV,1)),IPOSW(3,max(NCOUNTW,1)),
     $         JPOSF(max(NCOUNTF,1)),  JPOSU(max(NCOUNTU,1)),
     $         JPOSV(max(NCOUNTV,1)),  JPOSW(max(NCOUNTW,1)),
     $         STAT=IERR)
C
      NCOUNTF2 = 0
      NN = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
C
         DO K=KS,KE
         DO J=JS,JE
         DO I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTF2 = NCOUNTF2 + 1
                  IPOSF(1,NCOUNTF2)=I
                  IPOSF(2,NCOUNTF2)=J
                  IPOSF(3,NCOUNTF2)=K
                  JPOSF(NCOUNTF2)=NN+1
C
                  NCOUNTF2 = NCOUNTF2 + 1
                  IPOSF(1,NCOUNTF2)=I
                  IPOSF(2,NCOUNTF2)=J
                  IPOSF(3,NCOUNTF2)=K
                  JPOSF(NCOUNTF2)=NN+2
               ENDIF
               NN=NN+2
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDDO
c      write(100+myrank,*) '1:nn=',nn
C
      NCOUNTU2 = 0
      NN = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
C
         DO K=KS,KE
         DO J=JS,JE
         DO I=IS,IE+1
            IF(I.LE.IS+1 .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTU2 = NCOUNTU2 + 1
                  IPOSU(1,NCOUNTU2)=I
                  IPOSU(2,NCOUNTU2)=J
                  IPOSU(3,NCOUNTU2)=K
                  JPOSU(NCOUNTU2)=NN+1
               ENDIF
               NN=NN+1
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDDO
c      write(100+myrank,*) '2:nn=',nn
C
      NCOUNTV2 = 0
      NN = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
C
         DO K=KS,KE
         DO J=JS,JE+1
         DO I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS+1 .OR. J.GE.JE) THEN
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $            JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTV2 = NCOUNTV2 + 1
                  IPOSV(1,NCOUNTV2)=I
                  IPOSV(2,NCOUNTV2)=J
                  IPOSV(3,NCOUNTV2)=K
                  JPOSV(NCOUNTV2)=NN+1
               ENDIF
               NN=NN+1
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDDO
c      write(100+myrank,*) '3:nn=',nn
C
      NCOUNTW2 = 0
      NN = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
C
         DO K=KS,KE+1
         DO J=JS,JE
         DO I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTW2 = NCOUNTW2 + 1
                  IPOSW(1,NCOUNTW2)=I
                  IPOSW(2,NCOUNTW2)=J
                  IPOSW(3,NCOUNTW2)=K
                  JPOSW(NCOUNTW2)=NN+1
               ENDIF
               NN=NN+1
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDDO
c      write(100+myrank,*) '4:nn=',nn
C
      IF( NCOUNTF /= NCOUNTF2 .OR.
     $    NCOUNTU /= NCOUNTU2 .OR.
     $    NCOUNTV /= NCOUNTV2 .OR.
     $    NCOUNTW /= NCOUNTW2 ) THEN
         WRITE(*,*) 'ERROR AT MAKE_GGV_LIST'
         WRITE(*,*) '  NCOUNTF=',NCOUNTF,NCOUNTF2
         WRITE(*,*) '  NCOUNTU=',NCOUNTU,NCOUNTU2
         WRITE(*,*) '  NCOUNTV=',NCOUNTV,NCOUNTV2
         WRITE(*,*) '  NCOUNTW=',NCOUNTW,NCOUNTW2
         CALL VF_ZXMG_ABORT(IERR)
      ENDIF
C
      CALL VF_ZXMP_ALLSMI(NCOUNTF,NCOUNTFSUM,IERR)
      CALL VF_ZXMP_ALLSMI(NCOUNTU,NCOUNTUSUM,IERR)
      CALL VF_ZXMP_ALLSMI(NCOUNTV,NCOUNTVSUM,IERR)
      CALL VF_ZXMP_ALLSMI(NCOUNTW,NCOUNTWSUM,IERR)
C
c      write(100+myrank,*) 'ncountf,ncountfsum= ',ncountf,ncountfsum
c      write(100+myrank,*) 'ncountu,ncountusum= ',ncountu,ncountusum
c      write(100+myrank,*) 'ncountv,ncountvsum= ',ncountv,ncountvsum
c      write(100+myrank,*) 'ncountw,ncountwsum= ',ncountw,ncountwsum
C
      ALLOCATE(NCOUNTFALL(NPROCS),NDISPFALL(NPROCS),
     $         NCOUNTUALL(NPROCS),NDISPUALL(NPROCS),
     $         NCOUNTVALL(NPROCS),NDISPVALL(NPROCS),
     $         NCOUNTWALL(NPROCS),NDISPWALL(NPROCS),
     $         STAT=IERR)
C
      NCOUNTFALL(:)=0
      NCOUNTUALL(:)=0
      NCOUNTVALL(:)=0
      NCOUNTWALL(:)=0
      NDISPFALL(:)=0
      NDISPUALL(:)=0
      NDISPVALL(:)=0
      NDISPWALL(:)=0
C
      CALL VF_ZXMP_GATHERI(NCOUNTF,1,NCOUNTFALL,NPROCS,IERR)
      CALL VF_ZXMP_GATHERI(NCOUNTU,1,NCOUNTUALL,NPROCS,IERR)
      CALL VF_ZXMP_GATHERI(NCOUNTV,1,NCOUNTVALL,NPROCS,IERR)
      CALL VF_ZXMP_GATHERI(NCOUNTW,1,NCOUNTWALL,NPROCS,IERR)
C
      IF(MYRANK==0) THEN
c         write(100+myrank,*) 'ncountfall=',ncountfall
c         write(100+myrank,*) 'ncountuall=',ncountuall
c         write(100+myrank,*) 'ncountvall=',ncountvall
c         write(100+myrank,*) 'ncountwall=',ncountwall
c
         ALLOCATE(JPOSFALL(NCOUNTFSUM),
     $            JPOSUALL(NCOUNTUSUM),
     $            JPOSVALL(NCOUNTVSUM),
     $            JPOSWALL(NCOUNTWSUM),STAT=IERR)
c         write(100+myrank,*) '8:ierr=',ierr
C
         NDISPFALL(1)=0
         NDISPUALL(1)=0
         NDISPVALL(1)=0
         NDISPWALL(1)=0
         DO N=2,NPROCS
            NDISPFALL(N)=NDISPFALL(N-1)+NCOUNTFALL(N-1)
            NDISPUALL(N)=NDISPUALL(N-1)+NCOUNTUALL(N-1)
            NDISPVALL(N)=NDISPVALL(N-1)+NCOUNTVALL(N-1)
            NDISPWALL(N)=NDISPWALL(N-1)+NCOUNTWALL(N-1)
         ENDDO
C
c         write(100+myrank,*) 'ndispfall=',ndispfall
c         write(100+myrank,*) 'ndispuall=',ndispuall
c         write(100+myrank,*) 'ndispvall=',ndispvall
c         write(100+myrank,*) 'ndispwall=',ndispwall
      ELSE
         ALLOCATE(JPOSFALL(1),JPOSUALL(1),
     $            JPOSVALL(1),JPOSWALL(1),STAT=IERR)
      ENDIF
C
      JPOSFALL(:)=0
      JPOSUALL(:)=0
      JPOSVALL(:)=0
      JPOSWALL(:)=0
C
      CALL VF_ZXMP_GATHERVI(JPOSF,NCOUNTF,JPOSFALL,NCOUNTFSUM,
     $                      NCOUNTFALL,NDISPFALL,NPROCS,IERR)
      CALL VF_ZXMP_GATHERVI(JPOSU,NCOUNTU,JPOSUALL,NCOUNTUSUM,
     $                      NCOUNTUALL,NDISPUALL,NPROCS,IERR)
      CALL VF_ZXMP_GATHERVI(JPOSV,NCOUNTV,JPOSVALL,NCOUNTVSUM,
     $                      NCOUNTVALL,NDISPVALL,NPROCS,IERR)
      CALL VF_ZXMP_GATHERVI(JPOSW,NCOUNTW,JPOSWALL,NCOUNTWSUM,
     $                      NCOUNTWALL,NDISPWALL,NPROCS,IERR)
C
CDEBUG      write(100+myrank,*) 'jposf=',jposf
CDEBUG      write(100+myrank,*) 'jposu=',jposu
CDEBUG      write(100+myrank,*) 'jposv=',jposv
CDEBUG      write(100+myrank,*) 'jposw=',jposw
CDEBUG      write(100+myrank,*) 'jposfall=',jposfall
CDEBUG      write(100+myrank,*) 'jposuall=',jposuall
CDEBUG      write(100+myrank,*) 'jposvall=',jposvall
CDEBUG      write(100+myrank,*) 'jposwall=',jposwall
CDEBUG      call vf_zxmp_barri(ierr)
c
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE DELETE_GGV_LIST(MYRANK,NPROCS)
C----------------------------------------
C     親から子への地形データの通信に用いるリストを削除する
C----------------------------------------
      INTEGER,INTENT(IN):: MYRANK,NPROCS
C
      INTEGER:: IERR
C
C
c      write(100+myrank,*) 'start:delete_ggv_list'
      NCOUNTF=0
      NCOUNTU=0
      NCOUNTV=0
      NCOUNTW=0
      NCOUNTFSUM=0
      NCOUNTUSUM=0
      NCOUNTVSUM=0
      NCOUNTWSUM=0
      DEALLOCATE(IPOSF,IPOSU,
     $           IPOSV,IPOSW,
     $           JPOSF,JPOSU,
     $           JPOSV,JPOSW,STAT=IERR)
      DEALLOCATE(NCOUNTFALL,NDISPFALL,
     $           NCOUNTUALL,NDISPUALL,
     $           NCOUNTVALL,NDISPVALL,
     $           NCOUNTWALL,NDISPWALL,
     $           STAT=IERR)
      DEALLOCATE(JPOSFALL,JPOSUALL,
     $           JPOSVALL,JPOSWALL,STAT=IERR)
C
c      write(100+myrank,*) 'end:delete_ggv_list'
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE SET_GGV_F(GWRK,NGWRK,GGV,NF,NUMI,NUMJ,NUMK,
     $                     MYRANK,NPROCS,MYGIS,MYGJS)
C----------------------------------------
C     親から子への送信するための送信バッファに地形データを集める1
C----------------------------------------
      DOUBLE PRECISION,INTENT(OUT):: GWRK(NGWRK)
      DOUBLE PRECISION,INTENT(IN) ::
     $   GGV(MYGIS:MYGIS+NUMI-1,MYGJS:MYGJS+NUMJ-1,NUMK)
      INTEGER,INTENT(IN)          ::
     $   NF(MYGIS:MYGIS+NUMI-1,MYGJS:MYGJS+NUMJ-1,NUMK)
      INTEGER,INTENT(IN)          :: NGWRK
      INTEGER,INTENT(IN)          :: NUMI,NUMJ,NUMK
      INTEGER,INTENT(IN)          :: MYRANK,NPROCS
      INTEGER,INTENT(IN)          :: MYGIS,MYGJS
C
      DOUBLE PRECISION,ALLOCATABLE :: GWRK2(:)
      DOUBLE PRECISION,ALLOCATABLE :: GWRKLOCAL(:)
      INTEGER:: I,J,K,M,N,IERR
C
C
      if(myrank==0.and.ngwrk.ne.ncountfsum) then
         write(*,*) 'Error: ngwrk is not eqaul to ncountfsum'
         write(*,*) '     : ngwrk      =',ngwrk
         write(*,*) '     : ncountfsum =',ncountfsum
         call vf_zxmg_abort(ierr)
      endif
C
      ALLOCATE(GWRK2(NGWRK),GWRKLOCAL(max(NCOUNTF,1)),STAT=IERR)
C
      DO N=1,NCOUNTF
         I=IPOSF(1,N)
         J=IPOSF(2,N)
         K=IPOSF(3,N)
         IF(MOD(N,2).EQ.1) GWRKLOCAL(N)=GGV(I,J,K)
         IF(MOD(N,2).EQ.0) GWRKLOCAL(N)=DBLE(NF(I,J,K))
      ENDDO
C
c      write(100+myrank,*) 'ncountf,ncountfsum=',ncountf,ncountfsum
      GWRK2(:)=HUGE(1)
      CALL VF_ZXMP_GATHERVD(GWRKLOCAL,NCOUNTF,GWRK2,NCOUNTFSUM,
     $                      NCOUNTFALL,NDISPFALL,NPROCS,IERR)
C
      IF( MYRANK==0 ) THEN
         GWRK(:)=HUGE(1)
         DO N=1,NCOUNTFSUM
            M=JPOSFALL(N)
            GWRK(M)=GWRK2(N)
         ENDDO
      ENDIF
C
      DEALLOCATE(GWRK2,GWRKLOCAL)
C
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE SET_GGV_UVW(GWRK,NGWRK,GGXYZ,NUMI,NUMJ,NUMK,
     $                       IFLAG,MYRANK,NPROCS,MYGIS,MYGJS)
C----------------------------------------
C     親から子への送信するための送信バッファに地形データを集める1
C----------------------------------------
      DOUBLE PRECISION,INTENT(OUT):: GWRK(NGWRK)
      DOUBLE PRECISION,INTENT(IN) ::
     $   GGXYZ(MYGIS:MYGIS+NUMI-1,MYGJS:MYGJS+NUMJ-1,NUMK)
      INTEGER,INTENT(IN)          :: NGWRK
      INTEGER,INTENT(IN)          :: NUMI,NUMJ,NUMK
      INTEGER,INTENT(IN)          :: IFLAG,MYRANK,NPROCS
      INTEGER,INTENT(IN)          :: MYGIS,MYGJS
C
      DOUBLE PRECISION,ALLOCATABLE :: GWRK2(:)
      DOUBLE PRECISION,ALLOCATABLE :: GWRKLOCAL(:)
      INTEGER:: nc,ncsum
      INTEGER:: I,J,K,M,N,IERR
C
C
      IF(IFLAG==1) NC=NCOUNTU
      IF(IFLAG==2) NC=NCOUNTV
      IF(IFLAG==3) NC=NCOUNTW
C
      IF(IFLAG==1) NCSUM=NCOUNTUSUM
      IF(IFLAG==2) NCSUM=NCOUNTVSUM
      IF(IFLAG==3) NCSUM=NCOUNTWSUM
C
      if(myrank==0.and.ngwrk.ne.ncsum) then
         write(*,*) 'Error: ngwrk is not eqaul to nc'
         write(*,*) '     : iflag=',iflag
         call vf_zxmg_abort(ierr)
      endif
C
      ALLOCATE(GWRK2(NGWRK),GWRKLOCAL(NC),STAT=IERR)
C
c      write(100+myrank,*) 'nc=',nc
c      write(100+myrank,*) 'is,ie=',MYGIS,MYGIS+NUMI-1
c      write(100+myrank,*) 'js,je=',MYGJS,MYGJS+NUMJ-1
      DO N=1,NC
         IF(IFLAG==1) THEN
            I=IPOSU(1,N)
            J=IPOSU(2,N)
            K=IPOSU(3,N)
         ELSEIF(IFLAG==2) THEN
            I=IPOSV(1,N)
            J=IPOSV(2,N)
            K=IPOSV(3,N)
         ELSE
            I=IPOSW(1,N)
            J=IPOSW(2,N)
            K=IPOSW(3,N)
         ENDIF
         GWRKLOCAL(N)=GGXYZ(I,J,K)
      ENDDO
C
      GWRK2(:)=HUGE(1)
      IF(IFLAG==1) CALL VF_ZXMP_GATHERVD(GWRKLOCAL,NC,GWRK2,NCSUM,
     $                           NCOUNTUALL,NDISPUALL,NPROCS,IERR)
      IF(IFLAG==2) CALL VF_ZXMP_GATHERVD(GWRKLOCAL,NC,GWRK2,NCSUM,
     $                           NCOUNTVALL,NDISPVALL,NPROCS,IERR)
      IF(IFLAG==3) CALL VF_ZXMP_GATHERVD(GWRKLOCAL,NC,GWRK2,NCSUM,
     $                           NCOUNTWALL,NDISPWALL,NPROCS,IERR)
C
      IF( MYRANK==0 ) THEN
         GWRK(:)=HUGE(1)
         DO N=1,NCSUM
            IF(IFLAG==1) M=JPOSUALL(N)
            IF(IFLAG==2) M=JPOSVALL(N)
            IF(IFLAG==3) M=JPOSWALL(N)
            GWRK(M)=GWRK2(N)
         ENDDO
      ENDIF
C
      DEALLOCATE(GWRK2,GWRKLOCAL)
C
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE MAKE_P2C_LIST(MGCPOS,MGCINF,MGCNUM,MYRANK,NPROCS,
     $                         ISG,JSG,IEG,JEG,MGNV)
C----------------------------------------
C     親から子への地形データの通信に用いるリストを作成する
C----------------------------------------
      INTEGER,INTENT(IN):: MGCPOS(6,MGCNUM),MGCINF(9,MGCNUM),MGCNUM
      INTEGER,INTENT(IN):: MYRANK,NPROCS
      INTEGER,INTENT(IN):: ISG,IEG,JSG,JEG
      INTEGER,INTENT(IN):: MGNV
C
      INTEGER:: I,J,K,M,N,NN,IERR
      INTEGER:: IS,IE,JS,JE,KS,KE
      INTEGER:: NCOUNTS2
C
C
      NCOUNTS = 0
C
c      write(100+myrank,*) 'make_p2c: isg,ieg=',isg,ieg
c      write(100+myrank,*) 'make_p2c: jsg,jeg=',jsg,jeg
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
c      write(100+myrank,*) 'make_p2c: n=',n
c      write(100+myrank,*) 'make_p2c: is,ie=',is,ie
c      write(100+myrank,*) 'make_p2c: js,je=',js,je
C
         IF(MGCINF(4,N).EQ.0) THEN
            I=IS
            DO K=KS,KE
            DO J=JS,JE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTS = NCOUNTS + MGNV
               ENDIF
            ENDDO
            ENDDO
         ENDIF
         IF(MGCINF(7,N).EQ.0) THEN
            I=IE+1
            DO K=KS,KE
            DO J=JS,JE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTS = NCOUNTS + MGNV
               ENDIF
            ENDDO
            ENDDO
         ENDIF
         IF(MGCINF(5,N).EQ.0) THEN
            J=JS
            DO K=KS,KE
            DO I=IS,IE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTS = NCOUNTS + MGNV
               ENDIF
            ENDDO
            ENDDO
         ENDIF
         IF (MGCINF(8,N).EQ.0) THEN
            J=JE+1
            DO K=KS,KE
            DO I=IS,IE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTS = NCOUNTS + MGNV
               ENDIF
            ENDDO
            ENDDO
         ENDIF
      ENDDO
C
ccccc      ALLOCATE(IPOSS(3,max(NCOUNTS,1)),JPOSS(max(NCOUNTS,1)),STAT=IERR)
      ALLOCATE(NSEND(MGCNUM),STAT=IERR)
      ALLOCATE(JPOSS(max(NCOUNTS,1)),STAT=IERR)
C
      NCOUNTS2 = 0
      NN = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
C
         IF(MGCINF(4,N).EQ.0) THEN
            I=IS
            DO K=KS,KE
            DO J=JS,JE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  DO M=1,MGNV
                     JPOSS(NCOUNTS2+M)=NN+M
                  ENDDO
                  NCOUNTS2 = NCOUNTS2 + MGNV
               ENDIF
               NN=NN+MGNV
            ENDDO
            ENDDO
         ENDIF
         IF(MGCINF(7,N).EQ.0) THEN
            I=IE+1
            DO K=KS,KE
            DO J=JS,JE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  DO M=1,MGNV
                     JPOSS(NCOUNTS2+M)=NN+M
                  ENDDO
                  NCOUNTS2 = NCOUNTS2 + MGNV
               ENDIF
               NN=NN+MGNV
            ENDDO
            ENDDO
         ENDIF
         IF(MGCINF(5,N).EQ.0) THEN
            J=JS
            DO K=KS,KE
            DO I=IS,IE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  DO M=1,MGNV
                     JPOSS(NCOUNTS2+M)=NN+M
                  ENDDO
                  NCOUNTS2 = NCOUNTS2 + MGNV
               ENDIF
               NN=NN+MGNV
            ENDDO
            ENDDO
         ENDIF
         IF (MGCINF(8,N).EQ.0) THEN
            J=JE+1
            DO K=KS,KE
            DO I=IS,IE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  DO M=1,MGNV
                     JPOSS(NCOUNTS2+M)=NN+M
                  ENDDO
                  NCOUNTS2 = NCOUNTS2 + MGNV
               ENDIF
               NN=NN+MGNV
            ENDDO
            ENDDO
         ENDIF
C
         NSEND(N)=NN
      ENDDO
C
      DO N=MGCNUM,2,-1
         NSEND(N)=NSEND(N)-NSEND(N-1)
      ENDDO
C
      IF( NCOUNTS /= NCOUNTS2 ) THEN
         WRITE(*,*) 'ERROR AT MAKE_P2C_LIST'
         WRITE(*,*) '  NCOUNTS=',NCOUNTS,NCOUNTS2
         CALL VF_ZXMG_ABORT(IERR)
      ENDIF
C
      CALL VF_ZXMP_ALLSMI(NCOUNTS,NCOUNTSSUM,IERR)
C
      ALLOCATE(NCOUNTSALL(NPROCS),NDISPSALL(NPROCS),STAT=IERR)
C
      NCOUNTSALL(:)=0
      NDISPSALL(:)=0
C
      CALL VF_ZXMP_GATHERI(NCOUNTS,1,NCOUNTSALL,NPROCS,IERR)
C
      IF(MYRANK==0) THEN
         ALLOCATE(JPOSSALL(NCOUNTSSUM),STAT=IERR)
C
         NDISPSALL(1)=0
         DO N=2,NPROCS
            NDISPSALL(N)=NDISPSALL(N-1)+NCOUNTSALL(N-1)
         ENDDO
      ELSE
         ALLOCATE(JPOSSALL(1),STAT=IERR)
      ENDIF
C
      JPOSSALL(:)=0
C
      CALL VF_ZXMP_GATHERVI(JPOSS,NCOUNTS,JPOSSALL,NCOUNTSSUM,
     $                      NCOUNTSALL,NDISPSALL,NPROCS,IERR)
C
c      write(100+myrank,*) 'ncounts=',ncounts
c      write(100+myrank,*) 'ncountssum=',ncountssum
C
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE DELETE_P2C_LIST(MYRANK,NPROCS)
C----------------------------------------
C     境界面の流量の通信に用いるリストを削除する
C----------------------------------------
      INTEGER,INTENT(IN):: MYRANK,NPROCS
C
      INTEGER:: IERR
C
C
      NCOUNTS=0
      NCOUNTSSUM=0
      DEALLOCATE(JPOSS,STAT=IERR)
      DEALLOCATE(NCOUNTSALL,NDISPSALL,STAT=IERR)
      DEALLOCATE(JPOSSALL,STAT=IERR)
C
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE SET_P2C(GWRK,GWRKLOCAL,MYRANK,NPROCS)
C----------------------------------------
C     親から子への送信するための送信バッファに地形データを集める
C----------------------------------------
      DOUBLE PRECISION,INTENT(OUT):: GWRK(NCOUNTSSUM)
      DOUBLE PRECISION,INTENT(IN) :: GWRKLOCAL(NCOUNTS)
      INTEGER,INTENT(IN)          :: MYRANK,NPROCS
C
      INTEGER:: M,N,IERR
C
C
      CALL VF_ZXMP_GATHERVD(GWRKLOCAL,NCOUNTS,RBUFWRK,NCOUNTSSUM,
     $                      NCOUNTSALL,NDISPSALL,NPROCS,IERR)
C
      IF( MYRANK==0 ) THEN
         GWRK(:)=HUGE(1)
         DO N=1,NCOUNTSSUM
            M=JPOSSALL(N)
            GWRK(M)=RBUFWRK(N)
         ENDDO
      ENDIF
C
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE MAKE_C2P_LIST(MGCPOS,MGCINF,MGCNUM,MYRANK,NPROCS,
     $                         ISG,JSG,IEG,JEG,MGNV)
C----------------------------------------
C     親から子への地形データの通信に用いるリストを作成する
C----------------------------------------
      INTEGER,INTENT(IN):: MGCPOS(6,MGCNUM),MGCINF(9,MGCNUM),MGCNUM
      INTEGER,INTENT(IN):: MYRANK,NPROCS
      INTEGER,INTENT(IN):: ISG,IEG,JSG,JEG
      INTEGER,INTENT(IN):: MGNV
C
      INTEGER:: I,J,K,M,N,NN,IERR
      INTEGER:: IS,IE,JS,JE,KS,KE
      INTEGER:: NCOUNTR2
C
C
      NCOUNTR = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
         IF(MGCINF(4,N).EQ.0) IS=IS+1
         IF(MGCINF(5,N).EQ.0) JS=JS+1
         IF(MGCINF(7,N).EQ.0) IE=IE-1
         IF(MGCINF(8,N).EQ.0) JE=JE-1
C
         IF(MGCINF(4,N).EQ.0) THEN
            I=IS
            DO K=KS,KE
            DO J=JS,JE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTR = NCOUNTR + MGNV
               ENDIF
            ENDDO
            ENDDO
         ENDIF
         IF(MGCINF(7,N).EQ.0) THEN
            I=IE+1
            DO K=KS,KE
            DO J=JS,JE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTR = NCOUNTR + MGNV
               ENDIF
            ENDDO
            ENDDO
         ENDIF
         IF(MGCINF(5,N).EQ.0) THEN
            J=JS
            DO K=KS,KE
            DO I=IS,IE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTR = NCOUNTR + MGNV
               ENDIF
            ENDDO
            ENDDO
         ENDIF
         IF (MGCINF(8,N).EQ.0) THEN
            J=JE+1
            DO K=KS,KE
            DO I=IS,IE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  NCOUNTR = NCOUNTR + MGNV
               ENDIF
            ENDDO
            ENDDO
         ENDIF
      ENDDO
C
      ALLOCATE(NRECV(MGCNUM),STAT=IERR)
      ALLOCATE(JPOSR(max(NCOUNTR,1)),STAT=IERR)
C
      NCOUNTR2 = 0
      NN = 0
C
      DO N=1,MGCNUM
         IS=MGCPOS(1,N)
         JS=MGCPOS(2,N)
         KS=MGCPOS(3,N)
         IE=MGCPOS(4,N)
         JE=MGCPOS(5,N)
         KE=MGCPOS(6,N)
         IF(MGCINF(4,N).EQ.0) IS=IS+1
         IF(MGCINF(5,N).EQ.0) JS=JS+1
         IF(MGCINF(7,N).EQ.0) IE=IE-1
         IF(MGCINF(8,N).EQ.0) JE=JE-1
C
         IF(MGCINF(4,N).EQ.0) THEN
            I=IS
            DO K=KS,KE
            DO J=JS,JE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  DO M=1,MGNV
                     JPOSR(NCOUNTR2+M)=NN+M
                  ENDDO
                  NCOUNTR2 = NCOUNTR2 + MGNV
               ENDIF
               NN=NN+MGNV
            ENDDO
            ENDDO
         ENDIF
         IF(MGCINF(7,N).EQ.0) THEN
            I=IE+1
            DO K=KS,KE
            DO J=JS,JE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  DO M=1,MGNV
                     JPOSR(NCOUNTR2+M)=NN+M
                  ENDDO
                  NCOUNTR2 = NCOUNTR2 + MGNV
               ENDIF
               NN=NN+MGNV
            ENDDO
            ENDDO
         ENDIF
         IF(MGCINF(5,N).EQ.0) THEN
            J=JS
            DO K=KS,KE
            DO I=IS,IE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  DO M=1,MGNV
                     JPOSR(NCOUNTR2+M)=NN+M
                  ENDDO
                  NCOUNTR2 = NCOUNTR2 + MGNV
               ENDIF
               NN=NN+MGNV
            ENDDO
            ENDDO
         ENDIF
         IF (MGCINF(8,N).EQ.0) THEN
            J=JE+1
            DO K=KS,KE
            DO I=IS,IE
               IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $             JSG.LE.J.AND.J.LE.JEG ) THEN
                  DO M=1,MGNV
                     JPOSR(NCOUNTR2+M)=NN+M
                  ENDDO
                  NCOUNTR2 = NCOUNTR2 + MGNV
               ENDIF
               NN=NN+MGNV
            ENDDO
            ENDDO
         ENDIF
C
         NRECV(N)=NN
      ENDDO
C
      DO N=MGCNUM,2,-1
         NRECV(N)=NRECV(N)-NRECV(N-1)
      ENDDO
C
      IF( NCOUNTR /= NCOUNTR2 ) THEN
         WRITE(*,*) 'ERROR AT MAKE_C2P_LIST'
         WRITE(*,*) '  NCOUNTR=',NCOUNTR,NCOUNTR2
         CALL VF_ZXMG_ABORT(IERR)
      ENDIF
C
      CALL VF_ZXMP_ALLSMI(NCOUNTR,NCOUNTRSUM,IERR)
C
      ALLOCATE(NCOUNTRALL(NPROCS),NDISPRALL(NPROCS),STAT=IERR)
C
      NCOUNTRALL(:)=0
      NDISPRALL(:)=0
C
      CALL VF_ZXMP_GATHERI(NCOUNTR,1,NCOUNTRALL,NPROCS,IERR)
C
      IF(MYRANK==0) THEN
         ALLOCATE(JPOSRALL(NCOUNTRSUM),STAT=IERR)
C
         NDISPRALL(1)=0
         DO N=2,NPROCS
            NDISPRALL(N)=NDISPRALL(N-1)+NCOUNTRALL(N-1)
         ENDDO
      ELSE
         ALLOCATE(JPOSRALL(1),STAT=IERR)
      ENDIF
C
      JPOSRALL(:)=0
C
      CALL VF_ZXMP_GATHERVI(JPOSR,NCOUNTR,JPOSRALL,NCOUNTRSUM,
     $                      NCOUNTRALL,NDISPRALL,NPROCS,IERR)
C
c      write(100+myrank,*) 'ncountr=',ncountr
c      write(100+myrank,*) 'ncountrsum=',ncountrsum
C
      ALLOCATE(RBUF(MAX(NCOUNTSSUM,NCOUNTRSUM)),
     $         RBUFWRK(MAX(NCOUNTSSUM,NCOUNTRSUM)),
     $         STAT=IERR)
C
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE DELETE_C2P_LIST(MYRANK,NPROCS)
C----------------------------------------
C     境界面の流量の通信に用いるリストを削除する
C----------------------------------------
      INTEGER,INTENT(IN):: MYRANK,NPROCS
C
      INTEGER:: IERR
C
C
      NCOUNTR=0
      NCOUNTRSUM=0
      DEALLOCATE(JPOSR,STAT=IERR)
      DEALLOCATE(NCOUNTRALL,NDISPRALL,STAT=IERR)
      DEALLOCATE(JPOSRALL,STAT=IERR)
C
      RETURN
      END SUBROUTINE
C
C
      SUBROUTINE SET_C2P(GWRK,GWRKLOCAL,MYRANK,NPROCS)
C----------------------------------------
C     親が子から受信した受信バッファのデータを配る
C----------------------------------------
      DOUBLE PRECISION,INTENT(IN) :: GWRK(NCOUNTRSUM)
      DOUBLE PRECISION,INTENT(OUT):: GWRKLOCAL(NCOUNTR)
      INTEGER,INTENT(IN)          :: MYRANK,NPROCS
C
      INTEGER:: M,N,IERR
C
C
      IF( MYRANK==0 ) THEN
         RBUFWRK(:)=HUGE(1)
         DO N=1,NCOUNTRSUM
            M=JPOSRALL(N)
            RBUFWRK(N)=GWRK(M)
         ENDDO
      ENDIF
C
      CALL VF_ZXMP_SCATTERVD(RBUFWRK,NCOUNTRSUM,NCOUNTRALL,NDISPRALL,
     $                       NPROCS,GWRKLOCAL,NCOUNTR,IERR)
C
      RETURN
      END SUBROUTINE
C
C
      END MODULE MOD_APARA
