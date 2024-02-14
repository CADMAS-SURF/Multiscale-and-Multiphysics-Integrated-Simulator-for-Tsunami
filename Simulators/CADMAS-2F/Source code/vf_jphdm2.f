      SUBROUTINE VF_JPHDM2(ISW,DT,XX,YY,ZZ,UU,VV,WW,PP,PPBK,xxx,
     &                     GGVOLD,GGVNOW,GGVELO,GGVELN,
     &                     NF,INDX,INDY,INDZ,INDB,
     &                     WK01,WK02,WK03,WK04,WK05,
     &                     WK06,WK07,WK08,WK09,WK10,
     &                     WK11,WK12,WK13,WK14,WK15,
     &                     WK16,WK17,DBUF,RBUF,INDC2)

CD=== 概要 ===========================================================

CDT   VF_JPHDM2:HiDEMとのインターフェイス(2):圧力と体積多孔率他の交換

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
      INCLUDE 'VF_ATIMER.h'
      INCLUDE  'mpif.h'

CD    -- 引数 --
CD    ISW                 : IN  : I*4 : 動作フラグ
CD                        :     :     : =0 圧力の計算をしない(初期値)
CD                        :     :     : =1 圧力の計算をする
CD    DT                  : IN  : R*8 : 時間刻幅
CD    XX(MAXG1,NUMI)      : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)      : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)      : IN  : R*8 : z方向格子座標等
CD    PP(@FOR-3D@)        : IN  : R*8 : 圧力
CD    xxx(@FOR-3D@)      : --- : R*8 : HiDEMへ送る圧力
CD    GGVOLD(IPRNP)       : I/O : R*8 : 前の時刻ブロックの空隙率
CD    GGVNOW(IPRNP)       : I/O : R*8 : 現在の時刻ブロックの空隙率
CD    GGVELO(3,IPRNB)     : I/O : R*8 : 前の時刻ブロックの障害物移動速度
CD    GGVELN(3,IPRNB)     : I/O : R*8 : 現在の時刻ブロックの障害物移動速度
CD    NF(@FOR-3D@)        : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)      : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)      : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)      : IN  : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB)    : IN  : I*4 : 境界面のインデックス
CD    WK01-17(@FOR-3D@)   : --- : R*8 : ワーク配列
CD    DBUF(NUMBUF*MAXBUF) : --- : R*8 : 並列用のバッファ
CD    INDC2(@FOR-3D@)     : --- : I*4 : セルの計算状態を示すインデックス(HiDEMとの連成用)
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU(NUMI,NUMJ,NUMK),VV(NUMI,NUMJ,NUMK)
      DIMENSION WW(NUMI,NUMJ,NUMK)
      DIMENSION PP(NUMI,NUMJ,NUMK),xxx(NUMI*NUMJ*NUMK)
      DIMENSION PPBK(NUMI,NUMJ,NUMK)
      DIMENSION GGVOLD(IPRNP),GGVNOW(IPRNP)
      DIMENSION GGVELO(3,IPRNP),GGVELN(3,IPRNP)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)    
      DIMENSION WK01(NUMI,NUMJ,NUMK),WK02(NUMI,NUMJ,NUMK)
      DIMENSION WK03(NUMI,NUMJ,NUMK),WK04(NUMI,NUMJ,NUMK)
      DIMENSION WK05(NUMI,NUMJ,NUMK),WK06(NUMI,NUMJ,NUMK)
      DIMENSION WK07(NUMI,NUMJ,NUMK),WK08(NUMI,NUMJ,NUMK)
      DIMENSION WK09(NUMI,NUMJ,NUMK),WK10(NUMI,NUMJ,NUMK)
      DIMENSION WK11(NUMI,NUMJ,NUMK),WK12(NUMI,NUMJ,NUMK)
      DIMENSION WK13(NUMI,NUMJ,NUMK),WK14(NUMI,NUMJ,NUMK)
      DIMENSION WK15(NUMI,NUMJ,NUMK),WK16(NUMI,NUMJ,NUMK)
      DIMENSION WK17(NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF),RBUF(IPRNP)
      DIMENSION INDC2(NUMI,NUMJ,NUMK)

      real(8),allocatable :: buff(:,:,:,:)

CD    -- 主要制御変数 --
CD    IPRNT : I*4 : 空隙率の時間方向のデータ数
CD                  =0:空隙率ファイルを読み込まない
CD                  =1:時間依存データでは無い
CD                  >1:時間依存データ
CD    IPRNB : I*4 : 時間依存型空隙率の空間ブロックの最大数
CD    IPRNP : I*4 : 時間依存型空隙率の設定セル数

      DIMENSION DUMMY(1000)
      DIMENSION ISTT(MPI_STATUS_SIZE)

C==== 実行 ===========================================================

CD    -- エリアのスワップ --
      IF (ISW.EQ.1) THEN
        GGVOLD(  :) = GGVNOW(  :)
        GGVELO(:,:) = GGVELN(:,:)
      ENDIF

CD    -- HiDEM側に時間刻幅を送る --
      IF (MYRANK.EQ.0) THEN
        CALL MPI_SEND(DT,1,MPI_DOUBLE_PRECISION,IHIDM,0,
     &                comm_2fc_dem,IERR)
      END IF

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

      IF (ISW.EQ.0) THEN

        do lpe=0,nsize_dem-1
          nx = ncadm_sub(1,lpe)
          ny = ncadm_sub(2,lpe)
          nz = ncadm_sub(3,lpe)
          if(nx*ny*nz == 0) cycle

          is = jcadm_sub(1,lpe)
          js = jcadm_sub(2,lpe)
          ks = jcadm_sub(3,lpe)
          ie = jcadm_sub(4,lpe)
          je = jcadm_sub(5,lpe)
          ke = jcadm_sub(6,lpe)

          allocate(buff(1,is:ie,js:je,ks:ke))

CD    -- HiDEM側に送る連成解析計算領域の圧力を求める --
          DO 22 K=ks,ke
            DO 21 J=js,je
              DO 20 I=is,ie
                buff(1,I,J,K)=(PP(I,J,K)+PPBK(I,J,K))*0.5D0
 20           CONTINUE
 21         CONTINUE
 22       CONTINUE

CD    -- HiDEM側に連成解析計算領域の圧力を送る --
          call mpi_send(buff,nx*ny*nz,MPI_REAL8
     &                 ,nrank_dem(lpe),myrwld,comm_2fc_dem,ierr)

          deallocate(buff)
        end do

      ELSE

C@        WRITE(21,*) '@@@@@@@@@@@@@@ PP @@@@@@@@@@@@@@@@@@@@'
C@        CALL VF_OL3DR(PP,0)
        CALL VF_JPCOEF(XX,YY,ZZ,PP,
     &                 WK01,WK02,WK03,WK04,WK05,WK06,WK07,WK08,WK09,
     &                 NF,INDX,INDY,INDZ,INDC2)
        CALL VF_VPSOL (1,DBUF,
     &                 WK01,WK02,WK03,WK04,WK05,WK06,WK07,WK08,WK09,
     &                 WK10,WK11,WK12,WK13,WK14,WK15,WK16,WK17,INDC2)
C@        WRITE(21,*) '@@@@@@@@@@@@@@ WK @@@@@@@@@@@@@@@@@@@@'
C@        CALL VF_OL3DR(WK09,0)

        do lpe=0,nsize_dem-1
          nx = ncadm_sub(1,lpe)
          ny = ncadm_sub(2,lpe)
          nz = ncadm_sub(3,lpe)
          if(nx*ny*nz == 0) cycle

          is = jcadm_sub(1,lpe)
          js = jcadm_sub(2,lpe)
          ks = jcadm_sub(3,lpe)
          ie = jcadm_sub(4,lpe)
          je = jcadm_sub(5,lpe)
          ke = jcadm_sub(6,lpe)

          allocate(buff(1,is:ie,js:je,ks:ke))

CD    -- HiDEM側に送る連成解析計算領域の圧力を求める --
          DO 32 K=ks,ke
            DO 31 J=js,je
              DO 30 I=is,ie
                PP(I,J,K)=WK09(I,J,K)
                buff(1,I,J,K)=(PP(I,J,K)+PPBK(I,J,K))*0.5D0
 30           CONTINUE
 31         CONTINUE
 32       CONTINUE

CD    -- HiDEM側に連成解析計算領域の圧力を送る --
          call mpi_send(buff,nx*ny*nz,MPI_REAL8
     &                 ,nrank_dem(lpe),myrwld,comm_2fc_dem,ierr)

          deallocate(buff)
        end do

      ENDIF

CD    -- HiDEM側より体積多孔率,群速度3成分をもらう --
      GGVNOW(  :) = 0.0D0
      GGVELN(:,:) = 0.0D0

      IP = MYGIS - 1
      JP = MYGJS - 1

      do lpe=0,nsize_dem-1
        nx = ncadm_sub(1,lpe)
        ny = ncadm_sub(2,lpe)
        nz = ncadm_sub(3,lpe)
        if(nx*ny*nz == 0) cycle

        is = jcadm_sub(1,lpe)
        js = jcadm_sub(2,lpe)
        ks = jcadm_sub(3,lpe)
        ie = jcadm_sub(4,lpe)
        je = jcadm_sub(5,lpe)
        ke = jcadm_sub(6,lpe)

        allocate(buff(4,is:ie,js:je,ks:ke))

        CALL MPI_RECV(buff,4*nx*ny*nz,MPI_REAL8
     &               ,nrank_dem(lpe),myrwld
     &               ,comm_2fc_dem,ISTT,IERR)

        DO K=ks,ke
!         LK = (K - jcadm(3))*ncadm(1)*ncadm(2)
          LK = (K - iprara(3,1))*ncadm(1)*ncadm(2)
          DO J=js,je
!           LJK = (J - jcadm(2))*ncadm(1) + LK 
            LJK = (J + JP - iprara(2,1))*ncadm(1) + LK 
            DO I=is,ie
!             LIJK = 1 + (I - jcadm(1)) + LJK
              LIJK = 1 + (I + IP - iprara(1,1)) + LJK
              GGVNOW(    LIJK) = GGVNOW(    LIJK) + buff(1  ,I,J,K)
              GGVELN(1:3,LIJK) = GGVELN(1:3,LIJK) + buff(2:4,I,J,K)
            END DO
          END DO
        END DO

        deallocate(buff)
      end do

      do k=jcadm(3),jcadm(6)
        LK = (K - iprara(3,1))*ncadm(1)*ncadm(2)
        do j=jcadm(2),jcadm(5)
          LJK = (J + JP - iprara(2,1))*ncadm(1) + LK 
          do i=jcadm(1),jcadm(4)
            LIJK = 1 + (I + IP - iprara(1,1)) + LJK
            VOL = GGVNOW(LIJK)
            if(VOL>0.0D0) THEN
              VOLCEL = (XX(1,I+1) - XX(1,I))
     &                *(YY(1,J+1) - YY(1,J))
     &                *(ZZ(1,K+1) - ZZ(1,K))
              GGVNOW(    LIJK) = min(VOL/VOLCEL,1.0D0)
              GGVELN(1:3,LIJK) = GGVELN(1:3,LIJK)/VOL
            END IF
          end do
        end do
      end do

C@@@@
      V1= 1.0D+30
      V2=-1.0D+30
C@@@@
      DO 200 L=1,IPRNP
C@      GGVNOW(L)=MAX(1.0D0-GGVNOW(L),@@@@@@@@@@)
C@      GGVNOW(L)=MIN(1.0D0,MAX(1.0D0-GGVNOW(L),0.09D0))
        GGVNOW(L)=MIN(1.0D0,MAX(1.0D0-GGVNOW(L),0.001D0))
C@@@@
        IF (V1.GT.GGVNOW(L)) V1=GGVNOW(L)
        IF (V2.LT.GGVNOW(L)) V2=GGVNOW(L)
C@@@@
 200  CONTINUE
C@@@@
C@    WRITE(*,*) 'CADM GGV',V1,V2
C@@@@

C@@@@
      V1= 1.0D+30
      V2=-1.0D+30
C@@@@
      DO 210 I=1,IPRNP
C@@@@
        IF (V1.GT.RBUF(I)) V1=RBUF(I)
        IF (V2.LT.RBUF(I)) V2=RBUF(I)
C@@@@
 210  CONTINUE
C@@@@
C@      WRITE(*,*) 'CADM UU',V1,V2
C@@@@

C@@@@
      V1= 1.0D+30
      V2=-1.0D+30
C@@@@
      DO 220 I=1,IPRNP
C@@@@
        IF (V1.GT.RBUF(I)) V1=RBUF(I)
        IF (V2.LT.RBUF(I)) V2=RBUF(I)
C@@@@
 220  CONTINUE
C@@@@
C@      WRITE(*,*) 'CADM VV',V1,V2
C@@@@

C@@@@
      V1= 1.0D+30
      V2=-1.0D+30
C@@@@
      DO 230 I=1,IPRNP
C@@@@
        IF (V1.GT.RBUF(I)) V1=RBUF(I)
        IF (V2.LT.RBUF(I)) V2=RBUF(I)
C@@@@
 230  CONTINUE
C@@@@
C@      WRITE(*,*) 'CADM WW',V1,V2
C@@@@

C@      IF (MYRWLD.EQ.ICADM) THEN
C@        L=0
C@        DO 323 IB=1,IPRNB
C@          DO 322 K=IPRARA(3,IB),IPRARA(6,IB)
C@            DO 321 J=IPRARA(2,IB),IPRARA(5,IB)
C@              DO 320 I=IPRARA(1,IB),IPRARA(4,IB)
C@                L=L+1
C@                IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
C@                  IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
C@                    PP(I-IP,J-JP,K)=PHDM(L)
C@                    UU(I-IP  ,J-JP,K)=GGVELN(1,L)
C@                    UU(I-IP+1,J-JP,K)=GGVELN(1,L)
C@                    VV(I-IP,J-JP  ,K)=GGVELN(2,L)
C@                    VV(I-IP,J-JP+1,K)=GGVELN(2,L)
C@                    WW(I-IP,J-JP,K  )=GGVELN(3,L)
C@                    WW(I-IP,J-JP,K+1)=GGVELN(3,L)
C@                  ENDIF
C@                ENDIF
C@ 320          CONTINUE
C@ 321        CONTINUE
C@ 322      CONTINUE
C@ 323    CONTINUE
C@      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
C     WRITE(IDBGF,'(A)') '(JPHDM2) BOTTOM.'
      RETURN
      END
