      SUBROUTINE VF_CGGVH(ISW,W,DTOLD,GGV,GGV0,GGVOLD,GGVNOW,
     &                    GGVEL,GGVELO,GGVELN,NF)

CD=== 概要 ===========================================================

CDT   VF_CGGVH:時間依存型の空隙率を設定する(HiDEMとの連成用)

C==== 宣言 ===========================================================

      use mod_dem

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE  'mpif.h'

CD    -- 引数 --
CD    ISW             : IN  : I*4 : スイッチ
CD                                  = 0:GGVに設定する
CD                                  !=0:GGV0に設定する
CD    DTNOW           : IN  : R*8 : サブループの時間刻み幅
CD    DTOLD           : IN  : R*8 : 前のステップの時間刻み幅
CD    GGV(@FOR-3D@)   : I/O : R*8 : 空隙率
CD    GGV0(@FOR-3D@)  : OUT : R*8 : 空隙率(前サブループでの値)
CD    GGVOLD(IPRNP)   : IN  : R*8 : n-1ステップの空隙率
CD    GGVNOW(IPRNP)   : IN  : R*8 : nステップの時刻の空隙率
CD    GGVEL(3,IPRNP)  : I/O : R*8 : 障害物移動速度
CD    GGVELO(3,IPRNP) : IN  : R*8 : n-1ステップの時刻の障害物移動速度
CD    GGVELN(3,IPRNP) : IN  : R*8 : nステップの時刻の障害物移動速度
CD    NF(@FOR-3D@)    : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION GGV(NUMI,NUMJ,NUMK),GGV0(NUMI,NUMJ,NUMK)
      DIMENSION GGVOLD(IPRNP),GGVNOW(IPRNP)
      DIMENSION GGVEL(3,IPRNP)
      DIMENSION GGVELO(3,IPRNP),GGVELN(3,IPRNP)
      DIMENSION NF(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- nステップでの値（HiDEMの値） --
      IF (ISW.EQ.0) THEN
        L=0
        DO 230 IB=1,IPRNB
          DO 220 K=IPRARA(3,IB),IPRARA(6,IB)
            DO 210 J=IPRARA(2,IB),IPRARA(5,IB)
              DO 200 I=IPRARA(1,IB),IPRARA(4,IB)
                L=L+1
                IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                  IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                    IF (NF(I-IP,J-JP,K).NE.-1) THEN
                      GGV(I-IP,J-JP,K)=GGVNOW(L)
                      GGVEL(1,L)=GGVELN(1,L)
                      GGVEL(2,L)=GGVELN(2,L)
                      GGVEL(3,L)=GGVELN(3,L)
                    ENDIF
                  ENDIF
                ENDIF
 200          CONTINUE
 210        CONTINUE
 220      CONTINUE
 230    CONTINUE

      ELSE
CD    -- サブループ計算中での値 --
C       * 時間依存部分以外は同じ
        DO 320 K=1,NUMK
          DO 310 J=1,NUMJ
            DO 300 I=1,NUMI
              GGV0(I,J,K)=GGV(I,J,K)
 300        CONTINUE
 310      CONTINUE
 320    CONTINUE

C       * 時間依存部分を計算
C@        W=DTNOW/DTOLD
        L=0
        DO 430 IB=1,IPRNB
          DO 420 K=IPRARA(3,IB),IPRARA(6,IB)
            DO 410 J=IPRARA(2,IB),IPRARA(5,IB)
              DO 400 I=IPRARA(1,IB),IPRARA(4,IB)
                L=L+1
                IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                  IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                    IF (NF(I-IP,J-JP,K).NE.-1) THEN
C@                      GGV0(I-IP,J-JP,K)=GGV0(I-IP,J-JP,K)
C@     &                                  +W*(GGVNOW(L)-GGVOLD(L))
C@                      GGVEL(1,L)=GGVEL(1,L)+W*(GGVELN(1,L)-GGVELO(1,L))
C@                      GGVEL(2,L)=GGVEL(2,L)+W*(GGVELN(2,L)-GGVELO(2,L))
C@                      GGVEL(3,L)=GGVEL(3,L)+W*(GGVELN(3,L)-GGVELO(3,L))
                      GGV0(I-IP,J-JP,K)=GGVOLD(L)
     &                                  +W*(GGVNOW(L)-GGVOLD(L))
                      GGVEL(1,L)=GGVELO(1,L)+W*(GGVELN(1,L)-GGVELO(1,L))
                      GGVEL(2,L)=GGVELO(2,L)+W*(GGVELN(2,L)-GGVELO(2,L))
                      GGVEL(3,L)=GGVELO(3,L)+W*(GGVELN(3,L)-GGVELO(3,L))
C@                      write(*,*) j,k,GGV0(I-IP,J-JP,K),GGVNOW(L)
                    ENDIF
                  ENDIF
                ENDIF
 400          CONTINUE
 410        CONTINUE
 420      CONTINUE
 430    CONTINUE

C     -- DEBUG --
CDBG    WRITE(1001,'(A,1P,2E12.5,A,3E12.5)')
CDBG &   '(CGGVH ) T0,TN=',T0,TN,' GGVEL=',(GGVEL(I,1),I=1,3)

      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
