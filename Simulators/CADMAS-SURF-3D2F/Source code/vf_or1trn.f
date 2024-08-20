      SUBROUTINE VF_OR1TRN(DT,UU,VV,WW,PP,FF,GGV,BCU,BCV,BCW,BCP,BCF,
     &                     AK,AE,BCK,BCE,TT,BCT,CC,BCC,
     &                     TBUB,DROPTX,DROPTY,DROPTZ,
     &                     DROPUU,DROPVV,DROPWW,
     &                     NF)

CD=== 概要 ===========================================================

CDT   VF_OR1TRN:解析結果を詳細ファイルに出力する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'SF_STRUCT.h'
C----------------------------------------------------for MG/2FC coupling
      INCLUDE 'VF_APARAI.h'
C----------------------------------------------------for MG/2FC coupling

CD    -- 引数 --
CD    DT               : IN  : R*8 : 次のステップの時間刻み幅
CD    UU(@FOR-3D@)     : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)     : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    PP(@FOR-3D@)     : IN  : R*8 : 圧力
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    GGV(@FOR-3D@)    : IN  : R*8 : 空隙率
CD    BCU(NUMB,3)      : IN  : R*8 : x方向流速の境界値
CD    BCV(NUMB,3)      : IN  : R*8 : y方向流速の境界値
CD    BCW(NUMB,3)      : IN  : R*8 : z方向流速の境界値
CD    BCP(NUMB,3)      : IN  : R*8 : 圧力の境界値
CD    BCF(NUMB)        : IN  : R*8 : VOF関数Fの境界値
CD    AK(@FOR-3D@)     : IN  : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)     : IN  : R*8 : 乱流エネルギ散逸
CD    BCK(NUMB,3)      : IN  : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB,3)      : IN  : R*8 : 乱流エネルギ散逸の境界値
CD    TT(@FOR-3D@)     : IN  : R*8 : 温度
CD    BCT(NUMB)        : IN  : R*8 : 温度の境界値
CD    CC(@FOR-3D@,LEQC) : IN : R*8 : 濃度
CD    BCC(NUMB,LEQC)    : IN : R*8 : 濃度の境界値
CD    TBUB(NUMK)       : IN  : R*8 : 気泡上昇処理を最後に行った時間
CD    DROPTX(@FOR-3D@) : IN  : R*8 : 自由落下処理を最後に行った時間(x)
CD    DROPTY(@FOR-3D@) : IN  : R*8 : 自由落下処理を最後に行った時間(y)
CD    DROPTZ(@FOR-3D@) : IN  : R*8 : 自由落下処理を最後に行った時間(z)
CD    DROPUU(@FOR-3D@) : IN  : R*8 : 自由落下のx方向速度
CD    DROPVV(@FOR-3D@) : IN  : R*8 : 自由落下のy方向速度
CD    DROPWW(@FOR-3D@) : IN  : R*8 : 自由落下のz方向速度
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION UU    (NUMI,NUMJ,NUMK),VV    (NUMI,NUMJ,NUMK)
      DIMENSION WW    (NUMI,NUMJ,NUMK),PP    (NUMI,NUMJ,NUMK)
      DIMENSION FF    (NUMI,NUMJ,NUMK),GGV   (NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB,3),BCV(NUMB,3),BCW(NUMB,3)
      DIMENSION BCP(NUMB,3),BCF(NUMB)
      DIMENSION AK    (NUMI,NUMJ,NUMK),AE    (NUMI,NUMJ,NUMK)
      DIMENSION BCK   (NUMB,3)        ,BCE   (NUMB,3)
      DIMENSION TT    (NUMI,NUMJ,NUMK),BCT   (NUMB)
      DIMENSION CC    (NUMI,NUMJ,NUMK,LEQC),BCC(NUMB,LEQC)
      DIMENSION TBUB(NUMK)
      DIMENSION DROPTX(NUMI,NUMJ,NUMK),DROPTY(NUMI,NUMJ,NUMK)
      DIMENSION DROPTZ(NUMI,NUMJ,NUMK),DROPUU(NUMI,NUMJ,NUMK)
      DIMENSION DROPVV(NUMI,NUMJ,NUMK),DROPWW(NUMI,NUMJ,NUMK)
      DIMENSION NF    (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 出力の判定 --
      IO=0
C     * ステップ間隔出力の場合
      IF     (IRSTYP.EQ.1) THEN
        IF (NNOW.GE.IRSTRN(1) .AND. NNOW.LE.IRSTRN(2)) THEN
          IF (MOD(NNOW-IRSTRN(1),IRSTRN(3)).EQ.0) IO=1
        ENDIF
C     * 時間間隔出力の場合
      ELSEIF (IRSTYP.EQ.2) THEN
        IF (TNOW.GE.RRSTRN(1)-ZERO .AND. TNOW.LE.RRSTRN(2)+ZERO) THEN
          W=(TNOW+0.5D0*DT)-RRSTRN(4)
          IF (W.GE.0.0D0) THEN
            IO=1
            RRSTRN(4)=RRSTRN(4)+DBLE(INT(W/RRSTRN(3))+1)*RRSTRN(3)
          ENDIF
        ENDIF
      ENDIF

      IOR=IO

CD    -- 非出力ならば抜ける --
      IF (IO.EQ.0) GOTO 9000

CD    -- メッセージの出力 --
      WRITE(ILPFIL,9510) NNOW,TNOW

      IF (ICPL.GT.0 .OR. ISTM.EQ.1) GOTO 9000

CD    -- 計算情報を出力 --
      WRITE(IRSFIL,ERR=9010) NNOW,TNOW,DTNOW
      WRITE(IRSFIL,ERR=9010) FSUM,FCUT,CGBNRM,CGXNRM,ICGITR

CD    -- セルの状態を示すインデックスを出力 --
      WRITE(IRSFIL,ERR=9010)
     &        (((NF(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)

CD    -- 流速を出力 --
      WRITE(IRSFIL,ERR=9010)
     &        (((UU(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
      WRITE(IRSFIL,ERR=9010) ((BCU(L1,L2),L1=1,NUMB),L2=1,3)
      WRITE(IRSFIL,ERR=9010)
     &        (((VV(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
      WRITE(IRSFIL,ERR=9010) ((BCV(L1,L2),L1=1,NUMB),L2=1,3)
      WRITE(IRSFIL,ERR=9010)
     &        (((WW(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
      WRITE(IRSFIL,ERR=9010) ((BCW(L1,L2),L1=1,NUMB),L2=1,3)

CD    -- 圧力を出力 --
      WRITE(IRSFIL,ERR=9010)
     &        (((PP(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
      WRITE(IRSFIL,ERR=9010) ((BCP(L1,L2),L1=1,NUMB),L2=1,3)

CD    -- VOF関数Fを出力 --
      WRITE(IRSFIL,ERR=9010)
     &        (((FF(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
      WRITE(IRSFIL,ERR=9010) (BCF(L),L=1,NUMB)

CD    -- 乱流エネルギを出力 --
      IF (LEQK.NE.0) THEN
        WRITE(IRSFIL,ERR=9010)
     &          (((AK(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        WRITE(IRSFIL,ERR=9010) ((BCK(L1,L2),L1=1,NUMB),L2=1,3)
      ENDIF

CD    -- 乱流エネルギ散逸を出力 --
      IF (LEQK.NE.0) THEN
        WRITE(IRSFIL,ERR=9010)
     &          (((AE(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        WRITE(IRSFIL,ERR=9010) ((BCE(L1,L2),L1=1,NUMB),L2=1,3)
      ENDIF

CD    -- 温度を出力 --
      IF (LEQT.NE.0) THEN
        WRITE(IRSFIL,ERR=9010)
     &          (((TT(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        WRITE(IRSFIL,ERR=9010) (BCT(L),L=1,NUMB)
      ENDIF

CD    -- スカラー量を出力 --
      DO 100 LC=1,LEQC
        WRITE(IRSFIL,ERR=9010)
     &          (((CC(I,J,K,LC),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
        WRITE(IRSFIL,ERR=9010) (BCC(L,LC),L=1,NUMB)
 100  CONTINUE

CD    -- 時間依存型空隙率を出力 --
      IF (IPRNT.GT.1) THEN
        WRITE(IRSFIL,ERR=9010)
     &          (((GGV(I,J,K),I=1,NUMI),J=1,NUMJ),K=1,NUMK)
      ENDIF

CD    -- Timer Door用データを出力 --
      WRITE(IRSFIL,ERR=9010) (TBUB(L),L=1,NUMK)
      IF (IDROP.GE.1) THEN
C       * 水滴の数を数える
        ND=0
        DO 220 K=2,NUMK-1
          DO 210 J=2,NUMJ-1
            DO 200 I=2,NUMI-1
              IF (DROPTX(I,J,K).GE.0.0D0) ND=ND+1
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE
C       * 出力する
        WRITE(IRSFIL,ERR=9010) ND
        DO 320 K=2,NUMK-1
          DO 310 J=2,NUMJ-1
            DO 300 I=2,NUMI-1
              IF (DROPTX(I,J,K).GE.0.0D0) THEN
                WRITE(IRSFIL,ERR=9010) I,J,K,
     &                  DROPTX(I,J,K),DROPTY(I,J,K),DROPTZ(I,J,K),
     &                  DROPUU(I,J,K),DROPVV(I,J,K),DROPWW(I,J,K)
              ENDIF
 300        CONTINUE
 310      CONTINUE
 320    CONTINUE
      ENDIF

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
C----------------------------------------------------for MG/2FC coupling
C     CALL VF_A2ERR('VF_OR1TRN','WRITE ERROR (data.rsl).')
      CALL VF_A2ERR('VF_OR1TRN','WRITE ERROR ('
     &                 //TRIM(MGNAME(MGRANK+1))//'.rsl).')
C----------------------------------------------------for MG/2FC coupling
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-RSL : OUT : STEP=',I6,' : TIME= ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
