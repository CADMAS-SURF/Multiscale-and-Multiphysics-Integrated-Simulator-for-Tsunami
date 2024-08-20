      SUBROUTINE VF_PMGSTS(XYZC,XYZP,IJKS,IJKE,NC,NP,NPS,NPE)

CD=== 概要 ===========================================================

CDT   VF_PMGSTS:マルチグリッド環境の親子関係の格子のチェック

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'

CD    -- 引数 --
cmod20160721      DIMENSION :: XYZC(0:NC),XYZP(MAXG1,NP)
      DIMENSION :: XYZC(0:NC),XYZP(1,2:NP+1)

CD    -- 局所変数 --

C==== 実行 ===========================================================

      IERR = 0
      IP = NPS
      IJKS = 0
C
C  子の格子点ICについてのループ
C
      DO 110 IC=0,NC
C
 111    CONTINUE
C
C  子の格子点ICが親の格子点IPとIP+1の間にあるかどうかのチェック
C
        IF(XYZC(IC).GE.XYZP(1,IP  ) .AND.
     &     XYZC(IC).LT.XYZP(1,IP+1)      ) THEN
C
C      子の格子点の開始点(IC=0)なら、
C      その格子点は親の格子点と一致していなければならない
C      その親の格子点番号を IJKS に設定
C
          IF(IC.EQ.0) THEN
            IJKS = IP
            IF(XYZC(IC).NE.XYZP(1,IP)) THEN
              IERR = 1
              GO TO 112
            END IF
          END IF
C
C      XYZC(IC)に親の格子点からの補間係数を設定
C         IP <= XYZC(IC) < IP+1
C
          XYZC(IC) = (IP - IJKS) + (XYZC(IC)     - XYZP(1,IP))
     &                            /(XYZP(1,IP+1) - XYZP(1,IP))
C
C  子の格子点が親の格子点IPとIP+1の間にない場合
C
        ELSE IF(IC.EQ.0) THEN
C
C     子の格子点の開始点(IC=0)の処理であれば
C     親の格子点IPをインクリメントしてチェックに戻る
C
          IP = IP+1
          IF(IP.EQ.NPE+1) THEN
             write(*,*) 'NP,NPE=',NP,NPE
ccc
            IERR = 1
            GO TO 112
          END IF
          GO TO 111
C
C     子の格子点の開始点以外で、チェックにひっかからないなら
C     当該格子点は親のIP+1の格子点と一致していなければならない。
C
        ELSE IF(XYZC(IC).EQ.XYZP(1,IP+1)) THEN
C
C      XYZC(IC)に親の格子点からの補間係数を設定
C
          XYZC(IC) = (IP - IJKS) + 1
C
C      子の最後の格子点なら、IPの値をIJKEに設定してループを抜ける
C
          IF(IC.EQ.NC) THEN
            IJKE = IP
            GO TO 112
          END IF
C
C      親の格子点をインクリメント
C
          IP = IP+1
          IF(IP.EQ.NPE+1) THEN
            IERR = 1
            GO TO 112
          END IF
C
        ELSE
          IERR = 1
          GO TO 112
C
        END IF
C
 110  CONTINUE
 112  CONTINUE
      IF(IERR.NE.0) THEN
        CALL VF_A2ERR('VF_PMGSTS','INVALID VALUE.')
      END IF
      
C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
