      SUBROUTINE VF_BSPPFL(PP,FF,DBUF,PP0,NF,NF0)

CD=== 概要 ===========================================================

CDT   VF_BSPPFL:気体セルから流体セルへ変化したセルの圧力を設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    PP(@FOR-3D@)  : I/O : R*8 : 圧力
CD    FF(@FOR-3D@)  : IN  : R*8 : VOF関数F
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    PP0(@FOR-3D@) : OUT : R*8 : ワーク(更新前の圧力)
CD    NF(@FOR-3D@)  : IN  : I*4 : セルの状態を示すインデックス
CD    NF0(@FOR-3D@) : IN  : I*4 : 更新前のNF
      DIMENSION PP (NUMI,NUMJ,NUMK),FF (NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF),PP0(NUMI,NUMJ,NUMK)
      DIMENSION NF (NUMI,NUMJ,NUMK),NF0(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 更新前の圧力を退避する --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            PP0(I,J,K)=PP(I,J,K)
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 圧力を更新する(単純な平均とする) --
      DO 320 K=2,NUMK-1
        DO 310 J=MYJS,MYJE
          DO 300 I=MYIS,MYIE
            IF (NF(I,J,K).EQ.0 .AND. NF0(I,J,K).EQ.8) THEN
              SUMF=0.0D0
              SUMP=0.0D0
              DO 220 KP=-1,1
                DO 210 JP=-1,1
                  DO 200 IP=-1,1
C@@@@                    IF (IP.NE.0 .OR. JP.NE.0 .OR. KP.NE.0) THEN
                      IF (NF(I+IP,J+JP,K+KP).EQ.0) THEN
                        F   =FF(I+IP,J+JP,K+KP)
                        SUMF=SUMF+F
                        SUMP=SUMP+F*PP0(I+IP,J+JP,K+KP)
                      ENDIF
C@@@@                    ENDIF
 200              CONTINUE
 210            CONTINUE
 220          CONTINUE
              IF (SUMF.LT.ZERO) THEN
                PP(I,J,K)=0.0D0
              ELSE
                PP(I,J,K)=SUMP/SUMF
              ENDIF
            ENDIF
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE

      CALL VF_P3SRD2(PP,DBUF,0)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
