      SUBROUTINE VF_IIOBST(NF,IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IIOBST:障害物データ(OBST)を解釈する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    NF(@FOR-3D@) : I/O : I*4 : セルの状態を示すインデックス
CD    IS(MAXWDS)   : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS)   : IN  : I*4   : n番目の単語の終了位置
CD    NWD          : IN  : I*4   : 単語の数
CD    TEXT         : IN  : C*(*) : 入力した文字列
      DIMENSION NF(NUMI,NUMJ,NUMK)
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- OBSTを解釈する --
      IF (NWD.LT.7) CALL VF_A2ERR('VF_IIOBST','SYNTAX ERROR.')
      CALL VF_ZSTOI(I1,TEXT(IS(2):IE(2)))
      CALL VF_ZSTOI(J1,TEXT(IS(3):IE(3)))
      CALL VF_ZSTOI(K1,TEXT(IS(4):IE(4)))
      CALL VF_ZSTOI(I2,TEXT(IS(5):IE(5)))
      CALL VF_ZSTOI(J2,TEXT(IS(6):IE(6)))
      CALL VF_ZSTOI(K2,TEXT(IS(7):IE(7)))
      IF (I1.LT.1 .OR. I2.GT.NUMI0-2 .OR. I1.GT.I2 .OR.
     &    J1.LT.1 .OR. J2.GT.NUMJ0-2 .OR. J1.GT.J2 .OR.
     &    K1.LT.1 .OR. K2.GT.NUMK -2 .OR. K1.GT.K2     ) 
     &                CALL VF_A2ERR('VF_IIOBST','INVALID VALUE.')
      DO 220 K=K1+1,K2+1
        DO 210 J=J1+1,J2+1
          IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
            DO 200 I=I1+1,I2+1
              IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                NF(I-IP,J-JP,K)=-1
              ENDIF
 200        CONTINUE
          ENDIF
 210    CONTINUE
 220  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
