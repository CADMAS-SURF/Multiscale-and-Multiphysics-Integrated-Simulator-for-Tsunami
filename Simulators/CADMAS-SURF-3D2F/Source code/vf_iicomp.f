      SUBROUTINE VF_IICOMP(IS,IE,NWD,TEXT)

CD=== 概要 ===========================================================

CDT   VF_IICOMP:数値解法関連データ(COMP)を解釈する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'

CD    -- 引数 --
CD    IS(MAXWDS) : IN  : I*4   : n番目の単語の開始位置
CD    IE(MAXWDS) : IN  : I*4   : n番目の単語の終了位置
CD    NWD        : IN  : I*4   : 単語の数
CD    TEXT       : IN  : C*(*) : 入力した文字列
      DIMENSION IS(MAXWDS),IE(MAXWDS)
      CHARACTER*(MAXCHR) TEXT

C==== 実行 ===========================================================

CD    -- 単語数のチェック --
      IF (NWD.LT.2) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')

CD    -- COMP SCHMを解釈する --
      IF     (TEXT(IS(2):IE(2)).EQ.'SCHM') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')
C       * COMP SCHM VP-DONOR
        IF     (TEXT(IS(3):IE(3)).EQ.'VP-DONOR') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')
          CALL VF_ZSTOR(SCMVP,TEXT(IS(4):IE(4)))
          IF (SCMVP.LT.0.0D0 .OR. SCMVP.GT.1.0D0)
     &                 CALL VF_A2ERR('VF_IICOMP','INVALID VALUE.')
          ISCMVP=0
C       * COMP SCHM FF-DN-AC
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'FF-DN-AC') THEN
          ISCMFF=0
C       * COMP SCHM FF-SLOPE
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'FF-SLOPE') THEN
          ISCMFF=1
C       * COMP SCHM KE-DONOR
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'KE-DONOR') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')
          CALL VF_ZSTOR(SCMK,TEXT(IS(4):IE(4)))
          IF (SCMK.LT.0.0D0 .OR. SCMK.GT.1.0D0)
     &                 CALL VF_A2ERR('VF_IICOMP','INVALID VALUE.')
          ISCMK=0
C       * COMP SCHM T-DONOR
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'T-DONOR') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')
          CALL VF_ZSTOR(SCMT,TEXT(IS(4):IE(4)))
          IF (SCMT.LT.0.0D0 .OR. SCMT.GT.1.0D0)
     &                 CALL VF_A2ERR('VF_IICOMP','INVALID VALUE.')
          ISCMT=0
C       * COMP SCHM C-DONOR
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'C-DONOR') THEN
          IF (NWD.LT.5) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')
          CALL VF_ZSTOI(LC,TEXT(IS(4):IE(4)))
          CALL VF_ZSTOR(SC,TEXT(IS(5):IE(5)))
          IF (LC.LE.0 .OR. LC.GT.LEQC)
     &                 CALL VF_A2ERR('VF_IICOMP','INVALID VALUE.')
          IF (SC.LT.0.0D0 .OR. SC.GT.1.0D0)
     &                 CALL VF_A2ERR('VF_IICOMP','INVALID VALUE.')
          ISCMC(LC)=0
          SCMC (LC)=SC
C       * 解釈できない単語によるエラー
        ELSE
          CALL VF_A2ERR('VF_IICOMP','UNKNOWN WORD.')
        ENDIF

CD    -- COMP MTRXを解釈する --
      ELSEIF (TEXT(IS(2):IE(2)).EQ.'MTRX') THEN
        IF (NWD.LT.3) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')
C       * COMP MTRX ILUBCGSTAB
        IF     (TEXT(IS(3):IE(3)).EQ.'ILUBCGSTAB') THEN
          ICGTYP=0
C       * COMP MTRX M-ILUBCGSTAB
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'M-ILUBCGSTAB') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')
          CALL VF_ZSTOR(CGPARA,TEXT(IS(4):IE(4)))
          IF (CGPARA.LT.0.0D0 .OR. CGPARA.GT.1.0D0)
     &                 CALL VF_A2ERR('VF_IICOMP','INVALID VALUE.')
          ICGTYP=1
C       * COMP MTRX MAX-ITR
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'MAX-ITR') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')
          CALL VF_ZSTOI(ICGMAX,TEXT(IS(4):IE(4)))
          IF (ICGMAX.LT.0) CALL VF_A2ERR('VF_IICOMP','INVALID VALUE.')
C       * COMP MTRX A-ERROR
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'A-ERROR') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')
          CALL VF_ZSTOR(CGEPSA,TEXT(IS(4):IE(4)))
          IF (CGEPSA.LT.0.0D0)
     &                 CALL VF_A2ERR('VF_IICOMP','INVALID VALUE.')
C       * COMP MTRX R-ERROR
        ELSEIF (TEXT(IS(3):IE(3)).EQ.'R-ERROR') THEN
          IF (NWD.LT.4) CALL VF_A2ERR('VF_IICOMP','SYNTAX ERROR.')
          CALL VF_ZSTOR(CGEPSR,TEXT(IS(4):IE(4)))
          IF (CGEPSR.LT.0.0D0)
     &                 CALL VF_A2ERR('VF_IICOMP','INVALID VALUE.')
C       * 解釈できない単語によるエラー
        ELSE
          CALL VF_A2ERR('VF_IICOMP','UNKNOWN WORD.')
        ENDIF

CD    -- 解釈できない単語によるエラー --
      ELSE
        CALL VF_A2ERR('VF_IICOMP','UNKNOWN WORD.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
