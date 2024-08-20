      SUBROUTINE VF_A2CLOS()

CD=== 概要 ===========================================================

CDT   VF_A2CLOS:ファイルをクローズする

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'

C==== 実行 ===========================================================

CD    -- オープンされているファイルをクローズする --
      IF (IENFIL.NE.0) CLOSE(IENFIL)
      IF (IINFIL.NE.0) CLOSE(IINFIL)
      IF (IMTFIL.NE.0) CLOSE(IMTFIL)
      IF (IMTFIL2.NE.0) CLOSE(IMTFIL2)
      IF (IREFIL.NE.0) CLOSE(IREFIL)
      IF (IPRFIL.NE.0) CLOSE(IPRFIL)
      IF (ILPFIL.NE.0) CLOSE(ILPFIL)
      IF (IGRFIL.NE.0) CLOSE(IGRFIL)
      IF (IRSFIL.NE.0) CLOSE(IRSFIL)
      IF (ITRFIL.NE.0) CLOSE(ITRFIL)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
