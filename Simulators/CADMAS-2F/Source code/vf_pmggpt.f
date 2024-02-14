      SUBROUTINE VF_PMGGPT(GGV,GGX,GGY,GGZ,NF,IC,IS,JS,KS,IE,JE,KE)
C----------------------------------------------------------2011.04 end

CD=== 概要 ===========================================================

CDT   VF_PMGGPT:マルチグリッド環境の親のポーラス値の子への送信

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    GGV(@FOR-3D@)    : I/O : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : I/O : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : I/O : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : I/O : R*8 : z方向面積透過率
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION NF(NUMI,NUMJ,NUMK)

CD    -- 局所変数 --
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: GWRK

C==== 実行 ===========================================================

      NX = IE - IS + 1
      NY = JE - JS + 1
      NZ = KE - KS + 1

CD    -- GGV & NV --
      NGWRK = (NX*NY - MAX((NX-2)*(NY-2),0))*NZ *2
      ALLOCATE(GWRK(NGWRK))
      IGWRK = 0
      DO 100 K=KS,KE
        DO 110 J=JS,JE
          DO 120 I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
              GWRK(IGWRK+1) = GGV(I,J,K)
              GWRK(IGWRK+2) = NF (I,J,K)
              IGWRK = IGWRK + 2
            END IF
 120      CONTINUE
 110    CONTINUE
 100  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGPT','PROGRAM ERROR(GGV).')
      END IF
      CALL VF_ZXMG_ISENDD(GWRK,NGWRK,IC,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
      DEALLOCATE(GWRK)

CD    -- GGX --
      NGWRK = ((NX+1)*NY - MAX((NX-3)*(NY-2),0))*NZ
      ALLOCATE(GWRK(NGWRK))
      IGWRK = 0
      DO 200 K=KS,KE
        DO 210 J=JS,JE
          DO 220 I=IS,IE+1
            IF(I.LE.IS+1 .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
              GWRK(IGWRK+1) = GGX(I,J,K)
              IGWRK = IGWRK + 1
            END IF
 220      CONTINUE
 210    CONTINUE
 200  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGPT','PROGRAM ERROR(GGX).')
      END IF
      CALL VF_ZXMG_ISENDD(GWRK,NGWRK,IC,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
      DEALLOCATE(GWRK)

CD    -- GGY --
      NGWRK = (NX*(NY+1) - MAX((NX-2)*(NY-3),0))*NZ
      ALLOCATE(GWRK(NGWRK))
      IGWRK = 0
      DO 300 K=KS,KE
        DO 310 J=JS,JE+1
          DO 320 I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS+1 .OR. J.GE.JE) THEN
              GWRK(IGWRK+1) = GGY(I,J,K)
              IGWRK = IGWRK + 1
            END IF
 320      CONTINUE
 310    CONTINUE
 300  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGPT','PROGRAM ERROR(GGY).')
      END IF
      CALL VF_ZXMG_ISENDD(GWRK,NGWRK,IC,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
      DEALLOCATE(GWRK)

CD    -- GGZ --
      NGWRK = (NX*NY - MAX((NX-2)*(NY-2),0))*(NZ+1)
      ALLOCATE(GWRK(NGWRK))
      IGWRK = 0
      DO 400 K=KS,KE+1
        DO 410 J=JS,JE
          DO 420 I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
              GWRK(IGWRK+1) = GGZ(I,J,K)
              IGWRK = IGWRK + 1
            END IF
 420      CONTINUE
 410    CONTINUE
 400  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGPT','PROGRAM ERROR(GGZ).')
      END IF
      CALL VF_ZXMG_ISENDD(GWRK,NGWRK,IC,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
      DEALLOCATE(GWRK)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
