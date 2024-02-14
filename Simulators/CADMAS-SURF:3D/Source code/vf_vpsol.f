      SUBROUTINE VF_VPSOL(DBUF,
     &                    AD ,ALI,ALJ,ALK,AUI,AUJ,AUK,BB ,PT ,
     &                    DD ,XW ,YY ,R0 ,RR ,PP ,AP ,AE ,INDC)

CD=== 概要 ===========================================================

CDT   VF_VPSOL:ポテンシャル関数の連立1次方程式を解く

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    AD(@FOR-3D@)   : IN  : R*8 : 非対称行列Aの対角成分
CD    ALI(@FOR-3D@)  : IN  : R*8 : 非対称行列AのI-1に関する成分
CD    ALJ(@FOR-3D@)  : IN  : R*8 : 非対称行列AのJ-1に関する成分
CD    ALK(@FOR-3D@)  : IN  : R*8 : 非対称行列AのK-1に関する成分
CD    AUI(@FOR-3D@)  : IN  : R*8 : 非対称行列AのI+1に関する成分
CD    AUJ(@FOR-3D@)  : IN  : R*8 : 非対称行列AのJ+1に関する成分
CD    AUK(@FOR-3D@)  : IN  : R*8 : 非対称行列AのK+1に関する成分
CD    BB(@FOR-3D@)   : IN  : R*8 : 非対称連立1次方程式の右辺
CD    PT(@FOR-3D@)   : OUT : R*8 : ポテンシャル関数
CD                                  収束しない場合：初期値
CD                                  収束した場合  ：解ベクトル
CD    DD(@FOR-3D@)
CD     -AE(@FOR-3D@) : OUT : R*8 : 作業用
CD    INDC(@FOR-3D@) : IN  : I*4 : セルの計算状態を示すインデックス
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION AD  (NUMI,NUMJ,NUMK),ALI (NUMI,NUMJ,NUMK)
      DIMENSION ALJ (NUMI,NUMJ,NUMK),ALK (NUMI,NUMJ,NUMK)
      DIMENSION AUI (NUMI,NUMJ,NUMK),AUJ (NUMI,NUMJ,NUMK)
      DIMENSION AUK (NUMI,NUMJ,NUMK),BB  (NUMI,NUMJ,NUMK)
      DIMENSION PT  (NUMI,NUMJ,NUMK)
      DIMENSION DD  (NUMI,NUMJ,NUMK),XW  (NUMI,NUMJ,NUMK)
      DIMENSION YY  (NUMI,NUMJ,NUMK),R0  (NUMI,NUMJ,NUMK)
      DIMENSION RR  (NUMI,NUMJ,NUMK),PP  (NUMI,NUMJ,NUMK)
      DIMENSION AP  (NUMI,NUMJ,NUMK),AE  (NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 解ベクトルの初期値 --
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            PT(I,J,K)=0.0D0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- (M)ILU-BiCGSTAB法により非対称連立1次方程式を解く --
C     エラーチェッカーをだますため
      MI=NUMI
      MJ=NUMJ
      MK=NUMK
      CALL VF_M1BCGS(DBUF,
     &               AD ,ALI,ALJ,ALK,AUI,AUJ,AUK,BB ,PT ,
     &               DD ,XW ,YY ,R0 ,RR ,PP ,AP ,AE ,
     &               CGPARA,CGEPSA,CGEPSR,CGDIV,CGBNRM,CGXNRM,
     &               INDC,MI,MJ,MK,NUMI,NUMJ,NUMK,
     &               ICGTYP,ICGMAX,ICGITR)
      IF (ICGITR.GT.ICGMAX)
     &           CALL VF_A2ERR('VF_VPSOL','NOT CONVERGED.')
      IF (ICGITR.EQ.-2    )
     &           CALL VF_A2ERR('VF_VPSOL','INSTABILITY.')
      IF (ICGITR.EQ.-1    )
     &           CALL VF_A2ERR('VF_VPSOL','CAN NOT DECOMPOSITION.')

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
