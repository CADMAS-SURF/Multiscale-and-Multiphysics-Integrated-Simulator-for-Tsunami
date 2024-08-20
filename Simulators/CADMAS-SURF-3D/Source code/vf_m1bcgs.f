      SUBROUTINE VF_M1BCGS(DBUF,
     &                     AD ,ALI,ALJ,ALK,AUI,AUJ,AUK,BB ,X0 ,
     &                     DD ,XW ,YY ,R0 ,RR ,PP ,AP ,AE ,
     &                     CGPARA,CGEPSA,CGEPSR,CGDIV,BNORM,XNORM,
     &                     INDC,MI,MJ,MK,NI,NJ,NK,ICGTYP,ICGMAX,ITER)

CD=== 概要 ===========================================================

CDT   VF_M1BCGS:(M)ILU-BiCGSTAB法により非対称連立1次方程式を解く
CD      (1)A*x=bを解く
CD      (2)仮想セルは計算対象外セルであることを前提とし
CD      (3)計算対象外セルは計算をスキップしているため、
CD      (4)ユーザは、入力値を以下の条件で作成する必要がある
CD      (5)・計算対象外セルに関する非対角成分をゼロとしなければならない
CD      (6)・計算対象外セルに関する対角成分は参照されない
CD      (7)・計算対象外セルに関する右辺をゼロとしなければならない

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    AD (MI,MJ,MK)  : IN  : R*8 : 非対称行列Aの対角成分
CD    ALI(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのI-1に関する成分
CD    ALJ(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのJ-1に関する成分
CD    ALK(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのK-1に関する成分
CD    AUI(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのI+1に関する成分
CD    AUJ(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのJ+1に関する成分
CD    AUK(MI,MJ,MK)  : IN  : R*8 : 非対称行列AのK+1に関する成分
CD    BB (MI,MJ,MK)  : IN  : R*8 : 非対称連立1次方程式の右辺
CD    X0 (MI,MJ,MK)  : I/O : R*8 : IN :解ベクトルの初期値
CD                                 OUT:収束しない場合：初期値
CD                                 OUT:収束した場合  ：解ベクトル
CD    DD(MI,MJ,MK)
CD     -AE(MI,MJ,MK) : OUT : R*8 : 作業用
CD    CGPARA         : IN  : R*8 : MILU用パラメータ
CD    CGEPSA         : IN  : R*8 : 収束判定値(絶対誤差)
CD    CGEPSR         : IN  : R*8 : 収束判定値(相対誤差)
CD    CGDIV          : IN  : R*8 : 発散判定値
CD    BNORM          : OUT : R*8 : MINV*BBのノルム
CD    XNORM          : OUT : R*8 : 収束時の残差のノルム
CD    INDC(MI,MJ,MK) : IN  : I*4 : インデックス
CD    MI             : IN  : I*4 : x方向最大格子数+1
CD    MJ             : IN  : I*4 : y方向最大格子数+1
CD    MK             : IN  : I*4 : z方向最大格子数+1
CD    NI             : IN  : I*4 : x方向格子数+1
CD    NJ             : IN  : I*4 : y方向格子数+1
CD    NK             : IN  : I*4 : z方向格子数+1
CD    ICGTYP         : IN  : I*4 : 前処理の種類
CD                                 = 0:不完全LU分解(ILU)
CD                                 !=0:修正不完全LU分解(MILU)
CD    ICGMAX         : IN  : I*4 : 最大反復回数
CD    ITER           : OUT : I*4 : 収束の成否
CD                                 > ICGMAX:最大反復回数を越えた
CD                                 >=     0:収束した:反復回数
CD                                 =     -1:不完全LU分解に失敗した
CD                                 =     -2:発散した
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION AD  (MI,MJ,MK),ALI (MI,MJ,MK),ALJ (MI,MJ,MK)
      DIMENSION ALK (MI,MJ,MK),AUI (MI,MJ,MK),AUJ (MI,MJ,MK)
      DIMENSION AUK (MI,MJ,MK),BB  (MI,MJ,MK),X0  (MI,MJ,MK)
      DIMENSION DD  (MI,MJ,MK),XW  (MI,MJ,MK),YY  (MI,MJ,MK)
      DIMENSION R0  (MI,MJ,MK),RR  (MI,MJ,MK),PP  (MI,MJ,MK)
      DIMENSION AP  (MI,MJ,MK),AE  (MI,MJ,MK)
      DIMENSION INDC(MI,MJ,MK)

C==== 実行 ===========================================================

      CALL VF_A2CPUT(0,ICPUST,KCP9M1)

CD    -- XW=X0とゼロクリア --
      DO 120 K=1,NK
        DO 110 J=1,NJ
          DO 100 I=1,NI
            XW(I,J,K)=X0(I,J,K)
C@            IF (INDC(I,J,K).EQ.-1) THEN
              DD(I,J,K)=0.0D0
C@              XW(I,J,K)=0.0D0
              YY(I,J,K)=0.0D0
              R0(I,J,K)=0.0D0
              RR(I,J,K)=0.0D0
              PP(I,J,K)=0.0D0
              AP(I,J,K)=0.0D0
              AE(I,J,K)=0.0D0
C@            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- 不完全LU分解 --
      ITER=-1
      BNORM=0.0D0
      XNORM=0.0D0
      CALL VF_MZDCMP(AD,ALI,ALJ,ALK,AUI,AUJ,AUK,DD,CGPARA,
     &               INDC,MI,MJ,MK,NI,NJ,NK,ICGTYP,ISW)
      IF (ISW.EQ.0) GOTO 9000

CD    -- 判定値等 --
      EPSA2=CGEPSA*CGEPSA
      EPSR2=CGEPSR*CGEPSR
      DIV2 =CGDIV *CGDIV

CD    * YY=MINV*BB
      CALL VF_MZMINV(DD,ALI,ALJ,ALK,AUI,AUJ,AUK,YY,BB,
     &               INDC,MI,MJ,MK,NI,NJ,NK)

CD    * BNORM=(YY,YY)
      CALL VF_MZIP(YY,YY,BNORM,INDC,MI,MJ,MK,NI,NJ,NK)

CD    -- 初期値設定 --
      ITER=0

CD    * YY=A*XW
      CALL VF_MZAX(AD,ALI,ALJ,ALK,AUI,AUJ,AUK,XW,YY,
     &             INDC,MI,MJ,MK,NI,NJ,NK)

CD    * YY=BB-YY
      DO 220 K=2,NK-1
        DO 210 J=MYJS,MYJE
          DO 200 I=MYIS,MYIE
            IF (INDC(I,J,K).NE.-1) THEN
              YY(I,J,K)=BB(I,J,K)-YY(I,J,K)
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    * R0=MINV*YY
      CALL VF_MZMINV(DD,ALI,ALJ,ALK,AUI,AUJ,AUK,R0,YY,
     &               INDC,MI,MJ,MK,NI,NJ,NK)

CD    * S=(R0,R0)
      CALL VF_MZIP(R0,R0,S,INDC,MI,MJ,MK,NI,NJ,NK)

CD    * 収束判定
      XNORM=S
      IF (XNORM.LE.BNORM*EPSR2+EPSA2) GOTO 8000

CD    * RR=R0,PP=R0
      DO 320 K=2,NK-1
        DO 310 J=MYJS,MYJE
          DO 300 I=MYIS,MYIE
            IF (INDC(I,J,K).NE.-1) THEN
              RR(I,J,K)=R0(I,J,K)
              PP(I,J,K)=R0(I,J,K)
            ENDIF
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE

CD    -- 収束計算 --

CD    ** 中判定反復 **
 1000 CONTINUE
        ITER=ITER+1

CD      ** 最大反復回数を越えるまで **
        IF (ITER.GT.ICGMAX) GOTO 2000

CD      * YY=A*PP
        CALL VF_P3SRD1(PP,DBUF,0)
        CALL VF_MZAX(AD,ALI,ALJ,ALK,AUI,AUJ,AUK,PP,YY,
     &               INDC,MI,MJ,MK,NI,NJ,NK)

CD      * AP=MINV*YY
        CALL VF_MZMINV(DD,ALI,ALJ,ALK,AUI,AUJ,AUK,AP,YY,
     &                 INDC,MI,MJ,MK,NI,NJ,NK)

CD      * ALPHA=S/(R0,AP)
        CALL VF_MZIP(R0,AP,W1,INDC,MI,MJ,MK,NI,NJ,NK)
        ALPHA=S/W1

CD      * RR=RR-ALPHA*AP
        DO 1120 K=2,NK-1
          DO 1110 J=MYJS,MYJE
            DO 1100 I=MYIS,MYIE
              IF (INDC(I,J,K).NE.-1) THEN
                RR(I,J,K)=RR(I,J,K)-ALPHA*AP(I,J,K)
              ENDIF
 1100       CONTINUE
 1110     CONTINUE
 1120   CONTINUE

CD      * YY=A*RR
        CALL VF_P3SRD1(RR,DBUF,0)
        CALL VF_MZAX(AD,ALI,ALJ,ALK,AUI,AUJ,AUK,RR,YY,
     &               INDC,MI,MJ,MK,NI,NJ,NK)

CD      * AE=MINV*YY
        CALL VF_MZMINV(DD,ALI,ALJ,ALK,AUI,AUJ,AUK,AE,YY,
     &                 INDC,MI,MJ,MK,NI,NJ,NK)

CD      * OMEGA=(RR,AE)/((AE,AE)
        CALL VF_MZIP(RR,AE,W1,INDC,MI,MJ,MK,NI,NJ,NK)
        CALL VF_MZIP(AE,AE,W2,INDC,MI,MJ,MK,NI,NJ,NK)
        OMEGA=W1/W2

CD      * XW=XW+ALPHA*PP+OMEGA*RR , RR=RR-OMEGA*AE
        DO 1220 K=2,NK-1
          DO 1210 J=MYJS,MYJE
            DO 1200 I=MYIS,MYIE
              IF (INDC(I,J,K).NE.-1) THEN
                XW(I,J,K)=XW(I,J,K)+(ALPHA*PP(I,J,K)+OMEGA*RR(I,J,K))
                RR(I,J,K)=RR(I,J,K)-OMEGA*AE(I,J,K)
              ENDIF
 1200       CONTINUE
 1210     CONTINUE
 1220   CONTINUE

CD      * 収束判定
        CALL VF_MZIP(RR,RR,XNORM,INDC,MI,MJ,MK,NI,NJ,NK)
        IF (XNORM.LE.BNORM*EPSR2+EPSA2) GOTO 2000
        IF (XNORM.GT.DIV2) THEN
          ITER=-2
          GOTO 2000
        ENDIF

CD      * BETA=(R0,RR)*ALPHA/(S*OMEGA) , W1=(R0,RR)
        CALL VF_MZIP(R0,RR,W1,INDC,MI,MJ,MK,NI,NJ,NK)
        BETA=W1*ALPHA/(S*OMEGA)

CD      * PP=RR+BETA*(PP-OMEGA*AP)
        DO 1320 K=2,NK-1
          DO 1310 J=MYJS,MYJE
            DO 1300 I=MYIS,MYIE
              IF (INDC(I,J,K).NE.-1) THEN
                PP(I,J,K)=RR(I,J,K)+BETA*(PP(I,J,K)-OMEGA*AP(I,J,K))
              ENDIF
 1300       CONTINUE
 1310     CONTINUE
 1320   CONTINUE

CD      * S=W1=(R0,RR)
        S=W1

CD    ** 反復終了 **
        GOTO 1000
 2000 CONTINUE

CD    -- 収束したならばX0=XWとする --
 8000 CONTINUE
      BNORM=SQRT(BNORM)
      XNORM=SQRT(XNORM)
      IF (ITER.GE.0) THEN
        DO 8120 K=2,NK-1
          DO 8110 J=MYJS,MYJE
            DO 8100 I=MYIS,MYIE
              IF (INDC(I,J,K).NE.-1) THEN
                X0(I,J,K)=XW(I,J,K)
              ENDIF
 8100       CONTINUE
 8110     CONTINUE
 8120   CONTINUE
      ENDIF
      CALL VF_P3SRD1(X0,DBUF,0)

C     -- 実行文の終了 --
 9000 CONTINUE
      CALL VF_A2CPUT(0,ICPUEN,KCP9M1)
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
