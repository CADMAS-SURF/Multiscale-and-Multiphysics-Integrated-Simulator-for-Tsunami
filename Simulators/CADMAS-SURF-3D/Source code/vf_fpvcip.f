      SUBROUTINE VF_FPVCIP(NF,IPVC,IBUF,NLIM)

CD=== 概要 ===========================================================

CDT   VF_FPVCIP:空気圧計算用インデックスを設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'

CD    -- 引数 --
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
CD    IPVC(@FOR-3D@)   : OUT : I*4 : 空気圧の計算用インデックス
CD    IBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
CD    NLIM(@FOR-3D@)   : OUT : I*4 : ワーク配列
      DIMENSION NF  (NUMI,NUMJ,NUMK),IPVC(NUMI*NUMJ*NUMK)
      DIMENSION IBUF(NUMBUF*MAXBUF) ,NLIM(NUMI*NUMJ*NUMK)

C==== 実行 ===========================================================

CD    -- インデックスを初期化する(EorSセルはIPVC=-1) --
      IJK=0
      DO 120 K=1,NUMK
        DO 110 J=1,NUMJ
          DO 100 I=1,NUMI
            IJK=IJK+1
            IPVC(IJK)=0
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE
      DO 170 K=2,NUMK-1
        DO 160 J=MYJS,MYJE
          DO 150 I=MYIS,MYIE
            IJK=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
            IF (NF(I,J,K).GT.0) IPVC(IJK)=-1
 150      CONTINUE
 160    CONTINUE
 170  CONTINUE

CD    -- 局所的インデックスを設定する --
      I1=-1
      I2= 1
      J1=-NUMI
      J2= NUMI
      K1=-NUMI*NUMJ
      K2= NUMI*NUMJ
      NPVCB=0
      DO 270 K=2,NUMK-1
        DO 260 J=MYJS,MYJE
          DO 250 I=MYIS,MYIE
            IJK=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
            IF (IPVC(IJK).EQ.-1) THEN
              NSTK=0
              NPVCB=NPVCB+1
 200          CONTINUE
                IPVC(IJK)=NPVCB
                IP=IJK+I1
                IF (IPVC(IP).EQ.-1) THEN
                  NSTK=NSTK+1
                  NLIM(NSTK)=IP
                  IPVC(IP)  =-2
                ENDIF
                IP=IJK+I2
                IF (IPVC(IP).EQ.-1) THEN
                  NSTK=NSTK+1
                  NLIM(NSTK)=IP
                  IPVC(IP)  =-2
                ENDIF
                IP=IJK+J1
                IF (IPVC(IP).EQ.-1) THEN
                  NSTK=NSTK+1
                  NLIM(NSTK)=IP
                  IPVC(IP)  =-2
                ENDIF
                IP=IJK+J2
                IF (IPVC(IP).EQ.-1) THEN
                  NSTK=NSTK+1
                  NLIM(NSTK)=IP
                  IPVC(IP)  =-2
                ENDIF
                IP=IJK+K1
                IF (IPVC(IP).EQ.-1) THEN
                  NSTK=NSTK+1
                  NLIM(NSTK)=IP
                  IPVC(IP)  =-2
                ENDIF
                IP=IJK+K2
                IF (IPVC(IP).EQ.-1) THEN
                  NSTK=NSTK+1
                  NLIM(NSTK)=IP
                  IPVC(IP)  =-2
                ENDIF
                IF (NSTK.EQ.0) GOTO 210
                IJK=NLIM(NSTK)
                NSTK=NSTK-1
                GOTO 200
 210          CONTINUE
            ENDIF
 250      CONTINUE
 260    CONTINUE
 270  CONTINUE

CD    -- 大域的インデックスを設定する --
      IF (NPROCS.NE.1) THEN
        NS=0
        DO 300 IP=0,NPROCS-1
          IF (MYRANK.GE.IP) THEN
            N=0
            CALL VF_P1SUMI(N,I)
          ELSE
            N=NPVCB
            CALL VF_P1SUMI(N,I)
          ENDIF
          IF (MYRANK.EQ.IP) NS=I
 300    CONTINUE
        DO 370 K=2,NUMK-1
          DO 360 J=MYJS,MYJE
            DO 350 I=MYIS,MYIE
              IJK=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
              IF (IPVC(IJK).GE.1) IPVC(IJK)=IPVC(IJK)+NS
 350        CONTINUE
 360      CONTINUE
 370    CONTINUE
        N=NPVCB
        CALL VF_P1SUMI(N,NPVCB)
      ENDIF
      IF (NPVCB.GT.MAXPVC) CALL VF_A2ERR('VF_FPVCIP','AREA IS FULL.')

CD    -- 入れ替えがなくなるまで繰り返す --
      NSWAP=0
 1000 CONTINUE
        NSWAP=NSWAP+1
        ISWAP=0

CD      -- マージン部分の最小値をとる --
        IF (NPROCS.NE.1) THEN
          CALL VF_P3SRI2(IPVC,IBUF,0)
          DO 400 IB=1,NPVCB
            NLIM(IB)=IB
 400      CONTINUE
          J1=MYJS
          J2=MYJE
          L =NUMI
          DO 420 K=2,NUMK-1
            DO 410 I=MYIS,MYIE
              IJK=I+NUMI*(J1-1)+NUMI*NUMJ*(K-1)
              IB =IPVC(IJK  )
              IBB=IPVC(IJK-L)
              IF (IB.GE.1 .AND. IBB.GE.1) THEN
                IF (IB.GT.IBB .AND. NLIM(IB).GT.IBB) THEN
                  NLIM(IB)=IBB
                  ISWAP=ISWAP+1
                ENDIF
              ENDIF
              IJK=I+NUMI*(J2-1)+NUMI*NUMJ*(K-1)
              IB =IPVC(IJK  )
              IBB=IPVC(IJK+L)
              IF (IB.GE.1 .AND. IBB.GE.1) THEN
                IF (IB.GT.IBB .AND. NLIM(IB).GT.IBB) THEN
                  NLIM(IB)=IBB
                  ISWAP=ISWAP+1
                ENDIF
              ENDIF
 410        CONTINUE
 420      CONTINUE
          I1=MYIS
          I2=MYIE
          L =1
          DO 440 K=2,NUMK-1
            DO 430 J=MYJS,MYJE
              IJK=I1+NUMI*(J-1)+NUMI*NUMJ*(K-1)
              IB =IPVC(IJK  )
              IBB=IPVC(IJK-L)
              IF (IB.GE.1 .AND. IBB.GE.1) THEN
                IF (IB.GT.IBB .AND. NLIM(IB).GT.IBB) THEN
                  NLIM(IB)=IBB
                  ISWAP=ISWAP+1
                ENDIF
              ENDIF
              IJK=I2+NUMI*(J-1)+NUMI*NUMJ*(K-1)
              IB =IPVC(IJK  )
              IBB=IPVC(IJK+L)
              IF (IB.GE.1 .AND. IBB.GE.1) THEN
                IF (IB.GT.IBB .AND. NLIM(IB).GT.IBB) THEN
                  NLIM(IB)=IBB
                  ISWAP=ISWAP+1
                ENDIF
              ENDIF
 430        CONTINUE
 440      CONTINUE
          DO 450 IB=1,NPVCB
            N=NLIM(IB)
            CALL VF_P1MINI(N,NLIM(IB))
 450      CONTINUE 
          N=ISWAP
          CALL VF_P1SUMI(N,ISWAP)
        ENDIF

CD      -- 気泡番号をユニークにし、空番号をつめる --
        IF (NPROCS.NE.1 .AND. ISWAP.GT.0) THEN
 500      CONTINUE
            L=0
            DO 510 IB=1,NPVCB
              IF (IB.NE.NLIM(IB)) THEN
                IBB=NLIM(IB)
                IF (IBB.NE.NLIM(IBB)) THEN
                  NLIM(IB)=NLIM(IBB)
                  L=L+1
                ENDIF
              ENDIF
 510        CONTINUE
          IF (L.GT.0) GOTO 500
          DO 550 IB=1,NPVCB
            L =0
            L2=0
            DO 520 IBB=IB,NPVCB
              IF (NLIM(IBB).EQ.IB) THEN
                L=1
                GOTO 530
              ENDIF
              IF (L2.EQ.0) THEN
                IF (NLIM(IBB).GT.IB) L2=NLIM(IBB)
              ENDIF
 520        CONTINUE
 530        CONTINUE
            IF (L.EQ.0 .AND. L2.GT.0) THEN
              DO 540 IBB=IB,NPVCB
                IF (NLIM(IBB).EQ.L2) NLIM(IBB)=IB
 540          CONTINUE
            ENDIF
 550      CONTINUE
          IJK=0
          DO 580 K=1,NUMK
            DO 570 J=1,NUMJ
              DO 560 I=1,NUMI
                IJK=IJK+1
                IF (IPVC(IJK).GE.1) IPVC(IJK)=NLIM(IPVC(IJK))
 560          CONTINUE
 570        CONTINUE
 580      CONTINUE
          N=0
          DO 590 IB=1,NPVCB
            IF (N.LT.NLIM(IB)) N=NLIM(IB)
 590      CONTINUE
          NPVCB=N
        ENDIF

CD    -- 入れ替えがあれば1000にもどる --
        IF (NSWAP.GT.MAXPVC) CALL VF_A2ERR('VF_FPVCIP','P.G ERROR.')
      IF (ISWAP.GT.0) GOTO 1000

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
