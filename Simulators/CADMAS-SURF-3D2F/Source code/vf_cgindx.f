      SUBROUTINE VF_CGINDX(UU,VV,WW,GGVEL,NF,INDB0,INDC,INDS,
     &                     INDX,INDY,INDZ,INDX0,INDY0,INDZ0,
     &                     GGV,FF,BCF,IBUF,NWK1,IPVC)

CD=== 概要 ===========================================================

CDT   VF_CGINDX:空隙率を基に面状態インデックスINDX,INDY,INDZを修正する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE  'mpif.h'

CD    -- 引数 --
CD    NF(@FOR-3D@)        : I/O : I*4 : セルの状態を示すインデックス
CD    INDB0(MAXB1,NUMB0)  : I/O : I*4 : 境界面のインデックス(修正前)
CD    INDC(@FOR-3D@)      : I/O : I*4 : セルの計算状態を示すインデックス
CD    INDS(@FOR-1D@)      : OUT : I*4 : 表面セルのI,J,K座標
CD    INDX(@FOR-3D@)      : OUT : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)      : OUT : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)      : OUT : I*4 : z面の状態を示すインデックス
CD    INDX0(@FOR-3D@)     : IN  : I*4 : x面の状態を示すインデックス(修正前)
CD    INDY0(@FOR-3D@)     : IN  : I*4 : y面の状態を示すインデックス(修正前)
CD    INDZ0(@FOR-3D@)     : IN  : I*4 : z面の状態を示すインデックス(修正前)
CD    GGV(@FOR-3D@)       : IN  : R*8 : 空隙率
CD    FF(@FOR-3D@)        : I/O  : R*8 : VOF関数F
CD    BCF(NUMB)           : I/O : R*8 : VOF関数Fの境界値
CD    IBUF(NUMBUF*MAXBUF) : --- : I*4 : 並列用のバッファ
CD    NWK1(@FOR-3D@)      : --- : I*4 : 補正を行うためのワーク
      DIMENSION UU(NUMI,NUMJ,NUMK),VV(NUMI,NUMJ,NUMK)
      DIMENSION WW(NUMI,NUMJ,NUMK)
      DIMENSION GGVEL(3,IPRNP)
      DIMENSION NF   (NUMI,NUMJ,NUMK),INDB0(MAXB1,NUMB0)
      DIMENSION INDC (NUMI,NUMJ,NUMK),INDS (NUMI*NUMJ*NUMK)
      DIMENSION INDX (NUMI,NUMJ,NUMK),INDY (NUMI,NUMJ,NUMK)
      DIMENSION INDZ (NUMI,NUMJ,NUMK)
      DIMENSION INDX0(NUMI,NUMJ,NUMK),INDY0(NUMI,NUMJ,NUMK)
      DIMENSION INDZ0(NUMI,NUMJ,NUMK)
      DIMENSION GGV  (NUMI,NUMJ,NUMK),FF   (NUMI,NUMJ,NUMK)
      DIMENSION BCF  (NUMB)
      DIMENSION IBUF (NUMBUF*MAXBUF) ,NWK1 (NUMI,NUMJ,NUMK)
      DIMENSION IPVC (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================
CC      IDBGF=2001+MYRANK

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 移動構造物セルへとする閾値 --
C@    GVMIN=PLOWER*1.0D0
C@    GVMIN=0.0D0
      GVMIN=0.5D0

CD    -- 初期可 --
      NUMB=NUMB0
      DO 12 K=1,NUMK
        DO 11 J=1,NUMJ
          DO 10 I=1,NUMI
            INDX(I,J,K)=INDX0(I,J,K)
            INDY(I,J,K)=INDY0(I,J,K)
            INDZ(I,J,K)=INDZ0(I,J,K)
 10       CONTINUE
 11     CONTINUE
 12   CONTINUE
      DO 15 IB=1,NUMB0
        IF (INDB0(1,IB).LT.0) INDB0(1,IB)=-INDB0(1,IB)-1
 15   CONTINUE

CD    -- 空隙率の変化によるNFの変更 --
      L=0
      DO 130 IB=1,IPRNB
        DO 120 K=IPRARA(3,IB),IPRARA(6,IB)
          DO 110 J=IPRARA(2,IB),IPRARA(5,IB)
            DO 100 I=IPRARA(1,IB),IPRARA(4,IB)
              L=L+1
C             IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
C               IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
C                 IL=I-IP
C                 JL=J-JP
              IL=I-IP
              JL=J-JP
              IF (MYJS.LE.JL .AND. JL.LE.MYJE) THEN
                IF (MYIS.LE.IL .AND. IL.LE.MYIE) THEN
                  IF (GGV(IL,JL,K).LE.GVMIN) THEN
                    GGV(IL,JL,K)=GVMIN
C@                  IF (NF(IL,JL,K).GT.-1) THEN
                    IF (NF(IL,JL,K).NE.-1) THEN
                      NF(IL,JL,K)=-2
                      INDC(IL,JL,K)=-1
                      FF(IL,JL,K)=1.0D0
C@@@@@@@@@@
                      UU(I-IP  ,J-JP,K)=GGVEL(1,L)
                      UU(I-IP+1,J-JP,K)=GGVEL(1,L)
                      VV(I-IP,J-JP  ,K)=GGVEL(2,L)
                      VV(I-IP,J-JP+1,K)=GGVEL(2,L)
                      WW(I-IP,J-JP,K  )=GGVEL(3,L)
                      WW(I-IP,J-JP,K+1)=GGVEL(3,L)
C@@@@@@@@@@
CC                      IF (NNOW.GT.260) THEN
CC                        WRITE(IDBGF,'(A,3I,1P,E10.2)')
CC     &                              'OBST',IL,JL,K,GGV(IL,JL,K)
CC                      ENDIF
                    ENDIF
                  ELSE
                    IF (NF(IL,JL,K).EQ.-2) THEN
C                     -- （簡易モデル）水平方向隣接セルのFの平均 --
                      NUM=0
                      FF1=0.0D0
                      IF (NF(IL-1,JL  ,K).GT.-1) THEN
                        NUM=NUM+1
                        FF1=FF1+FF(IL-1,JL  ,K)
                      ENDIF
                      IF (NF(IL+1,JL  ,K).GT.-1) THEN
                        NUM=NUM+1
                        FF1=FF1+FF(IL+1,JL  ,K)
                      ENDIF
                      IF (NF(IL  ,JL-1,K).GT.-1) THEN
                        NUM=NUM+1
                        FF1=FF1+FF(IL  ,JL-1,K)
                      ENDIF
                      IF (NF(IL  ,JL+1,K).GT.-1) THEN
                        NUM=NUM+1
                        FF1=FF1+FF(IL  ,JL+1,K)
                      ENDIF
                      NF(IL,JL,K)=0
                      INDC(IL,JL,K)=0
                      FF1=FF1/DBLE(NUM)
                      IF( FF1.LT.PLOWER ) FF1=0.0D0
                      FF(IL,JL,K)=FF1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@                      FF(IL,JL,K)=1.0D0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

CC                      IF (NNOW.GT.260) THEN
CC                        WRITE(IDBGF,'(A,3I,1P,E10.2)')
CC     &                              'Liq.',IL,JL,K,GGV(IL,JL,K)
CC                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
 100        CONTINUE
 110      CONTINUE
 120    CONTINUE
 130  CONTINUE
                    
      CALL VF_FPVCIP(NF,IPVC,IBUF,NWK1)
      WRITE(*,*) '@@@ NPVCB=',NPVCB
      DO 145 L=1,NPVCB
        WRITE(*,*) '@@@       ',L,IPVCBC(L)
 145  CONTINUE

      DO 142 K=2,NUMK-1
        DO 141 J=2,NUMJ-1
          DO 140 I=2,NUMI-1
            L=IPVC(I,J,K)
            IF (L.GE.1) THEN
              IF (IPVCBC(L).LE.1000) THEN
                NF  (I,J,K)=-2
                INDC(I,J,K)=-1
                FF  (I,J,K)=1.0D0
              ENDIF
            ENDIF
 140      CONTINUE
 141    CONTINUE
 142  CONTINUE

CD    -- NFおよびINDCの設定 --
      CALL VF_FNFINI(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)
      CALL VF_FNFPRV(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)
      CALL VF_FNFBUB(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF,NWK1)
      CALL VF_CINDC(NF,INDC)
                    
CD    -- INDXを設定 --
      DO 230 IB=1,IPRNB
        DO 220 K=IPRARA(3,IB),IPRARA(6,IB)
          DO 210 J=IPRARA(2,IB),IPRARA(5,IB)
            DO 200 I=IPRARA(1,IB),IPRARA(4,IB)+1
C             IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
C               IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
C                 IL=I-IP
C                 JL=J-JP
              IL=I-IP
              JL=J-JP
              IF (MYJS.LE.JL .AND. JL.LE.MYJE) THEN
                IF (MYIS.LE.IL .AND. IL.LE.MYIE+1) THEN
                  INDL=INDX(IL,JL,K)
                  IF (INDL.NE.-1) THEN
                    N1=NF(IL-1,JL,K)
                    N2=NF(IL  ,JL,K)
                    IF (N1.LE.-1 .AND. N2.LE.-1) THEN
                      IF (INDL.GT.0) INDB0(1,INDL)=-INDB0(1,INDL)-1
                      INDX(IL,JL,K)=-2
CC                      WRITE(2003,'(A,3I,A,I)') 'INDX',IL,JL,K,'-',INDL
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE
 230  CONTINUE

      DO 280 IB=1,IPRNB
        DO 270 K=IPRARA(3,IB),IPRARA(6,IB)
          DO 260 J=IPRARA(2,IB),IPRARA(5,IB)
            DO 250 I=IPRARA(1,IB),IPRARA(4,IB)+1
C             IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
C               IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
C                 IL=I-IP
C                 JL=J-JP
              IL=I-IP
              JL=J-JP
              IF (MYJS.LE.JL .AND. JL.LE.MYJE) THEN
                IF (MYIS.LE.IL .AND. IL.LE.MYIE+1) THEN
                  IF (      INDX(IL,JL,K).GT.-1
     &                .AND. INDX(IL,JL,K).LE.NUMB0) THEN
                    N1=NF(IL-1,JL,K)
                    N2=NF(IL  ,JL,K)
                    IF (N1.EQ.-2 .OR. N2.EQ.-2) THEN
                      IF     (IL.EQ.2    .AND. N2.LE.-1) THEN
                      ELSEIF (IL.EQ.NUMI .AND. N1.LE.-1) THEN
                      ELSE
                        NUMB=NUMB+1
                        INDX(IL,JL,K)=NUMB
CC                        WRITE(2003,'(A,3I,A,I)') 'INDX',IL,JL,K,'+',NUMB
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
 250        CONTINUE
 260      CONTINUE
 270    CONTINUE
 280  CONTINUE

CD    -- INDYを設定 --
      DO 330 IB=1,IPRNB
        DO 320 K=IPRARA(3,IB),IPRARA(6,IB)
          DO 310 J=IPRARA(2,IB),IPRARA(5,IB)+1
            DO 300 I=IPRARA(1,IB),IPRARA(4,IB)
C             IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
C               IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
C                 IL=I-IP
C                 JL=J-JP
              IL=I-IP
              JL=J-JP
              IF (MYJS.LE.JL .AND. JL.LE.MYJE+1) THEN
                IF (MYIS.LE.IL .AND. IL.LE.MYIE) THEN
                  INDL=INDY(IL,JL,K)
                  IF (INDL.NE.-1) THEN
                    N1=NF(IL,JL-1,K)
                    N2=NF(IL,JL  ,K)
                    IF (N1.LE.-1 .AND. N2.LE.-1) THEN
                      IF (INDL.GT.0) INDB0(1,INDL)=-INDB0(1,INDL)-1
                      INDY(IL,JL,K)=-2
CC                      WRITE(2003,'(A,3I,A,I)') 'INDY',IL,JL,K,'-',INDL
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
 300        CONTINUE
 310      CONTINUE
 320    CONTINUE
 330  CONTINUE

      DO 380 IB=1,IPRNB
        DO 370 K=IPRARA(3,IB),IPRARA(6,IB)
          DO 360 J=IPRARA(2,IB),IPRARA(5,IB)+1
            DO 350 I=IPRARA(1,IB),IPRARA(4,IB)
C             IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
C               IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
C                 IL=I-IP
C                 JL=J-JP
              IL=I-IP
              JL=J-JP
              IF (MYJS.LE.JL .AND. JL.LE.MYJE+1) THEN
                IF (MYIS.LE.IL .AND. IL.LE.MYIE) THEN
                  IF (      INDY(IL,JL,K).GT.-1
     &                .AND. INDY(IL,JL,K).LE.NUMB0) THEN
                    N1=NF(IL,JL-1,K)
                    N2=NF(IL,JL  ,K)
                    IF (N1.EQ.-2 .OR. N2.EQ.-2) THEN
                      IF     (JL.EQ.2    .AND. N2.LE.-1) THEN
                      ELSEIF (JL.EQ.NUMJ .AND. N1.LE.-1) THEN
                      ELSE
                        NUMB=NUMB+1
                        INDY(IL,JL,K)=NUMB
CC                        WRITE(2003,'(A,3I,A,I)') 'INDY',IL,JL,K,'+',NUMB
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
 350        CONTINUE
 360      CONTINUE
 370    CONTINUE
 380  CONTINUE

CD    -- INDZを設定 --
      DO 430 IB=1,IPRNB
        DO 420 K=IPRARA(3,IB),IPRARA(6,IB)+1
          DO 410 J=IPRARA(2,IB),IPRARA(5,IB)
            DO 400 I=IPRARA(1,IB),IPRARA(4,IB)
C             IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
C               IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
C                 IL=I-IP
C                 JL=J-JP
              IL=I-IP
              JL=J-JP
              IF (MYJS.LE.JL .AND. JL.LE.MYJE) THEN
                IF (MYIS.LE.IL .AND. IL.LE.MYIE) THEN
                  INDL=INDZ(IL,JL,K)
                  IF (INDL.NE.-1) THEN
                    N1=NF(IL,JL,K-1)
                    N2=NF(IL,JL,K  )
                    IF (N1.LE.-1 .AND. N2.LE.-1) THEN
                      IF (INDL.GT.0) INDB0(1,INDL)=-INDB0(1,INDL)-1
                      INDZ(IL,JL,K)=-2
CC                      WRITE(2003,'(A,3I,A,I)') 'INDZ',IL,JL,K,'-',INDL
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
 400        CONTINUE
 410      CONTINUE
 420    CONTINUE
 430  CONTINUE

      DO 480 IB=1,IPRNB
        DO 470 K=IPRARA(3,IB),IPRARA(6,IB)+1
          DO 460 J=IPRARA(2,IB),IPRARA(5,IB)
            DO 450 I=IPRARA(1,IB),IPRARA(4,IB)
C             IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
C               IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
C                 IL=I-IP
C                 JL=J-JP
              IL=I-IP
              JL=J-JP
              IF (MYJS.LE.JL .AND. JL.LE.MYJE) THEN
                IF (MYIS.LE.IL .AND. IL.LE.MYIE) THEN
                  IF (      INDZ(IL,JL,K).GT.-1 
     &                .AND. INDZ(IL,JL,K).LE.NUMB0) THEN
                    N1=NF(IL,JL,K-1)
                    N2=NF(IL,JL,K  )
                    IF (N1.EQ.-2 .OR. N2.EQ.-2) THEN
                      NUMB=NUMB+1
                      INDZ(IL,JL,K)=NUMB
CC                      WRITE(2003,'(A,3I,A,I)') 'INDZ',IL,JL,K,'+',NUMB
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
 450        CONTINUE
 460      CONTINUE
 470    CONTINUE
 480  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
