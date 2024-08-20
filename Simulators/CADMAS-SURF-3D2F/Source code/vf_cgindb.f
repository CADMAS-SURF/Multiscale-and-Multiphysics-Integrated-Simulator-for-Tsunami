      SUBROUTINE VF_CGINDB(NF,INDX,INDY,INDZ,INDB0,INDB,INDWK)

CD=== 概要 ===========================================================

CDT   VF_CGINDB:移動障害物の境界面のインデックスINDBを設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'

CD    -- 引数 --
CD    NF(@FOR-3D@)      : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)    : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)    : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)    : IN  : I*4 : z面の状態を示すインデックス
CD    INDB0(MAXB1,NUMB0): IN  : I*4 : 境界面のインデックス
CD    INDB(MAXM1,NUMB)  : OUT : I*4 : 境界面のインデックス(移動障害物処理用)
CD    INDWK(@FOR-3D@)   : --- : I*4 : (作業用)移動障害物データへのポインタ
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB0(MAXB1,NUMB0),INDB(MAXB1,NUMB)
      DIMENSION INDWK(NUMI,NUMJ,NUMK)

      DIMENSION IDUM(10),JDUM(10)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 初期可 --
      DO 603 K=1,NUMK
        DO 602 J=1,NUMJ
          DO 601 I=1,NUMI
            INDWK(I,J,K)=0
 601      CONTINUE
 602    CONTINUE
 603  CONTINUE

CD    -- 移動障害物データへのポインタの設定
      L=0
      DO 530 IB=1,IPRNB
        DO 520 K=IPRARA(3,IB),IPRARA(6,IB)
          DO 510 J=IPRARA(2,IB),IPRARA(5,IB)
            DO 500 I=IPRARA(1,IB),IPRARA(4,IB)
              L=L+1
              IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                  IF (NF(I-IP,J-JP,K).NE.-1) INDWK(I-IP,J-JP,K)=L
                ENDIF
              ENDIF
 500        CONTINUE
 510      CONTINUE
 520    CONTINUE
 530  CONTINUE

CD    -- 既存境界面のデータの複写 --
      DO 20 IB=1,NUMB0
        DO 10 I=1,MAXB1
          INDB(I,IB)=INDB0(I,IB)
 10     CONTINUE
 20   CONTINUE

CD    -- x面に関する設定 --
      DO 130 K=2,NUMK-1
        DO 120 J=2,NUMJ-1
          DO 110 I=2,NUMI
            N2=NF  (I,J,K)
            NB=INDX(I,J,K)
            IF (NB.GT.NUMB0) THEN
              INDB(1,NB)=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
              IF (N2.GT.-1) THEN
                INDB(2,NB)=1
                INDB(6,NB)=INDWK(I-1,J,K)
              ELSE
                INDB(2,NB)=2
                INDB(6,NB)=INDWK(I,J,K)
              ENDIF
              INDB(3,NB)=IPRTB(1)
              INDB(4,NB)=IPRTB(2)
              INDB(5,NB)=IPRTB(3)
            ENDIF
 110      CONTINUE
 120    CONTINUE
 130  CONTINUE

CD    -- y面に関する設定 --
      DO 230 K=2,NUMK-1
        DO 220 J=2,NUMJ
          DO 210 I=2,NUMI-1
            N2=NF  (I,J,K)
            NB=INDY(I,J,K)
            IF (NB.GT.NUMB0) THEN
              INDB(1,NB)=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
              IF (N2.GT.-1) THEN
                INDB(2,NB)=3
                INDB(6,NB)=INDWK(I,J-1,K)
              ELSE
                INDB(2,NB)=4
                INDB(6,NB)=INDWK(I,J,K)
              ENDIF
              INDB(3,NB)=IPRTB(1)
              INDB(4,NB)=IPRTB(2)
              INDB(5,NB)=IPRTB(3)
            ENDIF
 210      CONTINUE
 220    CONTINUE
 230  CONTINUE

CD    -- z面に関する設定 --
      DO 330 K=2,NUMK
        DO 320 J=2,NUMJ-1
          DO 310 I=2,NUMI-1
            N2=NF  (I,J,K)
            NB=INDZ(I,J,K)
            IF (NB.GT.NUMB0) THEN
              INDB(1,NB)=I+NUMI*(J-1)+NUMI*NUMJ*(K-1)
              IF (N2.GT.-1) THEN
                INDB(2,NB)=5
                INDB(6,NB)=INDWK(I,J,K-1)
              ELSE
                INDB(2,NB)=6
                INDB(6,NB)=INDWK(I,J,K)
              ENDIF
              INDB(3,NB)=IPRTB(1)
              INDB(4,NB)=IPRTB(2)
              INDB(5,NB)=IPRTB(3)
            ENDIF
 310      CONTINUE
 320    CONTINUE
 330  CONTINUE

C     -- デバッグ出力 --
C#      WRITE(2002,'(A,I,A,I,A,I)')
C#     &  ' <CGINDB> INDB(',NUMB,') <= INDB0(',NUMB0,') ADD=',NUMB-NUMB0
C#      IF (NUMB.GT.NUMB0) THEN
C#        DO 410 IB=1,NUMB
C#          IJK=ABS(INDB(1,IB))
C#          IF (IJK.NE.0) THEN
C#            K  =(IJK-1)/(NUMI*NUMJ)+1
C#            IJK=IJK-NUMI*NUMJ*(K-1)
C#            J  =(IJK-1)/NUMI+1
C#            I  =IJK-NUMI*(J-1)
C#          ELSE
C#            I=0
C#            J=0
C#            K=0
C#          ENDIF
C#          IF (IB.LE.NUMB0) THEN
C#CDBG            DO 400 L=1,MAXB1
C#CDBG              IDUM(L)=INDB(L,IB)
C#CDBG              JDUM(L)=INDB0(L,IB)
C#CDBG 400        CONTINUE
C#CDBG            WRITE(2002,'(I,A,3I4,A,I,5I4,A,I,5I4)')
C#CDBG     &                  IB,':',I,J,K,':',(IDUM(L),L=1,MAXB1),
C#CDBG     &                              '<=',(JDUM(L),L=1,MAXB1)
C#          ELSE
C#            DO 405 L=1,MAXB1
C#              IDUM(L)=INDB(L,IB)
C# 405        CONTINUE
C#            WRITE(2002,'(I,A,3I4,A,I,5I4)')
C#     &                  IB,':',I,J,K,':',(IDUM(L),L=1,MAXB1)
C#          ENDIF
C# 410    CONTINUE
C#       ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
