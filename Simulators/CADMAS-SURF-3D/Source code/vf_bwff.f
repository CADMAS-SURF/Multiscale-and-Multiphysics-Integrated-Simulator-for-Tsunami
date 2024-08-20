      SUBROUTINE VF_BWFF(ZZ,FF,BCF,INDX,INDY,INDB)

CD=== 概要 ===========================================================

CDT   VF_BWFF:境界面のVOF関数Fを設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_ASTOCR.h'

CD    -- 引数 --
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION ZZ(MAXG1,NUMK)
      DIMENSION FF(NUMI,NUMJ,NUMK),BCF(NUMB)
      DIMENSION INDX(NUMI,NUMJ,NUMK),INDY(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 境界面のVOF関数Fを設定する(境界面のみのループ) --
      DO 100 L=1,NUMB
        IJK=INDB(1,L)
        NS =INDB(2,L)
        IB =INDB(4,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)

CD      -- 値固定 --
        IF     (IB.EQ.1) THEN
          BCF(L)=BCF(L)

CD      -- フリー --
        ELSEIF (IB.EQ.2) THEN
          IF     (NS.EQ.1) THEN
            BCF(L)=FF(I  ,J,K)
          ELSEIF (NS.EQ.2) THEN
            BCF(L)=FF(I-1,J,K)
          ELSEIF (NS.EQ.3) THEN
            BCF(L)=FF(I,J  ,K)
          ELSEIF (NS.EQ.4) THEN
            BCF(L)=FF(I,J-1,K)
          ELSEIF (NS.EQ.5) THEN
            BCF(L)=FF(I,J,K  )
          ELSEIF (NS.EQ.6) THEN
            BCF(L)=FF(I,J,K-1)
          ENDIF

CD      -- 造波境界 --
        ELSEIF (IB.EQ.5) THEN
          IF     (NS.EQ.1) THEN
            BCF(L)=FF(I  ,J,K)
          ELSEIF (NS.EQ.2) THEN
            BCF(L)=FF(I-1,J,K)
          ELSEIF (NS.EQ.3) THEN
            BCF(L)=FF(I,J  ,K)
          ELSEIF (NS.EQ.4) THEN
            BCF(L)=FF(I,J-1,K)
          ELSE
            CALL VF_A2ERR('VF_BWFF','P.G ERROR.')
          ENDIF

CD      -- 放射境界 --
        ELSEIF (IB.EQ.7) THEN
          IF     (NS.EQ.1) THEN
            BCF(L)=FF(I  ,J,K)
          ELSEIF (NS.EQ.2) THEN
            BCF(L)=FF(I-1,J,K)
          ELSEIF (NS.EQ.3) THEN
            BCF(L)=FF(I,J  ,K)
          ELSEIF (NS.EQ.4) THEN
            BCF(L)=FF(I,J-1,K)
          ELSE
            CALL VF_A2ERR('VF_BWFF','P.G ERROR.')
          ENDIF

CD      -- プログラムエラー --
        ELSE
          CALL VF_A2ERR('VF_BWFF','P.G ERROR.')
        ENDIF
 100  CONTINUE

C     --- STOC(F) ---
      IF (NB_SC.GT.0) THEN
        I1=MIST(1)
        I2=MIST(NIST+1)
        DO 630 KST=1,NKST
          DO 620 JST=1,NJST
            F1=UWST(JST,KST,4)
            F2=UEST(JST,KST,4)
            DO 610 J=MJST(JST),MJST(JST+1)-1
              DZ=ZZ(1,MKST(KST+1))-ZZ(1,MKST(KST))
              Z1=F1*DZ
              Z2=F2*DZ
              DO 600 K=MKST(KST),MKST(KST+1)-1
                D=ZZ(2,K)
                L=INDX(I1,J,K)
                IF     (Z1.GE.D    ) THEN
                  IF (L.GE.1 .AND. IWST.EQ.1) BCF(L)=1.0D0
                  Z1=Z1-D
                ELSEIF (Z1.LE.0.0D0) THEN
                  IF (L.GE.1 .AND. IWST.EQ.1) BCF(L)=0.0D0
                ELSE
                  IF (L.GE.1 .AND. IWST.EQ.1) BCF(L)=Z1/D
                  Z1=0.0D0
                ENDIF
                L=INDX(I2,J,K)
                IF     (Z2.GE.D    ) THEN
                  IF (L.GE.1 .AND. IEST.EQ.1) BCF(L)=1.0D0
                  Z2=Z2-D
                ELSEIF (Z2.LE.0.0D0) THEN
                  IF (L.GE.1 .AND. IEST.EQ.1) BCF(L)=0.0D0
                ELSE
                  IF (L.GE.1 .AND. IEST.EQ.1) BCF(L)=Z2/D
                  Z2=0.0D0
                ENDIF
 600          CONTINUE
 610        CONTINUE
 620      CONTINUE
 630    CONTINUE
        J1=MJST(1)
        J2=MJST(NJST+1)
        DO 730 KST=1,NKST
          DO 720 IST=1,NIST
            F1=VSST(IST,KST,4)
            F2=VNST(IST,KST,4)
            DO 710 I=MIST(IST),MIST(IST+1)-1
              DZ=ZZ(1,MKST(KST+1))-ZZ(1,MKST(KST))
              Z1=F1*DZ
              Z2=F2*DZ
              DO 700 K=MKST(KST),MKST(KST+1)-1
                D=ZZ(2,K)
                L=INDY(I,J1,K)
                IF     (Z1.GE.D    ) THEN
                  IF (L.GE.1 .AND. JSST.EQ.1) BCF(L)=1.0D0
                  Z1=Z1-D
                ELSEIF (Z1.LE.0.0D0) THEN
                   IF (L.GE.1 .AND. JSST.EQ.1) BCF(L)=0.0D0
                ELSE
                   IF (L.GE.1 .AND. JSST.EQ.1) BCF(L)=Z1/D
                   Z1=-1.0D0
                ENDIF
                L=INDY(I,J2,K)
                IF     (Z2.GE.D    ) THEN
                  IF (L.GE.1 .AND. JNST.EQ.1) BCF(L)=1.0D0
                  Z2=Z2-D
                ELSEIF (Z2.LE.0.0D0) THEN
                  IF (L.GE.1 .AND. JNST.EQ.1) BCF(L)=0.0D0
                ELSE
                  IF (L.GE.1 .AND. JNST.EQ.1) BCF(L)=Z2/D
                  Z2=-1.0D0
                ENDIF
 700          CONTINUE
 710        CONTINUE
 720      CONTINUE
 730    CONTINUE
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
