      SUBROUTINE VF_BWPP(PP,BCP,INDB)

CD=== 概要 ===========================================================

CDT   VF_BWPP:境界面の圧力を設定する
CD      (1)境界面の値は参照されないため、勾配ゼロを仮定

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

CD    -- 引数 --
CD    PP(@FOR-3D@)     : IN  : R*8 : 圧力
CD    BCP(NUMB)        : OUT : R*8 : 圧力の境界値
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION PP(NUMI,NUMJ,NUMK)
      DIMENSION BCP(NUMB)
      DIMENSION INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 境界面の圧力を設定する(境界面のみのループ) --
      DO 100 L=1,NUMB
        IJK=INDB(1,L)
        NS =INDB(2,L)
        K  =(IJK-1)/(NUMI*NUMJ)+1
        IJK=IJK-NUMI*NUMJ*(K-1)
        J  =(IJK-1)/NUMI+1
        I  =IJK-NUMI*(J-1)

CD      -- 勾配ゼロ --
        IF     (NS.EQ.1) THEN
          BCP(L)=PP(I  ,J,K)
        ELSEIF (NS.EQ.2) THEN
          BCP(L)=PP(I-1,J,K)
        ELSEIF (NS.EQ.3) THEN
          BCP(L)=PP(I,J  ,K)
        ELSEIF (NS.EQ.4) THEN
          BCP(L)=PP(I,J-1,K)
        ELSEIF (NS.EQ.5) THEN
          BCP(L)=PP(I,J,K  )
        ELSEIF (NS.EQ.6) THEN
          BCP(L)=PP(I,J,K-1)
        ENDIF

 100  CONTINUE

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
