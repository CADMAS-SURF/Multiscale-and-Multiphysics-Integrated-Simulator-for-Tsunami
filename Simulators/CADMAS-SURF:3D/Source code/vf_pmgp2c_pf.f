      SUBROUTINE VF_PMGP2C_PF(BC,
     &                        XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                        BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                        I,J,K,IDIR,MGNV)

CD=== 概要 ===========================================================

CDT   VF_PMGP2C_PF:マルチグリッド環境の子へ送信する格子面の値を設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ATIMER.h' 

CD    -- 引数 --
CD    BC(MGNV)         : OUT : R*8 : 格子面上の値
CD                                 : |IDIR|=1
CD                                 :   BC(1)=F BC(2)=U BC(3)=V BC(4)=W
CD                                 :   BC(5)=DF/DY*
CD                                 :   BC(6)=DU/DY* BC(7)=DU/DZ*
CD                                 :   BC(8)=DV/DY* BC(9)=DW/DZ*
CD                                 : |IDIR|=2
CD                                 :   BC(1)=F BC(2)=U BC(3)=V BC(4)=W
CD                                 :   BC(5)=DF/DX*
CD                                 :   BC(6)=DV/DX* BC(7)=DU/DZ*
CD                                 :   BC(8)=DU/DX* BC(9)=DW/DZ*
CD                                 : DX*, DY*, DZ* 親格子幅で正規化された値
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION BC(MGNV)
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION GGV(NUMI,NUMJ,NUMK)
      DIMENSION GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
      DIMENSION BCU (NUMB),BCV(NUMB),BCW(NUMB),BCF(NUMB)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)
C
C
C==== 実行 ===========================================================
      IUP = 0
      IF(IDIR.LT.0) IUP = -1
C
C X
      IF(ABS(IDIR).EQ.1) THEN
        L = INDX(I,J,K)
C
C    障害物面
        IF(L.LT.0) THEN
          BC(1) = 0.0D0
          BC(2) = 0.0D0
          BC(3) = 0.0D0
          BC(4) = 0.0D0
          BC(5) = 0.0D0
          BC(6) = 0.0D0
          BC(7) = 0.0D0
          BC(8) = 0.0D0
          BC(9) = 0.0D0
C
C    境界面
        ELSE IF(L.GT.0) THEN
          BC(1) = BCF(L)
          BC(2) = BCU(L)
          BC(3) = BCV(L)
          BC(4) = BCW(L)
          BC(5) = 0.0D0
          BC(6) = 0.0D0
          BC(7) = 0.0D0
          BC(8) = 0.0D0
          BC(9) = 0.0D0
C
C    通常面
        ELSE

          IF(NF(I+IUP,J+1,K).LT.0) THEN
            FJP = FF(I+IUP,J,K)
          ELSE
            FJP = 0.5D0*(FF(I+IUP,J+1,K) + FF(I+IUP,J,K))
          END IF
          IF(NF(I+IUP,J-1,K).LT.0) THEN
            FJM = FF(I+IUP,J,K)
          ELSE
            FJM = 0.5D0*(FF(I+IUP,J-1,K) + FF(I+IUP,J,K))
          END IF

          L = INDX(I,J+1,K)
          IF(L.LT.0) THEN
            UJP = UU(I,J,K)
          ELSE IF(L.GT.0) THEN
            UJP = BCU(L)
          ELSE
            UJP = 0.5D0*(UU(I,J+1,K) + UU(I,J,K))
          END IF

          L = INDX(I,J-1,K)
          IF(L.LT.0) THEN
            UJM = UU(I,J,K)
          ELSE IF(L.GT.0) THEN
            UJM = BCU(L)
          ELSE
            UJM = 0.5D0*(UU(I,J-1,K) + UU(I,J,K))
          END IF

          L = INDX(I,J,K+1)
          IF(L.LT.0) THEN
            UKP = UU(I,J,K)
          ELSE IF(L.GT.0) THEN
            UKP = BCU(L)
          ELSE
            UKP = 0.5D0*(UU(I,J,K+1) + UU(I,J,K))
          END IF

          L = INDX(I,J,K-1)
          IF(L.LT.0) THEN
            UKM = UU(I,J,K)
          ELSE IF(L.GT.0) THEN
            UKM = BCU(L)
          ELSE
            UKM = 0.5D0*(UU(I,J,K-1) + UU(I,J,K))
          END IF

C---------------------------------------------------
C         FLF = 0.5D0*(FF(I-1,J,K) + FF(I,J,K))
C         FLF = FF(I+IUP,J,K)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  DTNOW*GGX(I,J,K)*UU(I,J,K)がunderflowをおこすとゼロ割する
C
C         IF(IDIR*UU(I,J,K).LT.0.0D0) THEN
          IF(IDIR*DTNOW*GGX(I,J,K)*UU(I,J,K).LT.-1.0D-20) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            CALL VF_PMGFLX(FLF,DTNOW*GGX(I,J,K)*UU(I,J,K)
     &                    ,FF,BCF,XX,GGV,FLOWER,NF,INDX
     &                    ,I,J,K,NUMI,NUMJ,NUMK,MAXG1,NUMB)
          ELSE
            FLF = FF(I+IUP,J,K)
          END IF
C---------------------------------------------------
          BC(1) = FLF
          BC(2) = UU(I,J,K)
C---------------------------------------------------
          IF(UU(I,J,K).EQ.0.0D0 .AND. K.GT.2) BC(2) = UU(I,J,K-1)
C---------------------------------------------------
C         BC(3) = 0.25D0*(VV(I-1,J  ,K)+VV(I,J  ,K)
C    &                   +VV(I-1,J+1,K)+VV(I,J+1,K))
C         BC(4) = 0.25D0*(WW(I-1,J,K  )+WW(I,J,K  )
C    &                   +WW(I-1,J,K+1)+WW(I,J,K+1))
          BC(3) = 0.5D0*(VV(I+IUP,J,K)+VV(I+IUP,J+1,K))
          BC(4) = 0.5D0*(WW(I+IUP,J,K)+WW(I+IUP,J,K+1))
          BC(5) = FJP-FJM
          BC(6) = UJP-UJM
          BC(7) = UKP-UKM
          BC(8) = VV(I+IUP,J+1,K) - VV(I+IUP,J,K)
          BC(9) = WW(I+IUP,J,K+1) - WW(I+IUP,J,K)
C         BC(5) = 0.0D0
C         BC(6) = 0.0D0
C         BC(7) = 0.0D0
C         BC(8) = 0.0D0
C         BC(9) = 0.0D0
        END IF
C
C Y
      ELSE IF(ABS(IDIR).EQ.2) THEN 
        L = INDY(I,J,K)
C
C    障害物面
        IF(L.LT.0) THEN
          BC(1) = 0.0D0
          BC(2) = 0.0D0
          BC(3) = 0.0D0
          BC(4) = 0.0D0
          BC(5) = 0.0D0
          BC(6) = 0.0D0
          BC(7) = 0.0D0
          BC(8) = 0.0D0
          BC(9) = 0.0D0
C
C    境界面
        ELSE IF(L.GT.0) THEN
          BC(1) = BCF(L)
          BC(2) = BCU(L)
          BC(3) = BCV(L)
          BC(4) = BCW(L)
          BC(5) = 0.0D0
          BC(6) = 0.0D0
          BC(7) = 0.0D0
          BC(8) = 0.0D0
          BC(9) = 0.0D0
C
C    通常面
        ELSE

          IF(NF(I+1,J+IUP,K).LT.0) THEN
            FIP = FF(I,J+IUP,K)
          ELSE
            FIP = 0.5D0*(FF(I+1,J+IUP,K) + FF(I,J+IUP,K))
          END IF
          IF(NF(I-1,J+IUP,K).LT.0) THEN
            FIM = FF(I,J+IUP,K)
          ELSE
            FIM = 0.5D0*(FF(I-1,J+IUP,K) + FF(I,J+IUP,K))
          END IF

          L = INDX(I+1,J,K)
          IF(L.LT.0) THEN
            VIP = VV(I,J,K)
          ELSE IF(L.GT.0) THEN
            VIP = BCV(L)
          ELSE
            VIP = 0.5D0*(VV(I+1,J,K) + VV(I,J,K))
          END IF

          L = INDX(I-1,J,K)
          IF(L.LT.0) THEN
            VIM = VV(I,J,K)
          ELSE IF(L.GT.0) THEN
            VIM = BCV(L)
          ELSE
            VIM = 0.5D0*(VV(I-1,J,K) + VV(I,J,K))
          END IF

          L = INDX(I,J,K+1)
          IF(L.LT.0) THEN
            VKP = VV(I,J,K)
          ELSE IF(L.GT.0) THEN
            VKP = BCV(L)
          ELSE
            VKP = 0.5D0*(VV(I,J,K+1) + VV(I,J,K))
          END IF

          L = INDX(I,J,K-1)
          IF(L.LT.0) THEN
            VKM = VV(I,J,K)
          ELSE IF(L.GT.0) THEN
            VKM = BCV(L)
          ELSE
            VKM = 0.5D0*(VV(I,J,K-1) + VV(I,J,K))
          END IF

C---------------------------------------------------
C         FLF = 0.5D0*(FF(I,J-1,K)+FF(I,J,K))
C         FLF = FF(I,J+IUP,K)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  DTNOW*GGY(I,J,K)*VV(I,J,K)がunderflowをおこすとゼロ割する
C
C         IF(IDIR*VV(I,J,K).LT.0.0D0) THEN
          IF(IDIR*DTNOW*GGY(I,J,K)*VV(I,J,K).LT.-1.0D-20) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            CALL VF_PMGFLY(FLF,DTNOW*GGY(I,J,K)*VV(I,J,K)
     &                    ,FF,BCF,YY,GGV,FLOWER,NF,INDY
     &                    ,I,J,K,NUMI,NUMJ,NUMK,MAXG1,NUMB)
          ELSE
            FLF = FF(I,J+IUP,K)
          END IF
C---------------------------------------------------
          BC(1) = FLF
C         BC(2) = 0.25D0*(UU(I  ,J-1,K)+UU(I  ,J,K)
C    &                   +UU(I+1,J-1,K)+UU(I+1,J,K))
          BC(2) = 0.5D0*(UU(I,J+IUP,K)+UU(I+1,J+IUP,K))
          BC(3) = VV(I,J,K)
C---------------------------------------------------
          IF(VV(I,J,K).EQ.0.0D0 .AND. K.GT.2) BC(3) = VV(I,J,K-1)
C---------------------------------------------------
C         BC(4) = 0.25D0*(WW(I,J-1,K  )+WW(I,J,K  )
C    &                   +WW(I,J-1,K+1)+WW(I,J,K+1))
          BC(4) = 0.5D0*(WW(I,J+IUP,K)+WW(I,J+IUP,K+1))
          BC(5) = FIP-FIM
          BC(6) = VIP-VIM
          BC(7) = VKP-VKM
          BC(8) = UU(I+1,J+IUP,K  ) - UU(I,J+IUP,K)
          BC(9) = WW(I  ,J+IUP,K+1) - WW(I,J+IUP,K)
C         BC(5) = 0.0D0
C         BC(6) = 0.0D0
C         BC(7) = 0.0D0
C         BC(8) = 0.0D0
C         BC(9) = 0.0D0
        END IF
C        
      END IF

C     -- 実行文の終了 --

      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
