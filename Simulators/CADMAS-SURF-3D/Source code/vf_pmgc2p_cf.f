      SUBROUTINE VF_PMGC2P_CF(BC,
     &                        XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                        XPF,YPF,ZPF,IPF,JPF,KPF,
     &                        BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                        IP,JP,KP,IDIR,MGNV)

CD=== 概要 ===========================================================

CDT   VF_PMGC2P_CF:マルチグリッド環境の親へ送信する格子面の値を設定する

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
CD                                 : BC(1)=F BC(2)=U BC(3)=V BC(4)=W
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
CD    XPF(NUMI)        : IN  : R*8 : x方向の親格子に対する補間係数
CD    YPF(NUMJ)        : IN  : R*8 : y方向の親格子に対する補間係数
CD    ZPF(NUMK)        : IN  : R*8 : z方向の親格子に対する補間係数
CD    IPF(MGPINF(1))   : IN  : I*4 : x方向の親格子1に対する格子の数
CD    JPF(MGPINF(2))   : IN  : I*4 : y方向の親格子1に対する格子の数
CD    KPF(MGPINF(3))   : IN  : I*4 : z方向の親格子1に対する格子の数
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
      DIMENSION XPF(NUMI),YPF(NUMJ),ZPF(NUMK)
      DIMENSION IPF(0:MGPINF(1)),JPF(0:MGPINF(2)),KPF(0:MGPINF(3))
      DIMENSION BCU (NUMB),BCV(NUMB),BCW(NUMB),BCF(NUMB)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)
C
C
C==== 実行 ===========================================================
C
      IUP = 0
      IF(IDIR.GT.0) IUP = -1
C
      IOBS = 0
C
      BC(1) = 0.0D0
      BC(2) = 0.0D0
      BC(3) = 0.0D0
      BC(4) = 0.0D0
C
C X
      IF(ABS(IDIR).EQ.1) THEN
C
        IC = IPF(IP-1)+1
        DO 110 KC=KPF(KP-1)+1,KPF(KP)
          DO 100 JC=JPF(JP-1)+1,JPF(JP)
            L = INDX(IC,JC,KC)

            FACT = (YPF(JC+1)-YPF(JC))*(ZPF(KC+1)-ZPF(KC))
C
C        障害物面
            IF(L.LT.0) THEN
              IOBS = 1
C
C        境界面
            ELSE IF(L.GT.0) THEN
              BC(1) = BC(1) + BCF(L)*FACT
              BC(2) = BC(2) + BCU(L)*FACT
              BC(3) = BC(3) + BCV(L)*FACT
              BC(4) = BC(4) + BCW(L)*FACT
C
C        通常面
            ELSE
C---------------------------------------------------
C             FLF = 0.5D0*(FF(IC-1,JC,KC)+FF(IC,JC,KC))
C             FLF = FF(IC+IUP,JC,KC)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DTNOW*GGX(IC,JC,KC)*UU(IC,JC,KC)がunderflowをおこすとゼロ割する
C
C             IF(IDIR*UU(IC,JC,KC).GT.0.0D0) THEN
              IF(IDIR*DTNOW*GGX(IC,JC,KC)*UU(IC,JC,KC).GT.1.0D-20) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                CALL VF_PMGFLX(FLF,DTNOW*GGX(IC,JC,KC)*UU(IC,JC,KC)
     &                        ,FF,BCF,XX,GGV,FLOWER,NF,INDX
     &                        ,IC,JC,KC,NUMI,NUMJ,NUMK,MAXG1,NUMB)
              ELSE
                FLF = FF(IC+IUP,JC,KC)
              END IF
C---------------------------------------------------
              BC(1) = BC(1) + FLF*FACT
              BC(2) = BC(2) + UU(IC,JC,KC)*FACT
C---------------------------------------------------
              IF(UU(IC,JC,KC).EQ.0.0D0 .AND. KC.GT.2)
     &          BC(2) = BC(2) + UU(IC,JC,KC-1)*FACT
C---------------------------------------------------
C             BC(3) = BC(3) + 0.25D0*(VV(IC-1,JC  ,KC)+VV(IC,JC  ,KC)
C    &                               +VV(IC-1,JC+1,KC)+VV(IC,JC+1,KC))
C    &                              *FACT
C             BC(4) = BC(4) + 0.25D0*(WW(IC-1,JC,KC  )+WW(IC,JC,KC  )
C    &                               +WW(IC-1,JC,KC+1)+WW(IC,JC,KC+1))
C    &                              *FACT
              BC(3) = BC(3) + 0.5D0*(VV(IC+IUP,JC  ,KC)
     &                              +VV(IC+IUP,JC+1,KC))*FACT
              BC(4) = BC(4) + 0.5D0*(WW(IC+IUP,JC,KC  )
     &                              +WW(IC+IUP,JC,KC+1))*FACT
            END IF 
 100      CONTINUE
 110    CONTINUE
C
C Y
      ELSE IF(ABS(IDIR).EQ.2) THEN 
C
        JC = JPF(JP-1)+1
        DO 210 KC=KPF(KP-1)+1,KPF(KP)
          DO 200 IC=IPF(IP-1)+1,IPF(IP)
            L = INDY(IC,JC,KC)

            FACT = (XPF(IC+1)-XPF(IC))*(ZPF(KC+1)-ZPF(KC))
C
C        障害物面
            IF(L.LT.0) THEN
              IOBS = 1
C
C        境界面
            ELSE IF(L.GT.0) THEN
              BC(1) = BC(1) + BCF(L)*FACT
              BC(2) = BC(2) + BCU(L)*FACT
              BC(3) = BC(3) + BCV(L)*FACT
              BC(4) = BC(4) + BCW(L)*FACT
C
C        通常面
            ELSE
C---------------------------------------------------
C             FLF = 0.5D0*(FF(IC,JC,KC)+FF(IC,JC-1,KC))
C             FLF = FF(IC,JC+IUP,KC)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DTNOW*GGY(IC,JC,KC)*VV(IC,JC,KC)がunderflowをおこすとゼロ割する
C
C             IF(IDIR*VV(IC,JC,KC).GT.0.0D0) THEN
              IF(IDIR*DTNOW*GGY(IC,JC,KC)*VV(IC,JC,KC).GT.1.0D-20) THEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                CALL VF_PMGFLY(FLF,DTNOW*GGY(IC,JC,KC)*VV(IC,JC,KC)
     &                        ,FF,BCF,YY,GGV,FLOWER,NF,INDY
     &                        ,IC,JC,KC,NUMI,NUMJ,NUMK,MAXG1,NUMB)
              ELSE
                FLF = FF(IC,JC+IUP,KC)
              END IF
C---------------------------------------------------
              BC(1) = BC(1) + FLF*FACT
C             BC(2) = BC(2) + 0.25D0*(UU(IC  ,JC-1,KC)+UU(IC  ,JC,KC)
C    &                               +UU(IC+1,JC-1,KC)+UU(IC+1,JC,KC))
C    &                              *FACT
              BC(2) = BC(2) + 0.5D0*(UU(IC  ,JC+IUP,KC)
     &                              +UU(IC+1,JC+IUP,KC))*FACT
              BC(3) = BC(3) + VV(IC,JC,KC)*FACT
C---------------------------------------------------
              IF(VV(IC,JC,KC).EQ.0.0D0 .AND. KC.GT.2)
     &          BC(3) = BC(3) + VV(IC,JC,KC-1)*FACT
C---------------------------------------------------
C             BC(4) = BC(4) + 0.25D0*(WW(IC,JC-1,KC  )+WW(IC,JC,KC  )
C    &                               +WW(IC,JC-1,KC+1)+WW(IC,JC,KC+1))
C    &                              *FACT
              BC(4) = BC(4) + 0.5D0*(WW(IC,JC+IUP,KC  )
     &                              +WW(IC,JC+IUP,KC+1))*FACT
            END IF
 200      CONTINUE
 210    CONTINUE
C        
      END IF

      IF(IOBS.EQ.1) THEN
        BC(1) = 0.0D0
        BC(2) = 0.0D0
        BC(3) = 0.0D0
        BC(4) = 0.0D0
      END IF

C     -- 実行文の終了 --

      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
