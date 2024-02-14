      SUBROUTINE VF_PMGP2C_CF(BC,
     &                        XX,YY,ZZ,UU,VV,WW,FF,
     &                        XPF,YPF,ZPF,IPF,JPF,KPF,
     &                        BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                        I,J,K,IDIR,MGNV)

CD=== 概要 ===========================================================

CDT   VF_PMGP2C_CF:マルチグリッド環境の親から受信する格子面の値を設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    BC(MGNV)         : IN  : R*8 : 格子面上の値
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
      IP = -1
      IF(IDIR.GT.0) IP = 0
C
C X
      IF(ABS(IDIR).EQ.1) THEN
        JS = JPF(J-1)+1
        JE = JPF(J)
        KS = KPF(K-1)+1
        KE = KPF(K)
        Y0 = YPF(JS) + 0.5D0
        Z0 = ZPF(KS) + 0.5D0
        DO 110 KC=KS,KE
          FACTZ = 0.5D0*(ZPF(KC) + ZPF(KC+1)) - Z0
          RDZ = 1.0D0/(ZPF(KC+1) - ZPF(KC))
          DO 100 JC=JS,JE
            L=INDX(I,JC,KC)
            IF (L.GT.0) THEN
              FACTY = 0.5D0*(YPF(JC) + YPF(JC+1)) - Y0
              BCF(L) = (BC(1) + BC(5)*FACTY - (ZPF(KC) - ZPF(KS)))*RDZ
              BCF(L) = MIN(MAX(BCF(L),0.0D0),1.0D0)
              BCU(L) = BC(2) + BC(6)*FACTY + BC(7)*FACTZ
              BCV(L) = BC(3)
              BCW(L) = BC(4)
              UU(I,JC,KC) = BCU(L)
              VV(I+IP,JC,KC) = BCV(L) + (YPF(JC) - Y0)*BC(8)
              IF(JC.EQ.JE) VV(I+IP,JC+1,KC) = BCV(L) + 0.5D0*BC(8)
              WW(I+IP,JC,KC) = BCW(L) + (ZPF(KC) - Z0)*BC(9)
              IF(KC.EQ.KE) WW(I+IP,JC,KC+1) = BCW(L) + 0.5D0*BC(9)
            ENDIF
 100      CONTINUE
 110    CONTINUE
C
C Y
      ELSE IF(ABS(IDIR).EQ.2) THEN 
        IS = IPF(I-1)+1
        IE = IPF(I)
        KS = KPF(K-1)+1
        KE = KPF(K)
        X0 = XPF(IS) + 0.5D0
        Z0 = ZPF(KS) + 0.5D0
        DO 210 KC=KS,KE
          FACTZ = 0.5D0*(ZPF(KC) + ZPF(KC+1)) - Z0
          RDZ = 1.0D0/(ZPF(KC+1) - ZPF(KC))
          DO 200 IC=IS,IE
            L=INDY(IC,J,KC)
            IF (L.GT.0) THEN
              FACTX = 0.5D0*(XPF(IC) + XPF(IC+1)) - X0
              BCF(L) = (BC(1) + BC(5)*FACTX - (ZPF(KC) - ZPF(KS)))*RDZ
              BCF(L) = MIN(MAX(BCF(L),0.0D0),1.0D0)
              BCU(L) = BC(2)
              BCV(L) = BC(3) + BC(6)*FACTX + BC(7)*FACTZ
              BCW(L) = BC(4)
              VV(IC,J,KC) = BCV(L)
              UU(IC,J+IP,KC) = BCU(L) + (XPF(IC) - X0)*BC(8)
              IF(IC.EQ.IE) UU(IC+1,J+IP,KC) = BCU(L) + 0.5D0*BC(8)
              WW(IC,J+IP,KC) = BCW(L) + (ZPF(KC) - Z0)*BC(9) 
              IF(KC.EQ.KE) WW(IC,J+IP,KC+1) = BCW(L) + 0.5D0*BC(9)
            ENDIF
 200      CONTINUE
 210    CONTINUE
C        
      END IF

C     -- 実行文の終了 --

      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
