      SUBROUTINE VF_A2CPUT(ILP,IC,KC)

CD=== 概要 ===========================================================

CDT   VF_A2CPUT:CPU時間の計測関連

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    ILP : IN : I*4 : CPU時間を出力するファイル番号
CD    IC  : IN : I*4 : 処理フラグ(VF_ACPUTR.hを参照)
CD    KC  : IN : I*4 : 種別フラグ(VF_ACPUTR.hを参照)

C==== 実行 ===========================================================

CD    -- タイマーを初期化する --
      IF     (IC.EQ.ICPUIN) THEN
        CALL VF_ZSETR1(CPUS, 0.0D0,KCP0AL)
        CALL VF_ZSETR1(CPUW,-1.0D0,KCP0AL)

CD    -- タイマーをスタートする --
      ELSEIF (IC.EQ.ICPUST) THEN
        IF (KC.LT.1           ) CALL VF_A2ERR('VF_A2CPUT','P.G. ERROR.')
        IF (KC.GT.KCP0AL      ) CALL VF_A2ERR('VF_A2CPUT','P.G. ERROR.')
        IF (CPUW(KC).GT.-0.1D0) CALL VF_A2ERR('VF_A2CPUT','P.G. ERROR.')
        CALL VF_ZTIMEC(TALL,TUSER,TSYS)
        CPUW(KC)=TUSER

CD    -- タイマーを止めて,合計をとる --
      ELSEIF (IC.EQ.ICPUEN) THEN
        IF (KC.LT.1           ) CALL VF_A2ERR('VF_A2CPUT','P.G. ERROR.')
        IF (KC.GT.KCP0AL      ) CALL VF_A2ERR('VF_A2CPUT','P.G. ERROR.')
        IF (CPUW(KC).LT.-0.1D0) CALL VF_A2ERR('VF_A2CPUT','P.G. ERROR.')
        CALL VF_ZTIMEC(TALL,TUSER,TSYS)
        CPUS(KC)=CPUS(KC)+(TUSER-CPUW(KC))
        CPUW(KC)=-1.0D0

CD    -- CPU時間を出力する --
      ELSEIF (IC.EQ.ICPUOU) THEN
        WRITE(ILP,9510)
        WRITE(ILP,9520)
        WRITE(ILP,9530) CPUS(KCP0AL)
        WRITE(ILP,9540) CPUS(KCP1PR)
        WRITE(ILP,9550) CPUS(KCP1CL)
        WRITE(ILP,9560) CPUS(KCP2FL)
        WRITE(ILP,9570) CPUS(KCP2VL)
        WRITE(ILP,9580) CPUS(KCPVFL)
        WRITE(ILP,9590) CPUS(KCPVGN)
        WRITE(ILP,9600) CPUS(KCPVEL)
        WRITE(ILP,9610) CPUS(KCPVPC)
        WRITE(ILP,9620) CPUS(KCPVPS)
        WRITE(ILP,9630) CPUS(KCPVMD)
        WRITE(ILP,9640) CPUS(KCP2VL)-CPUS(KCPVFL)-CPUS(KCPVGN)
     &                              -CPUS(KCPVEL)-CPUS(KCPVPC)
     &                              -CPUS(KCPVPS)-CPUS(KCPVMD)
        WRITE(ILP,9650) CPUS(KCP2TT)
        WRITE(ILP,9660) CPUS(KCP2SS)
        WRITE(ILP,9670) CPUS(KCP2KE)
        WRITE(ILP,9680) CPUS(KCP2FF)
        WRITE(ILP,9690) CPUS(KCPFFL)
        WRITE(ILP,9700) CPUS(KCPFEL)
        WRITE(ILP,9710) CPUS(KCPFMD)
        WRITE(ILP,9720) CPUS(KCPFNF)
        WRITE(ILP,9730) CPUS(KCP2FF)-CPUS(KCPFFL)-CPUS(KCPFEL)
     &                              -CPUS(KCPFMD)-CPUS(KCPFNF)
        WRITE(ILP,9740) CPUS(KCP1CL)-CPUS(KCP2FL)-CPUS(KCP2VL)
     &                              -CPUS(KCP2TT)-CPUS(KCP2SS)
     &                              -CPUS(KCP2KE)-CPUS(KCP2FF)
        WRITE(ILP,9750) CPUS(KCP0AL)-CPUS(KCP1PR)-CPUS(KCP1CL)
        WRITE(ILP,9760)
        WRITE(ILP,9770) CPUS(KCP9PL)
        WRITE(ILP,9780) CPUS(KCP9M1)
        WRITE(ILP,9790) CPUS(KCP9NF)

CD    -- プログラムエラー --
      ELSE
        CALL VF_A2ERR('VF_A2CPUT','P.G. ERROR.')
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ','##### CPU TIME [S] ##########################')
 9520 FORMAT(/' ','##  <<FLOW>>')
 9530 FORMAT( ' ','##  TOTAL                      ',F11.2)
 9540 FORMAT( ' ','##  +-- PRE PROCESS            ',F11.2)
 9550 FORMAT( ' ','##  +-- CALCULATION            ',F11.2)
 9560 FORMAT( ' ','##  |    +-- FILE I/O          ',F11.2)
 9570 FORMAT( ' ','##  |    +-- VELO & PRES       ',F11.2)
 9580 FORMAT( ' ','##  |    |    +-- CONV & VISC  ',F11.2)
 9590 FORMAT( ' ','##  |    |    +-- GENERATION   ',F11.2)
 9600 FORMAT( ' ','##  |    |    +-- INTEGRATION  ',F11.2)
 9610 FORMAT( ' ','##  |    |    +-- POISSON COEF ',F11.2)
 9620 FORMAT( ' ','##  |    |    +-- POISSON SOLV ',F11.2)
 9630 FORMAT( ' ','##  |    |    +-- V & P MODIF  ',F11.2)
 9640 FORMAT( ' ','##  |    |    +-- E.T.C.       ',F11.2)
 9650 FORMAT( ' ','##  |    +-- TEMPERATURE       ',F11.2)
 9660 FORMAT( ' ','##  |    +-- CONCENTRATION     ',F11.2)
 9670 FORMAT( ' ','##  |    +-- K-EPSIRON         ',F11.2)
 9680 FORMAT( ' ','##  |    +-- VOF FUNCTION      ',F11.2)
 9690 FORMAT( ' ','##  |    |    +-- CONVECTION   ',F11.2)
 9700 FORMAT( ' ','##  |    |    +-- INTEGRATION  ',F11.2)
 9710 FORMAT( ' ','##  |    |    +-- MODIF & CUT  ',F11.2)
 9720 FORMAT( ' ','##  |    |    +-- NF & T-DOOR  ',F11.2)
 9730 FORMAT( ' ','##  |    |    +-- E.T.C.       ',F11.2)
 9740 FORMAT( ' ','##  |    +-- E.T.C.            ',F11.2)
 9750 FORMAT( ' ','##  +-- E.T.C.                 ',F11.2)
 9760 FORMAT(/' ','##  <<ROUTINE>>')
 9770 FORMAT( ' ','##  +-- VF_P*****              ',F11.2)
 9780 FORMAT( ' ','##  +-- VF_M1BCGS              ',F11.2)
 9790 FORMAT( ' ','##  +-- VF_FDROPF              ',F11.2)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
