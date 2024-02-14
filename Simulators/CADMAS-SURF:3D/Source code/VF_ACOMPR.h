C-*- mode:fortran; -*-
      COMMON /VF_ACOMPR/ CGPARA,CGEPSA,CGEPSR,CGDIV ,CGBNRM,CGXNRM,
     &                   FEPS  ,FLOWER,FUPPER,FSUM  ,FCUT  ,PLOWER,
     &                   SCMVP ,SCMK  ,SCMT  ,SCMC(MAXNC)

CD=== 概要 ===========================================================

CDT   VF_ACOMPR.h:数値解法関連(各種パラメータおよび反復回数等):実数

C==== 内容 ===========================================================

CD    CGPARA : CNS : R*8 : 連立1次方程式の解法のMILU用パラメータ
CD    CGEPSA : CNS : R*8 : 連立1次方程式の解法の収束判定値(絶対誤差)
CD    CGEPSR : CNS : R*8 : 連立1次方程式の解法の収束判定値(相対誤差)
CD    CGDIV  : CNS : R*8 : 連立1次方程式の解法の発散判定値
CD    CGBNRM : TRN : R*8 : 連立1次方程式の解法の右辺のノルム
CD    CGXNRM : TRN : R*8 : 連立1次方程式の解法の残差のノルム
CD    FEPS   : CNS : R*8 : VOF関数Fのゼロ判定値
CD    FLOWER : CNS : R*8 : VOF関数Fの下限値
CD    FUPPER : CNS : R*8 : VOF関数Fの上限値
CD    FSUM   : TRN : R*8 : VOF関数Fの空間積分値
CD    FCUT   : TRN : R*8 : VOF関数Fのカットオフ値
CD    PLOWER : CNS : R*8 : ポーラス値の下限値
CD    SCMVP  : CNS : R*8 : DONORスキームのパラメータ(流速用)
CD                         =0.0:2次中心
CD                         =1.0:1次風上
CD    SCMK   : CNS : R*8 : DONORスキームのパラメータ(k-ε用)
CD                         =0.0:2次中心
CD                         =1.0:1次風上
CD    SCMT   : CNS : R*8 : DONORスキームのパラメータ(温度用)
CD                         =0.0:2次中心
CD                         =1.0:1次風上
CD    SCMC(MAXNC) : CNS : R*8 : DONORスキームのパラメータ(濃度用)
CD                         =0.0:2次中心
CD                         =1.0:1次風上
