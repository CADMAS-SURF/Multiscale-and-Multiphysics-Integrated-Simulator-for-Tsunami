C-*- mode:fortran; -*-
      COMMON /VF_ACOMPI/ ICGTYP,ICGMAX,ICGITR,ISCMVP,ISCMFF,
     &                   ISCMK,ISCMT,ISCMC(MAXNC),IBSUW0

CD=== 概要 ===========================================================

CDT   VF_ACOMPI.h:数値解法関連(各種パラメータおよび反復回数等):整数

C==== 内容 ===========================================================

CD    ICGTYP : CNS : I*4 : 連立1次方程式の解法の前処理の種類
CD                         = 0:不完全LU分解(ILU)
CD                         !=0:修正不完全LU分解(MILU)
CD    ICGMAX : CNS : I*4 : 連立1次方程式の解法の最大反復回数
CD    ICGITR : TRN : I*4 : 連立1次方程式の解法の反復回数
CD    ISCMVP : CNS : I*4 : 流速の対流項の差分スキーム
CD                         = 0:DONOR
CD                         !=0:????? CAKIY
CD    ISCMFF : CNS : I*4 : VOF関数Fの対流項の差分スキーム
CD                         = 0:ドナー・アクセプタ法
CD                         !=0:界面の傾きを考慮した方法
CD    ISCMK  : CNS : I*4 : k-εの移流項の差分スキーム
CD                         = 0:DONOR
CD                         !=0:?????
CD    ISCMT  : CNS : I*4 : 温度の移流項の差分スキーム
CD                         = 0:DONOR
CD                         !=0:?????
CD    ISCMC(MAXNC) : CNS : I*4 : 濃度の移流項の差分スキーム
CD                         = 0:DONOR
CD                         !=0:?????
CD    IBSUW0 : CNS : I*4 : 表面セルの流速計算法
CD                         =0 :流体側から補外できる場合は線形補外
CD                         !=0:勾配ゼロ
