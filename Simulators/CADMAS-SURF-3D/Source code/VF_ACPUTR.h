C-*- mode:fortran; -*-
      PARAMETER (ICPUIN=  0, ICPUST=  1, ICPUEN=  2, ICPUOU=  3,
     &           KCP0AL= 22,
     &           KCP1PR=  1, KCP1CL=  2,
     &           KCP2FL=  3, KCP2VL=  4, KCP2TT=  5, KCP2SS=  6,
     &           KCP2KE=  7, KCP2FF=  8,
     &           KCPVFL=  9, KCPVGN= 10, KCPVEL= 11, KCPVPC= 12,
     &           KCPVPS= 13, KCPVMD= 14,
     &           KCPFFL= 15, KCPFEL= 16, KCPFMD= 17, KCPFNF= 18,
     &           KCP9PL= 19, KCP9M1= 20, KCP9NF= 21             )
      COMMON /VF_ACPUTR/ CPUS(KCP0AL), CPUW(KCP0AL)

CD=== 概要 ===========================================================

CDT   VF_ACPUTR.h:CPU時間の計測関連(CPU時間等):パラメータと実数

C==== 内容 ===========================================================

CD    ICPUIN : PRM : I*4 : タイマー処理フラグ(初期化)
CD    ICPUST : PRM : I*4 : タイマー処理フラグ(スタート)
CD    ICPUEN : PRM : I*4 : タイマー処理フラグ(止めて,合計をとる)
CD    ICPUOU : PRM : I*4 : タイマー処理フラグ(CPU時間を出力する)
CD    KCP0AL : PRM : I*4 : タイマー種別フラグ(全体,種別フラグ数を兼ねる)
CD    KCP1PR : PRM : I*4 : タイマー種別フラグ(前処理）
CD    KCP1CL : PRM : I*4 : タイマー種別フラグ(計算部）
CD    KCP2FL : PRM : I*4 : タイマー種別フラグ(計算部/ファイル入出力）
CD    KCP2VL : PRM : I*4 : タイマー種別フラグ(計算部/流速・圧力）
CD    KCP2TT : PRM : I*4 : タイマー種別フラグ(計算部/温度）
CD    KCP2SS : PRM : I*4 : タイマー種別フラグ(計算部/スカラー）
CD    KCP2KE : PRM : I*4 : タイマー種別フラグ(計算部/k-εモデル）
CD    KCP2FF : PRM : I*4 : タイマー種別フラグ(計算部/VOF関数F）
CD    KCPVFL : PRM : I*4 : タイマー種別フラグ(流速・圧力関連）
CD    KCPVGN : PRM : I*4 : ...
CD    KCPVEL : PRM : I*4 : ...
CD    KCPVPC : PRM : I*4 : ...
CD    KCPVPS : PRM : I*4 : ...
CD    KCPVMD : PRM : I*4 : ...
CD    KCPFFL : PRM : I*4 : タイマー種別フラグ(VOF関数F関連）
CD    KCPFEL : PRM : I*4 : ...
CD    KCPFMD : PRM : I*4 : ...
CD    KCPFNF : PRM : I*4 : ...
CD    KCP9PL : PRM : I*4 : タイマー種別フラグ(ルーチン/VF_P****）
CD    KCP9M1 : PRM : I*4 : タイマー種別フラグ(ルーチン/VF_M1BCGS）
CD    KCP9NF : PRM : I*4 : タイマー種別フラグ(ルーチン/VF_FDROPF）
CD    CPUS(KCP0AL) : TRN : R*8 : CPU時間の合計
CD    CPUW(KCP0AL) : TRN : R*8 : タイマーが起動された時間
