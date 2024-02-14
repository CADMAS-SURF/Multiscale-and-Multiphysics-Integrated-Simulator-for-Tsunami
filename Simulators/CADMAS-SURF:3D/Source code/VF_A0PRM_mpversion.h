C-*- mode:fortran; -*-
      PARAMETER (IVR001=  1, IVR002=  5,
     &           MAXNPI= 10, MAXNPJ= 10, MAXBUF=  8,
     &           MFILIN= 11, MFILMT= 12, MFILRE= 13, MFILPR= 14,
     &           MFILLP= 21, MFILGR= 22, MFILRS= 23, MFILTR= 24,
     &           MAXCHR=256, MAXWDS=128, MAXG1 =  6, MAXB1 =  4,
     &           MAXNC = 10,
     &           MAXTR =10000, MAXTR1=  8,
     &           MAXPRB= 10, MAXPVC=500,
     &           MAXDRG= 10,
     &           MAXLIN=100,
c+++++  add W-SRC - start  ++++++++++++++++++++++++++++++++++++++++++++++
     &           MAXSRC= 100,
     &           MAXSRCI=1000, MAXSRCJ=1000,
c+++++  add W-SRC - end  ++++++++++++++++++++++++++++++++++++++++++++++++
     &           ZERO  =1.0D-20,ZEROG = 1.0D-6,
     &           PI    =3.141592653589794D0)

CD=== 概要 ===========================================================

CDT   VF_A0PRM.h:PARAMETER文を集めたファイル

C==== 内容 ===========================================================

CD    IVR001 : PRM : I*4 : バージョンの1桁目
CD    IVR002 : PRM : I*4 : バージョンの2桁目
CD    MAXNPI : PRM : I*4 : x方向最大プロセス数(並列用)
CD    MAXNPJ : PRM : I*4 : y方向最大プロセス数(並列用)
CD    MAXBUF : PRM : I*4 : バッファ用データの本数(並列用)
CD    MFILIN : PRM : I*4 : 入力ファイルのファイル番号
CD    MFILMT : PRM : I*4 : マトリクスデータファイルのファイル番号
CD    MFILRE : PRM : I*4 : リスタートファイルのファイル番号
CD    MFILPR : PRM : I*4 : 時間依存型空隙率ファイルのファイル番号
CD    MFILLP : PRM : I*4 : リストファイルのファイル番号
CD    MFILGR : PRM : I*4 : 図化ファイルのファイル番号
CD    MFILRS : PRM : I*4 : 詳細ファイルのファイル番号
CD    MFILTR : PRM : I*4 : 時系列ファイルのファイル番号
CD    MAXCHR : PRM : I*4 : 1行の最大文字数(入力ファイル)
CD    MAXWDS : PRM : I*4 : 1行の最大単語数(入力ファイル)
CD    MAXG1  : PRM : I*4 : XX,YYおよびZZの第1配列サイズ
CD    MAXB1  : PRM : I*4 : INDBの第1配列サイズ
CD    MAXNC  : PRM : I*4 : 濃度の最大成分数
CD    MAXTR  : PRM : I*4 : 時系列ファイルへの出力対象データ最大数
CD    MAXTR1 : PRM : I*4 : ITRPRMの第1配列サイズ
CD    MAXPRB : PRM : I*4 : 時間依存型空隙率の空間ブロックの最大数
CD    MAXPVC : PRM : I*4 : 気泡の最大数(:空気圧計算用)
CD    MAXDRG : PRM : I*4 : Dupuit-Forheimer式の係数の最大数
c+++++  add W-SRC - start  ++++++++++++++++++++++++++++++++++++++++++++++
CD    MAXSRC : PRM : I*4 : 造波ソースの最大数
CD    MAXSRCI: PRM : I*4 : 造波ソースのx方向最大セル数
CD    MAXSRCJ: PRM : I*4 : 造波ソースのy方向最大セル数
c+++++  add W-SRC - end  ++++++++++++++++++++++++++++++++++++++++++++++++
CD    MAXLIN : PRM : I*4 : 入力データのまとめ読みの行数
CD    ZERO   : PRM : R*8 : ゼロ判定値
CD    ZEROG  : PRM : R*8 : ゼロ判定値(格子間隔判定用)
CD    PI     : PRM : R*8 : 円周率
