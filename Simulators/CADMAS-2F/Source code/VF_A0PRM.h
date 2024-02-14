      PARAMETER (IVR001=  0, IVR002=  2,
C----------------------------------------------------------for MG interface
C    &           MAXNPI= 100, MAXNPJ= 100, MAXBUF=  8,
     &           MAXNPI= 100, MAXNPJ= 100, MAXBUF=  9*2*4,
     &           MFILEN= 10,
C----------------------------------------------------------for MG interface
     &           MFILIN= 11, MFILMT= 12, MFILRE= 13, MFILPR= 14,
     &           MFILLP= 21, MFILGR= 22, MFILRS= 23, MFILTR= 24,
     &           MFILTP= 25, MFILBT= 26,MFILBT2= 27,
     &           MAXCHR=256, MAXWDS=128, MAXG1 =  6, MAXB1 =  6,
     &           MAXBK1=  2, MAXBE1=  2, MAXNC = 10, MAXM1 =  5,
     &           MAXTR =10000, MAXTR1=  8, 
     &           MAXPRB= 10, MAXPVC=500,
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
CD    MFILPR : PRM : I*4 : 時間依存型空隙率(or構造物データ)ファイルのファイル番号
CD    MFILLP : PRM : I*4 : リストファイルのファイル番号
CD    MFILGR : PRM : I*4 : 図化ファイルのファイル番号
CD    MFILRS : PRM : I*4 : 詳細ファイルのファイル番号
CD    MFILTR : PRM : I*4 : 時系列ファイルのファイル番号
CD    MFILBT : PRM : I*4 : 水位変動量入力ファイルのファイル番号
CD    MFILBT2: PRM : I*4 : 分割形式の水位変動量入力ファイルのファイル番号
CD    MFILTP : PRM : I*4 : 構造物表面圧力ファイルのファイル番号
CD    MAXCHR : PRM : I*4 : 1行の最大文字数(入力ファイル)
CD    MAXWDS : PRM : I*4 : 1行の最大単語数(入力ファイル)
CD    MAXG1  : PRM : I*4 : XX,YYおよびZZの第1配列サイズ
CD    MAXB1  : PRM : I*4 : INDBの第1配列サイズ
CD    MAXBK1 : PRM : I*4 : INDBKの第1配列サイズ
CD    MAXBE1 : PRM : I*4 : INDBEの第1配列サイズ
CD    MAXNC  : PRM : I*4 : 濃度の最大成分数
CD    MAXM1  : PRM : I*4 : INDBMの第1配列サイズ
CD    MAXTR  : PRM : I*4 : 時系列ファイルへの出力対象データ最大数
CD    MAXTR1 : PRM : I*4 : ITRPRMの第1配列サイズ
CD    MAXPRB : PRM : I*4 : 時間依存型空隙率の空間ブロックの最大数
CD    ZERO   : PRM : R*8 : ゼロ判定値
CD    ZEROG  : PRM : R*8 : ゼロ判定値(格子間隔判定用)
CD    PI     : PRM : R*8 : 円周率
