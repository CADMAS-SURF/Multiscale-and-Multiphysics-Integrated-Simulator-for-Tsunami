C-*- mode:fortran; -*-
      CHARACTER*(MAXCHR) MGNAME
      COMMON /VF_APARAI/ MGPROC,MGRANK,MGCOMM,
     &                   MGARAN,
     &                   MGNAME(MAXPRO),MGNLEN(MAXPRO),
     &                   MGNPIN(MAXPRO),MGPARE(MAXPRO),MGAREA(MAXPRO),
     &                   MGPRNK,MGPINF(9),
     &                   MGCNUM,MGCRNK(MAXPRO),MGCINF(9,MAXPRO),
     &                   MGCPOS(6,MAXPRO),
     &                   NPROCS,NUMNPI,NUMNPJ,
     &                   MYRANK,MYRI  ,MYRJ  ,
     &                   NUMI0 ,NUMJ0 ,
     &                   MYIS  ,MYIE  ,MYJS  ,MYJE  ,
     &                   MYMIS ,MYMIE ,MYMJS ,MYMJE ,
     &                   MYGIS ,MYGIE ,MYGJS ,MYGJE ,
     &                   NUMBUF,
     &                   IPROCS(0:MAXNPI),JPROCS(0:MAXNPJ),
     &                   FC_RANK,MA_RANK

CD=== 概要 ===========================================================

CDT   VF_APARAI.h:並列化関連:整数

C==== 内容 ===========================================================

CD    -- マルチグリッド全体に関する変数 --
CD    MGPROC           : CNS : I*4 : 全体のプロセス数
CD    MGRANK           : CNS : I*4 : 全体の中の自分のランク
CD    MGCOMM           : CNS : I*4 : 領域毎のコミュニケータ
CD    MGARAN           : CNS : I*4 : 領域数
CD    MGNAME(MAXPRO)   : CNS : C** : 領域名
CD    MGNLEN(MAXPRO)   : CNS : I*4 : 領域名の文字数
CD    MGNPIN(MAXPRO)   : CNS : I*4 : 領域毎のプロセス数
CD    MGPARE(MAXPRO)   : CNS : I*4 : 親の領域番号
CD    MGAREA(MAXPRO)   : CNS : I*4 : 領域番号
CD    MGPRNK           : CNS : I*4 : 全体の中の親のランク
CD                                   < 0:親無し
CD                                   >=0:親のランク
CD    MGPINF(9)        : CNS : I*4 : 親の情報
CD                                   (1):自分に接するx方向セル数
CD                                   (2):自分に接するy方向セル数
CD                                   (3):自分に接するz方向セル数
CD                                   親との通信
CD                                   (4):x-:しない(=1)、する(=0)
CD                                   (5):y-:しない(=1)、する(=0)
CD                                   (6):z-:しない(=1)、する(=0)
CD                                   (7):x+:しない(=1)、する(=0)
CD                                   (8):y+:しない(=1)、する(=0)
CD                                   (9):z+:しない(=1)、する(=0)
CD    MGCNUM           : CNS : I*4 : 子供の数
CD    MGCRNK(MAXPRO)   : CNS : I*4 : 全体の中の子のランク
CD    MGCINF(9,MAXPRO) : CNS : I*4 : 子の情報
CD                                   (1,*):x方向セル数,仮想含まず
CD                                   (2,*):y方向セル数,仮想含まず
CD                                   (3,*):z方向セル数,仮想含まず
CD                                   子との通信
CD                                   (4,*):x-:しない(=1)、する(=0)
CD                                   (5,*):y-:しない(=1)、する(=0)
CD                                   (6,*):z-:しない(=1)、する(=0)
CD                                   (7,*):x+:しない(=1)、する(=0)
CD                                   (8,*):y+:しない(=1)、する(=0)
CD                                   (9,*):z+:しない(=1)、する(=0)
CD    MGCPOS(6,MAXPRO) : CNS : I*4 : 自分の中の子の位置
CD                                   (1,*):x方向セル番号(開始)
CD                                   (2,*):y方向セル番号(開始)
CD                                   (3,*):z方向セル番号(開始)
CD                                   (4,*):x方向セル番号(終了)
CD                                   (5,*):y方向セル番号(終了)
CD                                   (6,*):z方向セル番号(終了)

CD    -- 各グリッドに関する変数 --
CD    NPROCS           : CNS : I*4 : プロセス数
CD    NUMNPI           : CNS : I*4 : x方向プロセス数
CD    NUMNPJ           : CNS : I*4 : y方向プロセス数
CD    MYRANK           : CNS : I*4 : 自分のランク
CD    MYRI             : CNS : I*4 : 自分のx方向ランク
CD    MYRJ             : CNS : I*4 : 自分のy方向ランク
CD    NUMI0            : CNS : I*4 : 全体のx方向格子数+1
CD    NUMJ0            : CNS : I*4 : 全体のy方向格子数+1
CD    MYIS             : CNS : I*4 : x方向セル番号(開始,仮想含まず,局所)
CD    MYIE             : CNS : I*4 : x方向セル番号(終了,仮想含まず,局所)
CD    MYJS             : CNS : I*4 : y方向セル番号(開始,仮想含まず,局所)
CD    MYJE             : CNS : I*4 : y方向セル番号(終了,仮想含まず,局所)
CD    MYMIS            : CNS : I*4 : x方向セル番号(開始,仮想の厚み)
CD    MYMIE            : CNS : I*4 : x方向セル番号(終了,仮想の厚み)
CD    MYMJS            : CNS : I*4 : y方向セル番号(開始,仮想の厚み)
CD    MYMJE            : CNS : I*4 : y方向セル番号(終了,仮想の厚み)
CD    MYGIS            : CNS : I*4 : x方向セル番号(開始,仮想含む,大域)
CD    MYGIE            : CNS : I*4 : x方向セル番号(終了,仮想含む,大域)
CD    MYGJS            : CNS : I*4 : y方向セル番号(開始,仮想含む,大域)
CD    MYGJE            : CNS : I*4 : y方向セル番号(終了,仮想含む,大域)
CD    NUMBUF           : CNS : I*4 : バッファ用データの数(1本分)
CD    IPROCS(0:MAXNPI) : CNS : I*4 : x方向の終了セル番号
CD    JPROCS(0:MAXNPJ) : CNS : I*4 : y方向の終了セル番号

CD    -- 他モジュールとの連成に関する変数 --
CD    FC_RANK          : CNS : I*4 : comm_modelにおける2FCモデルのランク
CD    MA_RANK          : CNS : I*4 : comm_mlicdsmg2fc_mltにおけるマルチエージェントモデルのランク
