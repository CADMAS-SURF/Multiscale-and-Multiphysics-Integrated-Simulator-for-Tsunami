      PARAMETER (IFLOU=10,IFLIN=21,MPROCS= 900)
      COMMON /GF_CONV/ NPROCS,NUMI0,NUMJ0,NUMK0,
     &                 IG1,JG1,KG1,IG2,JG2,KG2,
     &                 ISWLN,ISWLV,ISWLP,ISWLF,ISWLK,
     &                 ISWLT,ISWLS,ISWLG,ISWL1,ISWL2,
     &                 NBX  (MPROCS),NBY  (MPROCS),NBZ  (MPROCS),
     &                 NUMI (MPROCS),NUMJ (MPROCS),
     &                 MYIS (MPROCS),MYIE (MPROCS),
     &                 MYJS (MPROCS),MYJE (MPROCS),
     &                 MYMIS(MPROCS),MYMIE(MPROCS),
     &                 MYMJS(MPROCS),MYMJE(MPROCS),
     &                 MYGIS(MPROCS),MYGIE(MPROCS),
     &                 MYGJS(MPROCS),MYGJE(MPROCS)

CD=== 概要 ===========================================================

CDT   GF_CONV.h:CADMAS-SURF/3Dの並列用グラフィックデータ変換用

C==== 内容 ===========================================================

CD    IFLOU         : PRM : I*4 : 出力ファイル番号
CD    IFLIN         : PRM : I*4 : 入力ファイル番号の開始番号
CD    MPROCS        : PRM : I*4 : 最大プロセス数
CD @  NPROCS        : CNS : I*4 : プロセス数
CD @  NUMI0         : CNS : I*4 : 全体の格子数
CD @  NUMJ0         : CNS : I*4 : ..
CD @  NUMK0         : CNS : I*4 : ..
CD @  IG1           : CNS : I*4 : 全体の図化ファイルの出力範囲
CD @  JG1           : CNS : I*4 : ..
CD @  KG1           : CNS : I*4 : ..
CD @  IG2           : CNS : I*4 : ..
CD @  JG2           : CNS : I*4 : ..
CD @  KG2           : CNS : I*4 : ..
CD @  ISWLN         : CNS : I*4 : 出力フラグ
CD @  ISWLV         : CNS : I*4 : ..
CD @  ISWLP         : CNS : I*4 : ..
CD @  ISWLF         : CNS : I*4 : ..
CD @  ISWLK         : CNS : I*4 : ..
CD @  ISWLT         : CNS : I*4 : ..
CD @  ISWLS         : CNS : I*4 : ..
CD @  ISWLG         : CNS : I*4 : ..
CD @  ISWL1         : CNS : I*4 : ..
CD @  ISWL2         : CNS : I*4 : ..
CD @  NBX  (MPROCS) : CNS : I*4 : 境界値の出力数
CD @  NBY  (MPROCS) : CNS : I*4 : ..
CD @  NBZ  (MPROCS) : CNS : I*4 : ..
CD @  NUMI (MPROCS) : CNS : I*4 : x方向格子数
CD @  NUMJ (MPROCS) : CNS : I*4 : y方向格子数
CD @  MYIS (MPROCS) : CNS : I*4 : x方向セル番号(開始,仮想含まず,局所)
CD @  MYIE (MPROCS) : CNS : I*4 : x方向セル番号(終了,仮想含まず,局所)
CD @  MYJS (MPROCS) : CNS : I*4 : y方向セル番号(開始,仮想含まず,局所)
CD @  MYJE (MPROCS) : CNS : I*4 : y方向セル番号(終了,仮想含まず,局所)
CD @  MYMIS(MPROCS) : CNS : I*4 : x方向セル番号(開始,仮想の厚み)
CD @  MYMIE(MPROCS) : CNS : I*4 : x方向セル番号(終了,仮想の厚み)
CD @  MYMJS(MPROCS) : CNS : I*4 : y方向セル番号(開始,仮想の厚み)
CD @  MYMJE(MPROCS) : CNS : I*4 : y方向セル番号(終了,仮想の厚み)
CD @  MYGIS(MPROCS) : CNS : I*4 : x方向セル番号(開始,仮想含む,大域)
CD @  MYGIE(MPROCS) : CNS : I*4 : x方向セル番号(終了,仮想含む,大域)
CD @  MYGJS(MPROCS) : CNS : I*4 : y方向セル番号(開始,仮想含む,大域)
CD @  MYGJE(MPROCS) : CNS : I*4 : y方向セル番号(終了,仮想含む,大域)
