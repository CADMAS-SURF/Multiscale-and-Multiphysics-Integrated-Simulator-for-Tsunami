C-*- mode:fortran; -*-
      COMMON /VF_ATIMEI/ NEND,NNOW,IDTTYP,LOOPS

CD=== 概要 ===========================================================

CDT   VF_ATIMEI.h:時間制御関連(解析時刻および時間刻み幅等):整数

C==== 内容 ===========================================================

CD    NEND   : CNS : I*4 : 解析終了ステップ
CD    NNOW   : TRN : I*4 : 解析ステップ
CD    IDTTYP : CNS : I*4 : 時間刻み幅の計算方法
CD                         = 0:一定
CD                         !=0:自動
CD    LOOPS  : CNS : I*4 : 流速・圧力計算のサブループ回数
CD                         =1 :通常計算
CD                         >1 :サブループ有り
