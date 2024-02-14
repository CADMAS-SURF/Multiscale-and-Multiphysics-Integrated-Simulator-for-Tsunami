C-*- mode:fortran; -*-
      COMMON /VF_ATIMER/ TEND,TNOW,
     &                   DTNOW,DTCNST,DTINIT,DTMIN,DTMAX,DTSAFE

CD=== 概要 ===========================================================

CDT   VF_ATIMER.h:時間制御関連(解析時刻および時間刻み幅等):実数

C==== 内容 ===========================================================

CD    TEND   : CNS : R*8 : 解析終了時刻
CD    TNOW   : TRN : R*8 : 解析時刻
CD    DTNOW  : TRN : R*8 : 時間刻み幅
CD    DTCNST : CNS : R*8 : 時間刻み幅の一定値
CD    DTINIT : CNS : R*8 : 時間刻み幅の初期値
CD    DTMIN  : CNS : R*8 : 時間刻み幅の最小値
CD    DTMAX  : CNS : R*8 : 時間刻み幅の最大値
CD    DTSAFE : CNS : R*8 : 時間刻み幅の安全率
