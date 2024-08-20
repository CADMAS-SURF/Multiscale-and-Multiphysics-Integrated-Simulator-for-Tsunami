      REAL(8):: UWST(MAX_NJST,MAX_NKST,4),UEST(MAX_NJST,MAX_NKST,4)
      REAL(8):: VSST(MAX_NIST,MAX_NKST,4),VNST(MAX_NIST,MAX_NKST,4)
      COMMON /VF_ASTOCR/UWST,UEST,VSST,VNST

CD=== 概要 ===========================================================

CDT   VF_ASTOCR.h:CADMAS-STOC連成関連:実数

C==== 内容 ===========================================================

CD    -- STOCとCADMASの境界条件に関する変数 --
CD    UWST(NJST,NKST,4): TRN : R*8 : STOCから受け取る西側境界値
CD    UEST(NJST,NKST,4): TRN : R*8 : STOCから受け取る東側境界値
CD    VSST(NIST,NKST,4): TRN : R*8 : STOCから受け取る南側境界値
CD    VNST(NIST,NKST,4): TRN : R*8 : STOCから受け取る北側境界値
CD                                   上記4配列の内容は全て以下の通り
CD                                   (*,*,1)：境界面の面中心の流速U
CD                                   (*,*,2)：境界面の面中心の流速V
CD                                   (*,*,3)：境界面の面中心の流速W
CD                                   (*,*,4)：境界面の面中心のF値
