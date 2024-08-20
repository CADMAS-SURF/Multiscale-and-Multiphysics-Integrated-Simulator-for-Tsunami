      COMMON /VF_ANUMBI/ NUMI,NUMJ,NUMK,NUMB,NUMB0,NUMS,LEQK,LEQT,LEQC

CD=== 概要 ===========================================================

CDT   VF_ANUMBI.h:データ数関連(配列サイズおよび格子数等):整数

C==== 内容 ===========================================================

CD    NUMI  : CNS : I*4 : x方向格子数+1(x方向セル数+2)
CD    NUMJ  : CNS : I*4 : y方向格子数+1(y方向セル数+2)
CD    NUMK  : CNS : I*4 : z方向格子数+1(z方向セル数+2)
CD    NUMB  : CNS : I*4 : 境界面の数
CD    NUMB0 : CNS : I*4 : 境界面の数(移動障害物処理用)
CD    NUMS  : TRN : I*4 : 表面セルの数
CD    LEQK  : CNS : I*4 : k-εを計算するかしないか
CD                        = 0:計算しない
CD                        !=0:計算する
CD    LEQT  : CNS : I*4 : 温度を計算するかしないか
CD                        = 0:計算しない
CD                        !=0:計算する
CD    LEQC  : CNS : I*4 : 濃度を計算するかしないか
CD                        = 0:計算しない
CD                        >=1:成分数
