C-*- mode:fortran; -*-
      COMMON /VF_APHYSI/ IBCTYP(4,4),IDAMP(4),IDROP,ISCTYP(2),
     &                   NPVCB,IPVCBC(MAXPVC),IDRGN

CD=== 概要 ===========================================================

CDT   VF_APHYSI.h:物理事象関連(物理量および物理モデル等):整数

C==== 内容 ===========================================================

CD    IBCTYP(4,4) : CNS : I*4 : 特殊境界に関する情報
CD                              (*,1):x座標最小位置の境界
CD                              (*,2):x座標最大位置の境界
CD                              (*,3):y座標最小位置の境界
CD                              (*,4):y座標最大位置の境界
CD                              (1,*):特殊境界の種別 
CD                                    =0:無し
CD                                    =1:法線方向への造波境界
CD                                    =2:法線方向への開境界
CD                              (2,*):特殊境界の種別の詳細
CD                                    造波境界の場合
CD                                    =-4:マトリクスデータ-2 (BCのみ)
CD                                    =-3:マトリクスデータ-1
CD                                    =-2:Stokes波(第5次近似解)
CD                                    =-1:Cnoidal波(第3次近似解)
CD                                    = 0:Stokes波またはCnoidal波
CD                                    > 0:流れ関数法Bとその次数
CD                                    開境界の場合
CD                                    = 0:放射境界(微小振幅波の波速)
CD                              (3,*):特殊境界の始点セル番号
CD                              (4,*):特殊境界の終点セル番号
CD    IDAMP(4)    : CNS : I*4 : 減衰領域の設定フラグ
CD                              (1):x座標最小位置近傍
CD                              (2):x座標最大位置近傍
CD                              (3):y座標最小位置近傍
CD                              (4):y座標最大位置近傍
CD                                  = -1:使用しない
CD                                  >= 0:使用する,かつ,減衰関数の次数
CD    IDROP  : CNS : I*4 : 水滴の自由落下処理(TimerDoor法)
CD                         =0:処理を行わない
CD                         =1:処理を行う
CD    ISCTYP(2)   : CNS : I*4 : 造波ソースに関する情報
CD                              (1):造波ソースの種別 
CD                                  =0:無し
CD                                  >0:x方向への造波ソース(ABS=I)
CD                                  <0:y方向への造波ソース(ABS=J)
CD                              (2):造波関数
CD                                  =-3:マトリクスデータ
CD                                  =-2:Stokes波(第5次近似解)
CD                                  =-1:Cnoidal波(第3次近似解)
CD                                  = 0:Stokes波またはCnoidal波
CD                                  > 0:流れ関数法Bとその次数
CD    NPVCB          : TRN : I*4 : 気泡の数(:空気圧計算用)
CD    IPVCBC(MAXPVC) : TRN : R*8 : 空気圧計算用のワーク
CD    IDRGN  : CNS : I*4 : Dupuit-Forheimer式による抵抗力
CD                         =0:抵抗力は既存の処理
CD                         >0:Dupuit-Forheimer式(粒経の入力数)
