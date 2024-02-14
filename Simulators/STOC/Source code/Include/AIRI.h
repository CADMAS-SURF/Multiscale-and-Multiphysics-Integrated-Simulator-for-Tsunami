C----------------------------------------------------------------------
C     風場計算用コモン(整数型)
C
C     MZA    : 鉛直方向のメッシュ分割数(仮想セル含む)
C     MZMA   : MZA-1
C     LTURBA : 乱流モデルの指定(=0:使用しない, =1:LESモデル, =2;:k-εモデル
C     IBCAIRWES : 西側境界値の指定方法(=0:一定値, =1:ファイル読込, -1:スリップ壁)
C     IBCAIREAS : 東側境界値の指定方法(=0:一定値, =1:ファイル読込, -1:スリップ壁)
C     IBCAIRSOU : 南側境界値の指定方法(=0:一定値, =1:ファイル読込, -1:スリップ壁)
C     IBCAIRNOR : 北側境界値の指定方法(=0:一定値, =1:ファイル読込, -1:スリップ壁)
C     IBCAIRTOP : 鉛直上面境界値の指定方法(-2:自由流入出境界,-1:スリップ壁)
C----------------------------------------------------------------------
C
      INTEGER :: MZA,MZMA,LTURBA
      INTEGER :: IBCAIRWES,IBCAIREAS,IBCAIRSOU,IBCAIRNOR,IBCAIRTOP
      COMMON /AIRI/ MZA,MZMA,LTURBA,
     $              IBCAIRWES,IBCAIREAS,IBCAIRSOU,IBCAIRNOR,IBCAIRTOP

      integer,parameter :: debug_air1 = 0
      integer,parameter :: debug_air2 = 0
      integer,parameter :: debug_air3 = 0
      integer,parameter :: debug_air4 = 0
      integer,parameter :: debug_air5 = 0
      integer,parameter :: debug_air6 = 0
      integer,parameter :: debug_air7 = 0
      integer,parameter :: debug_air8 = 0
      integer,parameter :: debug_air9 = 0
      integer,parameter :: debug_air10= 0
      integer,parameter :: debug_air11= 0

C...MODELIにLAIRを追加
C     LAIR ... 風場を計算するか否かのフラグ(=0:計算しない,=1:計算する)
