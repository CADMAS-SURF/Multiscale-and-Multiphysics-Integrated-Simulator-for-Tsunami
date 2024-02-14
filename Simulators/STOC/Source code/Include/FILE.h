      INTEGER :: INP,LP,IFLTM,IFLRI,IFLST,IFLRO,IFLGR,IFLHS
      INTEGER :: IFLDB,IFLSF,IFINI,IFLEN,IFLBO,IFLBI,IFLSB,IFLDP
      INTEGER :: IFLOF,IFLSD,IFLZB,IFLBD,IFLSB2,IFLAR,IFLFW,IFLABC,IFLMA
      INTEGER :: IFLLP
      COMMON /FILEDV/ INP,LP,IFLTM,IFLRI,IFLST,IFLRO,IFLGR,IFLHS,
     &                IFLDB,IFLSF,IFINI,IFLEN,IFLBO,IFLBI,IFLSB,IFLDP,
     &                IFLOF,IFLSD,IFLZB,IFLBD,IFLSB2,IFLAR,IFLFW,IFLABC,
     &                IFLMA,IFLLP
C
C     INP  =15 : 解析条件入力ファイルの(装置)番号：標準入力
C     LP   =16 : リスト出力の(装置)番号：標準出力
C     IFLTM=17 : 流速、水位の時系列変化ファイル(装置)番号：(*.tim)
C     IFLRI=11 : リスタートファイル(入力)の(装置)番号(*.rsi)
C     IFLST=12 : 3次元障害物データファイルの(装置)番号(*.str)
C     IFLSF=13 : 表面風速データファイルの(装置)番号(*.win)
C     IFINI=14 : 温度濃度初期値ファイルの(装置)番号(*.ini)
C     IFLRO=21 : リスタートファイル(出力)の(装置)番号(*.rso)
C     IFLGR=22 : グラフィック出力ファイルの(装置)番号(*.grp)
C     IFLHS=23 : 時系列ファイルの(装置)番号(*.hst)
C     IFLEN=24 : 集計処理データファイルの(装置)番号(*.end)
C     IFLDB=29 : デバッグ出力ファイルの(装置)番号(*.dbg)
C     IFLBO=26 : 境界条件出力ファイルの(装置)番号(*.bco)
C     IFLBI=27 : 境界条件入力ファイルの(装置)番号(*.bci)
C     IFLSB=25 : 海底変動量時間変化入力ファイルの(装置)番号(*.sbt)
C     IFLDP=30 : 透過性構造データファイルの(装置)番号(*.dpr)
C     IFLOF=31 : 越流位置指定ファイルの(装置)番号(*.ofl)
C     IFLSD=32 : オフライン土砂移動計算用ファイルの(装置)番号(*.osd)
C     IFLZB=33 : 初期掃流砂厚さ入力データの(装置)番号(*.zbd)
C     IFLBD=34 : 地形変化モデルで変化させない境界面位置の入力データの(装置)番号(*.ibd)
C     IFLSB2=35 : 海底変動量時間変化入力ファイルの(装置)番号(*.sbt)
C     IFLAR=36 : 自動リスタートで用いる前回計算終了時情報ファイルの(装置)番号(*.ars)
C     IFLFW=37 : 落水モデルの係数ファイルの(装置)番号(*.fwc)
C     IFLABC=38: 風速の側面境界値データファイルの(装置)番号(*.abc)
C     IFLMA=39 : マルチエージェントモデル用のファイルの(装置)番号(*.ma)
C     IFLLP=40 : バイナリ形式のリスト出力用のファイルの(装置)番号(*.list)
C
