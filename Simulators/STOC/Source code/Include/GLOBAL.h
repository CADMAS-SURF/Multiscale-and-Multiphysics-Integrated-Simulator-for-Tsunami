C     IAUTOD：自動領域分割の有無(=0:手動分割,=1:自動分割)
C     MYPROC: INDCOMにおける自分の領域の順番
C
C     自動領域分割前の全体領域における自分の領域の範囲のセルインデックス
C     MYIS: X方向開始位置
C     MYIE: X方向終了位置
C     MYJS: Y方向開始位置
C     MYJE: Y方向終了位置
C
      INTEGER :: IAUTOD,MYPROC,MYIS,MYIE,MYJS,MYJE
      INTEGER :: MXG,MYG,MZG,NPROC,CHILDCOMM
      INTEGER :: INDCOM(6,MAXPE),INDCM2(6,MAXPE)
      COMMON /GLOBAL/ MXG,MYG,MZG,NPROC,INDCOM,INDCM2,CHILDCOMM,
     $                IAUTOD,MYPROC,MYIS,MYIE,MYJS,MYJE
