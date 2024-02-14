MODULE M_MODEL
  IMPLICIT NONE
!----------------------------------------
!     計算モデル用データ
!
!     CM         付加質量係数
!     LNGDIV     漂流物の抗力計算時の分割基準長さ(m)
!     MAXDIV_L   漂流物の抗力計算時の分割数の上限値(長さ方向)
!     MAXDIV_B   漂流物の抗力計算時の分割数の上限値(幅方向)
!     SINK_MODEL 沈降モデルフラグ(=0:H21モデル, =1:H22モデル)
!     CDF1       水の抗力係数(対向時) CDF1>CDF2
!     CDF2       水の抗力係数(離向時)
!     CDW1       風の抗力係数(対向時) CDW1>CDW2
!     CDW2       風の抗力係数(離向時)
!
!     L_RAND     不確実性モデルを適用するか否か(=0:適用しない,=1:適用する)
!     MAXSEED    乱数の種の最大サイズ
!     SEEDSIZE   乱数の種のサイズ
!     I_RAND_SEED 乱数の種
!     AMNG_RANG  不確実性モデルで参照するマニングの粗度
!
!----------------------------------------
!
      REAL(8)::CM=2.0D0
      REAL(8)::LNGDIV=1.0D10
      REAL(8),PARAMETER::CDF1=0.8D0, CDF2=0.4D0   ! 水用
      REAL(8),PARAMETER::CDW1=0.8D0, CDW2=0.4D0   ! 風用
      INTEGER::MAXDIV_L=2000,MAXDIV_B=2000
      INTEGER::SINK_MODEL=1 
!
      INTEGER::L_RAND=0
      INTEGER,PARAMETER::MAXSEED=32
      INTEGER::SEEDSIZE=0
      INTEGER::I_RAND_SEED(MAXSEED)=0
      REAL(8)::AMNG_RAND=0.025D0
END MODULE M_MODEL
