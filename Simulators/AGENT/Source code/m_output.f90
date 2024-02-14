module m_output
  implicit none
!----------------------------------------
! ファイル出力制御データ
!--出力時刻--
! out_start              リスト出力の開始時刻(s)
! out_end                リスト出力の終了時刻(s)
! out_interval           リスト出力の出力間隔(s)
! out_time               次回リスト出力時刻(s)
!--出力の系列数--
! N_I_statistics         統計値(整数型)の系列数
! N_R_statistics         統計値(実数型)の系列数
! N_I_attribute_F        固定属性値(整数型)の系列数
! N_R_attribute_F        固定属性値(実数型)の系列数
! N_I_attribute_V        変動属性値(整数型)の系列数
! N_R_attribute_V        変動属性値(実数型)の系列数
!--出力データのラベル--
! String_I_statistics    統計値(整数型)のラベル
! String_R_statistics    統計値(実数型)のラベル
! String_I_attribute_F   固定属性値(整数型)のラベル
! String_R_attribute_F   固定属性値(実数型)のラベル
! String_I_attribute_V   変動属性値(整数型)のラベル
! String_R_attribute_V   変動属性値(実数型)のラベル
!--出力データ--
! I_statistics           統計値(整数型)
! R_statistics           統計値(実数型)
! I_attribute_F          固定属性値(整数型)
! R_attribute_F          固定属性値(実数型)
! I_attribute_V          変動属性値(整数型)
! R_attribute_V          変動属性値(実数型)
!----------------------------------------
!
  real(8):: out_start,out_end,out_interval,out_time
  integer,parameter:: N_I_statistics=3
  integer,parameter:: N_R_statistics=1
  integer,parameter:: N_I_attribute_F=1
  integer,parameter:: N_R_attribute_F=1
  integer,parameter:: N_I_attribute_V=1
  integer,parameter:: N_R_attribute_V=5
  character(32):: String_I_statistics(N_I_statistics) &
                & =(/'Number of escaped ','Number of moving  ','Number of dead    '/)
  character(32):: String_R_statistics(N_R_statistics) &
                & ='(Null)'
  character(32):: String_I_attribute_F(N_I_attribute_F) &
                & ='Age'
  character(32):: String_R_attribute_F(N_R_attribute_F) &
                & ='Height'
  character(32):: String_I_attribute_V(N_I_attribute_V) &
                & ='0:escaped/1or2:moving/3:dead'
  character(32):: String_R_attribute_V(N_R_attribute_V) &
                & =(/'X         ','Y         ','Z         ','Velocity.X','Velocity.Y'/)
  integer:: I_statistics(N_I_statistics)
  integer:: I_attribute_F(N_I_attribute_F)
  integer:: I_attribute_V(N_I_attribute_V)
  real(4):: R_statistics(N_R_statistics)
  real(4):: R_attribute_F(N_R_attribute_F)
  real(4):: R_attribute_V(N_R_attribute_V)

end module m_output
