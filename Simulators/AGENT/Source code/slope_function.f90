subroutine slope_function(S,F)
!----------------------------------------
! 斜面勾配による移動速度の変化率を計算
!
! <入力>
! S: 斜面勾配(=高低差/2点間の距離)[-]
!
! <出力>
! F: 移動速度の補正係数[-]
!----------------------------------------
  implicit none
!
![arguments]
  real(8),intent(in):: S
  real(8),intent(out):: F
!
![local variables]
!  integer:: i,j
!
!
! 以下を編集
! サンプル：ハイキング関数
  F=1.191246*exp(-3.5d0*abs(S+0.05d0))
!
  return
end subroutine slope_function
