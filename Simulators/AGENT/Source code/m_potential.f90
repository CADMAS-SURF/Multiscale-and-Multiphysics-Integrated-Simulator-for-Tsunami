module m_potential
  implicit none
!----------------------------------------
! 解析領域は、CADMAS座標上の[xpin,ypin]〜[xpin+dxy*ipmax,ypin+dxy*jpmax]の長方形となる
!   ※ただしソルバー内では、[0,0]〜[dxy*ipmax,dxy*jpmax]として計算している。
! xpin,ypin           ポテンシャル格子の基点座標[m]
! ipmax,jpmax         ポテンシャル配列のサイズ
! dxy                 ポテンシャルメッシュの格子間隔[m]
! move_boundary       隣接セルへの進入可否を指定する配列
!--道標--
! n_signpost          道標数（兼、OnOffフラグ）
! i_signpost          避難所位置
! j_signpost          避難所位置
! r_signpost          道標の有効半径[m]
! theta_signpost      各道標の示す方向
! narea_signpost      領域内での各道標の有効範囲
!--避難経路ポテンシャル--
! n_shelter           避難所数（兼、OnOffフラグ）
! i_shelter,j_shelter 避難所位置
! shelter_height         避難所標高
! agent_shelter       各避難所に何人避難したかカウント
! pot_shelter         避難経路ポテンシャル配列
!--群衆ポテンシャル--
! n_mob               群衆ポテンシャルOnOffフラグ
! r_mob               エージェント数カウント範囲半径[m]
! nsum_agent          セル内エージェント数の配列
! pot_mob             群衆ポテンシャル配列
! pot_mob_revise      群衆ポテンシャル配列（エージェント自身の影響除去）
!--個別経路選択ポテンシャル---
! n_move_boundary     エージェント毎の進入可否を指定する配列
! n_pot_shelter       エージェント毎の避難経路ポテンシャル配列
! m_pot_shelter       避難所毎の避難経路ポテンシャル配列
! mn_pot_shelter      避難所毎のエージェント毎の避難経路ポテンシャル配列
! HH                  津波遭遇回避経路選択時の現在のi方向セル
! SS                  津波遭遇回避経路選択時の現在のj方向セル
! SC                  選択したセル個数
! SCMAX               選択したセル最大値
!----------------------------------------
!
  integer:: ipmax,jpmax
  real(8):: xpin,ypin,dxy
  integer,allocatable:: move_boundary(:,:)
!signpost
  integer:: n_signpost
  integer,allocatable:: i_signpost(:),j_signpost(:),narea_signpost(:,:)
  real(8),allocatable:: r_signpost(:),theta_signpost(:)
!shelter
  integer:: n_shelter
  integer,allocatable:: i_shelter(:),j_shelter(:),shelter_height(:)
  integer,allocatable:: agent_shelter(:)
  real(8),allocatable:: pot_shelter(:,:)
!mob
  integer:: n_mob
  real(8):: r_mob
  integer,allocatable:: nsum_agent(:,:)
  real(8),allocatable:: pot_mob(:,:),pot_mob_revise(:,:)
!indivisual
  real(8),allocatable:: m_pot_shelter(:,:,:)
  real(8),allocatable:: n_potential(:,:,:)
  
contains
!
!
  subroutine allocate_move_boundary(ierr)
!----------------------------------------
! 道か進入禁止か指定する配列へのメモリ割り当て
!----------------------------------------
!  move_boundary=0        : 道
!  move_boundary=-1       : 道ではない(進入不可)
!----------------------------------------
    implicit none
!
!  [arguments]
    integer,intent(out):: ierr
!
!  [local variables]
    integer::ierror
    character(32):: rout='allocate_move_boundary'
!
    ierr=0
!
    allocate(move_boundary(ipmax,jpmax),stat=ierror)
    if(ierror/=0) then
       ierr=-10
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate move_boundary arrays'
    endif
!
!初期化
    move_boundary(:,:)=0
!
    return
  end subroutine allocate_move_boundary

  subroutine allocate_signpost(ierr)
!----------------------------------------
! 道標関連配列へのメモリ割り当て
!----------------------------------------
    implicit none
!
!  [arguments]
    integer,intent(out):: ierr
!
!  [local variables]
    integer::ierror
    character(32):: rout='allocate_signpost'
!
    ierr=0
!
    allocate(i_signpost(n_signpost),j_signpost(n_signpost),r_signpost(n_signpost), &
       &     theta_signpost(n_signpost),narea_signpost(ipmax,jpmax),stat=ierror)
    if(ierror/=0) then
       ierr=-10
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate signpost arrays'
    endif
!
    return
  end subroutine allocate_signpost
!
!
  subroutine allocate_pot_shelter(ierr)
!----------------------------------------
! 避難経路ポテンシャル用配列へのメモリ割り当て
!----------------------------------------
    implicit none
!
!  [arguments]
    integer,intent(out):: ierr
!
!  [local variables]
    integer::ierror
    character(32):: rout='allocate_pot_shelter'
!
    ierr=0
!
    allocate(i_shelter(n_shelter),j_shelter(n_shelter),shelter_height(n_shelter) &
       &  ,agent_shelter(n_shelter),pot_shelter(ipmax,jpmax),stat=ierror)
    if(ierror/=0) then
       ierr=-10
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate shelter arrays'
    endif
!
    return
  end subroutine allocate_pot_shelter
!
!
  subroutine allocate_pot_mob(ierr)
!----------------------------------------
! 群衆ポテンシャル用配列へのメモリ割り当て
!----------------------------------------
    implicit none
!
!  [arguments]
    integer,intent(out):: ierr
!
!  [local variables]
    integer::ierror
    character(32):: rout='allocate_pot_mob'
!
    ierr=0
!
    allocate(nsum_agent(ipmax,jpmax), &
       &     pot_mob(ipmax,jpmax),pot_mob_revise(ipmax,jpmax),stat=ierror)
    if(ierror/=0) then
       ierr=-10
       call errmsg(rout,ierr)
       write(*,*) 'cannot allocate mob arrays'
    endif
!
    return
  end subroutine allocate_pot_mob
!
!  subroutine allocate_pot_danger(ierr)
!----------------------------------------
! 危険回避ポテンシャル用配列へのメモリ割り当て
!----------------------------------------
!    implicit none
!
!  [arguments]
!    integer,intent(out):: ierr
!
!  [local variables]
!    integer::ierror
!    character(32):: rout='allocate_pot_danger'
!
!    ierr=0
!
!    allocate(pot_danger(ipmax,jpmax),stat=ierror)
!    if(ierror/=0) then
!       ierr=-10
!       call errmsg(rout,ierr)
!       write(*,*) 'cannot allocate danger arrays'
!    endif
!    
!    return
!  end subroutine allocate_pot_danger

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine allocate_n_potential(n,ierr)
    !------------------------------------------
    ! n_potential
    !------------------------------------------
        implicit none
    !
    !  [arguments]
        integer,intent(in)::n
        integer,intent(out)::ierr
    !
    !  [local variables]
        integer::ierror
        character(32):: rout='allocate_n_pottential'
    !
        ierr=0
    !    
       allocate(n_potential(n,ipmax,jpmax),stat=ierror)
       if(ierror/=0) then
        ierr=-10
        call errmsg(rout,ierr)
        write(*,*)'cannot allocate n_potential arrays'
       endif
    !
       return
    end subroutine allocate_n_potential

   subroutine allocate_m_pot_shelter(m,ierr)
    !------------------------------------------
    ! m_pot_shelter
    !------------------------------------------
        implicit none
    !
    !  [arguments]
        integer,intent(in)::m
        integer,intent(out)::ierr
    !
    !  [local variables]
        integer::ierror
        character(32):: rout='allocate_m_pot_shelter'
    !
        ierr=0
    !    
       allocate(m_pot_shelter(m,ipmax,jpmax),stat=ierror)
       if(ierror/=0) then
        ierr=-10
        call errmsg(rout,ierr)
        write(*,*)'cannot allocate m_po_shelter arrays'
       endif
    !
       return
    end subroutine allocate_m_pot_shelter

end module m_potential
