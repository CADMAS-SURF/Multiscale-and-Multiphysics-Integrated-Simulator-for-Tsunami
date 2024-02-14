recursive subroutine recursive_search_shelter(delta,inow,jnow,ierr)
!----------------------------------------
! 各セルの避難所からの距離を求める（再帰ルーチン）
! ※周囲8方向で探索するver.
!----------------------------------------
  use m_potential,only: ipmax,jpmax,move_boundary,pot_shelter,dxy
  use m_agent,only: n_slope
  use m_cadmas,only: height
  implicit none
!
![arguments]
  integer,intent(out):: ierr
  integer,intent(in):: inow,jnow !位置(この位置の周囲を探索することになる)
  real(8),intent(in):: delta     !避難所から(inow,jnow)の距離
!
![local variables]
  real(8),parameter:: root2=1.414213562373095d0
  real(8),parameter:: root5=2.236067977499789d0
  integer:: inext,jnext,inext1,jnext1,imid1,jmid1,imid2,jmid2
  real(8):: dnext,dr,S,F,hmid,S1,F1,S2,F2
  character(32):: rout='recursive_search_shelter'
!
  integer,save:: count=0
  integer:: nskip
!
  ierr=0
  nskip=0
!
!
!左方向探索
  inext=inow-1
  jnext=jnow
  inext1=min(max(1,inext),ipmax)
  jnext1=min(max(1,jnext),jpmax)
  dr=1.0d0
  if( n_slope==1 ) then
     S=-(height(inext1,jnext1)-height(inow,jnow))/(dr*dxy)
     call slope_function(S,F)
     dr=dr/F
  endif
  dnext=delta+dr
  if(inext<1)then                               !領域外のとき
     nskip=nskip+1
     !skip
  elseif(pot_shelter(inext,jnext)<=dnext      & !他により短い経路があるとき
   & .or.move_boundary(inext,jnext)==-1 & !左方向から進入出来ないのとき
   & )then
     nskip=nskip+1
     !skip
  else
     pot_shelter(inext,jnext)=dnext
     call recursive_search_shelter(dnext,inext,jnext,ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!右方向探索
  inext=inow+1
  jnext=jnow
  inext1=min(max(1,inext),ipmax)
  jnext1=min(max(1,jnext),jpmax)
  dr=1.0d0
  if( n_slope==1 ) then
     S=-(height(inext1,jnext1)-height(inow,jnow))/(dr*dxy)
     call slope_function(S,F)
     dr=dr/F
  endif
  dnext=delta+dr
  if(inext>ipmax)then                           !領域外のとき
     nskip=nskip+1
     !skip
  elseif(pot_shelter(inext,jnext)<=dnext      & !他により短い経路があるとき
   & .or.move_boundary(inext,jnext)==-1 & !右方向から進入出来ないのとき
   & )then
     nskip=nskip+1
     !skip
  else
     pot_shelter(inext,jnext)=dnext
     call recursive_search_shelter(dnext,inext,jnext,ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!下方向探索
  inext=inow
  jnext=jnow-1
  inext1=min(max(1,inext),ipmax)
  jnext1=min(max(1,jnext),jpmax)
  dr=1.0d0
  if( n_slope==1 ) then
     S=-(height(inext1,jnext1)-height(inow,jnow))/(dr*dxy)
     call slope_function(S,F)
     dr=dr/F
  endif
  dnext=delta+dr
  if(jnext<1)then                               !領域外のとき
     nskip=nskip+1
     !skip
  elseif(pot_shelter(inext,jnext)<=dnext      & !他により短い経路があるとき
   & .or.move_boundary(inext,jnext)==-1 & !下方向から進入出来ないのとき
   & )then
     nskip=nskip+1
     !skip
  else
     pot_shelter(inext,jnext)=dnext
     call recursive_search_shelter(dnext,inext,jnext,ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!上方向探索
  inext=inow  ; jnext=jnow+1
  inext1=min(max(1,inext),ipmax)
  jnext1=min(max(1,jnext),jpmax)
  dr=1.0d0
  if( n_slope==1 ) then
     S=-(height(inext1,jnext1)-height(inow,jnow))/(dr*dxy)
     call slope_function(S,F)
     dr=dr/F
  endif
  dnext=delta+dr
  if(jnext>jpmax)then                           !領域外のとき
     nskip=nskip+1
     !skip
  elseif(pot_shelter(inext,jnext)<=dnext      & !他により短い経路があるとき
   & .or.move_boundary(inext,jnext)==-1 & !上方向から進入出来ないのとき
   & )then
     nskip=nskip+1
     !skip
  else
     pot_shelter(inext,jnext)=dnext
     call recursive_search_shelter(dnext,inext,jnext,ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!左下方向探索
  inext=inow-1; jnext=jnow-1
  inext1=min(max(1,inext),ipmax)
  jnext1=min(max(1,jnext),jpmax)
  dr=root2
  if( n_slope==1 ) then
     S=-(height(inext1,jnext1)-height(inow,jnow))/(dr*dxy)
     call slope_function(S,F)
     dr=dr/F
  endif
  dnext=delta+dr
  if(inext<1.or.jnext<1)then                          !領域外のとき
     nskip=nskip+1
     !skip
  elseif(pot_shelter(inext,jnext)<=dnext            & !他により短い経路があるとき
   & .or.move_boundary(inext,jnext)==-1      & !左下方向から進入出来ないのとき
   & )then
     nskip=nskip+1
     !skip
  else
     pot_shelter(inext,jnext)=dnext
     call recursive_search_shelter(dnext,inext,jnext,ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!左上方向探索
  inext=inow-1; jnext=jnow+1
  inext1=min(max(1,inext),ipmax)
  jnext1=min(max(1,jnext),jpmax)
  dr=root2
  if( n_slope==1 ) then
     S=-(height(inext1,jnext1)-height(inow,jnow))/(dr*dxy)
     call slope_function(S,F)
     dr=dr/F
  endif
  dnext=delta+dr
  if(inext<1.or.jnext>jpmax)then                      !領域外のとき
     nskip=nskip+1
     !skip
  elseif(pot_shelter(inext,jnext)<=dnext            & !他により短い経路があるとき
   & .or.move_boundary(inext,jnext)==-1      & !左上方向から進入出来ないのとき
   & )then
     nskip=nskip+1
     !skip
  else
     pot_shelter(inext,jnext)=dnext
     call recursive_search_shelter(dnext,inext,jnext,ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!右下方向探索
  inext=inow+1; jnext=jnow-1
  inext1=min(max(1,inext),ipmax)
  jnext1=min(max(1,jnext),jpmax)
  dr=root2
  if( n_slope==1 ) then
     S=-(height(inext1,jnext1)-height(inow,jnow))/(dr*dxy)
     call slope_function(S,F)
     dr=dr/F
  endif
  dnext=delta+dr
  if(inext>ipmax.or.jnext<1)then                      !領域外のとき
     nskip=nskip+1
     !skip
  elseif(pot_shelter(inext,jnext)<=dnext            & !他により短い経路があるとき
   & .or.move_boundary(inext,jnext)==-1      & !右下方向から進入出来ないのとき
   & )then
     nskip=nskip+1
     !skip
  else
     pot_shelter(inext,jnext)=dnext
     call recursive_search_shelter(dnext,inext,jnext,ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
!右上方向探索
  inext=inow+1; jnext=jnow+1
  inext1=min(max(1,inext),ipmax)
  jnext1=min(max(1,jnext),jpmax)
  dr=root2
  if( n_slope==1 ) then
     S=-(height(inext1,jnext1)-height(inow,jnow))/(dr*dxy)
     call slope_function(S,F)
     dr=dr/F
  endif
  dnext=delta+dr
  if(inext>ipmax.or.jnext>jpmax)then                  !領域外のとき
     nskip=nskip+1
     !skip
  elseif(pot_shelter(inext,jnext)<=dnext            & !他により短い経路があるとき
   & .or.move_boundary(inext,jnext)==-1      & !右上方向から進入出来ないのとき
   & )then
     nskip=nskip+1
     !skip
  else
     pot_shelter(inext,jnext)=dnext
     call recursive_search_shelter(dnext,inext,jnext,ierr)
      if(ierr<0) call errstop(rout,ierr)
  endif
!
  if( nskip==8 ) then
     count=count+1
     !if( mod(count,100000)==0 ) &
     !     &     write(90,*) '... making potential in cell (',inow,',',jnow,'), count=',count
  endif
  return
end subroutine recursive_search_shelter
