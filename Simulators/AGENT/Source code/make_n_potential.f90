subroutine make_n_potential(n,flag,ierr)
!----------------------------------------
! エージェント毎の津波に遭遇しない経路の算出
!----------------------------------------
  use m_potential,only: ipmax,jpmax,dxy &
    & ,n_shelter,i_shelter,j_shelter &
    & ,m_pot_shelter &
    & ,move_boundary &
    & ,n_potential
  use m_agent,only: n_agent,i_agent,j_agent,n_slope,agent_status,agent_start
  use m_cadmas,only: height
  use m_prob,only: ini_prob,relaxation_rate,tsunami_prob_directory
  use m_danger,only: danger_path
  implicit none
!
![arguments]
  integer,intent(out):: ierr
  integer,intent(in):: n,flag
!
![local variables]
!
  integer i,j,k,m,mm,l,nskip,evacuate_flag &
          & ,i_now,j_now,i_next,j_next,i_next_temporary,j_next_temporary,i_prev,j_prev,cellstep,shelter_target
  integer counter,nt,dummy_int,ierr2
  integer,allocatable:: route_i(:),route_j(:)
  real(8):: m_pot_agentnow(n_shelter)
  integer:: sort_index(n_shelter)
  real(8) potmin,pot_next,dr,S,F,PT,EPT,EPT2,EPT_temporary,set_prob,set_prob_debug,dummy_real,start_time
  real(8),parameter:: root2=1.414213562373095d0
  real(8),allocatable:: potential(:,:),potential_temporary(:,:),tsunami_arrival_time(:,:),tsunami_arrival_prob(:,:)
  character(32):: rout='make_agent_route'
  character(128) prob_filename,tsunami_prob_path,filename,filename2


!!!!!道上にいない場合はretrun
    if(move_boundary(i_agent(n),j_agent(n)) == -1) then
        write(101,'(i4)',advance='no')i_agent(n)
        write(101,'(i4)',advance='no')j_agent(n)
        write(101,'(i4)',advance='no')n
        write(101,'(a)')":is not on the cell for roads"
        return
    endif   

!!!!避難所毎の避難者の初期位置のポテンシャルの値を、バブルソートし、インデックスをsort_index配列に格納
    i_now=i_agent(n)
    j_now=j_agent(n)
    do m=1,n_shelter
        m_pot_agentnow(m) = m_pot_shelter(m,i_now,j_now)
        sort_index(m) = m
    enddo
    do m=1,n_shelter-1
        do mm=m+1,n_shelter
            if(m_pot_agentnow(m) > m_pot_agentnow(mm)) then
                dummy_real = m_pot_agentnow(m)
                m_pot_agentnow(m) = m_pot_agentnow(mm)
                m_pot_agentnow(mm) = dummy_real
                dummy_int = sort_index(m)
                sort_index(m) = sort_index(mm)
                sort_index(mm) = dummy_int
            endif
        enddo
    enddo

    start_time = agent_start(n)

!!!!!!!!!!!!
!!!!最短!!!!
!!!!!!!!!!!!
    if(flag==0)then
        
        do j=1,jpmax
            do i=1,ipmax
                n_potential(n,i,j) = m_pot_shelter(sort_index(1),i,j)
            enddo
        enddo
        return

!!!!!!!!!!!!!!!
!!!!津波回避!!!!
!!!!!!!!!!!!!!!
    else if(flag==1)then

        allocate(route_i(2048))
        allocate(route_j(2048))
        allocate(potential(ipmax,jpmax))
        allocate(potential_temporary(ipmax,jpmax))
        allocate(tsunami_arrival_time(ipmax,jpmax))
        route_i=0
        route_j=0
        potential=0
        potential_temporary=0
        tsunami_arrival_time=0


        !!津波到達時間読み込み
        filename = trim(danger_path)
        open(60,file=filename,status='old')
        do j=jpmax,1,-1
            read(60,*)(tsunami_arrival_time(i,j),i=1,ipmax)
        enddo
        close(60)

        ierr=0

        potmin=9999d0
        EPT_temporary = 999999d0
        evacuate_flag = 0

        !write(110,'(a)',advance='no')"n_agent"
        !write(110,*)n

        counter = 0
        shelter_loop: do mm=1,n_shelter

            m=sort_index(mm)

            cellstep = 1

            i_now=i_agent(n)
            j_now=j_agent(n)

            !暫定の避難時間の方が、避難開始位置のポテンシャル場（＝避難所までの最短所要時間）より小さいとき、shelter_loopを抜ける
            if(EPT_temporary<=m_pot_shelter(m,i_now,j_now)) then
                exit shelter_loop
            endif

            route_i(:) = 0
            route_i(1) = i_now
            route_j(:) = 0
            route_j(1) = j_now

            !write(110,'(a)',advance='no')"shelter: "
            !write(110,*)m

            do j=1,jpmax
                do i=1,ipmax
                    potential(i,j) = m_pot_shelter(m,i,j)
                enddo
            enddo

            EPT = 0

            !write(112,*)potential

            !do j=jpmax,1,-1
            !    write(115,*)(potential(i,j),i=1,ipmax)
            !enddo

            !do j=jpmax,1,-1
            !    do i=1,ipmax
            !        dummy = potential(i,j)
            !        write(113,'(f16.4)',advance='no')dummy
            !    enddo
            !enddo

            search_loop: do
                
                potmin = potential(i_now,j_now)
                !write(110,*)"searching ",i_now,j_now,potmin,cellstep
                nskip = 0

                !右
                i_next = i_now+1
                j_next = j_now
                if(move_boundary(i_next,j_next) == -1) then
                    nskip = nskip + 1
                elseif(i_next>ipmax) then
                    nskip = nskip + 1
                else
                    pot_next = potential(i_next,j_next)
                    !write(110,*)"右"
                    !write(110,*)potmin,pot_next
                    if(pot_next>=9999) then
                        nskip = nskip + 1
                    elseif(pot_next <= potmin) then
                        potmin = pot_next
                        i_next_temporary = i_next
                        j_next_temporary = j_next
                        dr = 1.0
                    else
                        nskip = nskip + 1
                    endif
                endif

                !右下
                i_next = i_now+1
                j_next = j_now-1
                if(move_boundary(i_next,j_next) == -1) then
                    nskip = nskip + 1
                elseif(i_next>ipmax .or. j_next<=0) then 
                    nskip = nskip + 1
                else
                    pot_next = potential(i_next,j_next)
                    !write(110,*)"右下"
                    !write(110,*)potmin,pot_next
                    if(pot_next>=9999) then
                        nskip = nskip + 1
                    elseif(pot_next <= potmin) then
                        potmin = pot_next
                        i_next_temporary = i_next
                        j_next_temporary = j_next
                        dr = root2
                    else
                        nskip = nskip + 1
                    endif
                endif

                !下
                i_next = i_now
                j_next = j_now-1
                if(move_boundary(i_next,j_next) == -1) then
                    nskip = nskip + 1
                elseif(j_next<=0) then
                    nskip = nskip + 1
                else
                    pot_next = potential(i_next,j_next)
                    !write(110,*)"下"
                    !write(110,*)potmin,pot_next
                    if(pot_next>=9999) then
                        nskip = nskip + 1
                    elseif(pot_next <= potmin) then
                        potmin = pot_next
                        i_next_temporary = i_next
                        j_next_temporary = j_next
                        dr=1.0
                    else
                        nskip = nskip + 1
                    endif
                endif

                !左下
                i_next = i_now-1
                j_next = j_now-1
                if(move_boundary(i_next,j_next) == -1) then
                    nskip = nskip + 1
                elseif(i_next<=0 .or. j_next<=0) then
                    nskip = nskip + 1
                else
                    pot_next = potential(i_next,j_next)
                    !write(110,*)"左下"
                    !write(110,*)potmin,pot_next
                    if(pot_next>=9999) then
                        nskip = nskip + 1
                    elseif(pot_next <= potmin) then
                        potmin = pot_next
                        i_next_temporary = i_next
                        j_next_temporary = j_next
                        dr = root2
                    else
                        nskip = nskip + 1
                    endif
                endif

                !左
                i_next = i_now-1
                j_next = j_now
                if(move_boundary(i_next,j_next) == -1) then
                    nskip = nskip + 1
                elseif(i_next<=0) then
                    nskip = nskip + 1
                else
                    pot_next = potential(i_next,j_next)
                    !write(*,*)"左"
                    !write(110,*)potmin,pot_next
                    if(pot_next>=9999) then
                        nskip = nskip + 1
                    elseif(pot_next <= potmin) then 
                        potmin = pot_next
                        i_next_temporary = i_next
                        j_next_temporary = j_next
                        dr = 1.0
                    else
                        nskip = nskip + 1
                    endif
                endif

                !左上
                i_next = i_now-1
                j_next = j_now+1
                if(move_boundary(i_next,j_next) == -1) then
                    nskip = nskip + 1
                elseif(i_next<=0 .or. j_next>jpmax) then
                    nskip = nskip + 1
                else
                    pot_next = potential(i_next,j_next)
                    !write(110,*)"左上"
                    !write(110,*)potmin,pot_next
                    if(pot_next>=9999) then
                        nskip = nskip + 1
                    elseif(pot_next <= potmin) then
                        potmin = pot_next
                        i_next_temporary = i_next
                        j_next_temporary = j_next
                        dr = root2
                    else
                        nskip = nskip + 1
                    endif
                endif

                !上
                i_next = i_now
                j_next = j_now+1
                if(move_boundary(i_next,j_next) == -1) then
                    nskip = nskip + 1
                elseif(j_next>jpmax) then
                    nskip = nskip + 1
                else
                    pot_next = potential(i_next,j_next)
                    !write(110,*)"上"
                    !write(110,*)potmin,pot_next
                    if(pot_next>=9999) then
                        nskip = nskip + 1
                    elseif(pot_next <= potmin) then
                        potmin = pot_next
                        i_next_temporary = i_next
                        j_next_temporary = j_next
                        dr = 1.0
                    else
                        nskip = nskip + 1
                    endif
                endif
                
                !右上
                i_next = i_now+1
                j_next = j_now+1
                if(move_boundary(i_next,j_next) == -1) then
                    nskip = nskip + 1
                elseif(i_next>ipmax .or. j_next>jpmax) then 
                    nskip = nskip + 1
                else
                    pot_next = potential(i_next,j_next)
                    !write(110,*)"右上"
                    !write(110,*)potmin,pot_next
                    if(pot_next>=9999) then
                        nskip = nskip + 1
                    elseif(pot_next <= potmin) then
                        potmin = pot_next
                        i_next_temporary = i_next
                        j_next_temporary = j_next
                        dr = root2
                    else
                        nskip = nskip + 1
                    endif
                endif

                !write(110,*)"i_next_temp",i_next_temporary,"j_next_temp",j_next_temporary

                !停留点確認
                if(cellstep >= 3)then
                    if(route_i(cellstep)==route_i(cellstep-2) .and. route_j(cellstep)==route_j(cellstep-2))then
                        !write(110,'(a)',advance='no')"stop point: "
                        !write(110,'(i5)',advance='no')i_now
                        !write(110,'(i5)')j_now
                        potential(i_now,j_now) = 9999
                        cellstep = cellstep - 1
                        i_prev = route_i(cellstep)
                        j_prev = route_j(cellstep)
                        if(n_slope == 1) then
                            S = (height(i_prev,j_prev)-height(i_now,j_now))/(dxy*dr)
                            call slope_function(S,F)
                            dr = dr/F
                        endif
                        potential(i_now,j_now) = 9999
                        PT = dxy*dr
                        EPT = EPT - PT
                        i_now = i_prev
                        j_now = j_prev
                        go to 1000
                    endif
                endif

                !全方向進行不可
                if(nskip == 8) then
                    !検索位置が初期位置の場合
                    if((i_now==i_agent(n) .and. j_now==j_agent(n)) .or. cellstep == 1) then
                        !write(101,'(i5)',advance='no')n,m,set_prob
                        !write(101,'(a)')": cannot evacuate"
                        !write(101,*)"cannot evacuate n,m,set_prob"
                        !write(101,*)n,m,set_prob
                        !write(110,*)"start position"
                        exit search_loop

                    !それ以外＝１回分戻して再検索
                    else
                        !write(110,*)"There is no cell to advance "
                        cellstep = cellstep - 1
                        i_prev = route_i(cellstep)
                        j_prev = route_j(cellstep)
                        if(n_slope == 1) then
                            S = (height(i_prev,j_prev)-height(i_now,j_now))/(dxy*dr)
                            call slope_function(S,F)
                            dr = dr/F
                        endif
                        potential(i_now,j_now) = 9999
                        PT = dxy*dr
                        EPT = EPT - PT
                        i_now = i_prev
                        j_now = j_prev
                    endif

                !進行可能
                else
                    i_next = i_next_temporary
                    j_next = j_next_temporary

                    !避難時間計算
                    if(i_now /= i_next .and. j_now /= i_next) then
                        dr = root2
                    else
                        dr = 1.0d0
                    endif
                    if(n_slope == 1) then
                        S = (height(i_now,j_now)-height(i_next,j_next))/(dxy*dr)
                        call slope_function(S,F)
                        dr = dr/F
                    endif
                    PT = dxy*dr
                    EPT = EPT + PT
                    if(EPT<0)then
                        EPT = 0
                        !write(1100,*)"EPT is little than 0"
                    endif
                    EPT2 = start_time + EPT
                    nt = nint(EPT2)
                    !write(110,'(a)',advance='no')"EPT2: "
                    !write(110,'(f8.2)')EPT2

                    !津波到達時間と比較
                    if(EPT2 >= tsunami_arrival_time(i_next,j_next) .and. tsunami_arrival_time(i_next,j_next)/=0) then
                        !write(110,*)"tsunami arrival time is exceed"
                        !write(110,*)i_next,j_next,tsunami_arrival_time(i_next,j_next)
                        potential(i_next,j_next) = 9999
                        EPT = EPT - PT
                    else
                        i_now = i_next
                        j_now = j_next
                        cellstep = cellstep + 1
                        route_i(cellstep) = i_next
                        route_j(cellstep) = j_next
                        !避難完了の場合 ＝　EPT、pot、仮格納
                        if(i_shelter(m)==i_now .and. j_shelter(m)==j_now) then
                            !write(101,'(a)',advance='no')"evacuated: "
                            !write(101,'(i5)',advance='no')n
                            !write(101,'(i5)',advance='no')m
                            !write(101,'(f8.2)')EPT
                            if(EPT < EPT_temporary) then
                                do j=1,jpmax
                                    do i=1,ipmax
                                        potential_temporary(i,j) = potential(i,j)
                                    enddo
                                enddo
                                EPT_temporary = EPT
                                shelter_target = m
                                evacuate_flag = 1
                            endif
                            exit search_loop
                        endif
                    endif

                endif
                1000 continue

            enddo search_loop
        enddo shelter_loop

        if(evacuate_flag == 1) then
           do j=1,jpmax
               do i=1,ipmax
                   n_potential(n,i,j) = potential_temporary(i,j)
                enddo
           enddo
        else
           !write(101,'(a)',advance='no')"cannot evacuate: "
           !write(101,'(i5)')n

           do j=1,jpmax
               do i=1,ipmax
                    n_potential(n,i,j) = m_pot_shelter(sort_index(1),i,j) !避難不可=最短経路を入れる
                    EPT_temporary = m_pot_shelter(sort_index(1),i,j)
               enddo
           enddo
        endif


        !write(102,'(a)',advance='no')"agent debug: "
        !write(102,'(i5)',advance='no')n
        !write(102,'(i5)',advance='no')shelter_target
        !write(102,'(f8.2)')EPT_temporary

        deallocate(route_i,route_j,potential,potential_temporary,tsunami_arrival_time)
        return

!!!!!!!!!!!!!!!
!!!!確率回避!!!!
!!!!!!!!!!!!!!!
    elseif(flag==2)then
        allocate(route_i(2048))
        allocate(route_j(2048))
        allocate(potential(ipmax,jpmax))
        allocate(potential_temporary(ipmax,jpmax))
        allocate(tsunami_arrival_prob(ipmax,jpmax))
        route_i=0
        route_j=0
        potential=0
        potential_temporary=0
        tsunami_arrival_prob=0
    
        potmin=9999
        EPT_temporary = 9999
        evacuate_flag = 0
    
        !write(110,'(a)',advance='no')"n_agent"
        !write(110,*)n
    
        counter = 0
        set_prob = ini_prob
        do while (set_prob <= 100)
            set_prob = ini_prob + relaxation_rate*dble(counter)
            counter = counter + 1
    
            !write(110,'(a)',advance='no')"set_prob: "
            !write(110,*)set_prob
            
            shelter_loop2: do mm=1,n_shelter
                m=sort_index(mm)
    
                cellstep = 1    
    
                i_now=i_agent(n)
                j_now=j_agent(n)

                !暫定の避難時間より、避難開始位置のポテンシャル場（＝避難所までの最短所要時間）が小さいとき、shelter_loopを抜ける
                if(EPT_temporary<=m_pot_shelter(m,i_now,j_now)) then
                    exit shelter_loop2
                endif
    
                route_i(:) = 0
                route_i(1) = i_now
                route_j(:) = 0
                route_j(1) = j_now
    
                !write(110,'(a)',advance='no')"shelter: "
                !write(110,*)m
    
                do j=1,jpmax
                    do i=1,ipmax
                        potential(i,j) = m_pot_shelter(m,i,j)
                    enddo
                enddo
    
                EPT = 0

                search_loop2: do
    
                    potmin = potential(i_now,j_now)
                    !write(110,*)"searching ",i_now,j_now,potmin,cellstep
                    nskip = 0
    
                    !右
                    i_next = i_now+1
                    j_next = j_now
                    if(move_boundary(i_next,j_next) == -1) then
                        nskip = nskip + 1
                    elseif(i_next>ipmax) then
                        nskip = nskip + 1
                    else
                        pot_next = potential(i_next,j_next)
                        !write(110,*)"右"
                        !write(110,*)potmin,pot_next
                        if(pot_next>=9999) then
                            nskip = nskip + 1
                        elseif(pot_next <= potmin) then
                            potmin = pot_next
                            i_next_temporary = i_next
                            j_next_temporary = j_next
                        else
                            nskip = nskip + 1
                        endif
                    endif
    
                    !右下
                    i_next = i_now+1
                    j_next = j_now-1
                    if(move_boundary(i_next,j_next) == -1) then
                        nskip = nskip + 1
                    elseif(i_next>ipmax .or. j_next<=0) then 
                        nskip = nskip + 1
                    else
                        pot_next = potential(i_next,j_next)
                        if(n==1.and.m==1)then
                        !write(110,*)"右下"
                        !write(110,*)potmin,pot_next
                        endif
                        if(pot_next>=9999) then
                            nskip = nskip + 1
                        elseif(pot_next <= potmin) then
                            potmin = pot_next
                            i_next_temporary = i_next
                            j_next_temporary = j_next
                        else
                            nskip = nskip + 1
                        endif
                    endif
    
                    !下
                    i_next = i_now
                    j_next = j_now-1
                    if(move_boundary(i_next,j_next) == -1) then
                        nskip = nskip + 1
                    elseif(j_next<=0) then
                        nskip = nskip + 1
                    else
                        pot_next = potential(i_next,j_next)
                        !write(110,*)"下"
                        !write(110,*)potmin,pot_next
                        if(pot_next>=9999) then
                            nskip = nskip + 1
                        elseif(pot_next <= potmin) then
                            potmin = pot_next
                            i_next_temporary = i_next
                            j_next_temporary = j_next
                        else
                            nskip = nskip + 1
                        endif
                    endif
    
                    !左下
                    i_next = i_now-1
                    j_next = j_now-1
                    if(move_boundary(i_next,j_next) == -1) then
                        nskip = nskip + 1
                    elseif(i_next<=0 .or. j_next<=0) then
                        nskip = nskip + 1
                    else
                        pot_next = potential(i_next,j_next)
                        !write(110,*)"左下"
                        !write(110,*)potmin,pot_next
                        if(pot_next>=9999) then
                            nskip = nskip + 1
                        elseif(pot_next <= potmin) then
                            potmin = pot_next
                            i_next_temporary = i_next
                            j_next_temporary = j_next
                        else
                            nskip = nskip + 1
                        endif
                    endif
    
                    !左
                    i_next = i_now-1
                    j_next = j_now
                    if(move_boundary(i_next,j_next) == -1) then
                        nskip = nskip + 1
                    elseif(i_next<=0) then
                        nskip = nskip + 1
                    else
                        pot_next = potential(i_next,j_next)
                        !write(*,*)"左"
                        !write(110,*)potmin,pot_next
                        if(pot_next>=9999) then
                            nskip = nskip + 1
                        elseif(pot_next <= potmin) then 
                            potmin = pot_next
                            i_next_temporary = i_next
                            j_next_temporary = j_next
                        else
                            nskip = nskip + 1
                        endif
                    endif
    
                    !左上
                    i_next = i_now-1
                    j_next = j_now+1
                    if(move_boundary(i_next,j_next) == -1) then
                        nskip = nskip + 1
                    elseif(i_next<=0 .or. j_next>jpmax) then
                        nskip = nskip + 1
                    else
                        pot_next = potential(i_next,j_next)
                        !write(110,*)"左上"
                        !write(110,*)potmin,pot_next
                        if(pot_next>=9999) then
                            nskip = nskip + 1
                        elseif(pot_next <= potmin) then
                            potmin = pot_next
                            i_next_temporary = i_next
                            j_next_temporary = j_next
                        else
                            nskip = nskip + 1
                        endif
                    endif
    
                    !上
                    i_next = i_now
                    j_next = j_now+1
                    if(move_boundary(i_next,j_next) == -1) then
                        nskip = nskip + 1
                    elseif(j_next>jpmax) then
                        nskip = nskip + 1
                    else
                        pot_next = potential(i_next,j_next)
                        !write(110,*)"上"
                        !write(110,*)potmin,pot_next
                        if(pot_next>=9999) then
                            nskip = nskip + 1
                        elseif(pot_next <= potmin) then
                            potmin = pot_next
                            i_next_temporary = i_next
                            j_next_temporary = j_next
                        else
                            nskip = nskip + 1
                        endif
                    endif
                    
                    !右上
                    i_next = i_now+1
                    j_next = j_now+1
                    if(move_boundary(i_next,j_next) == -1) then
                        nskip = nskip + 1
                    elseif(i_next>ipmax .or. j_next>jpmax) then 
                        nskip = nskip + 1
                    else
                        pot_next = potential(i_next,j_next)
                        !write(110,*)"右上"
                        !write(110,*)potmin,pot_next
                        if(pot_next>=9999) then
                            nskip = nskip + 1
                        elseif(pot_next <= potmin) then
                            potmin = pot_next
                            i_next_temporary = i_next
                            j_next_temporary = j_next
                        else
                            nskip = nskip + 1
                        endif
                    endif
    
                    !write(110,*)"i_next_temp",i_next_temporary,"j_next_temp",j_next_temporary
    
                    !停留点確認
                    if(cellstep >= 3)then
                        if(route_i(cellstep)==route_i(cellstep-2) .and. route_j(cellstep)==route_j(cellstep-2))then
                            !write(110,'(a)',advance='no')"stop point: "
                            !write(110,'(i5)',advance='no')i_now
                            !write(110,'(i5)')j_now
                            potential(i_now,j_now) = 9999
                            cellstep = cellstep - 1
                            i_prev = route_i(cellstep)
                            j_prev = route_j(cellstep)
                            if(i_now /= i_prev .and. j_now /= j_prev) then
                                dr = root2
                            else
                                dr = 1.0d0
                            endif
                            if(n_slope == 1) then
                                S = (height(i_prev,j_prev)-height(i_now,j_now))/(dxy*dr)
                                call slope_function(S,F)
                                dr = dr/F
                            endif
                            potential(i_now,j_now) = 9999
                            PT = dxy*dr
                            EPT = EPT - PT
                            i_now = i_prev
                            j_now = j_prev
                            go to 1002
                        endif
                    endif
    
                    !全方向進行不可
                    if(nskip == 8) then
                        !検索位置が初期位置の場合
                        if((i_now==i_agent(n) .and. j_now==j_agent(n)) .or. cellstep == 1) then
                            !write(101,'(i5)',advance='no')n,m,set_prob
                            !write(101,'(a)')": cannot evacuate"
                            !write(101,*)"cannot evacuate n,m,set_prob"
                            !write(101,*)n,m,set_prob
                            !exit shelter_loop

                            !write(110,*)"start position"
                            exit search_loop2
    
                        !それ以外＝１回分戻して再検索
                        else
                            !write(110,*)"There is no cell to advance "
                            cellstep = cellstep - 1
                            i_prev = route_i(cellstep)
                            j_prev = route_j(cellstep)
                            if(i_now /= i_prev .and. j_now /= j_prev) then
                                dr = root2
                            else
                                dr = 1.0d0
                            endif
                            if(n_slope == 1) then
                                S = (height(i_prev,j_prev)-height(i_now,j_now))/(dxy*dr)
                                call slope_function(S,F)
                                dr = dr/F
                            endif
                            potential(i_now,j_now) = 9999
                            PT = dxy*dr
                            EPT = EPT - PT
                            i_now = i_prev
                            j_now = j_prev
                        endif
    
                    !進行可能
                    else
                        i_next = i_next_temporary
                        j_next = j_next_temporary
    
                        !避難時間計算
                        if(i_now /= i_next .and. j_now /= i_next) then
                            dr = root2
                        else
                            dr = 1.0d0
                        endif
                        if(n_slope == 1) then
                            S = (height(i_now,j_now)-height(i_next,j_next))/(dxy*dr)
                            call slope_function(S,F)
                            dr = dr/F
                        endif
                        PT = dxy*dr
                        EPT = EPT + PT
                        if(EPT<0)then
                            EPT = 0
                            !write(1100,*)"EPT is little than 0"
                        endif
                        nt = nint(start_time + EPT)
                        !write(110,*)"EPT: ",EPT
    
                        !津波浸水確率と比較
                        !call make_potential_danger(nt,ierr)
                        write(prob_filename,'(i4.4)')nt
                        prob_filename = trim(prob_filename) // ".txt"
                        tsunami_prob_path = trim(tsunami_prob_directory) // trim(prob_filename)
                        open(unit=70,file=trim(tsunami_prob_path),status='old')
                        do j=jpmax,1,-1
                            read(70,*)(tsunami_arrival_prob(i,j),i=1,ipmax)
                        end do
                        close(70)


                        if(tsunami_arrival_prob(i_next,j_next) >= set_prob) then
                            !write(110,*)"tsunami_prob is out of set_prob"
                            !write(110,*)i_next,j_next,tsunami_arrival_prob(i_next,j_next),set_prob
                            potential(i_next,j_next) = 9999
                            EPT = EPT - PT
                        else
                            i_now = i_next
                            j_now = j_next
                            cellstep = cellstep + 1
                            route_i(cellstep) = i_next
                            route_j(cellstep) = j_next
                            !避難完了の場合 ＝　EPT、pot、仮格納
                            if(i_shelter(m)==i_now .and. j_shelter(m)==j_now) then
                                !write(101,'(a)',advance='no')"evacuated: "
                                !write(101,'(i5)',advance='no')n
                                !write(101,'(i5)',advance='no')m
                                !write(101,'(f8.2)',advance='no')set_prob
                                !write(101,'(f8.2)',advance='no')m_pot_agentnow(mm)
                                !write(101,'(f8.2)')EPT
                                if(EPT < EPT_temporary) then
                                    do j=1,jpmax
                                        do i=1,ipmax
                                            potential_temporary(i,j) = potential(i,j)
                                        enddo
                                    enddo
                                    EPT_temporary = EPT
                                    set_prob_debug = set_prob
                                    shelter_target = m
                                    evacuate_flag = 1
                                endif
                                exit search_loop2
                            endif
                        endif
    
                    endif
                    1002 continue
    
                enddo search_loop2
            enddo shelter_loop2
    
            !あるagent(n)に対して最も低い津波浸水確率の経路探索が完了したため、ループを抜ける
            if(evacuate_flag == 1) then
                do j=1,jpmax
                    do i=1,ipmax
                        n_potential(n,i,j) = potential_temporary(i,j)
                    enddo
                enddo
                exit
            endif
            !100%までいってもない場合
            if(set_prob == 100) then
                !write(101,'(a)',advance='no')"cannot evacuate: "
                !write(101,'(i5)')n
                set_prob_debug = 100
                do j=1,jpmax
                    do i=1,ipmax
                        n_potential(n,i,j)= m_pot_shelter(sort_index(1),i,j) !避難不可=最短経路を入れる
                    enddo
                enddo
            endif
        enddo
    
        !write(102,'(a)',advance='no')"agent debug: "
        !write(102,'(i5)',advance='no')n
        !write(102,'(i5)',advance='no')shelter_target
        !write(102,'(f8.2)',advance='no')set_prob_debug
        !write(102,'(f8.2)')EPT_temporary
    
        deallocate(route_i,route_j,potential,potential_temporary,tsunami_arrival_prob)

        return

    else
        write(*,*)"error: make_n_potential flag is uncorrect"
    endif

    
end subroutine make_n_potential
