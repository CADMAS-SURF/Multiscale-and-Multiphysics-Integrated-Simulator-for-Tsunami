c====================================================================
c     2000年9月14日　記
c     東京大学大学院教授の磯部先生が作成された，
c     波速の第１定義による流れ関数法Bのプログラム(一部東大大学院院生有
c     川さんが修正)を榊山　勉(電力中央研究所)が波速の第２定義による流
c     れ関数法のプログラムに修正した．

c     修正内容
c      水面における流れ関数の値(プログラム中の変数pseta)が
c      質量輸送に相当する流量になるので，
c      平均水深(プログラム中の変数d)で割った値を，
c      入力値である一様流と同様に扱うようにプログラムを修正した．
c      これにより，オリジナルのプログラムを最小限の修正で済んだと思わ
c      れる．
c      修正個所には以下のコメントを挿入した．
c#### Sakakiyama for C def2,2000/09/10 #####
c      また，第１定義の計算時をコメントアウトしてあるので, 元に戻すこ
c      とも可能です．

c      オリジナルのプログラムでは係数の収束計算をニュートン法で行って
c      いるが，質量輸送の補正は代入法によったので収束が若干遅くったが，
c      現在の計算機ではほとんど問題ありません．

      subroutine vf_sfmb02C(n1,d1,t1,h1,u1,a1,rl1,rk1,hh1,
     &                      iconv1,e11,msg1)
      implicit real*8(a-h,o-z)
      real*8 e1,ekl(23),ek(23,23)
      real*4 a1(24),d1,t1,h1,u1,rl1,rk1,hh1,e11,xl,xl2,z
      common /vf_coef/ d,t,h,u,a(24),rk,sigma,b,pseta,nn,nn1,nn2
      common /vf_cnst/ pi,pi2,g
      common /vf_parm/ epsa,depsa(24),epsh,depsh,iteram,iterhm,irtry
      common /vf_euwt/ sit,cot,sh,ch,eta,epse,depse,
     &                 xl,xl2,z,iterem,iecon
      common /vf_iniv/ rkd0,rk0,rl0,b0,q0,deltq0,depsq
      common /vf_conv/ bq,hh,iconv,itera
      common /vf_derv/ e1,ekl,ek,detaa(23)
      common /vf_atmp/ a2(24)
      common /vf_msgl/ msg
      data ifirst/0/
c
      msg=msg1
      if(ifirst.ne.0) go to 11
      call vf_prep
      ifirst=1
C     ##### MOD BY AKIYAMA(FUJI-RIC):2004/1/05 #####
C     本ルーチンを2回コールすると結果が変わる、のを回避するため
      ifirst=0
      pseta=0.0d0
C     ##############################################
c
   11 xl=1.0e10
      xl2=2.0e10
      z=1.0e10
      nn=n1
      d=d1
      t=t1
      h=h1
      u=u1
      hh=0.0
c
      if(nn.lt.1 .or. nn.gt.22) nn=5
      if(d.le.0.0) go to 900
      if(t.le.0.0) go to 900
      if(h.le.0.0) go to 900
      c0=g*t/pi2
      rlo=c0*t
      if(h/rlo.gt.0.3) go to 900
      if(h/d.gt.1.5) go to 900
      if(u.lt.-c0/2.0) go to 900
      if(d.gt.2.0*rlo) d=2.0*rlo

c     初期値の設定
      call vf_init

      if(msg.lt.2) go to 91
      write(6,653) rkd0,rk0,rl0,b0,q0,deltq0,depsq
  653 format(/1h ,'kd0,k0,l0 ',10g12.4)
      write(6,654) depse,depsh,(depsa(k),k=1,nn2)
  654 format(/1h ,'deps,e,h,a',(t12,10g12.4))
   91 continue

      iconv=1
      bq=q0*1.01
      dq=deltq0
      rdiv=1.0
c     係数A(n)の初期値の設定
      call vf_inita
      do 51 k=1,nn2
      a2(k)=a(k)
   51 continue
c
      do 100 iterh=1,iterhm
c      write(*,*)iterh
c
      call vf_lstsq
cc
      if(msg.ge.1) write(6,655) iconv,iecon,bq,hh,e1,(a(k),k=1,nn2)
  655 format(1h ,'iconv,q,h',t12,2i6,3g12.4/(t12,10g12.4))
cc
      if( iterh .eq. 1 ) hhh0=hh-h
      hhh1=hh-h
c      write(*,*)'bq0',bq
      if(iconv .ne. 0) go to 21
      if(abs(hh-h) .lt. depsh) go to 200
c      if(hh .gt. h) go to 21
      if(hhh1 .gt. 0.d0 ) goto 21
c
   74 if(iterh .eq. 1) goto 71
      if(hhh0 .gt. 0.d0) goto 71
      if(hhh0 .lt. hhh1) goto71
      if( ifg0 .eq. 1) then
       dq=dq/2.0
       bq=bq-dq
       ifg1=0
      else
       dq=dq/2.0
       bq=bq+dq
       ifg1=1
      endif
      goto 72
c
   71 if( hh .lt. 0.d0) then
      if( ifg0 .eq. 1) then
       dq=dq/2.0
       bq=bq-dq
       ifg1=0
       if( iterh .eq. 1) then
        ifg0=0
       endif
      else
       bq=bq+dq
       ifg1=1
       if( iterh .eq. 1) then
        ifg0=1
       endif
      endif
       goto 72
      endif

c      dq=dq/rdiv
      bq=bq+dq
      ifg1=1
      if( iterh .eq. 1) then
       ifg0=1
      endif

   72 continue
c      write(*,*)'bq',bq,'dq',dq
c      write(*,*)'hh<h,',' hh',hh,'h',h,'hhh0',hhh0,'hhh1',hhh1
c      write(*,*)'b0',b0
c      pause
      do 52 k=1,nn2
      a2(k)=a(k)
   52 continue

      go to 22

   21 if( iconv .ne. 0 .and. hhh1 .lt. 0.d0) goto 74
      if( iterh .eq. 1) goto 81
      if( hhh0 .lt. 0.d0) goto 81
      if( hhh0 .gt. hhh1) goto81
      if( ifg0 .eq. 0) then
       dq=dq/2.0
       bq=bq+dq
       ifg1=1
      else
       dq=dq/2.0
       bq=bq-dq
       ifg1=0
      endif
      goto 82

   81 rdiv=2.0
      dq=dq/rdiv
      bq=bq-dq
      ifg1=0
      if(iterh .eq. 1) then
       ifg0=0
      endif

   82 continue
c      write(*,*)'hh>h,',' hh',hh,'h',h,'hhh0',hhh0,'hhh1',hhh1
c      write(*,*)'bq',bq,'dq',dq
c      write(*,*)'b0',b0
c      pause
      do 53 k=1,nn2
      a(k)=a2(k)
   53 continue
   22 continue
      if(iterh .eq. 1)goto 100
      hhh0=hhh1
      ifg0=ifg1
      if(dq .lt. depsq) go to 800
c
  100 continue
c
      iterh=iterhm+1
  800 write(6,600) hh,h,bq,dq,e1
  600 format(/1h ,'not conv. in <sfmb0>',3x,9g12.4)
      write(6,602) (a(k),k=1,nn2)
  602 format(/1h ,'a(k)',(t12,10g12.4))
      iconv1=iecon*10000+iterh*100+itera
      go to 41

c     与えられた精度内の収束した結果を引数に代入する．
  200 iconv1=0
   41 rl1=a(nn1)
      rk1=rk
      hh1=hh
      e11=e1
      if(iecon.eq.0) e11=e11*h/hh
      do 42 k=1,nn2
      a1(k)=a(k)
   42 continue
      return
c
  900 write(6,601) n1,d1,t1,h1
  601 format(/1h ,'input data error in <sfmb0>',i7,3g12.4)
      iconv1=-1
      e11=3.0e10
      return
      end
c////////////////////////////////////////////////////////////////////
      subroutine vf_prep
      implicit real*8(a-h,o-z)
      real*4 xl,xl2,z
      common /vf_cnst/ pi,pi2,g
      common /vf_sico/ si(201),co(201),imax,imax1
      common /vf_parm/ epsa,depsa(24),epsh,depsh,iteram,iterhm,irtry
      common /vf_euwt/ sit,cot,sh,ch,eta,epse,depse,
     &                 xl,xl2,z,iterem,iecon
      common /vf_msgl/ msg
c
      imax=50
      epsa=1.0e-3
      epsh=1.0e-3
      epse=1.0e-5
      iteram=50
      iterhm=200
      iterem=30
      irtry=5
c
      pi=3.14159265358979d0
      pi2=2.0*pi
c      g=9.8d0
C     ##### ADD BY AKIYAMA(FUJI-RIC):2000/9/24 #####
      g=9.8d0
C     ##############################################
cc
      if(msg.lt.1) go to 91
      write(6,654) epsa,epsh,epse,iteram,iterhm,iterem
  654 format(/1h ,'eps,iterm ',3g12.4,3i6)
      write(6,650) pi,pi2,g
  650 format(/1h ,'/prep/pi,g',4f12.7)
   91 continue
cc
c
      df=pi/float(imax)
      imax1=imax+1
      do 11 i=1,imax1
      f=df*float(i-1)
      si(i)=sin(f)
      co(i)=cos(f)
   11 continue
cc
      if(msg.lt.1) go to 92
      write(6,651) imax,imax1
  651 format(/1h ,6x,'imax',2i6)
      if(msg.lt.6) go to 92
      write(6,652) (si(i),i=1,imax1)
  652 format(/1h ,7x,'sin',(t12,10f12.7))
      write(6,653) (co(i),i=1,imax1)
  653 format(/1h ,7x,'cos',(t12,10f12.7))
   92 continue
cc
      return
      end
c////////////////////////////////////////////////////////////////////
      subroutine vf_init
      implicit real*8(a-h,o-z)
      real*4 xl,xl2,z
      common /vf_coef/ d,t,h,u,a(24),rk,sigma,b,pseta,nn,nn1,nn2
      common /vf_cnst/ pi,pi2,g
      common /vf_parm/ epsa,depsa(24),epsh,depsh,iteram,iterhm,irtry
      common /vf_iniv/ rkd0,rk0,rl0,b0,q0,deltq0,depsq
      common /vf_euwt/ sit,cot,sh,ch,eta,epse,depse,
     &                 xl,xl2,z,iterem,iecon
      common /vf_msgl/ msg

c     流れのある場での微小振幅波の分散関係式を解く
      sigma=pi2/t
      w2=sigma*sigma*d/g

c#### Sakakiyama for C def2,2000/09/10 #####
c      write(66,'(10(a,f10.5))') 'pseta=',pseta,' d=',d
c     #                         ,' psi_eta/h=',pseta/d

      usd=(u+pseta/d)/(sigma*d)
c      usd=u/(sigma*d)

      call vf_wcdspr(w2,usd,rkd0)
      rk0=rkd0/d
      rl0=pi2/rk0

c      write(6,'(a/(a,d14.5))')'initial value'
c     # ,' wave number rkd0=',rkd0,' wave lenghth rl0=',rl0

c#### Sakakiyama for C def2,2000/09/10 #####
      b0=rl0/t-(u+pseta/d)
c      b0=rl0/t-u

      q0=b0*b0/2.0
      deltq0=q0*0.01
      depsq=h*h*rk0/(8.0*sinh(2.0*rkd0))*epsh
c
      depse=epse*d
      nn1=nn+1
      nn2=nn+2
      depsa1=epsa*b0*h/2.0
      sh1=sinh(rkd0)
      ch1=cosh(rkd0)
      shn=sh1
      chn=ch1
      do 11 n=1,nn
      depsa(n)=depsa1/(float(n)*shn)
      c  =chn*ch1+shn*sh1
      shn=shn*ch1+chn*sh1
      chn=c
   11 continue
      depsa(nn1)=epsa*rl0
      depsa(nn2)=epsa*b0*h/2.0
      depsh=epsh*h
cc
      if(msg.lt.1) go to 91
      write(6,650) sigma,w2,usd,rkd0,rk0,rl0,b0,deltq0,q0,depsq
  650 format(/1h ,'sigma,w2  ',10g12.4)
      write(6,651) depse,depsh,(depsa(k),k=1,nn2)
  651 format(/1h ,'deps',(t12,10g12.4))
   91 continue
cc
      return
      end
c////////////////////////////////////////////////////////////////////
      subroutine vf_wcdspr(w2,usd,rkd)
      implicit real*8(a-h,o-z)
      data epsk/1.0e-6/, iterkm/10/
c
      uw2=2.0*usd*w2
      w=sqrt(w2)
      rkd=w/(1.0+w*usd)
      do 11 i=1,iterkm
      th=tanh(rkd)
      f=rkd*th-w2*(1.0-rkd*usd)**2
      df=th+rkd*(1.0-th*th)+uw2*(1.0-rkd*usd)
      dkd=f/df
      rkd=rkd-dkd
      if(abs(dkd/rkd).lt.epsk) return
   11 continue
      write(6,600) w2,usd,rkd,dkd,f,df
  600 format(/1h ,'/wcdspr/  ',10g12.4)
      return
      end
c////////////////////////////////////////////////////////////////////
      subroutine vf_lstsq
      implicit real*8(a-h,o-z)
      real*8 e1,ekl(23),ek(23,23),e11
      real*4 xl,xl2,z
      dimension da(23)
      common /vf_coef/ d,t,h,u,a(24),rk,sigma,b,pseta,nn,nn1,nn2
      common /vf_parm/ epsa,depsa(24),epsh,depsh,iteram,iterhm,irtry
      common /vf_euwt/ sit,cot,sh,ch,eta,epse,depse,
     &                 xl,xl2,z,iterem,iecon
      common /vf_conv/ bq,hh,iconv,itera
      common /vf_derv/ e1,ekl,ek,detaa(23)
      common /vf_msgl/ msg
cccccc      real*8 wk(23)
      dimension nans(23)
c
      itera=0
      hh=1.0e10
      iconv=1
      do 41 ifac=1,irtry
      call vf_psieta
c      write(*,*)'iecon in lstsq1',iecon
c      pause
      if(iecon.eq.0) go to 42
      call vf_fact
   41 continue
      iecon=2
      return
   42 continue
      sda1=1.0e10
      e11=1.0e10
c
      do 100 itera=1,iteram
cc
      if(msg.ge.3) write(6,652) (a(k),k=1,nn2)
  652 format(/1h ,'a(n)',6x,(t12,10g12.4))
cc
c
      iconv=1
      if(iecon.ne.0) go to 25
      call vf_dife
c
cc
      if(msg.lt.3) go to 91
      write(6,653) e1
  653 format(/1h ,'e1',t12,12g12.4)
      if(msg.lt.4) go to 91
      do 19 k=1,nn1
      write(6,650) k,(ek(k,ll),ll=1,nn1),ekl(k)
  650 format(1h ,'ek',i8,(t12,10g12.4))
   19 continue
   91 continue
cc
c
      if(e1.gt.e11) go to 25
      eps=a(nn1)*1.0d-14

c     倍精度で連立1次方程式を解く
cccccc      call \dlf1m(ek,nn1,23,ekl,eps,1,nans,wk,ier)
      call vf_ludcmp( ek,nn1,23,nans,dparm,eps )
      call vf_lubksb( ek,nn1,23,nans,ekl )
cccccc      if(ier.ne.0) return
cc
      if(msg.ge.3) write(6,651) (ekl(k),k=1,nn1)
  651 format(/1h ,'ekl',5x,(t12,10g12.4))
cc
      iconv=0
      sda=0.0
      do 21 k=1,nn1
      f=abs(ekl(k)/depsa(k))
      sda=sda+f
      if(f.ge.1.0) iconv=1
   21 continue
c
      facd=1.0
      if(sda.gt.sda1*0.5 .and. itera.gt.4) facd=0.5
      do 24 k=1,nn1
      da(k)=facd*ekl(k)
      a(k)=a(k)-da(k)
   24 continue
      e11=e1
      go to 30
c
   25 iconv=0
      do 23 k=1,nn1
      da(k)=da(k)/2.0
      a(k)=a(k)+da(k)
      if(abs(da(k)).ge.depsa(k)) iconv=1
   23 continue
c
   30 pseta0=a(nn2)
      call vf_psieta
c      write(*,*)'iecon in lstsq2',iecon
c      pause
      if(iecon.ne.0) go to 100
      if(abs(a(nn2)-pseta0).ge.depsa(nn2)) iconv=1
      if(iconv.eq.0) return
c
  100 continue
c
      iconv=1
      itera=iteram+1
      return
      end
c////////////////////////////////////
      subroutine vf_inita
      implicit real*8(a-h,o-z)
      real*8 e1,ekl(23),ek(23,23)
      common /vf_coef/ d,t,h,u,a(24),rk,sigma,b,pseta,nn,nn1,nn2
      common /vf_iniv/ rkd0,rk0,rl0,b0,q0,deltq0,depsq
      common /vf_derv/ e1,ekl,ek,detaa(23)
      common /vf_msgl/ msg
c
      hinit=min(0.142*rl0,0.82*d)*0.1
      a(1)=-hinit*b0/(2.0*sinh(rkd0))
      if(nn.le.1) go to 11
      do 12 n=2,nn
      a(n)=0.0
   12 continue
   11 a(nn1)=rl0
      a(nn2)=0.0
cc
      if(msg.ge.3) write(6,650) (a(k),k=1,nn2)
  650 format(/1h ,'init a(n) ',(t12,10g12.4))
cc
c
      do 13 k=1,nn1
      detaa(k)=0.0
   13 continue
      return
      end
c///////////////////////////////////////
      subroutine vf_fact
      implicit real*8(a-h,o-z)
      common /vf_coef/ d,t,h,u,a(24),rk,sigma,b,pseta,nn,nn1,nn2
      common /vf_iniv/ rkd0,rk0,rl0,b0,q0,deltq0,depsq
      common /vf_msgl/ msg
      data fac/0.8/
c
      do 11 n=1,nn
      a(n)=a(n)*fac
   11 continue
      a(nn1)=(a(nn1)-rl0)*fac+rl0
      a(nn2)=a(nn2)*fac
cc
      if(msg.ge.3) write(6,650) (a(k),k=1,nn2)
  650 format(/1h ,'/fact/a(k)',(t12,10g12.4))
cc
      return
      end
c////////////////////////////////
      subroutine vf_dife
      implicit real*8(a-h,o-z)
      real*8 e1,ekl(23),ek(23,23),sdetaa(23),sitn,cotn,shn,chn,c,qq,
     #       psz,psx,chc1,shs1,shc2,chs2,hchc1,hshc2,hchs2,se1
      real*4 xl,xl2,z
      dimension etaa(23),psza(23),psxa(23),shcn(22),qqk(23)
      common /vf_coef/ d,t,h,u,a(24),rk,sigma,b,pseta,nn,nn1,nn2
      common /vf_cnst/ pi,pi2,g
      common /vf_sico/ si(201),co(201),imax,imax1
      common /vf_parm/ epsa,depsa(24),epsh,depsh,iteram,iterhm,irtry
      common /vf_euwt/ sit,cot,sh,ch,eta,epse,depse,
     &                 xl,xl2,z,iterem,iecon
      common /vf_conv/ bq,hh,iconv,itera
      common /vf_derv/ e1,ekl,ek,detaa(23)
      common /vf_msgl/ msg

c     rl=a(nn1)   :波長
c     rk=pi2/rl   :波数
c     b=rl/t-u    :波速−定常流速, C-U
c     pseta=a(nn2): psi_eta: 自由表面で流れ関数の値

      rl=a(nn1)
      rk=pi2/rl

c#### Sakakiyama for C def2,2000/09/10 #####
      b=rl/t-(u+pseta/d)
c      b=rl/t-u
      pseta=a(nn2)
c
      do 11 k=1,nn1
      sdetaa(k)=0.0
      ekl(k)=0.0
      do 11 l=1,nn1
      ek(k,l)=0.0
   11 continue
      se1=0.0
      e1=2.0e10
c
      do 100 i=1,imax1
c
      sit=si(i)
      cot=co(i)
      call vf_caleta
      if(iecon.ne.0) return
      sitn=sit
      cotn=cot
      deta=d+eta
      rkdeta=rk*deta
      sh1=sinh(rkdeta)
      ch1=cosh(rkdeta)
      shn=sh1
      chn=ch1
c
       chc1=0.0
       shs1=0.0
       shc2=0.0
       chs2=0.0
      hchc1=0.0
      hshc2=0.0
      hchs2=0.0
c
      do 21 n=1,nn
c
      rkn=float(n)*rk
      psza(n)= rkn*chn*cotn
      psxa(n)=-rkn*shn*sitn
      shcn(n)= shn*cotn
c
      rkna=rkn*a(n)
      ach=rkna*chn
      ash=rkna*shn
      achc=ach*cotn
      ashs=ash*sitn
      ashc=rkn*ash*cotn
      achs=rkn*ach*sitn
c
       chc1= chc1+achc
       shs1= shs1+ashs
       shc2= shc2+ashc
       chs2= chs2+achs
      hchc1=hchc1+achc*deta
      hshc2=hshc2+ashc*deta
      hchs2=hchs2+achs*deta
c
      c   =cotn*cot-sitn*sit
      sitn=sitn*cot+cotn*sit
      cotn=c
      c  =chn*ch1+shn*sh1
      shn=shn*ch1+chn*sh1
      chn=c
   21 continue
c
      psz = chc1+b
      psx =-shs1
      psze= shc2
      psxe=-chs2
      psza(nn1)=1.0/t-(chc1+hshc2)/rl
      psxa(nn1)=      (shs1+hchs2)/rl
      shcn(nn1)=eta/t-hchc1/rl
      qq=(psz*psz+psx*psx)/2.0+g*eta-bq
      qe=psz*psze+psx*psxe+g
      fac=1.0
      if(i.eq.1 .or. i.eq.imax1) fac=0.5
c
      do 22 k=1,nn1
      etaa(k)=(detaa(k)-shcn(k))/psz
      qqkk=psz*psza(k)+psx*psxa(k)+qe*etaa(k)
      qqk(k)=qqkk
      ekl(k)=ekl(k)+fac*2.0*qq*qqkk
      do 22 l=1,k
      ek(k,l)=ek(k,l)+fac*2.0*qqkk*qqk(l)
   22 continue
      se1=se1+fac*qq*qq
cc
      if(msg.lt.6) go to 91
      write(6,651)i,eta,qq,qe,psz,psx,psze,psxe,shn,chn,sitn,cotn
  651 format(/1h ,'eta',i7,9g12.4,2f6.3)
      write(6,653) (psza(k),k=1,nn1)
  653 format(1h ,'psza',(t12,10g12.4))
      write(6,654) (psxa(k),k=1,nn1)
  654 format(1h ,'psxa',(t12,10g12.4))
      write(6,655) (shcn(k),k=1,nn1)
  655 format(1h ,'shcn',(t12,10g12.4))
      write(6,656) (etaa(k),k=1,nn1)
  656 format(1h ,'etaa',(t12,10g12.4))
      write(6,657) (qqk(k),k=1,nn1)
  657 format(1h ,'qqk',(t12,10g12.4))
   91 continue
cc
c
      do 23 n=1,nn
      sdetaa(n)=sdetaa(n)+fac*(chc1*etaa(n)+shcn(n))
   23 continue
      sdetaa(nn1)=sdetaa(nn1)+fac*(chc1*etaa(nn1)-hchc1/rl)
c
  100 continue
c
      do 31 k=1,nn1
      detaa(k)=sdetaa(k)/float(imax)
      ekl(k)=ekl(k)/float(imax)
      do 31 l=1,k
      ek(k,l)=ek(k,l)/float(imax)
      ek(l,k)=ek(k,l)
   31 continue
      e1=sqrt(se1/float(imax))/(g*h)
cc
      if(msg.ge.6) write(6,652) (detaa(k),k=1,nn1)
  652 format(/1h ,'detaa',(t12,10g12.4))
cc
      return
      end
c/////////////////////////////////////////////
      subroutine vf_psieta
      implicit real*8(a-h,o-z)
      real*8 shc,s,sitn,cotn,shn,chn,c
      real*4 xl,xl2,z
      common /vf_coef/ d,t,h,u,a(24),rk,sigma,b,pseta,nn,nn1,nn2
      common /vf_cnst/ pi,pi2,g
      common /vf_sico/ si(201),co(201),imax,imax1
      common /vf_euwt/ sit,cot,sh,ch,eta,epse,depse,
     &                 xl,xl2,z,iterem,iecon
      common /vf_conv/ bq,hh,iconv,itera
      common /vf_msgl/ msg
      common /vf_etai/ e(201)
      common /vf_test/ dd
c
      rl=a(nn1)
      rk=pi2/rl
c#### Sakakiyama for C def2,2000/09/10 #####
      b=rl/t-(u+pseta/d)
c      b=rl/t-u

      pseta=a(nn2)
      eta=h/2.0
      s=0.0
c
      do 11 i=1,imax1
      sit=si(i)
      cot=co(i)
      call vf_caleta
      if(iecon.ne.0) return
      e(i)=eta
      sitn=sit
      cotn=cot
      rkdeta=rk*(d+eta)
      sh1=sinh(rkdeta)
      ch1=cosh(rkdeta)
      shn=sh1
      chn=ch1
c
      shc=0.0
      do 12 n=1,nn
      shc=shc+a(n)*shn*cotn
      c   =cotn*cot-sitn*sit
      sitn=sitn*cot+cotn*sit
      cotn=c
      c  =chn*ch1+shn*sh1
      shn=shn*ch1+chn*sh1
      chn=c
   12 continue
c
      if(i.ne.1) go to 13
       etac=eta
c      write(*,*)'etac',eta
c      write(*,*)'dd',dd
      go to 14
   13 if(i.ne.imax1) go to 15
       etat=eta
c      write(*,*)'etat',eta
c      write(*,*)'dd',dd
   14 shc=shc/2.0
c
   15 s=s+shc
   11 continue
c
      hh=etac-etat
      pseta=s/float(imax)
cc
      if(msg.lt.4) go to 91
      if(msg.lt.5) go to 92
      write(6,651) (e(i),i=1,imax1)
  651 format(/1h ,'eta',(t12,10g12.4))
   92 write(6,650) pseta,a(nn2),hh
  650 format(/1h ,'psieta',t12,10g12.4)
   91 continue
cc
      a(nn2)=pseta
      return
      end

c///////////////////////////////////////////
      subroutine vf_caleta
      implicit real*8(a-h,o-z)
      real*8 shc,chc,sitn,cotn,shn,chn,c
      real*4 xl,xl2,z
      common /vf_coef/ d,t,h,u,a(24),rk,sigma,b,pseta,nn,nn1,nn2
      common /vf_euwt/ sit,cot,sh,ch,eta,epse,depse,
     &                 xl,xl2,z,iterem,iecon
      common /vf_msgl/ msg
      common /vf_test/ dd
c
      rkd=rk*d
      iecon=0
      do 11 j=1,iterem
c
      sitn=sit
      cotn=cot
      rketa=rk*eta
      if(abs(rketa).gt.2.0) then
c      write(*,*)'tirari- in <caleta>'
       goto 20
      endif
      rkdeta=rkd+rketa
      sh1=sinh(rkdeta)
      ch1=cosh(rkdeta)
      shn=sh1
      chn=ch1
c
      shc=b*eta-pseta
      chc=b
c
      do 12 n=1,nn
      ac=a(n)*cotn
      shc=shc+ac*shn
      chc=chc+float(n)*rk*ac*chn
      c   =cotn*cot-sitn*sit
      sitn=sitn*cot+cotn*sit
      cotn=c
      c  =chn*ch1+shn*sh1
      shn=shn*ch1+chn*sh1
      chn=c
   12 continue
c
      dd=shc/chc
      eta=eta-dd
      if(abs(dd).lt.depse) return
   11 continue
c
   20 if(msg.ge.1) write(6,600) eta,dd,shc,chc,sit,cot
  600 format(/1h ,'not conv. in <caleta>  ',9g12.4)
      iecon=1
      eta=0.0
c     write(*,*)'not conv. in <caleta>'
c     write(*,*)'dd',dd

      return
      end

c//// 水面波形を計算する /////////////////////////////////
      subroutine vf_sfmb1(xl1,zs1)
      implicit real*8(a-h,o-z)
      real*4 xl,xl1,xl2,zs1,z
      common /vf_cnst/ pi,pi2,g
      common /vf_euwt/ sit,cot,sh,ch,eta,epse,depse,
     &                 xl,xl2,z,iterem,iecon
c
      ax=abs(xl1-xl)
c      if(xl1.eq.xl) go to 20
      if(ax .lt. 1.0e-7) goto 20
      xl=xl1
      rkx=pi2*xl
      sit=sin(rkx)
      cot=cos(rkx)
c
      ax1=abs(xl2-xl)
c   20 if(xl2.eq.xl) go to 21
   20 if(ax1 .le. 1.0e-7) goto 21

      eta=0.0
      call vf_caleta
      xl2=xl
   21 zs1=eta

      return
      end

c//// 流速，加速度，圧力を計算する /////////////////////////
      subroutine vf_sfmb2(xl1,z1,u1,w1,ut1,wt1,p1)
      implicit real*8(a-h,o-z)
      real*4 xl1,z1,u1,w1,ut1,wt1,p1,xl,xl2,z
      real*8 chc,shs,chs,shc,sitn,cotn,shn,chn,c
      common /vf_coef/ d,t,h,u,a(24),rk,sigma,b,pseta,nn,nn1,nn2
      common /vf_cnst/ pi,pi2,g
      common /vf_euwt/ sit,cot,sh,ch,eta,epse,depse,
     &                 xl,xl2,z,iterem,iecon
      common /vf_conv/ bq,hh,iconv,itera
c
      ax2=abs(xl1-xl)
c      if(xl1.ne.xl) go to 10
      if(ax2 .ge. 1.0e-7) goto 10
      ax3=abs(z1-z)
c      if(z1.ne.z) go to 21
      if(ax3 .ge. 1.0e-7) goto 21
      go to 40
c
   10 xl=xl1
      rkx=pi2*xl
      sit=sin(rkx)
      cot=cos(rkx)
c
      az=abs(z1-z)
c      if(z1.eq.z) go to 30
      if(az .lt. 1.0e-7) goto 30
   21 z=z1
      rkdz=rk*(d+z)
      if(rkdz.lt.0.0) rkdz=0.0
      sh=sinh(rkdz)
      ch=cosh(rkdz)
c
   30 chc=0.0
      shs=0.0
      chs=0.0
      shc=0.0
      sitn=sit
      cotn=cot
      shn=sh
      chn=ch
c
      do 31 n=1,nn
      an=float(n)*rk*a(n)
      ach=an*chn
      ash=an*shn
      s=float(n)*sigma
c
      chc=chc+ach*cotn
      shs=shs+ash*sitn
      chs=chs+ach*sitn*s
      shc=shc+ash*cotn*s
c
      c   =cotn*cot-sitn*sit
      sitn=sitn*cot+cotn*sit
      cotn=c
      c  =chn*ch+shn*sh
      shn=shn*ch+chn*sh
      chn=c
c
   31 continue
c
c#### Sakakiyama for C def2,2000/09/10 #####
      uu1=-chc+u+pseta/d
c      uu1=-chc+u

      ww1=-shs
      cc=sigma/rk
      uut1=-chs*(1.0-uu1/cc)-shc*ww1/cc
      wwt1= shc*(1.0-uu1/cc)-chs*ww1/cc
      pp1=(bq-((uu1-cc)**2+ww1**2)/2.0)/g-z
c
   40 u1=uu1
      w1=ww1
      ut1=uut1
      wt1=wwt1
      p1=pp1
      return
      end

c//// 水面変動速度を計算する /////////////////////////////
      subroutine vf_sfmb3(xl1,zst1)
      implicit real*8(a-h,o-z)
      real*4 xl1,zst1,xl,xl2,z,xl3,zs3,uu,ww,uut,wwt
      real*4 pps
      common /vf_cnst/ pi,pi2,g
      common /vf_coef/ d,t,h,u,a(24),rk,sigma,b,pseta,nn,nn1,nn2
      common /vf_euwt/ sit,cot,sh,ch,eta,epse,depse,
     &                 xl,xl2,z,iterem,iecon
c
      ax4=abs(xl1-xl)
c      if(xl1.eq.xl) go to 20
      if(ax4 .lt. 1.0e-7) goto 20
      xl=xl1
      rkx=pi2*xl
      sit=sin(rkx)
      cot=cos(rkx)
c
      ax5=abs(xl2-xl)
c   20 if(xl2.eq.xl) go to 30
   20 if(ax5 .lt. 1.0e-7) goto 30
      eta=0.0
      call vf_caleta
      xl2=xl
c
   30 zs3=eta
      xl3=xl2
      call vf_sfmb2(xl3,zs3,uu,ww,uut,wwt,pps)
      zst1=ww/(1.0-uu*rk/sigma)
      return
      end

c====================================================================
      subroutine vf_ludcmp(a,n,np,indx,d,tiny)
      implicit real*8(a-h,o-z)
      integer n,np,indx(n),nmax
      real*8 d,a(np,np),tiny
      parameter (nmax=500)
c      parameter (nmax=500,tiny=1.0d-20)
      integer i,imax,j,k
      real*8 aamax,dum,sum,vv(nmax)
      d=1.
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
11      continue
        if (aamax.eq.0.) pause 'singular matrix in ludcmp'
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        do 14 i=1,j-1
          sum=a(i,j)
          do 13 k=1,i-1
            sum=sum-a(i,k)*a(k,j)
13        continue
          a(i,j)=sum
14      continue
        aamax=0.
        do 16 i=j,n
          sum=a(i,j)
          do 15 k=1,j-1
            sum=sum-a(i,k)*a(k,j)
15        continue
          a(i,j)=sum
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.)a(j,j)=tiny
        if(j.ne.n)then
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      return
      end
c  (c) copr. 1986-92 numerical recipes software 5n2.
c====================================================================
      subroutine vf_lubksb(a,n,np,indx,b)
      implicit real*8(a-h,o-z)
      integer n,np,indx(n)
      real*8 a(np,np),b(n)
      integer i,ii,j,ll
      real*8 sum
      ii=0
      do 12 i=1,n
        ll=indx(i)
        sum=b(ll)
        b(ll)=b(i)
        if (ii.ne.0)then
          do 11 j=ii,i-1
            sum=sum-a(i,j)*b(j)
11        continue
        else if (sum.ne.0.) then
          ii=i
        endif
        b(i)=sum
12    continue
      do 14 i=n,1,-1
        sum=b(i)
        do 13 j=i+1,n
          sum=sum-a(i,j)*b(j)
13      continue
        b(i)=sum/a(i,i)
14    continue
      return
      end
c  (c) copr. 1986-92 numerical recipes software 5n2.
