      subroutine cp_list_indcmm
     &     (myrank,nproc
     &     ,iprd,jprd,kprd
     &     ,inddom
     &     ,mcmr,mcms,ndn,ndt
     &     ,ncmr,ncms,indcmr,indcms
     &     ,mcmrbf,mcmsbf)
c
c 1. Make up index for communication boundary area
c
c [intent(in)]
      integer  myrank,nproc
      integer  iprd,jprd,kprd
      integer  inddom(6,0:nproc-1)
      integer  mcmr,mcms,ndn,ndt
c
c [intent(out)]
      integer  ncmr,ncms
      integer  indcmr(0:10,mcmr)
      integer  indcms(0:10,mcms)
      integer  mcmrbf,mcmsbf
c
c [local entities]
      integer  ldn(3,6),ldt(3,6)
      integer  i,j,l,n1,n2,ndx
      integer  is,ie,js,je,ks,ke
      integer  is1,ie1,js1,je1,ks1,ke1
      integer  ipr,jpr,kpr,ipdd,jpdd,kpdd
      integer  ix0,jy0,kz0
      integer  ncmrx,ncmsx
      integer  isd,jsd,ksd
c
c
c
c-< 1. Preliminary set >-
c
      do 100 j=1,6
      do 100 i=1,3
        ldn(i,j)=0
        ldt(i,j)=ndt
  100 continue
      ldn(1,1)=-1
      ldn(1,2)= 1
      ldn(2,3)=-1
      ldn(2,4)= 1
      ldn(3,5)=-1
      ldn(3,6)= 1
      ldt(1,1)= 0
      ldt(1,2)= 0
      ldt(2,3)= 0
      ldt(2,4)= 0
      ldt(3,5)= 0
      ldt(3,6)= 0
c
      ipdd=max(1,iprd)
      jpdd=max(1,jprd)
      kpdd=max(1,kprd)
c
      ndx=max(ndn,ndt)
c
c
c-< 2. Make up index for communication boundary area >-
c
      ncmr=0
      ncms=0
      do 200 n1=0,nproc-1
      do 201 n2=0,nproc-1
      if( n2.eq.n1 ) goto 201
      if( n1.ne.myrank .and. n2.ne.myrank ) goto 201
        do 211 kpr=-kprd,kprd,kpdd
        do 212 jpr=-jprd,jprd,jpdd
        do 213 ipr=-iprd,iprd,ipdd
        if( max(inddom(1,n1)-ndx,inddom(1,n2)+ipr).gt.
     &      min(inddom(2,n1)+ndx,inddom(2,n2)+ipr) )  goto 213
        if( max(inddom(3,n1)-ndx,inddom(3,n2)+jpr).gt.
     &      min(inddom(4,n1)+ndx,inddom(4,n2)+jpr) )  goto 213
        if( max(inddom(5,n1)-ndx,inddom(5,n2)+kpr).gt.
     &      min(inddom(6,n1)+ndx,inddom(6,n2)+kpr) )  goto 213
          do 221 l=1,6
          is1=inddom(1,n1)
          ie1=inddom(2,n1)
          js1=inddom(3,n1)
          je1=inddom(4,n1)
          ks1=inddom(5,n1)
          ke1=inddom(6,n1)
          if( l.eq.1 ) ie1=is1
          if( l.eq.2 ) is1=ie1
          if( l.eq.3 ) je1=js1
          if( l.eq.4 ) js1=je1
          if( l.eq.5 ) ke1=ks1
          if( l.eq.6 ) ks1=ke1
          is1=is1-ldt(1,l)+ldn(1,l)
          ie1=ie1+ldt(1,l)+ldn(1,l)*ndn
          js1=js1-ldt(2,l)+ldn(2,l)
          je1=je1+ldt(2,l)+ldn(2,l)*ndn
          ks1=ks1-ldt(3,l)+ldn(3,l)
          ke1=ke1+ldt(3,l)+ldn(3,l)*ndn
          is=max(min(is1,ie1),inddom(1,n2)+ipr)
          ie=min(max(is1,ie1),inddom(2,n2)+ipr)
          js=max(min(js1,je1),inddom(3,n2)+jpr)
          je=min(max(js1,je1),inddom(4,n2)+jpr)
          ks=max(min(ks1,ke1),inddom(5,n2)+kpr)
          ke=min(max(ks1,ke1),inddom(6,n2)+kpr)
          if( is.gt.ie ) goto 221
          if( js.gt.je ) goto 221
          if( ks.gt.ke ) goto 221
          isd=abs(is-inddom(1,n1))
          jsd=abs(js-inddom(3,n1))
          ksd=abs(ks-inddom(5,n1))
          if( n1.eq.myrank ) then
            ncmr=ncmr+1
            ncmrx=min(ncmr,mcmr)
            indcmr(0,ncmrx)=n2
            indcmr(1,ncmrx)=is
            indcmr(2,ncmrx)=ie
            indcmr(3,ncmrx)=js
            indcmr(4,ncmrx)=je
            indcmr(5,ncmrx)=ks
            indcmr(6,ncmrx)=ke
            indcmr(7,ncmrx)=l
            indcmr( 8,ncmrx)=isd
            indcmr( 9,ncmrx)=jsd
            indcmr(10,ncmrx)=ksd
          else
            ncms=ncms+1
            ncmsx=min(ncms,mcms)
            indcms(0,ncmsx)=n1
            indcms(1,ncmsx)=is-ipr
            indcms(2,ncmsx)=ie-ipr
            indcms(3,ncmsx)=js-jpr
            indcms(4,ncmsx)=je-jpr
            indcms(5,ncmsx)=ks-kpr
            indcms(6,ncmsx)=ke-kpr
            indcms(7,ncmsx)=l
            indcms( 8,ncmsx)=isd
            indcms( 9,ncmsx)=jsd
            indcms(10,ncmsx)=ksd
          endif
  221     continue
  213   continue
  212   continue
  211   continue
  201 continue
  200 continue
c
      if( ncmr.gt.mcmr ) goto 8888
      if( ncms.gt.mcms ) goto 8888
c
c
c-< 3. Convert to local index & cout up buffer size >-
c
      ix0=1-inddom(1,myrank)
      jy0=1-inddom(3,myrank)
      kz0=1-inddom(5,myrank)
c
      mcmsbf=0
      do 300 i=1,ncms
        mcmsbf=mcmsbf+(indcms(6,i)-indcms(5,i)+2)
     &               *(indcms(4,i)-indcms(3,i)+2)
     &               *(indcms(2,i)-indcms(1,i)+2)
        indcms(1,i)=indcms(1,i)+ix0
        indcms(2,i)=indcms(2,i)+ix0
        indcms(3,i)=indcms(3,i)+jy0
        indcms(4,i)=indcms(4,i)+jy0
        indcms(5,i)=indcms(5,i)+kz0
        indcms(6,i)=indcms(6,i)+kz0
  300 continue
      mcmsbf=max(1,mcmsbf)
c
      mcmrbf=0
      do 310 i=1,ncmr
        mcmrbf=mcmrbf+(indcmr(6,i)-indcmr(5,i)+2)
     &               *(indcmr(4,i)-indcmr(3,i)+2)
     &               *(indcmr(2,i)-indcmr(1,i)+2)
        indcmr(1,i)=indcmr(1,i)+ix0
        indcmr(2,i)=indcmr(2,i)+ix0
        indcmr(3,i)=indcmr(3,i)+jy0
        indcmr(4,i)=indcmr(4,i)+jy0
        indcmr(5,i)=indcmr(5,i)+kz0
        indcmr(6,i)=indcmr(6,i)+kz0
  310 continue
      mcmrbf=max(1,mcmrbf)
c
 8888 continue
      end
