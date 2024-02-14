      subroutine cp_dsr_dc2(mx,my,mz,ldno,ndt,dat)
c
c [global entities]
      use cp_module_indcmm,only :
     & ncms,indcms,ncmr,indcmr,mcmsbf,mcmrbf
      include 'mpif.h'
      include 'CONNEC.h'
      include 'GLOBAL.h'
      include 'FILE.h'
c
      integer,parameter :: lx=1,ly=1,lz=1
c
c [intent(in)]
      integer  mx,my,mz,ldno,ndt
c
c [intent(inout)]
      REAL*8   DAT(1-LX:MX-2+LX,1-LY:MY-2+LY,1-LZ:MZ-2+LZ,NDT)
ccc      real*8   dat(1-lx:mx+lx,1-ly:my+ly,1-lz:mz+lz,ndt)
c
c [local entities]
      integer  itag
      parameter( itag=0 )
      integer  status(mpi_status_size,nproc)
      integer  isbf(0:nproc),irbf(0:nproc)
      integer  ireqs(nproc),ireqr(nproc)
      real*8   sbuf(mcmsbf*ndt),rbuf(mcmrbf*ndt)
      integer  i,j,k,l,ld,ldn,ierr
      integer  np,nc,ns,ne,npb,nko,nqs,nqr
      integer  iv,jv,kv,kd,ks,ke
      integer  lv(3,6)
c
c
      if( nproc.lt.2 ) return
c
      ldn=abs(ldno)
      lv=0
      if( ldn.gt.0 ) then
        lv(ldn,:)=1
        lv(ldn,ldn+ldn-1)=0
        lv(ldn,ldn+ldn  )=0
        if( lx.lt.1 ) lv(1,:)=0
        if( ly.lt.1 ) lv(2,:)=0
        if( lz.lt.1 ) lv(3,:)=0
      endif
      ldn=-ldno
      KD=MIN(0,MZ-2)
c
      isbf=0
      irbf=0
c
      ne=0
      npb=0
      do 100 nc=1,ncms
        ns=ne
        ld=indcms(7,nc)
        if( (ld+1)/2.eq.ldn ) goto 100
        iv=min(lv(1,ld),max(0,lx-indcms( 8,nc)))
        jv=min(lv(2,ld),max(0,ly-indcms( 9,nc)))
        kv=min(lv(3,ld),max(0,lz-indcms(10,nc)))
        ks=max(   1-lz,indcms(5,nc)-kv+kd)
        ke=min(mz-2+lz,indcms(6,nc)+kd)
        do 110 l=1,ndt
        do 110 k=ks,ke
        do 110 j=indcms(3,nc)-jv,indcms(4,nc)
        do 110 i=indcms(1,nc)-iv,indcms(2,nc)
          ne=ne+1
          sbuf(ne)=dat(i,j,k,l)
  110   continue
        np=indcms(0,nc)
        if( np.lt.npb ) then
          CALL ERRMSG('CP_DSR_DC2',6230)
          write(LP,*) '### program error -1- (cp_dsr_dc2)'
          CALL ABORT1('')
        endif
        isbf(np)=isbf(np)+(ne-ns)
        npb=np
  100 continue
c
      do 120 nc=1,ncmr
        np=indcmr(0,nc)
        ld=indcmr(7,nc)
        if( (ld+1)/2.eq.ldn ) goto 120
        iv=min(lv(1,ld),max(0,lx-indcmr( 8,nc)))
        jv=min(lv(2,ld),max(0,ly-indcmr( 9,nc)))
        kv=min(lv(3,ld),max(0,lz-indcmr(10,nc)))
        ks=max(   1-lz,indcmr(5,nc)-kv+kd)
        ke=min(mz-2+lz,indcmr(6,nc)+kd)
        nko=(indcmr(2,nc)-indcmr(1,nc)+1+iv)
     &     *(indcmr(4,nc)-indcmr(3,nc)+1+jv)
     &     *(ke-ks+1)
     &     *ndt
        irbf(np)=irbf(np)+max(0,nko)
  120 continue
c
      ne=0
      nqs=0
      do 210 np=0,nproc-1
      if( isbf(np).gt.0 ) then
        nko=isbf(np)
        ns=ne+1
        ne=ne+nko
        nqs=nqs+1
        call mpi_isend(sbuf(ns),nko,mpi_double_precision,np,itag
     &      ,CHILDCOMM,ireqs(nqs),ierr)
      endif
  210 continue
c
      ne=0
      nqr=0
      do 220 np=0,nproc-1
      if( irbf(np).gt.0 ) then
        nko=irbf(np)
        ns=ne+1
        ne=ne+nko
        nqr=nqr+1
        call mpi_irecv(rbuf(ns),nko,mpi_double_precision,np,itag
     &      ,CHILDCOMM,ireqr(nqr),ierr)
      endif
  220 continue
c
      if( nqr.gt.0 ) call mpi_waitall(nqr,ireqr,status,ierr)
c
      ne=0
      npb=0
      do 300 nc=1,ncmr
        ld=indcmr(7,nc)
        if( (ld+1)/2.eq.ldn ) goto 300
        iv=min(lv(1,ld),max(0,lx-indcmr( 8,nc)))
        jv=min(lv(2,ld),max(0,ly-indcmr( 9,nc)))
        kv=min(lv(3,ld),max(0,lz-indcmr(10,nc)))
        ks=max(   1-lz,indcmr(5,nc)-kv+kd)
        ke=min(mz-2+lz,indcmr(6,nc)+kd)
        do 310 l=1,ndt
        do 310 k=ks,ke
        do 310 j=indcmr(3,nc)-jv,indcmr(4,nc)
        do 310 i=indcmr(1,nc)-iv,indcmr(2,nc)
          ne=ne+1
          dat(i,j,k,l)=rbuf(ne)
  310   continue
        np=indcmr(0,nc)
        if( np.lt.npb ) then
          CALL ERRMSG('CP_DSR_DC2',6231)
          write(LP,*) '### program error -2- (cp_dsr_dc2)'
          CALL ABORT1('')
        endif
        npb=np
  300 continue
c
      if( nqs.gt.0 ) call mpi_waitall(nqs,ireqs,status,ierr)
c
      end
