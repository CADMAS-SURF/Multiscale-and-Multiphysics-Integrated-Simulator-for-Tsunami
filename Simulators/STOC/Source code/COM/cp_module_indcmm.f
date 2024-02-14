      module cp_module_indcmm
      character(19),parameter,private :: modnam='(cp_module_indcmm)'
c
c 1. module for communication boundary area
c
      integer :: mcms=1,ncms=0
      integer :: mcmr=1,ncmr=0
      integer,save,allocatable :: indcms(:,:)
      integer,save,allocatable :: indcmr(:,:)
c
      integer :: mcmsbf=1,mcmrbf=1
c
c ncms          : no. of communication boundary areas ( send )
c ncmr          : no. of communication boundary areas ( receive )
c indcm*(1-6,:) : index range of the area (is,ie,js,je,ks,ke)
c indcm*(0,:)   : PE no. to be communicated with
c indcm*(7,:)   : dummy cell no.
c mcmsbf        : size of send buffer
c mcmrbf        : size of receive buffer
c
c///////////////////////////////////////////////////////////////////////
      contains
c
      subroutine mkindex
     &     (myrank,nproc,iprd,jprd,kprd,lx,ly,lz
     &     ,inddom,ierror)
      character(9),parameter :: subnam='(mkindex)'
c
c [global entities]
      include 'FILE.h'
c
c [dummy arguments]
      integer,intent(in)  :: myrank,nproc
      integer,intent(in)  :: iprd,jprd,kprd
      integer,intent(in)  :: lx,ly,lz
      integer,intent(in)  :: inddom(6,0:nproc-1)
      integer,intent(out) :: ierror
c
c [local entities]
      integer :: ndn,ndt,lop
      logical :: set=.false.
c
      if( set ) then
        CALL ERRMSG('CP_MODULE_INDCMM',6270)
        write(LP,*) '### program error -0- ',modnam//subnam
        CALL ABORT1('')
      endif
      set=.true.
c
      ierror=0
c
      mcmr=18
      mcms=mcmr
      ndn=max(lx,ly,lz)
      ndt=1
      do 100 lop=1,2
        allocate( indcmr(0:10,mcmr),indcms(0:10,mcms),stat=ierror )
        if( ierror.ne.0 ) then
          write(lp,*) '### error : allocation failed -1-'
          goto 9999
        endif
        call cp_list_indcmm
     &       (myrank,nproc
     &       ,iprd,jprd,kprd
     &       ,inddom
     &       ,mcmr,mcms,ndn,ndt
     &       ,ncmr,ncms,indcmr,indcms
     &       ,mcmrbf,mcmsbf)
        if( ncmr.le.mcmr.and.ncms.le.mcms ) goto 101
        mcmr=ncmr
        mcms=ncms
        deallocate( indcmr,indcms )
  100 continue
      CALL ERRMSG('CP_MODULE_INDCMM',6271)
      write(LP,*) '### program error -1- ',modnam//subnam
      CALL ABORT1('')
  101 continue
c
      return
 9999 continue
      write(lp,*) modnam//subnam
      ierror=1
      end subroutine mkindex
c
      end module cp_module_indcmm
