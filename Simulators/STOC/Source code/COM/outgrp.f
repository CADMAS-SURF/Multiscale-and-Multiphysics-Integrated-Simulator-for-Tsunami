      SUBROUTINE OUTGRP(UU,VV,WW,PP,TT,CC,AK,EP,TMU,FF,RHOW,
     $                  CSEDI,WRK1,WRK2,WRK3,IWRK1,
     $                  IWRK2,IWRK3,IWRK4,INDP)
C======================================================================
C     グラフィック出力を行う
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'GRID.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'OUTPUT.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'MODELI.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),TT(MX,MY,MZ),CC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::AK(MX,MY,MZ),EP(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(IN)::CSEDI(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK1(MX,MY,MZ),WRK2(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK3(MX,MY,MZ)
C
      INTEGER,INTENT(INOUT)::IWRK1(MX,MY,MZ),IWRK2(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::IWRK3(MX,MY,MZ),IWRK4(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
C
      REAL(8),ALLOCATABLE::BND1(:)
      REAL(8)::BND2(2)
      CHARACTER(16)::CPHYS1,CPHYS2
      INTEGER::IFLAG=0
C
      INTEGER::I,J,JFLAG,K,N,L,IERR
      INTEGER,SAVE::NM1,NM2
      REAL(8),ALLOCATABLE::RL(:,:,:)
C
C
C ... グラフィックファイルのヘッダ(格子データ、形状データ等)の出力
      IF( IFLAG.EQ.0 ) THEN
         IFLAG = 1
         CALL PLCOD(XGRID,YGRID,ZGRID,MXM,MYM,MZM,IFLGR)
         CALL PLIND(INDP,IWRK1,IWRK2,IWRK3,IWRK4,MXM,MYM,MZM,
     $              NOBSS,IOBSS,NM1,NM2,IFLGR)
         ALLOCATE(BND1(NM1),RL(MX,MY,MZ),STAT=IERR)
         IF( IERR.NE.0 ) THEN
            CALL ERRMSG('OUTGRP',7140)
            WRITE(LP,*) 'CANNOT ALLOCATE AREA OF BND1(NM1),',
     $                 ' NM1=',NM1
            CALL ABORT1('')
         END IF
         BND2(1) = 0.0D0
         BND2(2) = 0.0D0
      ELSE
         call mkindx(IWRK1,IWRK2,IWRK3,IWRK4,indp,mxm,mym,mzm,nm1,nm2,
     $               iobss)
         ALLOCATE(BND1(NM1),RL(MX,MY,MZ),STAT=IERR)
         bnd2(1) = 0.0d0
         bnd2(2) = 0.0d0
      END IF
C
C
C ... 出力指定時刻(ステップ)毎の出力
C
      JFLAG = 0
C
      DO 100 N=1,MGRPH
C
         IF( CGRPH(N).EQ.'U       ' .OR.
     $       CGRPH(N).EQ.'V       ' .OR.
     $       CGRPH(N).EQ.'W       ' ) THEN
            IF( JFLAG.EQ.0 ) THEN
               CPHYS1 = 'V               '
               CPHYS2 = 'VELOCITY        '
               CALL PLVEC(TIME,NXYZ,CPHYS1,CPHYS2,IFLGR)
C
               DO 110 K=2,MZM
               DO 110 J=2,MYM
               DO 110 I=2,MXM
                  WRK1(I,J,K) = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
                  WRK2(I,J,K) = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
                  WRK3(I,J,K) = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
  110          CONTINUE
C
               WRITE(IFLGR) ((( WRK1(I,J,K),I=2,MXM ),J=2,MYM ),K=2,MZM)
               WRITE(IFLGR) ((( WRK2(I,J,K),I=2,MXM ),J=2,MYM ),K=2,MZM)
               WRITE(IFLGR) ((( WRK3(I,J,K),I=2,MXM ),J=2,MYM ),K=2,MZM)
C
               CPHYS1 = 'V(SCALING)      '
               CPHYS2 = 'VELOCITY        '
               CALL PLVEC(TIME,NXYZ,CPHYS1,CPHYS2,IFLGR)
               WRITE(IFLGR) ((( WRK1(I,J,K),I=2,MXM ),J=2,MYM ),K=2,MZM)
               WRITE(IFLGR) ((( WRK2(I,J,K),I=2,MXM ),J=2,MYM ),K=2,MZM)
               WRITE(IFLGR) ((( WRK3(I,J,K)*5.0D0,I=2,MXM ),J=2,MYM ),
     $                          K=2,MZM)
               JFLAG = 1
            END IF
C
         ELSE IF( CGRPH(N).EQ.'P       ' ) THEN
            CPHYS1 = 'P               '
            CPHYS2 = 'PRESSURE        '
            CALL PLSCA(PP,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE IF( CGRPH(N).EQ.'T       ' ) THEN
            CPHYS1 = 'T               '
            CPHYS2 = 'TEMPERATURE     '
            CALL PLSCA(TT,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE IF( CGRPH(N).EQ.'C       ' ) THEN
            CPHYS1 = 'C               '
            CPHYS2 = 'CONCENTRATION   '
            CALL PLSCA(CC,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE IF( CGRPH(N).EQ.'K       ' ) THEN
            CPHYS1 = 'K               '
            CPHYS2 = 'TURBULENT-ENERGY'
            CALL PLSCA(AK,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE IF( CGRPH(N).EQ.'Q2      ' ) THEN
            CPHYS1 = 'Q2               '
            CPHYS2 = 'TURBULENT-ENERGY'
            CALL PLSCA(AK,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE IF( CGRPH(N).EQ.'E       ' ) THEN
            CPHYS1 = 'E               '
            CPHYS2 = 'T.E.DIMINISHING '
            CALL PLSCA(EP,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE IF( CGRPH(N).EQ.'L       '.AND.LTURB.EQ.3 ) THEN
            DO 120 K=1,MZ
            DO 120 J=1,MY
            DO 120 I=1,MX
              IF(AK(I,J,K).NE.0.0D0) THEN
                RL(I,J,K) = 0.5D0*EP(I,J,K)/AK(I,J,K)
              ELSE
                RL(I,J,K) = 0.0D0
              END IF
  120       CONTINUE          
            CPHYS1 = 'L               '
            CPHYS2 = 'TURBULENT LENGTH'
            CALL PLSCA(RL,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE IF( CGRPH(N).EQ.'TMU     ' ) THEN
            CPHYS1 = 'TMU             '
            CPHYS2 = 'TURB.VISCOSITY  '
            CALL PLSCA(TMU,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE IF( CGRPH(N).EQ.'F       ' ) THEN
            CPHYS1 = 'F               '
            CPHYS2 = 'FLUID-RATE      '
            CALL PLSCA(FF,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE IF( CGRPH(N).EQ.'RHO     ' ) THEN
            CPHYS1 = 'RHO             '
            CPHYS2 = 'DENSITY         '
            CALL PLSCA(RHOW,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE IF( CGRPH(N).EQ.'CSEDI   ' ) THEN
            CPHYS1 = 'CSEDI           '
            CPHYS2 = 'CONCENTRATION   '
            CALL PLSCA(CSEDI,BND1,BND2,IWRK1,IWRK2,IWRK3,IWRK4,
     $                 TIME,MXM,MYM,MZM,NM1,NM2,CPHYS1,CPHYS2,IFLGR,LP)
C
         ELSE
            CALL ERRMS2('OUTGRP',7141)
            WRITE(LP,*) '   VARIABLE ',CGRPH(N),' IS NOT DEFINED'
         END IF
C
  100 CONTINUE
C
      DEALLOCATE(BND1,RL)
      RETURN
      END
c
      subroutine plcod(x,y,z,mxm,mym,mzm,iflgr)
c **-----------------------------------------------------------------**
c **               output coordinate data block                      **
c **-----------------------------------------------------------------**
      implicit none
c
      integer,intent(inout):: mxm,mym,mzm,iflgr
      real(8),intent(inout):: x(mxm),y(mym),z(mzm)
      integer:: nrec,nb,nbx,i,j,k
      real(8):: time
c
      character*16 blcknm, czr16, coord
      character*80 cbound
      data blcknm /'                '/
      data czr16  /'                '/
      data coord  /'COORDINATE-DES  '/
      data cbound( 1:20) /'NORMAL              '/
      data cbound(21:40) /'                    '/
      data cbound(41:60) /'                    '/
      data cbound(61:80) /'                    '/
      data nrec, nb, nbx / 5, 1, 0 /
      data time / 0.0d0 /
c
c
      cbound(1:6)='NORMAL'
      write(iflgr) coord, blcknm, czr16, nrec, time
      write(iflgr) mxm, mym, mzm, nb, cbound, nbx
      write(iflgr) ( x(i), i=1,mxm )
      write(iflgr) ( y(j), j=1,mym )
c@@@
c     write(iflgr) ( z(k)*5.0d0, k=1,mzm )
      write(iflgr) ( z(k), k=1,mzm )
c
      return
      end
c
c
      subroutine plind(indp,indi,indj,indk,indc,mxm,mym,mzm,
     $                 nobss,iobss,nm1,nm2,iflgr)
c **-----------------------------------------------------------------**
c **               output index data block                           **
c **-----------------------------------------------------------------**
      implicit none
      integer,intent(inout):: mxm,mym,mzm,nobss,nm1,nm2,iflgr
      integer,intent(inout):: indp(1:mxm+1,1:mym+1,1:mzm+1)
      integer,intent(inout):: indi(1:mxm,2:mym,2:mzm)
      integer,intent(inout):: indj(2:mxm,1:mym,2:mzm)
      integer,intent(inout):: indk(2:mxm,2:mym,1:mzm)
      integer,intent(inout):: indc(1:mxm+1,1:mym+1,1:mzm+1)
      integer,intent(inout):: iobss(6,300)
      integer:: nrec,nbnd1,nbnd2,nt,nu,nv,nw,i,j,k
      real(8):: time
c
      character*16 blcknm, czr16, cindex
      data blcknm /'                '/
      data czr16  /'                '/
      data cindex /'INDEX           '/
      data nrec / 6 /
      data time /0.0d0/
c
c
      call mkindx(indi,indj,indk,indc,indp,mxm,mym,mzm,nbnd1,nobss,
     $            iobss)
      nbnd2 = 0
      nm1 = max(1,nbnd1)
      nm2 = max(1,nbnd2) * 2
c
      nt  = ( mxm - 1 ) * ( mym - 1 ) * ( mzm - 1 )
      nu  =   mxm       * ( mym - 1 ) * ( mzm - 1 )
      nv  = ( mxm - 1 ) *   mym       * ( mzm - 1 )
      nw  = ( mxm - 1 ) * ( mym - 1 ) *   mzm
c
      write(iflgr) cindex, blcknm, czr16, nrec, time
      write(iflgr) nt, nu, nv, nw, nbnd1, nbnd2
c
      write(iflgr) ( ( ( indc(i,j,k), i=2,mxm ), j=2,mym ), k=2,mzm )
      write(iflgr) ( ( ( indi(i,j,k), i=1,mxm ), j=2,mym ), k=2,mzm )
      write(iflgr) ( ( ( indj(i,j,k), i=2,mxm ), j=1,mym ), k=2,mzm )
      write(iflgr) ( ( ( indk(i,j,k), i=2,mxm ), j=2,mym ), k=1,mzm )
c
      return
      end
c
c
      subroutine mkindx(indi,indj,indk,indc,indp,mxm,mym,mzm,
     $                  nbnd1,nobss,iobss)
c----------------------------------------------------------------------
c     make index data
c----------------------------------------------------------------------
      implicit none
c
      include 'FILE.h'
      integer,intent(inout):: mxm,mym,mzm,nbnd1,nobss
      integer,intent(inout):: indc(1:mxm+1,1:mym+1,1:mzm+1)
      integer,intent(inout):: indi(1:mxm,2:mym,2:mzm)
      integer,intent(inout):: indj(2:mxm,1:mym,2:mzm)
      integer,intent(inout):: indk(2:mxm,2:mym,1:mzm)
      integer,intent(inout):: indp(1:mxm+1,1:mym+1,1:mzm+1)
      integer,intent(inout):: iobss(6,300)
      integer:: i,j,k,iflag,jflag,kflag
      integer:: il,ir,jl,jr,kl,kr,iw,jw,kw
C
C
      do 100 k=1,mzm+1
      do 100 j=1,mym+1
      do 100 i=1,mxm+1
         if( indp(i,j,k).gt.0 ) then
            indc(i,j,k) = 0
         else
            indc(i,j,k) = -1
         end if
CCC         if( i .eq. 1 .or. i .eq. mxm+1 .or.
CCC     $       j .eq. 1 .or. j .eq. mym+1 .or.
CCC     $       k .eq. 1 .or. k .eq. mzm+1 ) then
CCC            indc(i,j,k) = -1
CCC         else
CCC            indc(i,j,k) = 0
CCC         end if
  100 continue
c
CCC      do 110 n=1,nobss
CCC         is = iobss(1,n)
CCC         js = iobss(3,n)
CCC         ks = iobss(5,n)
CCC         ie = iobss(2,n)
CCC         je = iobss(4,n)
CCC         ke = iobss(6,n)
CCC         do 120 k=ks,ke
CCC         do 120 j=js,je
CCC         do 120 i=is,ie
CCC            indc(i,j,k) = -1
CCC  120    continue
CCC  110 continue
c
      do 130 k=2,mzm
      do 130 j=2,mym
      do 130 i=2,mxm
         if( indc(i,j,k) .eq. 0 ) then
            iflag = 0
            jflag = 0
            kflag = 0
            if( indc(i-1,j,k) .eq. -1 ) iflag = iflag + 2
            if( indc(i+1,j,k) .eq. -1 ) iflag = iflag + 1
            if( indc(i,j-1,k) .eq. -1 ) jflag = jflag + 2
            if( indc(i,j+1,k) .eq. -1 ) jflag = jflag + 1
            if( indc(i,j,k-1) .eq. -1 ) kflag = kflag + 2
            if( indc(i,j,k+1) .eq. -1 ) kflag = kflag + 1
            indc(i,j,k) = iflag * 100 + jflag * 10 + kflag
         end if
  130 continue
C
C
      nbnd1 = 0
C
      do 200 k=2,mzm
      do 200 j=2,mym
      do 200 i=1,mxm
         if( i .eq. 1 ) then
            il = 0
         else if( indc(i  ,j,k) .lt. 0 ) then
            il = 0
         else
            iw = indc(i  ,j,k) / 100
            if( iw .eq. 1 .or. iw .eq. 3 ) then
               il = 1
            else
               il = 0
            end if
         end if
c
         if( i .eq. mxm ) then
            ir = 0
         else if( indc(i+1,j,k) .lt. 0 ) then
            ir = 0
         else
            iw = indc(i+1,j,k) / 100
            if( iw .eq. 2 .or. iw .eq. 3 ) then
               ir = 1
            else
               ir = 0
            end if
         end if
c
         if( il + ir .eq. 0 ) then
            indi(i,j,k) = 0
         else if( il + ir .eq. 1 ) then
            nbnd1 = nbnd1 + 1
            indi(i,j,k) = nbnd1
         else
            CALL ERRMSG('MKINDX',7142)
            write(lp,*) 'error: (mkindx)'
            write(lp,*) ' new 2-value boundary is found'
            write(lp,*) ' direction = x'
            write(lp,*) ' (i,j,k)   = ',i,j,k
            CALL ABORT1('')
         end if
  200 continue
c
c
      do 300 k=2,mzm
      do 300 j=1,mym
      do 300 i=2,mxm
         if( j .eq. 1 ) then
            jl = 0
         else if( indc(i,j  ,k) .lt. 0 ) then
            jl = 0
         else
            jw = indc(i,j  ,k) / 10 - indc(i,j  ,k) / 100 * 10
            if( jw .eq. 1 .or. jw .eq. 3 ) then
               jl = 1
            else
               jl = 0
            end if
         end if
c
         if( j .eq. mym ) then
            jr = 0
         else if( indc(i,j+1,k) .lt. 0 ) then
            jr = 0
         else
            jw = indc(i,j+1,k) / 10 - indc(i,j+1,k) / 100 * 10
            if( jw .eq. 2 .or. jw .eq. 3 ) then
               jr = 1
            else
               jr = 0
            end if
         end if
c
         if( jl + jr .eq. 0 ) then
            indj(i,j,k) = 0
         else if( jl + jr .eq. 1 ) then
            nbnd1 = nbnd1 + 1
            indj(i,j,k) = nbnd1
         else
            CALL ERRMSG('MKINDX',7143)
            write(lp,*) 'error: (mkindx)'
            write(lp,*) ' new 2-value boundary is found'
            write(lp,*) ' direction = y'
            write(lp,*) ' (i,j,k)   = ',i,j,k
            CALL ABORT1('')
         end if
  300 continue
c
c
      do 400 k=1,mzm
      do 400 j=2,mym
      do 400 i=2,mxm
         if( k .eq. 1 ) then
            kl = 0
         else if( indc(i,j,k  ) .lt. 0 ) then
            kl = 0
         else
            kw = indc(i,j,k  ) - indc(i,j,k  ) / 10 * 10
            if( kw .eq. 1 .or. kw .eq. 3 ) then
               kl = 1
            else
               kl = 0
            end if
         end if
c
         if( k .eq. mzm ) then
            kr = 0
         else if( indc(i,j,k+1) .lt. 0 ) then
            kr = 0
         else
            kw = indc(i,j,k+1) - indc(i,j,k+1) / 10 * 10
            if( kw .eq. 2 .or. kw .eq. 3 ) then
               kr = 1
            else
               kr = 0
            end if
         end if
c
         if( kl + kr .eq. 0 ) then
            indk(i,j,k) = 0
         else if( kl + kr .eq. 1 ) then
            nbnd1 = nbnd1 + 1
            indk(i,j,k) = nbnd1
         else
            CALL ERRMSG('MKINDX',7144)
            write(lp,*) 'error: (mkindx)'
            write(lp,*) ' new 2-value boundary is found'
            write(lp,*) ' direction = z'
            write(lp,*) ' (i,j,k)   = ',i,j,k
            CALL ABORT1('')
         end if
  400 continue
c
      write(lp,*) ' 1-value boundary information (routine=mkindx)'
      write(lp,*) '   nbnd1 = ',nbnd1
c
      return
      end
c
c
      subroutine plsca(phs,bnd1,bnd2,indi,indj,indk,indc,
     $                 time,mxm,mym,mzm,nm1,nm2,cphys1,cphys2,iflgr,lp)
c **-----------------------------------------------------------------**
c **               output scalar data block                          **
c **-----------------------------------------------------------------**
      implicit none
c
      integer,intent(inout):: mxm,mym,mzm,nm1,nm2,iflgr,lp
      real(8),intent(inout):: phs(mxm+1,mym+1,mzm+1)
      real(8),intent(inout):: bnd1(nm1)
      real(8),intent(inout):: bnd2(nm2)
      integer,intent(inout):: indi(1:mxm,2:mym,2:mzm)
      integer,intent(inout):: indj(2:mxm,1:mym,2:mzm)
      integer,intent(inout):: indk(2:mxm,2:mym,1:mzm)
      integer,intent(inout):: indc(1:mxm+1,1:mym+1,1:mzm+1)
      real(8),intent(inout):: time
      character*16,intent(inout):: cphys1, cphys2
      integer:: nrec,nt,n,i,j,k
c
      character*16 blcknm, cscal
      data blcknm /'                '/
      data cscal  /'SCA/CENTER      '/
      data nrec / 5 /
c
c
      write(iflgr) cphys1, blcknm, cscal, nrec, time
c
      nt  = ( mxm - 1 ) * ( mym - 1 ) * ( mzm - 1 )
      write(iflgr) cphys2, nt, nm1, nm2
c
c
c ... set boundary values
      do 100 k=2,mzm
      do 100 j=2,mym
      do 100 i=1,mxm
         if( indi(i,j,k) .gt. 0 ) then
            if( i .eq. 1 .or. indc(i,j,k) .lt. 0 ) then
               bnd1(indi(i,j,k)) = phs(i+1,j,k)
            else
               bnd1(indi(i,j,k)) = phs(i  ,j,k)
            end if
         else if( indi(i,j,k) .lt. 0 ) then
            CALL ERRMSG('PLSCA',7145)
            WRITE(LP,*) 'indi <= 0',indi(i,j,k)
            CALL ABORT1('')
         end if
  100 continue
c
      do 200 k=2,mzm
      do 200 j=1,mym
      do 200 i=2,mxm
         if( indj(i,j,k) .gt. 0 ) then
            if( j .eq. 1 .or. indc(i,j,k) .lt. 0 ) then
               bnd1(indj(i,j,k)) = phs(i,j+1,k)
            else
               bnd1(indj(i,j,k)) = phs(i,j  ,k)
            end if
         else if( indj(i,j,k) .lt. 0 ) then
            CALL ERRMSG('PLSCA',7146)
            WRITE(LP,*) 'indj <= 0',indj(i,j,k)
            CALL ABORT1('')
         end if
  200 continue
c
      do 300 k=1,mzm
      do 300 j=2,mym
      do 300 i=2,mxm
         if( indk(i,j,k) .gt. 0 ) then
            if( k .eq. 1 .or. indc(i,j,k) .lt. 0 ) then
               bnd1(indk(i,j,k)) = phs(i,j,k+1)
            else
               bnd1(indk(i,j,k)) = phs(i,j,k  )
            end if
         else if( indk(i,j,k) .lt. 0 ) then
            CALL ERRMSG('PLSCA',7147)
            WRITE(LP,*) 'indk <= 0',indk(i,j,k)
            CALL ABORT1('')
         end if
  300 continue
c
      write(iflgr) ( ( ( phs(i,j,k), i=2,mxm ), j=2,mym ), k=2,mzm )
      write(iflgr) ( bnd1(n), n=1,nm1 )
      write(iflgr) ( bnd2(n), n=1,nm2 )
c
      return
      end
c
c
      subroutine plvec(time,nxyz,cphys1,cphys2,iflgr)
c **-----------------------------------------------------------------**
c **               output vector data block                          **
c **-----------------------------------------------------------------**
      implicit none
C
      real(8),intent(inout):: time
      integer,intent(inout):: nxyz,iflgr
      character*16,intent(inout):: cphys1, cphys2
      integer:: nrec
      character*16 blcknm, cvect
      data blcknm /'                '/
      data cvect /'VEC/CENTER      '/
      data nrec / 5 /
c
c
      write(iflgr) cphys1, blcknm, cvect, nrec, time
      write(iflgr) cphys2, nxyz
c
      return
      end
