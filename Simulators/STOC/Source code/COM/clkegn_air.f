      SUBROUTINE CLKEGN_AIR(GS,UUA,VVA,WWA,TMUA,XC,YC,ZCA,
     $                      INDUA,INDVA,INDWA,INDPA,UT,VT,WT)
C----------------------------------------------------------------------
C     乱流モデルの生成項の計算
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      include 'TIMEI.h'
      include 'FILE.h'
C
      REAL(8),INTENT(OUT)::GS(MX,MY,MZA)
      REAL(8),INTENT(IN)::UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8),INTENT(IN)::TMUA(MX,MY,MZA)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZCA(8,MZA)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA),INDVA(MX,MY,MZA)
      INTEGER,INTENT(IN)::INDWA(MX,MY,MZA),INDPA(MX,MY,MZA)
      REAL(8)::UT(MX,MY,MZA),VT(MX,MY,MZA),WT(MX,MY,MZA)
C
      REAL(8)::SXX,SXY,SYY,SYZ,SZX,SZZ
      REAL(8)::UTVM,UTVP,UTWM,UTWP,VTUM,VTUP
      REAL(8)::VTWM,VTWP,WTUM,WTUP,WTVM,WTVP
      INTEGER::I,J,K
C
C
      CALL ZERCLR(GS,MXY*MZA,0.0D0)
      CALL ZERCLR(UT,MXY*MZA,0.0D0)
      CALL ZERCLR(VT,MXY*MZA,0.0D0)
      CALL ZERCLR(WT,MXY*MZA,0.0D0)
C
      DO K=2,MZMA
      DO J=2,MYM
      DO I=2,MXM
         IF(INDPA(I,J,K).NE.0) THEN
            UT(I,J,K) = 0.5D0*(UUA(I-1,J,K)+UUA(I,J,K))
            VT(I,J,K) = 0.5D0*(VVA(I,J-1,K)+VVA(I,J,K))
            WT(I,J,K) = 0.5D0*(WWA(I,J,K-1)+WWA(I,J,K))
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C
      CALL CP_DSR_DC2(MX,MY,MZA,0,1,UT)
      CALL CP_DSR_DC2(MX,MY,MZA,0,1,VT)
      CALL CP_DSR_DC2(MX,MY,MZA,0,1,WT)
C
C
      DO K=2,MZMA
      DO J=2,MYM
      DO I=2,MXM
         IF(INDPA(I,J,K).NE.0) THEN
            IF(INDVA(I,J,K).EQ.1) THEN
               UTVP=YC(8,J)*UT(I,J+1,K)+YC(7,J)*UT(I,J,K)
               WTVP=YC(8,J)*WT(I,J+1,K)+YC(7,J)*WT(I,J,K)
            ELSE
               UTVP=UT(I,J,K)
               WTVP=WT(I,J,K)
            ENDIF
C
            IF(INDVA(I,J-1,K).EQ.1) THEN
               UTVM=YC(8,J-1)*UT(I,J,K)+YC(7,J-1)*UT(I,J-1,K)
               WTVM=YC(8,J-1)*WT(I,J,K)+YC(7,J-1)*WT(I,J-1,K)
            ELSE
               UTVM=UT(I,J,K)
               WTVM=WT(I,J,K)
            ENDIF
C
            IF(INDWA(I,J,K).EQ.1) THEN
               UTWP=ZCA(8,K)*UT(I,J,K+1)+ZCA(7,K)*UT(I,J,K)
               VTWP=ZCA(8,K)*VT(I,J,K+1)+ZCA(7,K)*VT(I,J,K)
            ELSE
               UTWP=UT(I,J,K)
               VTWP=VT(I,J,K)
            ENDIF
C
            IF(INDWA(I,J,K-1).EQ.1) THEN
               UTWM=ZCA(8,K-1)*UT(I,J,K)+ZCA(7,K-1)*UT(I,J,K-1)
               VTWM=ZCA(8,K-1)*VT(I,J,K)+ZCA(7,K-1)*VT(I,J,K-1)
            ELSE
               UTWM=UT(I,J,K)
               VTWM=VT(I,J,K)
            ENDIF
C
            IF(INDUA(I,J,K).EQ.1) THEN
               VTUP=XC(8,I,J)*VT(I+1,J,K)+XC(7,I,J)*VT(I,J,K)
               WTUP=XC(8,I,J)*WT(I+1,J,K)+XC(7,I,J)*WT(I,J,K)
            ELSE
               VTUP=VT(I,J,K)
               WTUP=WT(I,J,K)
            ENDIF
C
            IF(INDUA(I-1,J,K).EQ.1) THEN
               VTUM=XC(8,I-1,J)*VT(I,J,K)+XC(7,I-1,J)*VT(I-1,J,K)
               WTUM=XC(8,I-1,J)*WT(I,J,K)+XC(7,I-1,J)*WT(I-1,J,K)
            ELSE
               VTUM=VT(I,J,K)
               WTUM=WT(I,J,K)
            ENDIF
C
            SXX=XC(6,I,J)*(UUA(I,J,K)-UUA(I-1,J,K))
            SYY=YC(6,J)*(VVA(I,J,K)-VVA(I,J-1,K))
            SZZ=ZCA(6,K)*(WWA(I,J,K)-WWA(I,J,K-1))
            SXY=(YC(6,J)*(UTVP-UTVM)+XC(6,I,J)*(VTUP-VTUM))
            SYZ=(ZCA(6,K)*(VTWP-VTWM)+YC(6,J)*(WTVP-WTVM))
            SZX=(XC(6,I,J)*(WTUP-WTUM)+ZCA(6,K)*(UTWP-UTWM))
C
            GS(I,J,K)=TMUA(I,J,K)
     $               *( 2.0D0*(SXX**2+SYY**2+SZZ**2)
     $               + SXY**2+SYZ**2+SZX**2 )
         ENDIF
      ENDDO
      ENDDO
      ENDDO
C
Cdbgc ... debug write
Cdbg      if( debug_air11.eq.1 ) then
Cdbg         do j=1,my
Cdbg            write(lp,*) 'uua j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(uua(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg         do j=1,my
Cdbg            write(lp,*) 'wwa j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(wwa(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg         do j=1,my
Cdbg            write(lp,*) 'tmua j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(tmua(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg         do j=1,my
Cdbg            write(lp,*) 'gs j=',j,' istep=',istep
Cdbg            write(lp,'(a5,<mx>i10)') ' k%i|',(i,i=1,mx)
Cdbg            do k=mza,1,-1
Cdbg               write(lp,'(i4,a1,<mx>e10.3)') k,'|',(gs(i,j,k),i=1,mx)
Cdbg            enddo
Cdbg            write(lp,*) ''
Cdbg         enddo
Cdbg      endif
      RETURN
      END
