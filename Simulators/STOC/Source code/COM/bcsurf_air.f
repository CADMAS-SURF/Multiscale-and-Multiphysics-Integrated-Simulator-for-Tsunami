      SUBROUTINE BCSURF_AIR(HH,ZCA,INDPA,INDUA,INDVA,INDWA,KFA,KFNA)
C======================================================================
C     インデックス INDPA, INDUA, INDVA, INDWA の値を更新する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
      INCLUDE 'FILE.h'
C
      REAL(8),INTENT(IN)::HH(MX,MY)
      REAL(8),INTENT(IN):: ZCA(8,MZA)
      INTEGER,INTENT(OUT)::INDPA(MX,MY,MZA),INDUA(MX,MY,MZA)
      INTEGER,INTENT(OUT)::INDVA(MX,MY,MZA),INDWA(MX,MY,MZA)
      INTEGER,INTENT(IN)::KFA(MX,MY),KFNA(MX,MY)
C
      INTEGER::I,J,K,KMIN,KMAX
C
C
C----------------------------------------------------------------------
C     (3) 水面
C----------------------------------------------------------------------
      DO J=2,MYM
      DO I=2,MXM
         DO K=2,MZMA
            IF( HH(I,J)+GZHAIR.LT.ZCA(1,K) ) THEN
               INDWA(I,J,K-1)=-2
               EXIT
            ELSEIF( K.EQ.MZMA ) THEN
               INDWA(I,J,K)=-4
               EXIT
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
      DO J=2,MYM
      DO I=2,MXM
C KFNA > KFA
         DO K=KFA(I,J),KFNA(I,J)-1
            INDWA(I,J,K)=1
            IF( K.EQ.MZMA.AND.IBCAIRTOP.LE.-2 ) INDWA(I,J,K)=0
            IF( K.EQ.MZMA.AND.IBCAIRTOP.GE.-1 ) INDWA(I,J,K)=-2
C
            INDPA(I,J,K)=1
         ENDDO
C
C KFA > KFNA
         DO K=KFNA(I,J),KFA(I,J)-1
            INDWA(I,J,K-1)=-4
C
            INDPA(I,J,K)=0
         ENDDO
      ENDDO
      ENDDO

C
      DO J=2,MYM
      DO I=2,MXM-1
         KMIN=MIN(KFA(I,J),KFA(I+1,J),KFNA(I,J),KFNA(I+1,J))
         KMAX=MAX(KFA(I,J),KFA(I+1,J),KFNA(I,J),KFNA(I+1,J))
         IF( KMAX.EQ.MZMA ) KMAX=MZMA
C
         DO K=KMIN,KMAX
            IF( INDPA(I,J,K).EQ.0.AND.INDPA(I+1,J,K).EQ.0 ) THEN
               INDUA(I,J,K)=-4
            ELSEIF( INDPA(I,J,K).EQ.0.OR.INDPA(I+1,J,K).EQ.0 ) THEN
               INDUA(I,J,K)=-2
            ELSE
               INDUA(I,J,K)=1
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
      DO J=2,MYM-1
      DO I=2,MXM
         KMIN=MIN(KFA(I,J),KFA(I,J+1),KFNA(I,J),KFNA(I,J+1))
         KMAX=MAX(KFA(I,J),KFA(I,J+1),KFNA(I,J),KFNA(I,J+1))
         IF( KMAX.EQ.MZMA ) KMAX=MZMA
C
         DO K=KMIN,KMAX
            IF( INDPA(I,J,K).EQ.0.AND.INDPA(I,J+1,K).EQ.0 ) THEN
               INDVA(I,J,K)=-4
            ELSEIF( INDPA(I,J,K).EQ.0.OR.INDPA(I,J+1,K).EQ.0 ) THEN
               INDVA(I,J,K)=-2
            ELSE
               INDVA(I,J,K)=1
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
C
Cdbgc ... debug write
Cdbg      if( debug_air1.eq.1 ) then
Cdbg      do j=1,my
Cdbg         write(lp,*) 'indpa j=',j
Cdbg         write(lp,'(a5,<mx>i4)') ' k%i|',(i,i=1,mx)
Cdbg         do k=mza,1,-1
Cdbg            write(lp,'(i4,a1,<mx>i4)') k,'|',(indpa(i,j,k),i=1,mx)
Cdbg         enddo
Cdbg         write(lp,*) ''
Cdbg      enddo
Cdbgc
Cdbg      do j=1,my
Cdbg         write(lp,*) 'indua j=',j
Cdbg         write(lp,'(a5,<mx>i4)') ' k%i|',(i,i=1,mx)
Cdbg         do k=mza,1,-1
Cdbg            write(lp,'(i4,a1,<mx>i4)') k,'|',(indua(i,j,k),i=1,mx)
Cdbg         enddo
Cdbg         write(lp,*) ''
Cdbg      enddo
Cdbgc
Cdbg      do j=1,my
Cdbg         write(lp,*) 'indva j=',j
Cdbg         write(lp,'(a5,<mx>i4)') ' k%i|',(i,i=1,mx)
Cdbg         do k=mza,1,-1
Cdbg            write(lp,'(i4,a1,<mx>i4)') k,'|',(indva(i,j,k),i=1,mx)
Cdbg         enddo
Cdbg         write(lp,*) ''
Cdbg      enddo
Cdbgc
Cdbg      do j=1,my
Cdbg         write(lp,*) 'indwa j=',j
Cdbg         write(lp,'(a5,<mx>i4)') ' k%i|',(i,i=1,mx)
Cdbg         do k=mza,1,-1
Cdbg            write(lp,'(i4,a1,<mx>i4)') k,'|',(indwa(i,j,k),i=1,mx)
Cdbg         enddo
Cdbg         write(lp,*) ''
Cdbg      enddo
Cdbg      endif
Cdbgc
      RETURN
      END
