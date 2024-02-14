      SUBROUTINE MKIND_AIR(HH,ZCA,INDPA,INDUA,INDVA,INDWA,KFA,KFNA)
C======================================================================
C     インデックス INDPA, INDUA, INDVA, INDWA の値を初期化する
C
C     (1) 流速固定境界を設定: 側面
C     (2) 自由流入出境界(0)を設定：上面
C     (3) 壁を設定：下面
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
      INTEGER,INTENT(OUT)::KFA(MX,MY),KFNA(MX,MY)
C
      INTEGER::I,J,K
C
C
C----------------------------------------------------------------------
C     (0) 全て流体セルとして初期化
C----------------------------------------------------------------------
      CALL ZERCLI(INDPA,MXY*MZA,1)
      CALL ZERCLI(INDUA,MXY*MZA,1)
      CALL ZERCLI(INDVA,MXY*MZA,1)
      CALL ZERCLI(INDWA,MXY*MZA,1)
      CALL ZERCLI(KFA,MXY,2)
C
C ... 仮想セルの設定
      INDPA( 1,:,:)=0
      INDPA(MX,:,:)=0
      INDPA(:, 1,:)=0
      INDPA(:,MY,:)=0
      INDPA(:,:, 1)=0
      INDPA(:,:,MZA)=0
C
      INDUA(MX,:,:)=-4
      INDUA(:, 1,:)=-4
      INDUA(:,MY,:)=-4
      INDUA(:,:, 1)=-4
      INDUA(:,:,MZA)=-4
C
      INDVA( 1,:,:)=-4
      INDVA(MX,:,:)=-4
      INDVA(:,MY,:)=-4
      INDVA(:,:, 1)=-4
      INDVA(:,:,MZA)=-4
C
      INDWA( 1,:,:)=-4
      INDWA(MX,:,:)=-4
      INDWA(:, 1,:)=-4
      INDWA(:,MY,:)=-4
      INDWA(:,:,MZA)=-4
C
C
C----------------------------------------------------------------------
C     (1) 流速固定境界(-1)を設定: 側面
C----------------------------------------------------------------------
      DO K=2,MZMA
      DO J=2,MYM
         IF( IBCAIRWES.GE.0 ) INDUA(  1,J,K)=-1
         IF( IBCAIRWES.LT.0 ) INDUA(  1,J,K)=-2
C
         IF( IBCAIREAS.GE.0 ) INDUA(MXM,J,K)=-1
         IF( IBCAIREAS.LT.0 ) INDUA(MXM,J,K)=-2
      ENDDO
      ENDDO
C
      DO K=2,MZMA
      DO I=2,MXM
         IF( IBCAIRSOU.GE.0 ) INDVA(I,  1,K)=-1
         IF( IBCAIRSOU.LT.0 ) INDVA(I,  1,K)=-2
C
         IF( IBCAIRNOR.GE.0 ) INDVA(I,MYM,K)=-1
         IF( IBCAIRNOR.LT.0 ) INDVA(I,MYM,K)=-2
      ENDDO
      ENDDO
C
C
C----------------------------------------------------------------------
C     (2) 自由流入出境界(0)を設定：上面
C----------------------------------------------------------------------
      DO J=2,MYM
      DO I=2,MXM
         IF( IBCAIRTOP.LE.-2 ) INDWA(I,J,MZMA)=0
         IF( IBCAIRTOP.GE.-1 ) INDWA(I,J,MZMA)=-2
      ENDDO
      ENDDO
C
C----------------------------------------------------------------------
C     (3) 水面
C----------------------------------------------------------------------
      DO J=2,MYM
      DO I=2,MXM
         DO K=2,MZMA
            IF( HH(I,J)+GZHAIR.LT.ZCA(1,K) ) THEN
                KFA(I,J)=K
               KFNA(I,J)=K
               INDWA(I,J,K-1)=-2
               EXIT
            ELSEIF( K.EQ.MZMA ) THEN
                KFA(I,J)=K+1
               KFNA(I,J)=K+1
               INDWA(I,J,K)=-4
               EXIT
            ENDIF
         ENDDO
C
         DO K=2,KFA(I,J)-1
            INDPA(I,J,K)=0
            INDWA(I,J,K-1)=-4
         ENDDO
      ENDDO
      ENDDO
C
      DO J=2,MYM
      DO I=2,MXM-1
         DO K=2,MAX(KFA(I,J),KFA(I+1,J))
            IF( INDPA(I,J,K).EQ.0.AND.INDPA(I+1,J,K).EQ.0 ) THEN
               INDUA(I,J,K)=-4
            ELSEIF( INDPA(I,J,K).EQ.0.OR.INDPA(I+1,J,K).EQ.0 ) THEN
               INDUA(I,J,K)=-2
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
      DO J=2,MYM-1
      DO I=2,MXM
         DO K=2,MAX(KFA(I,J),KFA(I,J+1))
            IF( INDPA(I,J,K).EQ.0.AND.INDPA(I,J+1,K).EQ.0 ) THEN
               INDVA(I,J,K)=-4
            ELSEIF( INDPA(I,J,K).EQ.0.OR.INDPA(I,J+1,K).EQ.0 ) THEN
               INDVA(I,J,K)=-2
            ENDIF
         ENDDO
      ENDDO
      ENDDO
C
C
c ... debug write
      if( debug_air1.eq.1 ) then
      do j=1,my
         write(lp,*) 'indpa j=',j
         do k=mza,1,-1
            write(lp,'(i4,a1,200i4)') k,'|',(indpa(i,j,k),i=1,mx)
         enddo
         write(lp,*) ''
      enddo
c
      do j=1,my
         write(lp,*) 'indua j=',j
         do k=mza,1,-1
            write(lp,'(i4,a1,200i4)') k,'|',(indua(i,j,k),i=1,mx)
         enddo
         write(lp,*) ''
      enddo
c
      do j=1,my
         write(lp,*) 'indva j=',j
         do k=mza,1,-1
            write(lp,'(i4,a1,200i4)') k,'|',(indva(i,j,k),i=1,mx)
         enddo
         write(lp,*) ''
      enddo
c
      do j=1,my
         write(lp,*) 'indwa j=',j
         do k=mza,1,-1
            write(lp,'(i4,a1,200i4)') k,'|',(indwa(i,j,k),i=1,mx)
         enddo
         write(lp,*) ''
      enddo
c
      do j=1,my
         write(lp,*) 'kfa j=',j
         write(lp,'(200i4)') (kfa(i,j),i=1,mx)
         write(lp,*) ''
      enddo
      endif
c
      RETURN
      END
