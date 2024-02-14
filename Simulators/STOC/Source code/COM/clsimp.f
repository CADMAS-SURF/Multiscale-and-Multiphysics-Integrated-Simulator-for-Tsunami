      SUBROUTINE CLSIMP(CC,TMUZ,HH,ZC,GV,GZ,KG,KF,ANUD,DTMU)
C======================================================================
C     スカラーの輸送方程式を解き、スカラー変数を更新する
C     (鉛直方向の拡散項を陰に解く場合の第2段階の処理)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'FILE.h'
C
      REAL(8),INTENT(INOUT)::CC(MX,MY,MZ)
      REAL(8),INTENT(IN)::TMUZ(MX,MY,MZ)
      REAL(8),INTENT(IN)::HH(MX,MY)
      REAL(8),INTENT(IN)::ZC(8,MZ),GV(MX,MY,MZ),GZ(MX,MY,MZ)
      INTEGER,INTENT(IN)::KG(MX,MY),KF(MX,MY)
      REAL(8),INTENT(IN)::ANUD,DTMU
C
      INTEGER::I,J,K,IERR
      REAL(8),ALLOCATABLE::P(:),Q(:)
      REAL(8)::R,T,A,B,C,D,DHZ,ENU1,ENU2,DFZ1,DFZ2
C
C
      ALLOCATE(P(MZ),Q(MZ),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('CLSIMP',6870)
         WRITE(LP,*) 'CANNOT ALLOCATE P,Q'
         CALL ABORT1('')
      ENDIF
C
      DO 100 J=2,MYM
      DO 100 I=2,MXM
C
         IF(KF(I,J).EQ.MZ.OR.KF(I,J).EQ.KG(I,J)) GOTO 100
C
         P(KG(I,J)-1) = 0.0D0
         Q(KG(I,J)-1) = 0.0D0
         CC(I,J,KF(I,J)+1) = 0.0D0
C
C ...... 下から上に向かって、係数P,Qを求める
         DO K=KG(I,J),KF(I,J)
            T = DT*ZC(6,K)/GV(I,J,K)
            IF( K.EQ.KF(I,J) ) THEN
               DHZ = 1.0D0/MAX(HH(I,J)-ZC(1,K-1),1.0D-8)
               T = DT*DHZ/GV(I,J,K)
            ENDIF
C
            ENU1 = ENU2
            ENU2 = ANUD+(TMUZ(I,J,K)*ZC(7,K)+TMUZ(I,J,K+1)*ZC(8,K))/DTMU
            IF( K.EQ.KG(I,J) ) THEN
C             <FLUX 0条件>
               ENU1 = 0.0D0
            ENDIF
            IF( K.EQ.KF(I,J) ) THEN
C             <FLUX 0条件>
               ENU2 = 0.0D0
            ENDIF
            DFZ1 = GZ(I,J,K-1)*ENU1*ZC(5,K-1) 
            DFZ2 = GZ(I,J,K  )*ENU2*ZC(5,K  ) 
C
C           IF( K.EQ.KG(I,J) ) THEN
C             <濃度固定条件>
C              DFZ1 = 2.0D0*(ANUD+TMUZ(I,J,K)/DTMU)*ZC(6,K)/GV(I,J,K)
C           ENDIF
C
            A = 1.0D0+T*(DFZ1+DFZ2)
            B = T*DFZ2
            C = T*DFZ1
            D = CC(I,J,K)
C
C           IF( K.EQ.KG(I,J) ) THEN
C             <濃度固定条件>
C              C = 0.0D0
C              D = D + T*DFZ1*CFIX
C           ENDIF
C
            R    = 1.0D0/(A-C*P(K-1))
            P(K) = B*R
            Q(K) = (D+C*Q(K-1))*R
         ENDDO
C
C ...... 上から下に向かってCを決める
         DO K=KF(I,J),KG(I,J),-1
            CC(I,J,K) = P(K)*CC(I,J,K+1)+Q(K)
         ENDDO
C
C ...... 水面上のセルにコピー
         CC(I,J,KF(I,J)+1) = CC(I,J,KF(I,J))
  100 CONTINUE
C
      DEALLOCATE(P,Q)
      RETURN
      END
