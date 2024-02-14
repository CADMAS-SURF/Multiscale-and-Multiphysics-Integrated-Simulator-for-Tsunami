      SUBROUTINE QBICGS(XX,AD,AL,AU,BB,DD,PI,SI,RI,R0,W1,W2,INDP,MZ0)
C======================================================================
C     BiCGstab法を用いて、非対称行列を解く
C     XX: 解ベクトル
C     AD: 行列の対角成分
C     AL: 行列の下側非対角成分
C     AU: 行列の上側非対角成分
C     BB: 右辺ベクトル
C======================================================================
      IMPLICIT NONE
C
      REAL(8),PARAMETER::DIVER=1.0D30
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'MATRIX.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'mpif.h'
      include 'TIMEI.h'
C
      REAL(8),INTENT(INOUT)::XX(MX,MY,MZ0),AD(MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::AL(3,MX,MY,MZ0),AU(3,MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::BB(MX,MY,MZ0),DD(MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::PI(MX,MY,MZ0),SI(MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::RI(MX,MY,MZ0),R0(MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::W1(MX,MY,MZ0),W2(MX,MY,MZ0)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ0)
      INTEGER,INTENT(IN)::MZ0
C
      REAL(8)::ZERO=1.0D-40
C
      REAL(8)::ALPHA,BETA,BNORM,DAD,OMEGA,S,T,U,V,XNRM1,XRAT,RIP
      INTEGER::I,J,K,IERR
C
C ... 右辺残差ノルムを計算
      ITRMTX = -1
      CALL  QIP(BB,BB,BNORM,INDP,MZ0)
      IF(NPROC.GT.1) THEN
        RIP = BNORM
        CALL FTIMER(79,0)
        CALL MPI_ALLREDUCE
     &      ( RIP,BNORM,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &        CHILDCOMM,IERR )
        CALL FTIMER(79,1)
      END IF
C
      BNORM = DSQRT(BNORM)
C          右辺はゼロか
      IF(BNORM.LE.EPSMTX) GO TO 1000
C
C ... 作業用配列を0クリア ＆ XXにDPの値をコピー
      DO 100 K=1,MZ0
      DO 100 J=1,MY
      DO 100 I=1,MX
C         XX(I,J,K) = DP(I,J,K)
         XX(I,J,K) = 0.0D0
         PI(I,J,K) = 0.0D0
         SI(I,J,K) = 0.0D0
         RI(I,J,K) = 0.0D0
         R0(I,J,K) = 0.0D0
         W1(I,J,K) = 0.0D0
         W2(I,J,K) = 0.0D0
C ... 係数行列のスケーリング
         DAD = 0.0D0
         IF(AD(I,J,K).NE.0.0D0) DAD=1.0D0/AD(I,J,K)
         AD(  I,J,K) = 1.0D0
         AL(1,I,J,K) = AL(1,I,J,K)*DAD
         AL(2,I,J,K) = AL(2,I,J,K)*DAD
         AL(3,I,J,K) = AL(3,I,J,K)*DAD
         AU(1,I,J,K) = AU(1,I,J,K)*DAD
         AU(2,I,J,K) = AU(2,I,J,K)*DAD
         AU(3,I,J,K) = AU(3,I,J,K)*DAD
         BB(  I,J,K) = BB(  I,J,K)*DAD
  100 CONTINUE
C
C ... 不完全LU分解
C
      CALL QILUDC(AD,AL,AU,DD,INDP,MZ0)
C
C ... 初期設定
C
      ITRMTX=0
C                                       R0=A*X
      CALL FTIMER(79,0)
      CALL CP_DSR_DC2(MX,MY,MZ0,0,1,XX)
      CALL FTIMER(79,1)
      CALL QAX(AD,AL,AU,XX,R0,INDP,MZ0)
C                                       R0=B-A*X
      DO 200 K=2,MZ0-1
      DO 200 J=2,MYM
      DO 200 I=2,MXM
         R0(I,J,K)=BB(I,J,K)-R0(I,J,K)
  200 CONTINUE
C                                       R0=(LU)**(-1)*(B-A*X)
      CALL FTIMER(79,0)
      CALL CP_DSR_DC2(MX,MY,MZ0,0,1,R0)
      CALL FTIMER(79,1)
      CALL QMINV(AL,AU,DD,R0,R0,INDP,MZ0)
C                                       W1=(LU)**(-1)*B
      CALL FTIMER(79,0)
      CALL CP_DSR_DC2(MX,MY,MZ0,0,1,W1)
      CALL FTIMER(79,1)
      CALL QMINV(AL,AU,DD,BB,W1,INDP,MZ0)
C                                       XNRM1=(W1,W1)
      CALL QIP(W1,W1,XNRM1,INDP,MZ0)
      IF(NPROC.GT.1) THEN
        RIP = XNRM1
        CALL FTIMER(79,0)
        CALL MPI_ALLREDUCE
     &      ( RIP,XNRM1,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &        CHILDCOMM,IERR )
        CALL FTIMER(79,1)
      END IF
C                                       S=(R0,R0)
      CALL QIP(R0,R0,S,INDP,MZ0)
      IF(NPROC.GT.1) THEN
        RIP = S
        CALL FTIMER(79,0)
        CALL MPI_ALLREDUCE
     &      ( RIP,S,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &        CHILDCOMM,IERR )
        CALL FTIMER(79,1)
      END IF
C
      XNRM1=DSQRT(XNRM1)
      RNRMTX=DSQRT(S)
      BNORM=XNRM1
      IF( XNRM1.LE.ZERO  ) XNRM1=1.0D0
      IF( RNRMTX.LE.EPSMTX ) GO TO 1000
C                                       PI=RI=R0=(LU)**(-1)*(B-A*X)
      DO 300 K=2,MZ0-1
      DO 300 J=2,MYM
      DO 300 I=2,MXM
        PI(I,J,K)=R0(I,J,K)
        RI(I,J,K)=R0(I,J,K)
C        R0(I,J,K)=SIGN(1.0D0,R0(I,J,K))*MAX(1.0D0,DABS(R0(I,J,K)))
  300 CONTINUE
C                                       S=(R0,RI)
      CALL QIP(R0,RI,S,INDP,MZ0)
      IF(NPROC.GT.1) THEN
        RIP = S
        CALL FTIMER(79,0)
        CALL MPI_ALLREDUCE
     &      ( RIP,S,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &        CHILDCOMM,IERR )
        CALL FTIMER(79,1)
      END IF
C
C ... 反復計算
C
  500 CONTINUE
      ITRMTX=ITRMTX+1
      IF( ITRMTX.GT.MAXMTX ) GO TO 1100
C                                       W1=A*PI
      CALL FTIMER(79,0)
      CALL CP_DSR_DC2(MX,MY,MZ0,0,1,PI)
      CALL FTIMER(79,1)
      CALL QAX(AD,AL,AU,PI,W1,INDP,MZ0)
C                                       W1=(LU)**(-1)*A*PI
      CALL FTIMER(79,0)
      CALL CP_DSR_DC2(MX,MY,MZ0,0,1,W1)
      CALL FTIMER(79,1)
      CALL QMINV(AL,AU,DD,W1,W1,INDP,MZ0)
C                                       V=(R0,W1)=(R0,(LU)**(-1)*A*PI)
      CALL QIP(R0,W1,V,INDP,MZ0)
      IF(NPROC.GT.1) THEN
        RIP = V
        CALL FTIMER(79,0)
        CALL MPI_ALLREDUCE
     &      ( RIP,V,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &        CHILDCOMM,IERR )
        CALL FTIMER(79,1)
      END IF
C
      IF( V.NE.0.0D0 ) THEN
        ALPHA=S/V
      ELSE IF( DABS(S).LT.ZERO ) THEN
        ALPHA=0.0D0
      ELSE
        WRITE(LP,*) 'ALPHA(V=0.0)'
        GO TO 1300
      END IF
C                                       SI=RI-ALPHA*(LU)**(-1)*A*PI
      DO 600 K=2,MZ0-1
      DO 600 J=2,MYM
      DO 600 I=2,MXM
        SI(I,J,K)=RI(I,J,K)-ALPHA*W1(I,J,K)
  600 CONTINUE
C                                       W2=A*SI
      CALL FTIMER(79,0)
      CALL CP_DSR_DC2(MX,MY,MZ0,0,1,SI)
      CALL FTIMER(79,1)
      CALL QAX(AD,AL,AU,SI,W2,INDP,MZ0)
C                                       W2=(LU)**(-1)*A*PI
      CALL FTIMER(79,0)
      CALL CP_DSR_DC2(MX,MY,MZ0,0,1,W2)
      CALL FTIMER(79,1)
      CALL QMINV(AL,AU,DD,W2,W2,INDP,MZ0)
C                                       U=((LU)**(-1)*A*PI,SI)
      CALL QIP(W2,SI,U,INDP,MZ0)
      IF(NPROC.GT.1) THEN
        RIP = U
        CALL FTIMER(79,0)
        CALL MPI_ALLREDUCE
     &      ( RIP,U,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &        CHILDCOMM,IERR )
        CALL FTIMER(79,1)
      END IF
C                                       V=((LU)**(-1)*A*PI,(LU)**(-1)*A*PI)
      CALL QIP(W2,W2,V,INDP,MZ0)
      IF(NPROC.GT.1) THEN
        RIP = V
        CALL FTIMER(79,0)
        CALL MPI_ALLREDUCE
     &      ( RIP,V,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &        CHILDCOMM,IERR )
        CALL FTIMER(79,1)
      END IF
C                                       OMEGA=(W2,SI)/(W2,W2)
      IF(V.NE.0.0D0) THEN
        OMEGA=U/V
      ELSE IF(ITRMTX.EQ.1) THEN
        V = 1.0D0
      ELSE
        OMEGA=0.0D0
        WRITE(LP,*) 'OMEGA(V=0.0)'
        GO TO 1300
      END IF
C                                       X=X+ALPHA*PI+OMEGA*SI
C                                       RI=SI-OMEGA*(LU)**(-1)*A*PI
      DO 700 K=2,MZ0-1
      DO 700 J=2,MYM
      DO 700 I=2,MXM
        XX(I,J,K)=XX(I,J,K)+ALPHA*PI(I,J,K)+OMEGA*SI(I,J,K)
        RI(I,J,K)=SI(I,J,K)-OMEGA*W2(I,J,K)
  700 CONTINUE
C                                       RNRMTX=(RI,RI)
      CALL QIP(RI,RI,RNRMTX,INDP,MZ0)
      IF(NPROC.GT.1) THEN
        RIP = RNRMTX
        CALL FTIMER(79,0)
        CALL MPI_ALLREDUCE
     &      ( RIP,RNRMTX,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &        CHILDCOMM,IERR )
        CALL FTIMER(79,1)
      END IF
      RNRMTX=DSQRT(RNRMTX)
      XRAT=RNRMTX/XNRM1
C
      IF( RNRMTX.LE.EPSMTX.OR.RNRMTX.LE.EPRMTX*BNORM ) GO TO 1200
      IF( RNRMTX.GT.DIVER ) GO TO 1300
C                                       T=(R0,RI)
      CALL QIP(R0,RI,T,INDP,MZ0)
      IF(NPROC.GT.1) THEN
        RIP = T
        CALL FTIMER(79,0)
        CALL MPI_ALLREDUCE
     &      ( RIP,T,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &        CHILDCOMM,IERR )
        CALL FTIMER(79,1)
      END IF
C                                       BETA=T*ALPHA/(S*OMEGA)
C
      IF(S.NE.0.0D0.AND.OMEGA.NE.0.0D0) THEN
        BETA=T*ALPHA/(S*OMEGA)
      ELSE
        WRITE(LP,*) 'BETA(S.OR.OMEGA=0.0) S.&.OMEGA=',S,OMEGA
        GO TO 1300
      END IF
C                                       PI=RI+BETA*(PI-OMEGA*A*PI)
      DO 800 K=2,MZ0-1
      DO 800 J=2,MYM
      DO 800 I=2,MXM
        PI(I,J,K)=RI(I,J,K)+BETA*(PI(I,J,K)-OMEGA*W1(I,J,K))
  800 CONTINUE
      S=T
      IF( LPRMTX.EQ.1 ) WRITE(LP,6000) ITRMTX,RNRMTX,XRAT,BNORM
      GO TO 500
C
C-----( ENDING )--------------------------------------------------------
C
 1000 CONTINUE
      IF( ITRMTX.EQ.-1 ) THEN
        IF( LPRMTX.EQ.1 ) WRITE(LP,6150) BNORM
        ITRMTX=0
      ELSE
        IF(XNRM1.LE.ZERO) XNRM1=1.0D0
        XRAT=RNRMTX/XNRM1
        IF( LPRMTX.EQ.1 ) WRITE(LP,6100) RNRMTX,XRAT,BNORM
      END IF
C
      CALL FTIMER(79,0)
      CALL CP_DSR_DC2(MX,MY,MZ0,0,1,XX)
      CALL FTIMER(79,1)
C
      RETURN
 1100 CONTINUE
      IF( LPRMTX.EQ.1 ) WRITE(LP,6200) RNRMTX,XRAT,BNORM
      WRITE(LP,6200) RNRMTX,XRAT,BNORM
      GO TO 1400
 1200 CONTINUE
      IF( LPRMTX.EQ.1 ) WRITE(LP,6000) ITRMTX,RNRMTX,XRAT,BNORM
      GO TO 1400
 1300 CONTINUE
      IF( LPRMTX.EQ.1 ) WRITE(LP,6300) RNRMTX,XRAT,BNORM
      WRITE(LP,6300) RNRMTX,XRAT,BNORM
C
 1400 CONTINUE
C
      CALL FTIMER(79,0)
      CALL CP_DSR_DC2(MX,MY,MZ0,0,1,XX)
      CALL FTIMER(79,1)
C
      RETURN
C
 6000 FORMAT(' ITR = ',I6,
     $       ' !B-A*X! = ',1PD12.5,' !B-A*X!/!B! = ',1PD12.5,
     $       ' !B! = ',1PD12.5)
 6100 FORMAT(' NO ITERATION',
     $       ' !B-A*X! = ',1PD12.5,' !B-A*X!/!B! = ',1PD12.5,
     $       ' !B! = ',1PD12.5)
 6150 FORMAT(' NO ITERATION','  RIGHT HAND VECTOR !B! = ',1PD12.5)
 6200 FORMAT(' NOT CONVEGED',
     $       ' !B-A*X! = ',1PD12.5,' !B-A*X!/!B! = ',1PD12.5,
     $       ' !B! = ',1PD12.5)
 6300 FORMAT(' INSTABILITY ',
     $       ' !B-A*X! = ',1PD12.5,' !B-A*X!/!B! = ',1PD12.5,
     $       ' !B! = ',1PD12.5)
      END
