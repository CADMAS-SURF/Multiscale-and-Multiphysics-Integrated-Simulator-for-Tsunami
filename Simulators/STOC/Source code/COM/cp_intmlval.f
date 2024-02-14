      SUBROUTINE CP_INTMLVAL(UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,
     1                       UF_ML,VF_ML,WF_ML,TF_ML,CF_ML,HF_ML,
     2                       UB_ML,VB_ML,WB_ML,TB_ML,CB_ML,HB_ML,
     3                       KF_ML,ZC_ML,MX_ML,MY_ML,MZ_ML,
     4                       IEAS,IWES,JSOU,JNOR,KBOT,KTOP,IFLAG)
C-----------------------------------------------------------------------
C     MLの境界値(流速,潮位)を時間補間用のエリアにセットする
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE  'CP_NESTBC.h'
      INCLUDE  'TIMER.h'
      INCLUDE  'FILE.h'
      INCLUDE  'BOUNDI.h'
      INCLUDE  'MODELI.h'
C
      REAL(8),INTENT(INOUT)::
     $   UU_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   VV_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   WW_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   TT_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   CC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   HH_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::
     $   UF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   VF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   WF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   TF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   CF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   HF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::
     $   UB_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   VB_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   WB_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   TB_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   CB_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   HB_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
      REAL(8),INTENT(INOUT)::ZC_ML(8,MZ_ML)
C
      INTEGER,INTENT(INOUT)::KF_ML(MX_ML,MY_ML)
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR,KBOT,KTOP
      INTEGER,INTENT(INOUT)::IFLAG
C
      INTEGER::IDB=0
      INTEGER::I,J,K
      REAL(8)::W1,W2,TIMVFO,TIMHFO
      SAVE TIMVFO,TIMHFO
C
      IF(IDB.NE.0) WRITE(6,601) IWES,IEAS,JSOU,JNOR,KBOT,KTOP,IFLAG
 601  FORMAT('CP_INTMLVAL IWES,IEAS,JSOU,JNOR,KBOT,KTOP,IFLAG=',7I5)
C
      IF(IPFLG.EQ.0) RETURN
C
      IF(IFLAG.EQ.1) THEN
C
        IF(IPFLG.LT.0) THEN
          TIMVB = TIMVFO
          DO 100 K=KBOT-1,KTOP+1
          DO 100 J=JSOU-1,JNOR+1
          DO 100 I=IWES-1,IEAS+1
            UB_ML(I,J,K) = UF_ML(I,J,K) 
            VB_ML(I,J,K) = VF_ML(I,J,K)
            WB_ML(I,J,K) = WF_ML(I,J,K)
            IF(LTEMP.EQ.1) TB_ML(I,J,K)=TF_ML(I,J,K)  
            IF(LCONC.EQ.1) CB_ML(I,J,K)=CF_ML(I,J,K)  
            UF_ML(I,J,K) = UU_ML(I,J,K) 
            VF_ML(I,J,K) = VV_ML(I,J,K)
            WF_ML(I,J,K) = WW_ML(I,J,K)
            IF(LTEMP.EQ.1) TF_ML(I,J,K)=TT_ML(I,J,K)  
            IF(LCONC.EQ.1) CF_ML(I,J,K)=CC_ML(I,J,K)  
  100     CONTINUE
          IPFLG = 1
        END IF
C
        W1 = (TIME-TIMVB)/(TIMVF-TIMVB)
        IF(TIMVB.GT.1.0D10) W1=1.0D0
        W2 = 1.0D0-W1
        DO 120 K=KBOT-1,KTOP+1
        DO 120 J=JSOU-1,JNOR+1
        DO 120 I=IWES-1,IEAS+1
          UU_ML(I,J,K) = W1*UF_ML(I,J,K)+W2*UB_ML(I,J,K) 
          VV_ML(I,J,K) = W1*VF_ML(I,J,K)+W2*VB_ML(I,J,K)
          WW_ML(I,J,K) = W1*WF_ML(I,J,K)+W2*WB_ML(I,J,K)  
          IF(LTEMP.EQ.1) TT_ML(I,J,K)=W1*TF_ML(I,J,K)+W2*TB_ML(I,J,K)
          IF(LCONC.EQ.1) CC_ML(I,J,K)=W1*CF_ML(I,J,K)+W2*CB_ML(I,J,K)
  120   CONTINUE
        TIMVFO = TIMVF
C
      ELSE IF(IFLAG.EQ.2) THEN
        IF(IPFLG.LT.0) THEN          
          TIMHB = TIMHFO
          DO 200 J=JSOU-1,JNOR+1
          DO 200 I=IWES-1,IEAS+1
            HB_ML(I,J) = HF_ML(I,J)
            HF_ML(I,J) = HH_ML(I,J)
  200     CONTINUE
          IPFLG = 1
        END IF
C
        W1 = (TIME-TIMHB)/(TIMHF-TIMHB)
        IF(TIMVB.GT.1.0D10) W1=1.0D0
        W2 = 1.0D0-W1
        DO 220 J=JSOU-1,JNOR+1
        DO 220 I=IWES-1,IEAS+1
          HH_ML(I,J) = W1*HF_ML(I,J)+W2*HB_ML(I,J)
          IF(KF_ML(I,J).GT.0.AND.KF_ML(I,J).LT.MZ_ML) THEN
            DO 230 K=2,MZ_ML-1
              IF(HH_ML(I,J).GE.ZC_ML(1,K-1)) THEN
                KF_ML(I,J) = K
                GO TO 220
              END IF
  230       CONTINUE
          END IF           
  220   CONTINUE
        TIMHFO = TIMHF
      ELSE
        CALL ERRMSG('CP_INTMLVAL',6250)
        WRITE(LP,*) '### PROGRAM ERROR ### IFLAG =',IFLAG
        CALL ABORT1('')
C
      END IF
C
      RETURN
      END
