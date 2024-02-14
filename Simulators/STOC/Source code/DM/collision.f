      SUBROUTINE COLLISION
C----------------------------------------
C     漂流物の衝突計算を行う
C----------------------------------------
      USE M_TIME
      USE M_GRID
      USE M_DRIFT
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
      USE M_GEOM
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
      USE M_OUTPUT,ONLY:IFL
      USE M_FENCE,ONLY:NFC,LFC,ZFC,EFC_LIMIT,XFCD,YFCD,
     $                    XFC1,XFC2,YFC1,YFC2
      USE M_SUBGRID,ONLY: ALLOCATE_SUBGRID,SET_DMIN_SUBGRID,
     $                    ND_SUB,I1_SUB,IPTR_SUB,I_SUB,J_SUB,
     $                    NX_SUB,NY_SUB
C
      IMPLICIT NONE
C
cs 2014/03/25 honda
c      REAL(8),PARAMETER::EPS=0.01D0    ! 衝突時に引き離す距離
      REAL(8)::EPS    ! 衝突時に引き離す距離
ce 2014/03/25 honda
      REAL(8),PARAMETER::EPSZ=0.0001D0 ! 衝突時に引き離す距離(上下)
C
      INTEGER::LLD(ND)             ! 地形との衝突状態格納用ワーク配列
C                                  ! =0:非衝突 =1:地形と衝突
      INTEGER::LLD2(ND)            ! 漂流物同士の衝突状態格納用ワーク配列
C                                  ! =0:非衝突 =1:漂流物同士で衝突
CMOD      REAL(8)::HF0,HT0
      REAL(8)::X,Y,XE,YE,XX,YY,P,Q,DLTX,DLTY
      REAL(8)::RLEN,COEF,WO,WP,WQ,FT
      REAL(8)::XA,YA,XB,YB,RA,RB,GAMMA,EA,EB
      REAL(8)::COEFA,COEFB,DLTXA,DLTYA,DLTXB,DLTYB
      REAL(8)::XEA,YEA,XEB,YEB,XXA,YYA,XXB,YYB,CA,CB
      REAL(8)::DZ1
      INTEGER::M,N,I,J,K,II,JJ,KK,IERR,IRTN
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
      REAL(8)::ENRG1,ENRG2,EPSENRG
      REAL(8),PARAMETER::DSTELMT=8.33D+2
      INTEGER::IWKDST,JWKDST,NSTP,MSTP
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
      INTEGER,SAVE:: ISUB_INI=0
C
C
cs 2014/03/25 honda
      EPS = 99999.9D0
      DO N=1,ND
        EPS = MIN(EPS,0.5D0*BB(N))
      ENDDO
      EPS = 0.01D0 * EPS
ce 2014/03/25 honda
      DO N=1,ND
         IF( LD(N).EQ.1 ) THEN
            LLD(N) = 0
         ELSE
            LLD(N) = 1
         ENDIF
      ENDDO
C
      LLD2 = 0
C
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
      DO N=1,ND
         DO NSTP=MXNSTP,1,-1
            IF( NSTP.NE.1 )THEN
               LBL1(N,NSTP) = LBL1(N,NSTP-1)
               LBL2(N,NSTP) = LBL2(N,NSTP-1)
            ELSE
               LBL1(N,NSTP) = 0
               LBL2(N,NSTP) = 0
            ENDIF
         ENDDO
      ENDDO
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
C
C----------------------------------------
C     (A) 地形・建物側面との衝突
C----------------------------------------
C----------------------------------------
C     (A.1) 重心位置における水位と標高を設定
C----------------------------------------
CMOD      DO N=1,ND
CMOD         IF( LLD(N).NE.0 ) CYCLE
CMODC
CMOD         CALL INTERP_H(HF0,HT0,XD1(N),YD1(N))
CMOD         HFD(N) = HF0
CMOD         HTD(N) = HT0
CMOD         HZD(N) = MAX(HF0-BD(N),HT0)
CMOD         HAD(N) = MAX(HF0-AD(N),HT0)
CMODC
CMOD         CALL SETKD(HFD(N),KFD(N))   ! HFDの高さを含む流体セルのインデックスKを定める
CMOD         CALL SETKD(HAD(N),KAD(N))   ! HADの高さを含む流体セルのインデックスKを定める
CMOD      ENDDO
C
C----------------------------------------
C     (A.2) 地形・建物側面との衝突
C----------------------------------------
      DO N=1,ND
         IF( LLD(N).NE.0 ) CYCLE
C
C ...... 漂流物と地形・建物側面との衝突判定
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
c         CALL COLLIS1(X,Y,P,Q,RLEN,LLD(N),HZD(N),N,IERR)
         CALL COLLIS1(X,Y,P,Q,RLEN,LLD(N),HZD(N),N,IWKDST,JWKDST,IERR)
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C
         IF( IERR.EQ.-2 .AND. INAR(N).EQ.1 ) THEN
            LD(N) = -2
            LLD(N) = 1
c            WRITE(*,*) '   SHIP NO.=',N
            CYCLE
         ENDIF
C
         IF( LLD(N).EQ.1 ) THEN
CDEBUG                  write(ifl,*) 'collision,chikei,t=',time
c            WRITE(*,*) 'COLLISION OCCURED ND=',N
            COEF = (0.5D0*BB(N)+EPS)/RLEN-1.0D0
            DLTX = COEF*P
            DLTY = COEF*Q
c            WRITE(*,*) '     DELTA-X,DELTA-Y=',DLTX,DLTY
C
            XD1(N) = XD1(N) + DLTX           ! 位置補正
            YD1(N) = YD1(N) + DLTY
C
            XE = X + EPS*P/RLEN
            YE = Y + EPS*Q/RLEN
C
            XX = XE - XD1(N)
            YY = YE - YD1(N)
            WO = RLEN**2/AM(N) + (XX*Q-YY*P)**2/AI(N)
            WP = P*(UD1(N)-OD1(N)*YY)
            WQ = Q*(VD1(N)+OD1(N)*XX)
C
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
            ENRG1 = 0.5 * AM(N) * (UD1(N)*UD1(N) + VD1(N)*VD1(N))
     &            + 0.5 * AI(N) * (OD1(N)*OD1(N))
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C
            FT = -(WP+WQ)/WO                ! 運動量の更新
            UD1(N) = UD1(N) + P/AM(N)*FT
            VD1(N) = VD1(N) + Q/AM(N)*FT
            OD1(N) = OD1(N) + (XX*Q-YY*P)/AI(N)*FT
C
C ......... VERTEXで定めた座標の補正
            CALL MODVERT(DLTX,DLTY,N)
C
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
            ENRG2 = 0.5 * AM(N) * (UD1(N)*UD1(N) + VD1(N)*VD1(N))
     &            + 0.5 * AI(N) * (OD1(N)*OD1(N))
            EPSENRG = ABS(ENRG1 - ENRG2)
C
            IF( EPSENRG.GT.DSTELMT .AND.
     &          IDST(INAR(N),IWKDST,JWKDST).GT.0 )THEN
               IDST(INAR(N),IWKDST,JWKDST) = -92
            ENDIF
            IF(IFLAG_D(N).EQ.0.AND.EPSENRG.GT.DST_LIMIT(N)) IFLAG_D(N)=1
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C
         ENDIF
      ENDDO
C
C
Cadd 20160304(subgrid)s
      IF(ISUB_INI==0) THEN
         CALL SET_DMIN_SUBGRID(BL,BB,XFC1,XFC2,YFC1,YFC2,ND,NFC)
         ISUB_INI=1
      ENDIF
      CALL ALLOCATE_SUBGRID(XD1,YD1,XFCD,YFCD,ND,NFC,IERR)
Cadd 20160304(subgrid)e
C----------------------------------------
C     (A.3) 漂流物とフェンスの衝突
C----------------------------------------
      IF( NFC>0 )THEN
      DO N=1,ND
         I=I_SUB(N)
         J=J_SUB(N)
         DO JJ=MAX(J-1,1),MIN(J+1,NY_SUB)
         DO II=MAX(I-1,1),MIN(I+1,NX_SUB)
           K=I1_SUB(II,JJ)
         DO KK=K,K+ND_SUB(II,JJ)-1
          IF( IPTR_SUB(KK)>0 ) CYCLE
          M=-IPTR_SUB(KK)
c         DO M=1,NFC
            IF( LLD(N).NE.0.OR.LFC(M).NE.0 ) CYCLE
C
C ......... 上下位置を比較して、ぶつからないときは衝突させない
            IF( HZD(N).GE.ZFC(M) ) THEN
               IRTN=0
            ELSE
C
C ......... 漂流物とフェンスとの衝突判定(N:AとM:B)
               CALL COLLIS3(X,Y,P,Q,RLEN,N,M,IRTN)
               IF( IRTN.EQ.1 .AND. HZDO(N).GE.ZFC(M) ) THEN
C                 上下面がぶつかったものとみなして、水平方向の接触モデルは適用しない
                  DZ1=ZFC(M)+EPSZ-HZD(N)
                  HZD(N)=HZD(N)+DZ1
                  HAZ(N)=HAZ(N)+DZ1
                  HAD(N)=HAD(N)+DZ1
                  WSD(N)=0.0D0
                  IRTN=0
               ENDIF
            ENDIF
C
            IF( IRTN.EQ.1 ) THEN
               IF( LLD(N).EQ.0 ) LLD2(N) = 1
c               WRITE(*,*) 'COLLISION OCCURED ND,MFC=',N,M,' TIME=',TIME
C
               COEF = (0.5D0*BB(N)+EPS)/RLEN-1.0D0
C
               DLTX = COEF*P
               DLTY = COEF*Q
C
               XD1(N) = XD1(N) + DLTX          ! 位置補正
               YD1(N) = YD1(N) + DLTY
C
               XE = X + EPS*P/RLEN
               YE = Y + EPS*Q/RLEN
C
               XX = XE - XD1(N)
               YY = YE - YD1(N)
C
               WO = RLEN**2/AM(N) + (XX*Q-YY*P)**2/AI(N)
               WP = P*(UD1(N)-OD1(N)*YY)
               WQ = Q*(VD1(N)+OD1(N)*XX)
C
               IF( IFLAG_D(N).EQ.0.OR.LFC(M).EQ.0 ) THEN
                  ENRG1 = 0.5 * AM(N) * (UD1(N)*UD1(N) + VD1(N)*VD1(N))
     $                  + 0.5 * AI(N) * (OD1(N)*OD1(N))
               ENDIF
C
               FT = -(WP+WQ)/WO                ! 運動量の更新
               UD1(N) = UD1(N) + P/AM(N)*FT
               VD1(N) = VD1(N) + Q/AM(N)*FT
               OD1(N) = OD1(N) + (XX*Q-YY*P)/AI(N)*FT
C
               IF( IFLAG_D(N).EQ.0.OR.LFC(M).EQ.0 ) THEN
                  ENRG2 = 0.5 * AM(N) * (UD1(N)*UD1(N) + VD1(N)*VD1(N))
     $                  + 0.5 * AI(N) * (OD1(N)*OD1(N))
                  EPSENRG = ABS(ENRG1 - ENRG2)
                  IF( IFLAG_D(N).EQ.0.AND.EPSENRG.GT.DST_LIMIT(N) )
     $               IFLAG_D(N)=1
                  IF( LFC(M).EQ.0.AND.EPSENRG.GT.EFC_LIMIT(M) ) THEN
                     LFC(M)=1
                     WRITE(IFL,*) 'FENCE IS DESTROYED BY DRIFT',
     $                            ' OBSTACLE. NO=',M
                  ENDIF
               ENDIF
C
C ............ VERTEXで定めた座標の補正
               IF( LLD(N).EQ.0 ) CALL MODVERT(DLTX,DLTY,N)
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDDO
      ENDIF
C
C
C----------------------------------------
C     (B) 漂流物同士の衝突
C----------------------------------------
      DO N=1,ND
         I=I_SUB(N)
         J=J_SUB(N)
         DO JJ=MAX(J-1,1),MIN(J+1,NY_SUB)
         DO II=MAX(I-1,1),MIN(I+1,NX_SUB)
           K=I1_SUB(II,JJ)
         DO KK=K,K+ND_SUB(II,JJ)-1
          IF( IPTR_SUB(KK)<=N ) CYCLE
          M=IPTR_SUB(KK)
C         DO M=N+1,ND
            IF( LLD(N).NE.0.AND.LLD(M).NE.0 ) CYCLE
C
C ......... 上下位置を比較して、ぶつからないときは衝突させない
            IF( HZD(M).GE.HAZ(N) .OR. HZD(N).GE.HAZ(M) ) THEN
               IRTN=0
            ELSE
C
C ......... 漂流物同士の衝突判定(N:AとM:B)
               CALL COLLIS2(XA,YA,XB,YB,P,Q,RLEN,N,M,IRTN)
               IF( IRTN.EQ.1 .AND.
     $             ( HZDO(M).GE.HAZO(N) .OR. HZDO(N).GE.HAZO(M) ) ) THEN
C                 上下面がぶつかったものとみなして、水平方向の接触モデルは適用しない
CDEBUG                  write(ifl,*) 'collision,t=',time
                  IF( HZDO(M).GE.HAZO(N) ) THEN
                     DZ1=HAZ(N)+EPSZ-HZD(M)
                     HZD(M)=HZD(M)+DZ1
                     HAZ(M)=HAZ(M)+DZ1
                     HAD(M)=HAD(M)+DZ1
                     WSD(M)=0.0D0
CDEBUG                     IF( IFLAG_S(M).GE.1.OR.IFLAG_D(M).GE.1 ) THEN
CDEBUG                        AD(M)=MAX(AD(M)-DZ1,AD0(M))
CDEBUG                        BD(M)=MAX(BD(M)-DZ1,BD0(M))
CDEBUG                     ENDIF
                  ELSE
                     DZ1=HAZ(M)+EPSZ-HZD(N)
                     HZD(N)=HZD(N)+DZ1
                     HAZ(N)=HAZ(N)+DZ1
                     HAD(N)=HAD(N)+DZ1
                     WSD(N)=0.0D0
CDEBUG                     IF( IFLAG_S(N).GE.1.OR.IFLAG_D(N).GE.1 ) THEN
CDEBUG                        AD(N)=MAX(AD(N)-DZ1,AD0(N))
CDEBUG                        BD(N)=MAX(BD(N)-DZ1,BD0(N))
CDEBUG                     ENDIF
                  ENDIF
                  IRTN=0
               ENDIF
            ENDIF
C
            IF( IRTN.EQ.1 ) THEN
               IF( LLD(N).EQ.0 ) LLD2(N) = 1
               IF( LLD(M).EQ.0 ) LLD2(M) = 1
c               WRITE(*,*) 'COLLISION OCCURED ND,MD=',N,M
               RA = 0.5D0*BB(N)
               RB = 0.5D0*BB(M)
C
               IF( LLD(N).EQ.0.AND.LLD(M).EQ.0 ) THEN
                  GAMMA = RLEN/(RA+RB)
                  X = XB + GAMMA*RB/RLEN*P
                  Y = YB + GAMMA*RB/RLEN*Q
                  EA = 0.5D0*EPS
                  EB = 0.5D0*EPS
                  CA = 1.0D0
                  CB = 1.0D0
               ELSEIF( LLD(N).NE.0 ) THEN
                  X = XB + (RLEN-RA)/RLEN*P
                  Y = YB + (RLEN-RA)/RLEN*Q
                  EA = 0.0D0
                  EB = EPS
                  CA = 0.0D0
                  CB = 1.0D0
               ELSEIF( LLD(M).NE.0 ) THEN
                  X = XB + RB/RLEN*P
                  Y = YB + RB/RLEN*Q
                  EA = EPS
                  EB = 0.0D0
                  CA = 1.0D0
                  CB = 0.0D0
               ELSE
                  CALL ERRMSG('COLLISION',-1)
               ENDIF
C
               COEFA = (RA+EA-SQRT((X-XA)**2+(Y-YA)**2))/RLEN
               COEFB = -(RB+EB-SQRT((X-XB)**2+(Y-YB)**2))/RLEN
C
               DLTXA = COEFA*P
               DLTYA = COEFA*Q
               DLTXB = COEFB*P
               DLTYB = COEFB*Q
C
c               WRITE(*,*) '     DELTA-XA,DELTA-YA=',DLTXA,DLTYA
c               WRITE(*,*) '     DELTA-XB,DELTA-YB=',DLTXB,DLTYB
C
               XD1(N) = XD1(N) + DLTXA          ! 位置補正
               YD1(N) = YD1(N) + DLTYA
               XD1(M) = XD1(M) + DLTXB
               YD1(M) = YD1(M) + DLTYB
C
               XEA = X + EA*P/RLEN
               YEA = Y + EA*Q/RLEN
               XEB = X - EB*P/RLEN
               YEB = Y - EB*Q/RLEN
C
               XXA = XEA - XD1(N)
               YYA = YEA - YD1(N)
               XXB = XEB - XD1(M)
               YYB = YEB - YD1(M)
C
               WO = (CA/AM(N)+CB/AM(M))*RLEN**2
     $            + (XXA*Q-YYA*P)**2/AI(N)*CA
     $            + (XXB*Q-YYB*P)**2/AI(M)*CB
               WP = P*(UD1(N)-OD1(N)*YYA-UD1(M)+OD1(M)*YYB)
               WQ = Q*(VD1(N)+OD1(N)*XXA-VD1(M)-OD1(M)*XXB)
C
               IF( IFLAG_D(N).EQ.0.OR.IFLAG_D(M).EQ.0 ) THEN
                  ENRG1 = 0.5 * AM(N) * (UD1(N)*UD1(N) + VD1(N)*VD1(N))
     $                  + 0.5 * AI(N) * (OD1(N)*OD1(N))
     $                  + 0.5 * AM(M) * (UD1(M)*UD1(M) + VD1(M)*VD1(M))
     $                  + 0.5 * AI(M) * (OD1(M)*OD1(M))
               ENDIF
C
               FT = -(WP+WQ)/WO                ! 運動量の更新
               UD1(N) = UD1(N) + CA*P/AM(N)*FT
               VD1(N) = VD1(N) + CA*Q/AM(N)*FT
               OD1(N) = OD1(N) + CA*(XXA*Q-YYA*P)/AI(N)*FT
               UD1(M) = UD1(M) - CB*P/AM(M)*FT
               VD1(M) = VD1(M) - CB*Q/AM(M)*FT
               OD1(M) = OD1(M) - CB*(XXB*Q-YYB*P)/AI(M)*FT
C
               IF( IFLAG_D(N).EQ.0.OR.IFLAG_D(M).EQ.0 ) THEN
                  ENRG2 = 0.5 * AM(N) * (UD1(N)*UD1(N) + VD1(N)*VD1(N))
     $                  + 0.5 * AI(N) * (OD1(N)*OD1(N))
     $                  + 0.5 * AM(M) * (UD1(M)*UD1(M) + VD1(M)*VD1(M))
     $                  + 0.5 * AI(M) * (OD1(M)*OD1(M))
                  EPSENRG = ABS(ENRG1 - ENRG2)
                  IF( IFLAG_D(N).EQ.0.AND.EPSENRG.GT.DST_LIMIT(N) )
     $               IFLAG_D(N)=1
                  IF( IFLAG_D(M).EQ.0.AND.EPSENRG.GT.DST_LIMIT(M) )
     $               IFLAG_D(M)=1
               ENDIF
C
C ............ VERTEXで定めた座標の補正
               IF( LLD(N).EQ.0 ) CALL MODVERT(DLTXA,DLTYA,N)
               IF( LLD(M).EQ.0 ) CALL MODVERT(DLTXB,DLTYB,M)
            ENDIF
         ENDDO
         ENDDO
         ENDDO
      ENDDO
C
C
C----------------------------------------
C     (C) 地形・建物側面との衝突(再判定)
C         地形と衝突しておらず、漂流物同士で衝突したもののみ
C----------------------------------------
C----------------------------------------
C     (C.1) 重心位置における水位と標高を設定
C----------------------------------------
CMOD      DO N=1,ND
CMOD         IF( LLD(N).NE.0.OR.LLD2(N).NE.1 ) CYCLE
CMODC
CMOD         CALL INTERP_H(HF0,HT0,XD1(N),YD1(N))
CMOD         HFD(N) = HF0
CMOD         HTD(N) = HT0
CMOD         HZD(N) = MAX(HF0-BD(N),HT0)
CMOD         HAD(N) = MAX(HF0-AD(N),HT0)
CMODC
CMOD         CALL SETKD(HFD(N),KFD(N))   ! HFDの高さを含む流体セルのインデックスKを定める
CMOD         CALL SETKD(HAD(N),KAD(N))   ! HADの高さを含む流体セルのインデックスKを定める
CMOD      ENDDO
C
C----------------------------------------
C     (C.2) 地形・建物側面との衝突
C----------------------------------------
      DO N=1,ND
         IF( LLD(N).NE.0.OR.LLD2(N).NE.1 ) CYCLE
C
C ...... 漂流物と地形・建物側面との衝突判定
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
c         CALL COLLIS1(X,Y,P,Q,RLEN,LLD(N),HZD(N),N,IERR)
         CALL COLLIS1(X,Y,P,Q,RLEN,LLD(N),HZD(N),N,IWKDST,JWKDST,IERR)
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C
         IF( IERR.EQ.-2 .AND. INAR(N).EQ.1 ) THEN
            LD(N) = -2
c            WRITE(*,*) '   SHIP NO.=',N
            CYCLE
         ENDIF
C
         IF( LLD(N).EQ.1 ) THEN
c            WRITE(*,*) 'COLLISION OCCURED ND=',N
            COEF = (0.5D0*BB(N)+EPS)/RLEN-1.0D0
            DLTX = COEF*P
            DLTY = COEF*Q
c            WRITE(*,*) '     DELTA-X,DELTA-Y=',DLTX,DLTY
C
            XD1(N) = XD1(N) + DLTX           ! 位置補正
            YD1(N) = YD1(N) + DLTY
C
            XE = X + EPS*P/RLEN
            YE = Y + EPS*Q/RLEN
C
            XX = XE - XD1(N)
            YY = YE - YD1(N)
            WO = RLEN**2/AM(N) + (XX*Q-YY*P)**2/AI(N)
            WP = P*(UD1(N)-OD1(N)*YY)
            WQ = Q*(VD1(N)+OD1(N)*XX)
C
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
            ENRG1 = 0.5 * AM(N) * (UD1(N)*UD1(N) + VD1(N)*VD1(N))
     &            + 0.5 * AI(N) * (OD1(N)*OD1(N))
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C
            FT = -(WP+WQ)/WO                ! 運動量の更新
            UD1(N) = UD1(N) + P/AM(N)*FT
            VD1(N) = VD1(N) + Q/AM(N)*FT
            OD1(N) = OD1(N) + (XX*Q-YY*P)/AI(N)*FT
C
C ......... VERTEXで定めた座標の補正
            CALL MODVERT(DLTX,DLTY,N)
C
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
            ENRG2 = 0.5 * AM(N) * (UD1(N)*UD1(N) + VD1(N)*VD1(N))
     &            + 0.5 * AI(N) * (OD1(N)*OD1(N))
            EPSENRG = ABS(ENRG1 - ENRG2)
C
            IF( EPSENRG.GT.DSTELMT .AND.
     &          IDST(INAR(N),IWKDST,JWKDST).GT.0 )THEN
               IDST(INAR(N),IWKDST,JWKDST) = -92
            ENDIF
            IF(IFLAG_D(N).EQ.0.AND.EPSENRG.GT.DST_LIMIT(N)) IFLAG_D(N)=1
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C
         ENDIF
      ENDDO
C
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
      DO N=1,ND
         IF( LBLC(N).EQ.1 ) CYCLE
         LBLC(N) = 0
         DO NSTP=1,MXNSTP/2
            DO MSTP=NSTP+1,NSTP+MXNSTP/2
               IF( LBL1(N,NSTP)*LBL1(N,MSTP).GE.0 ) CYCLE
               IF( LBL2(N,NSTP)*LBL2(N,MSTP).LE.0 ) CYCLE
               LBLC(N) = 2
            ENDDO
            IF( LBLC(N).EQ.2 ) EXIT
         ENDDO
      ENDDO
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
C
      RETURN
      END
