      SUBROUTINE BOUNDS(HU,HV,FF,PP,RHOW,HH,HOLD,HDEP,PATM,XC,YC,ZC,
     $                  YCOSP,INDU,INDV,KF,KG)
C----------------------------------------------------------
C     透過境界処理のため境界部の水位をセットする
C----------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'BOUNDI.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'FILE.h'
ckt ---
      INCLUDE 'BOUNDR.h'
      INCLUDE 'TABLER.h'
ckt ---
C
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),FF(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HOLD(MX,MY),HDEP(MX,MY),
     $                       PATM(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ),YCOSP(MY)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY)
C
      REAL(8)::EPSV=1.0D-10
C
      REAL(8)::A1,A2,A3,A4,CC,DHH,DPP,DX,DY,DZ,GMV,HH0,HH1
      REAL(8)::PSS,SA,SB,UUU,VVV,XX,YY,ZZZ
ckt ---
      REAL(8)::depth,fw,fe,fs,fn,ztdmm,HH2,RGD,MN,HHOLD
      INTEGER::IH1,IH2,L,N
ckt ---
      INTEGER::I,II,IOP,J,JJ,K,KF0,J1
C
C ... 今村ら(2001)のモデルに、角の処理を追加する(=1)か否(=0)か
      INTEGER,PARAMETER:: ikado=0
C
C----------------------------------------------------------
C
C     4辺とも開境界でなければ抜ける
      IF (NSOMER(1).EQ.0 .AND. NSOMER(2).EQ.0 .AND.
     &    NSOMER(3).EQ.0 .AND. NSOMER(4).EQ.0) GOTO 9000
C
C     XZ方向2次元で2辺とも開境界でなければ抜ける
      IF (NSOMER(2).EQ.0 .AND. NSOMER(3).EQ.0 .AND. MYM.EQ.2) GOTO 9000
C
C     YZ方向2次元で2辺とも開境界でなければ抜ける
      IF (NSOMER(1).EQ.0 .AND. NSOMER(4).EQ.0 .AND. MXM.EQ.2) GOTO 9000
C
C     開境界処理
      DO 1000 J=2,MYM
      DO 1000 I=2,MXM
        IOP=0
        IF (NSOMER(1).GT.0 .AND. J.EQ.2  ) IOP=NSOMER(1)
        IF (NSOMER(2).GT.0 .AND. I.EQ.2  ) IOP=NSOMER(2)
        IF (NSOMER(3).GT.0 .AND. I.EQ.MXM) IOP=NSOMER(3)
        IF (NSOMER(4).GT.0 .AND. J.EQ.MYM) IOP=NSOMER(4)
C
C       開境界に接していて、適用範囲ならば、処理に入る
        IF (IOP.NE.0 .AND. KF(I,J).NE.MZ .AND. HDEP(I,J).LE.0.0D0) THEN
C
C         XZ方向2次元
          IF     (MYM.EQ.2) THEN
            IF (I.EQ.2) THEN
              II=I+1
            ELSE
              II=I-1
            ENDIF
            JJ=J
            UUU=1.0D0
            VVV=0.0D0
            ZZZ=1.0D0
            IF (HOLD(II,JJ)-HDEP(II,JJ).LE.GXB) II=I
C
C         YZ方向2次元
          ELSEIF (MXM.EQ.2) THEN
            IF (J.EQ.2) THEN
              JJ=J+1
            ELSE
              JJ=J-1
            ENDIF
            II=I
            UUU=0.0D0
            VVV=1.0D0
            ZZZ=1.0D0
            IF (HOLD(II,JJ)-HDEP(II,JJ).LE.GXB) JJ=J
C
C         3次元
          ELSE
C
C           流速ベクトルの方向
            ZZZ=HOLD(I,J)-HDEP(I,J)
            UUU=0.5D0*(HU(I,J,MZ)+HU(I-1,J,MZ))/MAX(ZZZ,1.0D-2)
            VVV=0.5D0*(HV(I,J,MZ)/YCOSP(J)+HV(I,J-1,MZ)/YCOSP(J-1))
     $         /MAX(ZZZ,1.0D-2)
            ZZZ=DSQRT(UUU**2+VVV**2)
C
C           辺の方向
            II=0
            JJ=0
            IF (J.EQ.2  ) JJ=JJ+1
            IF (I.EQ.2  ) II=II+1
            IF (I.EQ.MXM) II=II-1
            IF (J.EQ.MYM) JJ=JJ-1
C
C           流速ベクトルがゼロならば辺(角)に垂直とする
            IF     (ZZZ.LE.EPSV) THEN
              UUU=DBLE(II)
              VVV=DBLE(JJ)
C
C           角で方向が合わない場合も角に垂直とする
            ELSEIF (II*JJ.NE.0) THEN
              IF (II*JJ.GT.0 .AND. UUU*VVV.LT.0.0D0) THEN
                UUU=DBLE(II)
                VVV=DBLE(JJ)
              ENDIF
              IF (II*JJ.LT.0 .AND. UUU*VVV.GT.0.0D0) THEN
                UUU=DBLE(II)
                VVV=DBLE(JJ)
              ENDIF
C
C           辺の場合の方向を決める(X-)
            ELSEIF (II.GT.0) THEN
              IF     (UUU*VVV.GT.0.0D0) THEN
                JJ= 1
              ELSEIF (UUU*VVV.LT.0.0D0) THEN
                JJ=-1
              ENDIF
C
C           辺の場合の方向を決める(X+)
            ELSEIF (II.LT.0) THEN
              IF     (UUU*VVV.GT.0.0D0) THEN
                JJ=-1
              ELSEIF (UUU*VVV.LT.0.0D0) THEN
                JJ= 1
              ENDIF
C
C           辺の場合の方向を決める(Y-)
            ELSEIF (JJ.GT.0) THEN
              IF     (UUU*VVV.GT.0.0D0) THEN
                II= 1
              ELSEIF (UUU*VVV.LT.0.0D0) THEN
                II=-1
              ENDIF
C
C           辺の場合の方向を決める(Y+)
            ELSE
              IF     (UUU*VVV.GT.0.0D0) THEN
                II=-1
              ELSEIF (UUU*VVV.LT.0.0D0) THEN
                II= 1
              ENDIF
C
            ENDIF
C
C           見るべき方向の水位が決定出来ない場合
            IF (HOLD(I+II,J)-HDEP(I+II,J).LE.GXB) THEN
              II=0
              UUU=0.0D0
            ENDIF
            IF (HOLD(I,J+JJ)-HDEP(I,J+JJ).LE.GXB) THEN
              JJ=0
              VVV=0.0D0
            ENDIF
C
C           最終的な方向
            ZZZ=DSQRT(UUU**2+VVV**2)
            IF (ZZZ.LE.EPSV) THEN
              II=0
              JJ=0
              UUU=0.0D0
              VVV=0.0D0
              ZZZ=1.0D0
            ENDIF
            II=I+II
            JJ=J+JJ
          ENDIF
C
C         方向が決まらなければ何もしない
          IF (I.EQ.II .AND. J.EQ.JJ) THEN
C
C         方向が決まれば処理する
          ELSE
            DX=XC(3,I,J)
            IF (II.LT.I) DX=XC(3,II,J)
            DY=YC(3,J)
            IF (JJ.LT.J) DY=YC(3,JJ)
            CC=DSQRT(GRAV*HDEP(I,J))*DT
            XX=MIN(DABS(UUU/ZZZ)*CC,DX)
            YY=MIN(DABS(VVV/ZZZ)*CC,DY)
            A1=    XX *    YY
            A2=(DX-XX)*    YY
            A3=(DX-XX)*(DY-YY)
            A4=    XX *(DY-YY)
            SA=0.D0
            SB=0.D0
            IF (HOLD(II,JJ)-HDEP(II,JJ).GT.GXB) THEN
              PSS=PATM(II,JJ)/(RHOW(II,JJ,KF(II,JJ))*GRAV)
              SA=SA+A1
              SB=SB+A1*(HOLD(II,JJ)-PSS)
            ENDIF
            IF (HOLD(I ,JJ)-HDEP(I ,JJ).GT.GXB) THEN
              PSS=PATM(I ,JJ)/(RHOW(I ,JJ,KF(I ,JJ))*GRAV)
              SA=SA+A2
              SB=SB+A2*(HOLD(I ,JJ)-PSS)
            ENDIF
            IF (HOLD(I ,J )-HDEP(I ,J ).GT.GXB) THEN
              PSS=PATM(I ,J )/(RHOW(I ,J ,KF(I ,J ))*GRAV)
              SA=SA+A3
              SB=SB+A3*(HOLD(I ,J )-PSS)
            ENDIF
            IF (HOLD(II,J )-HDEP(II,J ).GT.GXB) THEN
              PSS=PATM(II,J )/(RHOW(II,J ,KF(II,J ))*GRAV)
              SA=SA+A4
              SB=SB+A4*(HOLD(II,J )-PSS)
            ENDIF
            HH0=HH(I,J)
            KF0=KF(I,J)
            PSS=PATM(I,J)/(RHOW(I,J,KF(I,J))*GRAV)
            HH(I,J)=SB/SA+PSS

ckt --- 今村ほか,2001(日野・仲座(1988)を正確化)
cmod 20121107
              if(IOP.eq.2) then
                 HH2 = HTIDE0
                 HHOLD=HOLD(I,J)-HH2
              endif
cmod 20121107
              if(IOP.eq.3) then
ckt ↓↓↓SUBROUTINE BCTIDEから引用↓↓↓
              DO 100 N=1,NOUTLT
                 IH1 = IOUTLT(3,N)
                 IF(IH1.EQ.0) GO TO 100
C
                 IF(IH1.GT.0) THEN
                    HH1 = TABLE(IH1)
                 ELSE
                    IH2 = -IH1
                    HH1 = HTIDE(1,IH2)
                    DO 105 L=1,NTIDE
                       HH1 = HH1
     $                +RTIDE(1,L,IH2)*COS(ROMEG(L)*TIME-RTIDE(2,L,IH2))
  105               CONTINUE
                    HTIDE(2,IH2) = HH1
                 END IF
                 HH2 = HH1
  100         CONTINUE
ckt ↑↑↑SUBROUTINE BCTIDEから引用↑↑↑
              HHOLD=HOLD(I,J)-HTIDE0
              endif
c
            if(IOP.ne.1) then
              ztdmm=0.0d0
              depth=HOLD(I,J)-HDEP(I,J)
              RGD=sqrt(abs(GRAV*depth))
              fw=HU(I-1,J,MZ)
              fe=HU(I  ,J,MZ)
              fs=HV(I,J-1,MZ)/YCOSP(J-1)
              fn=HV(I,J  ,MZ)/YCOSP(J  )
c
              if(I.eq.  2.and.NSOMER(2).GT.0) then
                ztdmm=HHOLD-fe/RGD+(fs-fn)*YC(5,J)*DT
              else if(I.eq.MXM.and.NSOMER(3).GT.0) then
                ztdmm=HHOLD+fw/RGD+(fs-fn)*YC(5,J)*DT
              endif
c
              if(J.eq.  2.and.NSOMER(1).GT.0) then
                ztdmm=HHOLD-fn/RGD+(fw-fe)*XC(5,I,J)*DT
              else if(J.eq.MYM.and.NSOMER(4).GT.0) then
                ztdmm=HHOLD+fs/RGD+(fw-fe)*XC(5,I,J)*DT
              end if
c
c ........... 角の処理
              if( ikado.eq.1 ) then
              if( i.eq.2.and.j.eq.2.and.
     $            NSOMER(1).GT.0.and.NSOMER(2).GT.0 ) then
                 if( fe.lt.0.0d0.and.fn.lt.0.0d0 ) then
                    ztdmm=HHOLD+SQRT(fe**2+fn**2)/RGD
                 elseif( fe.gt.0.0d0.and.fn.gt.0.0d0 ) then
                    ztdmm=HHOLD-SQRT(fe**2+fn**2)/RGD
                 else
                    ztdmm=2.0d0*HHOLD
                 endif
c
              elseif( i.eq.MXM.and.j.eq.2.and.
     $            NSOMER(3).GT.0.and.NSOMER(1).GT.0 ) then
                 if( fw.gt.0.0d0.and.fn.lt.0.0d0 ) then
                    ztdmm=HHOLD+SQRT(fw**2+fn**2)/RGD
                 elseif( fw.lt.0.0d0.and.fn.gt.0.0d0 ) then
                    ztdmm=HHOLD-SQRT(fw**2+fn**2)/RGD
                 else
                    ztdmm=2.0d0*HHOLD
                 endif
c
              elseif( i.eq.2.and.j.eq.MYM.and.
     $                NSOMER(2).GT.0.and.NSOMER(4).GT.0 ) then
                 if( fe.lt.0.0d0.and.fs.gt.0.0d0 ) then
                    ztdmm=HHOLD+SQRT(fe**2+fs**2)/RGD
                 elseif( fe.gt.0.0d0.and.fs.lt.0.0d0 ) then
                    ztdmm=HHOLD-SQRT(fe**2+fs**2)/RGD
                 else
                    ztdmm=2.0d0*HHOLD
                 endif
c
              elseif( i.eq.MXM.and.j.eq.MYM.and.
     $                NSOMER(3).GT.0.and.NSOMER(4).GT.0) then
                 if( fw.gt.0.0d0.and.fs.gt.0.0d0 ) then
                    ztdmm=HHOLD+SQRT(fw**2+fs**2)/RGD
                 elseif( fw.lt.0.0d0.and.fs.lt.0.0d0 ) then
                    ztdmm=HHOLD-SQRT(fw**2+fs**2)/RGD
                 else
                    ztdmm=2.0d0*HHOLD
                 endif
              endif
              endif
c
cmod 20121107
c              if(IOP.eq.2) HH(i,j)=.5d0*ztdmm
c              if(IOP.eq.3) HH(i,j)=.5d0*ztdmm+HH0
              HH(i,j)=.5d0*ztdmm+HH2
cmod 20121107
            end if
ckt ---

            IF (HH(I,J).LE.HDEP(I,J)) HH(I,J)=HDEP(I,J)+EPSH
            HH1=HH(I,J)
            DO 200 K=KG(I,J),MZM
              IF     (ZC(1,K  ).LT.HH1) THEN
                FF(I,J,K)=1.0D0
              ELSEIF (ZC(1,K-1).GT.HH1) THEN
                FF(I,J,K)=0.0D0
              ELSE
                FF(I,J,K)=(HH1-ZC(1,K-1))*ZC(6,K)
                KF(I,J)=K
              ENDIF
 200        CONTINUE
            DHH=HH1-HH0
            DPP=-DHH*RHOW(I,J,KF0)*GRAV
            DO 300 K=KG(I,J),MZM
              IF (K.LE.KF(I,J) .OR. PP(I,J,K).NE.0.0D0) THEN
                PP(I,J,K) = PP(I,J,K)+DPP
              ENDIF
 300        CONTINUE
          ENDIF
        ENDIF
 1000 CONTINUE
C
C----------------------------------------------------------
 9000 CONTINUE
      RETURN
      END
