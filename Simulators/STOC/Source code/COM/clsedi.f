      SUBROUTINE CLSEDI(CSEDI,CSEDIN,CSDAVE,WEXSD,EXSDE,EXSDD,ZBED,
     $                  HU,HV,HW,TMUX,TMUY,TMUZ,
     $                  XC,YC,ZC,XCP,YCOS,YCOSP,GV,GX,GY,GZ,
     $                  HH,HX,HDEP,INDP,INDU,INDV,INDW,LLWALL,LLWALP,
     $                  KF,KH,KG,KP,CSDBCN,FU,FV,FW,SRCA,SRCB)
C======================================================================
C     浮遊砂濃度を計算する
C       FU: X方向流束,FV: Y方向流束,FW: Z方向流束
C       CC: 新しい時刻の濃度
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'MYCNST.h'
      INCLUDE 'SEDIMENT.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(INOUT)::CSEDI(MX,MY,MZ),CSEDIN(MX,MY,MZ)
      REAL(8),INTENT(OUT)  ::CSDAVE(MX,MY)
      REAL(8),INTENT(INOUT)::WEXSD(MX,MY),EXSDE(MX,MY),EXSDD(MX,MY)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY),HH(MX,MY),HX(MX,MY)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUX(MX,MY,MZ),TMUY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::XCP(8,MX,MY),YCOS(MY),YCOSP(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KH(MX,MY),KG(MX,MY),KP(MX,MY)
      REAL(8),INTENT(INOUT)::CSDBCN(NXY,MZ,4)
      REAL(8),INTENT(IN)   ::ZBED(MX,MY)
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ),FV(MX,MY,MZ),FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMUZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::SRCA(MX,MY,MZ),SRCB(MX,MY,MZ)
C
C ... ローカル変数
      REAL(8)::DH(MX,MY,MZ),HWWS(MX,MY,MZ)
      INTEGER::I,J,K,KK,LL
      INTEGER::NN=-1,IZCAL=0
      REAL(8)::SUMSD,EXSD,CSDDH
      REAL(8)::SDAMNT(MX,MY,MZ) !各セルの実質的な砂量[m]
                                !(k=MZに鉛直合計を格納しておく)
      REAL(8)::SDCAPA(MX,MY,MZ) !各セルの実質的な残り砂容量[m]
                                !(k=MZに鉛直合計を格納しておく)
      REAL(8)::WEXCS(2)         !交換砂量[m/s]の制限範囲(1:下限,2:上限)
      REAL(8)::EPSHSD           !水深ゼロ(極小)で浮遊砂濃度をゼロクリア
      INTEGER::IEPSHSD(MX,MY)
      REAL(8)::CSMIN,CSMAX
      INTEGER::NCSMIN,NCSMAX,ICSMIN,ICSMAX,JCSMIN,JCSMAX,KCSMIN,KCSMAX
C
C
      EPSHSD=EPSH*1.0D1
C
C      
      CALL CELLSC(CSEDI,CSEDIN,GV,XC,YC,ZC,HX,HDEP,INDP,KH,KG)
      CALL CHKCNS(CSEDIN,SUMSD)
C
      CALL FLUXSX(FU,CSEDI,HU,TMUX,GX,HDEP,HX,XC,ZC,INDP,INDU,LLWALL,
     $            KG,KP,KH,CSDBCN,NN,DIFHSD,SCTHSD,PARAMSD)
C
      CALL FLUXSY(FV,CSEDI,HV,TMUY,GY,HDEP,HX,YC,ZC,INDP,INDV,LLWALL,
     $            KG,KP,KH,CSDBCN,NN,DIFHSD,SCTHSD,PARAMSD)
C
      DO 100 K=1,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF(INDW(I,J,K).GE.-1 .AND. K.LT.KF(I,J) .AND. K.GE.KG(I,J))THEN
            HWWS(I,J,K)=HW(I,J,K)-WSEDI
         ELSE
            HWWS(I,J,K)=0.0D0
         ENDIF
  100 CONTINUE
C
      CALL FLUXSZ(FW,CSEDI,HWWS,TMUZ,GZ,ZC,INDP,INDW,LLWALL,
     $            KG,KP,KH,NN,DIFVSD,SCTVSD,PARAMSD,IZCAL)
C
C ... 板境界の流束を修正する
      LL=0
      CALL FLUXPL(FU,FV,FW,LLWALP,LL)
C
C ... 水深が浅い(制限以下)の場合はフラックスをゼロとする
      IEPSHSD=0
      DO 200 K=2,MZM
      DO 200 J=2,MYM
      DO 200 I=2,MXM
         IF(INDP(I,J,K).EQ.0 .OR. K.GT.KF(I,J)) THEN
            DH(I,J,K)=0.0D0
         ELSE IF(K.NE.KF(I,J))THEN
            DH(I,J,K)=ZC(4,K)*GV(I,J,K)
         ELSEIF(KF(I,J).NE.KG(I,J))THEN
            DH(I,J,K)=HH(I,J)-ZC(1,K-1)
         ELSE
            DH(I,J,K)=HH(I,J)-HDEP(I,J)
            IF(DH(I,J,K).LT.EPSHSD) IEPSHSD(I,J)=1
         ENDIF
C
         IF(DH(I,J,K).LT.ZLIMSD)THEN
            FU(I-1,J,K)=0.0D0
            FU(I  ,J,K)=0.0D0
            FV(I,J-1,K)=0.0D0
            FV(I,J  ,K)=0.0D0
            FW(I,J,K-1)=0.0D0
            FW(I,J,K  )=0.0D0
         ENDIF
  200 CONTINUE
C
C ... まず移流拡散のみを考慮して新しい時刻の浮遊砂濃度を計算する
      CALL ZERCLR(SRCA,MXYZ,0.0D0)
      CALL ZERCLR(SRCB,MXYZ,0.0D0)
      CALL CLSNEW(CSEDI,CSEDIN,FU,FV,FW,SRCA,SRCB,TMUZ,HH,
     $            XC,YC,ZC,XCP,YCOS,GV,GZ,
     $            INDP,INDU,INDV,KG,KP,KF,DIFVSD,SCTVSD,0)
CCCCCCC      CALL CHKVAL(CSEDI,HH,HDEP,KF,0.0D0)
C
C
C ... 交換砂量を加味した浮遊砂濃度および平均濃度の算出
      SDAMNT=0.0D0
      SDCAPA=0.0D0
      NCSMIN=0
      NCSMAX=0
      DO 300 J=2,MYM
      DO 300 I=2,MXM
C
C ...... 負浮遊砂濃度のゼロクリア／制限越浮遊砂濃度のカットオフ
         DO 310 K=2,MZM
            IF(INDP(I,J,K).EQ.0 .OR. K.GT.KF(I,J)) CYCLE
            IF(CSEDI(I,J,K).LT.0.0D0)THEN
               IF(CSEDI(I,J,K).LT.-CMAXSD*0.01D0
     $            .AND. DH(I,J,K).GT.ZLIMSD)THEN
                  NCSMIN=NCSMIN+1
                  IF(NCSMIN.EQ.1 .OR. CSEDI(I,J,K).LT.CSMIN)THEN
                     CSMIN =CSEDI(I,J,K)
                     ICSMIN=I
                     JCSMIN=J
                     KCSMIN=K
                  ENDIF
               ENDIF
               CSEDI(I,J,K)=0.0D0
            ELSEIF(CSEDI(I,J,K).GT.CMAXSD)THEN
               IF(CSEDI(I,J,K).GT.CMAXSD*1.01D0
     $            .AND. DH(I,J,K).GT.ZLIMSD)THEN
                  NCSMAX=NCSMAX+1
                  IF(NCSMAX.EQ.1 .OR. CSEDI(I,J,K).GT.CSMAX)THEN
                     CSMAX =CSEDI(I,J,K)
                     ICSMAX=I
                     JCSMAX=J
                     KCSMAX=K
                  ENDIF
               ENDIF
               CSEDI(I,J,K)=CMAXSD
            ENDIF
  310    CONTINUE
C
C ...... 濃度からくる交換砂量の考慮可能範囲を計算する
         DO 320 K=2,MZM
            IF(INDP(I,J,K).EQ.0 .OR. K.GT.KF(I,J)) CYCLE
            SDAMNT(I,J,K)=CSEDI(I,J,K)*DH(I,J,K)
            SDCAPA(I,J,K)=(CMAXSD-CSEDI(I,J,K))*DH(I,J,K)
            SDAMNT(I,J,MZ)=SDAMNT(I,J,MZ)+SDAMNT(I,J,K)
            SDCAPA(I,J,MZ)=SDCAPA(I,J,MZ)+SDCAPA(I,J,K)
  320    CONTINUE
         WEXCS(1)=-SDAMNT(I,J,MZ)/DT
         WEXCS(2)= SDCAPA(I,J,MZ)/DT
C
C ...... 交換砂量および累積(沈降／巻上)砂量を補正する
         IF(WEXSD(I,J).LE.WEXCS(1))THEN
            EXSDD(I,J)=EXSDD(I,J)-(WEXCS(1)-WEXSD(I,J))*DT
            WEXSD(I,J)=WEXCS(1)
         ELSEIF(WEXSD(I,J).GE.WEXCS(2))THEN
            EXSDE(I,J)=EXSDE(I,J)-(WEXSD(I,J)-WEXCS(2))*DT
            WEXSD(I,J)=WEXCS(2)
         ENDIF
C
C ...... 交換砂量を加味した浮遊砂濃度を計算する
         IF(KF(I,J).EQ.MZ)CYCLE
         EXSD=WEXSD(I,J)*DT
C ...... 交換砂量が正(巻上)の場合
         IF(EXSD.GT.0.0D0)THEN
            DO 330 K=2,KF(I,J)
               IF(INDP(I,J,K).EQ.0 .OR. K.GT.KF(I,J)) CYCLE
               IF(EXSD.GT.SDCAPA(I,J,K))THEN
                  CSEDI(I,J,K)=CMAXSD
                  EXSD=EXSD-SDCAPA(I,J,K)
               ELSE
                  CSEDI(I,J,K)=(EXSD+SDAMNT(I,J,K))/MAX(DH(I,J,K),EPSH)
                  EXIT
               ENDIF
  330       CONTINUE
C ...... 交換砂量が負(沈降)の場合
         ELSE
            DO 340 K=2,KF(I,J)
               IF(INDP(I,J,K).EQ.0 .OR. K.GT.KF(I,J)) CYCLE
               IF(EXSD.LT.-SDAMNT(I,J,K))THEN
                  CSEDI(I,J,K)=0.0D0
                  EXSD=EXSD+SDAMNT(I,J,K)
               ELSE
                  CSEDI(I,J,K)=(EXSD+SDAMNT(I,J,K))/MAX(DH(I,J,K),EPSH)
                  EXIT
               ENDIF
  340       CONTINUE
         ENDIF
C
C ...... 陸地濃度のゼロクリアと平均濃度の算出
         IF(IEPSHSD(I,J).EQ.1) CSEDI(I,J,KF(I,J))=0.0D0
         CSDDH=0.0D0
         DO 350 K=2,MZM
            IF(INDP(I,J,K).EQ.0 .OR. K.GT.KF(I,J)) CYCLE
            CSDDH=CSDDH+CSEDI(I,J,K)*DH(I,J,K)
  350    CONTINUE
         CSDAVE(I,J)=CSDDH/MAX(HH(I,J)-HDEP(I,J),EPSH)
  300 CONTINUE
C
C ... ゼロクリア／カットオフの補正量がCMAXSDの1%を超える場合には警告
      IF(NCSMIN.GT.0)THEN
         CALL ERRMS2('CLSEDI',6860)
         WRITE(LP,*) ' Number of CSEDI ZERO-Clear Cells :',NCSMIN
         WRITE(LP,800) ICSMIN,JCSMIN,KCSMIN,
     $      CSMIN,HDEP(ICSMIN,JCSMIN),DH(ICSMIN,JCSMIN,KCSMIN)
      ENDIF
      IF(NCSMAX.GT.0)THEN
         CALL ERRMS2('CLSEDI',6861)
         WRITE(LP,*) ' Number of CSEDI Cut-Off Cells :',NCSMAX
         WRITE(LP,800) ICSMAX,JCSMAX,KCSMAX,
     $      CSMAX,HDEP(ICSMAX,JCSMAX),DH(ICSMAX,JCSMAX,KCSMAX)
      ENDIF
  800 FORMAT('I,J,K,CSEDI,HDEP,DH=',3(I4,','),1P,2(E12.5,','),E12.5)
C
      RETURN
      END
