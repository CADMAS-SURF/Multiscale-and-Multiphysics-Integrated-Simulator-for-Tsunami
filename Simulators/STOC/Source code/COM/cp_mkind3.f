      SUBROUTINE CP_MKIND3(XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     1                     I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     2                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS)
C
C----------------------------------------------------------------------
C     (7-0)  ML領域とNS領域の相互参照インデックス
C----------------------------------------------------------------------
C
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'CONNEC.h'
C2006.05.12
      INCLUDE 'mpif.h'
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      REAL(8),INTENT(INOUT)::
     $   XC_ML(8,MX_ML),YC_ML(8,MY_ML),ZC_ML(8,MZ_ML)
      REAL(8),INTENT(INOUT)::
     $   XC_NS(8,MX_NS),YC_NS(8,MY_NS),ZC_NS(8,MZ_NS)
      INTEGER,INTENT(INOUT)::I_ML(2,MX_ML),J_ML(2,MY_ML),K_ML(2,MZ_ML)
      INTEGER,INTENT(INOUT)::I_NS(2,MX_NS),J_NS(2,MY_NS),K_NS(2,MZ_NS)
C
      INTEGER::IDB=0
      REAL(8)::DLEPS=1.0D-3
C
      REAL(8)::X1,X2,XI,XML,XNS,Y1,Y2,YJ,YML,YNS,Z1,Z2,ZK,DXI,DYI
      INTEGER::I,IEML,IER,IERROR,ISML,J,JEML,JSML,K,N,N1,N2,ICODE
C
      CALL ZERCLI(I_ML,2*MX_ML,0)
      CALL ZERCLI(J_ML,2*MY_ML,0)
      CALL ZERCLI(K_ML,2*MZ_ML,0)
      CALL ZERCLI(I_NS,2*MX_NS,0)
      CALL ZERCLI(J_NS,2*MY_NS,0)
      CALL ZERCLI(K_NS,2*MZ_NS,0)
C
C     I_NSを作成
C
      DO 100 I=1,MX_NS
        XI = XC_NS(1,I)
        DXI= XC_NS(4,I)
        DO 110 N=1,MX_ML
          IF(XC_ML(1,N).LT.XI) THEN
            N1 = N
            X1 = XC_ML(1,N)
          ELSE 
            N2 = N
            X2 = XC_ML(1,N)
            IF(N.EQ.1) X1=X2
            IF(N.GE.2.AND.XI-X1.LT.DLEPS*DXI) THEN
              I_NS(1,I) = N1
              I_NS(2,I) = N1
              GO TO 100
            ELSE 
              I_NS(1,I) = N2
              I_NS(2,I) = N2
              GO TO 100
            END IF
          END IF
  110   CONTINUE
  100 CONTINUE    
C
C     I_MLを作成
C
      DO 150 I=1,MX_ML
        XI = XC_ML(1,I)
        DO 160 N=1,MX_NS
          IF(XC_NS(1,N).LT.XI) THEN
            N1 = N
            X1 = XC_NS(1,N)
          ELSE 
            N2 = N
            X2 = XC_NS(1,N)
            DXI= XC_NS(4,N)
            IF(N.EQ.1) X1=X2
            IF(N.GE.2.AND.XI-X1.LT.DLEPS*DXI) THEN
              I_ML(1,I) = N1
              I_ML(2,I) = N1
              IF(DABS(XC_ML(1,I)-XC_NS(1,N1)).GT.DLEPS*DXI) THEN
                WRITE(LP,600) XC_ML(1,I),XC_NS(1,N1)
  600           FORMAT('### ML-XGRID POINT ERROR ###',
     $                 '  XC(1,ML),XC(1,NS) =',1P,2D12.5)
              END IF
              GO TO 150
            ELSE 
              IF(N2.EQ.1.AND.X2-XI.GT.DLEPS*DXI) N2=0
              I_ML(1,I) = N2
              I_ML(2,I) = N2
              IF(N2.NE.0) THEN
                 IF(DABS(XC_ML(1,I)-XC_NS(1,N2)).GT.DLEPS*DXI) THEN
                    WRITE(LP,600) XC_ML(1,I),XC_NS(1,N2)
                 END IF
              END IF
              GO TO 150
           END IF
        END IF
  160 CONTINUE
  150 CONTINUE
C
C     J_NSを作成
C
      DO 200 J=1,MY_NS
        YJ = YC_NS(1,J)
        DYI= YC_NS(4,J)
        DO 210 N=1,MY_ML
          IF(YC_ML(1,N).LT.YJ) THEN
            N1 = N
            Y1 = YC_ML(1,N)
          ELSE 
            N2 = N
            Y2 = YC_ML(1,N)
            IF(N.EQ.1) Y1=Y2
            IF(N.GE.2.AND.YJ-Y1.LT.DLEPS*DYI) THEN
              J_NS(1,J) = N1
              J_NS(2,J) = N1
              GO TO 200
            ELSE 
              J_NS(1,J) = N2
              J_NS(2,J) = N2
              GO TO 200
            END IF
          END IF
  210   CONTINUE
  200 CONTINUE    
C
C     J_MLを作成
C
      DO 250 J=1,MY_ML
        YJ = YC_ML(1,J)
        DO 260 N=1,MY_NS
          IF(YC_NS(1,N).LT.YJ) THEN
            N1 = N
            Y1 = YC_NS(1,N)
          ELSE 
            N2 = N
            Y2 = YC_NS(1,N)
            DYI= YC_NS(4,N)
            IF(N.EQ.1) Y1=Y2
            IF(N.GE.2.AND.YJ-Y1.LT.DLEPS*DYI) THEN
              J_ML(1,J) = N1
              J_ML(2,J) = N1
              IF(DABS(YC_ML(1,J)-YC_NS(1,N1)).GT.DLEPS*DYI) THEN
                WRITE(LP,610) YC_ML(1,J),YC_NS(1,N1)
  610           FORMAT('### ML-YGRID POINT ERROR ###',
     $                 '  YC(1,ML),YC(1,NS) =',1P,2D12.5)
              END IF
              GO TO 250
            ELSE 
              IF(N2.EQ.1.AND.Y2-YJ.GT.DLEPS*DYI) N2=0
              J_ML(1,J) = N2
              J_ML(2,J) = N2
              IF(N2.NE.0) THEN
                 IF(DABS(YC_ML(1,J)-YC_NS(1,N2)).GT.DLEPS*DYI) THEN
                    WRITE(LP,610) YC_ML(1,J),YC_NS(1,N1)
                 END IF
              END IF
              GO TO 250
            END IF
          END IF
  260   CONTINUE
  250 CONTINUE    
C
C     K_NSを作成
C
      DO 300 K=1,MZ_NS
        ZK = ZC_NS(1,K)
        DO 310 N=1,MZ_ML
          IF(ZC_ML(1,N).LT.ZK) THEN
            N1 = N
            Z1 = ZC_ML(1,N)
          ELSE 
            N2 = N
            Z2 = ZC_ML(1,N)
            IF(N.EQ.1) Z1=Z2
            IF(N.GE.2.AND.ZK-Z1.LT.DLEPS) THEN
              K_NS(1,K) = N1
              K_NS(2,K) = N1
              GO TO 300
            ELSE 
              K_NS(1,K) = N2
              K_NS(2,K) = N2
              GO TO 300
            END IF
          END IF
  310   CONTINUE
  300 CONTINUE    
C
C     K_MLを作成
C
      DO 350 K=1,MZ_ML
        ZK = ZC_ML(1,K)
        DO 360 N=1,MZ_NS
          IF(ZC_NS(1,N).LT.ZK) THEN
            N1 = N
            Z1 = ZC_NS(1,N)
          ELSE 
            N2 = N
            Z2 = ZC_NS(1,N)
            IF(N.EQ.1) Z1=Z2
            IF(N.GE.2.AND.ZK-Z1.LT.DLEPS) THEN
              K_ML(1,K) = N1
              K_ML(2,K) = N1
              IF(DABS(ZC_ML(1,K)-ZC_NS(1,N1)).GT.DLEPS) THEN
                WRITE(LP,620) ZC_ML(1,K),ZC_NS(1,N1)
  620           FORMAT('### ML-ZGRID POINT ERROR ###',
     $                 '  ZC(1,ML),ZC(1,NS) =',1P,2D12.5)
              END IF
              GO TO 350
            ELSE 
              IF(N2.EQ.1.AND.Z2-ZK.GT.DLEPS) N2=0
              K_ML(1,K) = N2
              K_ML(2,K) = N2
              IF(N2.NE.0) THEN
                 IF(DABS(ZC_ML(1,K)-ZC_NS(1,N2)).GT.DLEPS) THEN
                    WRITE(LP,620) ZC_ML(1,K),ZC_NS(1,N1)
                 END IF
              END IF
              GO TO 350
            END IF
          END IF
  360   CONTINUE
  350 CONTINUE    
C
      IF(IDB.NE.0) THEN
        WRITE(6,*) '  I_NS(1,*),XC_NS(1,*)='
        WRITE(6,630) (I_NS(1,I),XC_NS(1,I),I=1,MX_NS) 
        WRITE(6,*) '  J_NS(1,*),YC_NS(1,*)='
        WRITE(6,630) (J_NS(1,J),YC_NS(1,J),J=1,MY_NS) 
        WRITE(6,*) '  K_NS(1,*),ZC_NS(1,*)='
        WRITE(6,630) (K_NS(1,K),ZC_NS(1,K),K=1,MZ_NS) 
        WRITE(6,*) '  I_ML(1,*),XC_ML(1,*)='
        WRITE(6,630) (I_ML(1,I),XC_ML(1,I),I=1,MX_ML) 
        WRITE(6,*) '  J_ML(1,*),YC_ML(1,*)='
        WRITE(6,630) (J_ML(1,J),YC_ML(1,J),J=1,MY_ML) 
        WRITE(6,*) '  K_ML(1,*),ZC_ML(1,*)='
        WRITE(6,630) (K_ML(1,K),ZC_ML(1,K),K=1,MZ_ML)
  630   FORMAT(2X,10(I4,1P,D11.4))    
      END IF
C
C .......... オーバーラップ領域チェック
C
      IF(MY_NS.EQ.3) THEN
        NOVRLP(1) = 0
        NOVRLP(4) = 0
      END IF
      IF(MX_NS.EQ.3) THEN
        NOVRLP(2) = 0
        NOVRLP(3) = 0
      END IF
      IF(IPECON(4,NRANK+1).GE.0) NOVRLP(1)=0
      IF(IPECON(5,NRANK+1).GE.0) NOVRLP(2)=0
      IF(IPECON(6,NRANK+1).GE.0) NOVRLP(3)=0
      IF(IPECON(7,NRANK+1).GE.0) NOVRLP(4)=0
C
      DO 400 N=1,4
        NESNS(N) = NOVRLP(N)
        NESML(N) = 0
        NOVRLP(N) = 0
  400 CONTINUE
C    
      IER = 0
      IF(NESNS(1).NE.0) THEN
        JSML = J_NS(2,2)
        JEML = J_NS(2,2+NESNS(1)-1)
        YML = YC_ML(1,JEML)
        YNS = YC_NS(1,2+NESNS(1)-1)
        DYI = YC_NS(4,2+NESNS(1)-1)
        NOVRLP(1) = JEML-JSML+1 
        IF(ABS(YML-YNS).GT.DLEPS*DYI)  THEN
          IER = IER+1
          WRITE(LP,640) YML,YNS
  640     FORMAT('### OVERLAP DATA ERROR ( SOUTH SIDE ) ###'
     1          /'    ML-OVERLAP Y-COORD.( ML,NS ) =',1P,2D11.4)
        END IF
      END IF
C
      IF(NESNS(2).NE.0) THEN
        ISML = I_NS(2,2)
        IEML = I_NS(2,2+NESNS(2)-1)
        XML = XC_ML(1,IEML)
        XNS = XC_NS(1,2+NESNS(2)-1)
        DXI = XC_NS(4,2+NESNS(2)-1)
        NOVRLP(2) = IEML-ISML+1
        IF(ABS(XML-XNS).GT.DLEPS*DXI)  THEN
          IER = IER+1
          WRITE(LP,650) XML,XNS  
  650     FORMAT('### OVERLAP DATA ERROR ( WEST SIDE ) ###'
     1          /'    ML-OVERLAP X-COORD.( ML,NS ) =',1P,2D11.4)
        END IF
      END IF
C
      IF(NESNS(3).NE.0) THEN
        ISML = I_NS(2,MX_NS-NESNS(3))
        IEML = I_NS(2,MX_NS-1)
        XML = XC_ML(1,ISML-1)
        XNS = XC_NS(1,MX_NS-NESNS(3)-1)
        DXI = XC_NS(4,MX_NS-NESNS(3)-1)
        NOVRLP(3) = IEML-ISML+1
        IF(ABS(XML-XNS).GT.DLEPS*DXI)  THEN
          IER = IER+1
          WRITE(LP,660) XML,XNS  
  660     FORMAT('### OVERLAP DATA ERROR ( EAST SIDE ) ###'
     1          /'    ML-OVERLAP X-COORD.( ML,NS ) =',1P,2D11.4)
        END IF
      END IF
C
      IF(NESNS(4).NE.0) THEN
        JSML = J_NS(2,MY_NS-NESNS(4))
        JEML = J_NS(2,MY_NS-1)
        YML = YC_ML(1,JSML-1)
        YNS = YC_NS(1,MY_NS-NESNS(4)-1)
        DYI = YC_NS(4,MY_NS-NESNS(4)-1)
        NOVRLP(4) = JEML-JSML+1
        IF(ABS(YML-YNS).GT.DLEPS*DYI)  THEN
          IER = IER+1
          WRITE(LP,670) YML,YNS
  670     FORMAT('### OVERLAP DATA ERROR ( NORTH SIDE ) ###'
     1          /'    ML-OVERLAP Y-COORD.( ML,NS ) =',1P,2D11.4)
        END IF
      END IF
C
      IF((MY_NS-2.LE.NESNS(1)+NESNS(4)).OR.
     $   (MX_NS-2.LE.NESNS(2)+NESNS(3))) THEN
        IER = IER+1
        WRITE(LP,680) MY_NS-2,NESNS(1)+NESNS(4),
     $                MX_NS-2,NESNS(2)+NESNS(3)
  680   FORMAT('### OVERLAP DATA ERROR ( NS-AREA ) ###'
     $        /'    NS-AREA Y =',I5,'   OVERLAP-AREA Y =',I5
     $        /'    NS-AREA X =',I5,'   OVERLAP-AREA X =',I5)
      END IF
C
      IF((MY_ML-2.LE.NESNS(1)+NESNS(4)).OR.
     $   (MX_ML-2.LE.NESNS(2)+NESNS(3))) THEN
        IER = IER+1
        WRITE(LP,690) MY_ML-2,NESNS(1)+NESNS(4),
     $                MX_ML-2,NESNS(2)+NESNS(3)
  690   FORMAT('### OVERLAP DATA ERROR ( ML-AREA ) ###'
     $        /'    ML-AREA Y =',I5,'   OVERLAP-AREA Y =',I5
     $        /'    ML-AREA X =',I5,'   OVERLAP-AREA X =',I5)
      END IF
C
      IF(IER.NE.0) THEN
        CALL ERRMSG('CP_MKIND3',6260)
        WRITE(LP,700) IER
  700   FORMAT('### STOP IN CP_MKIND3 ###    IER =',I5)  
        CALL ABORT1('')
      END IF
C
C ... END
C
      RETURN
      END
