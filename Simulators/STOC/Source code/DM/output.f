      SUBROUTINE OUTPUT(IFLAG)
C----------------------------------------
C     漂流物の状態を出力する
C----------------------------------------
      USE M_TIME
      USE M_MODEL,ONLY:SINK_MODEL,L_RAND,I_RAND_SEED
      USE M_DRIFT
      USE M_FENCE,ONLY:NFC,XFC1,YFC1,XFC2,YFC2,ZFC
      USE M_GRID
      USE M_FILEIN
      USE M_OUTPUT
      USE M_FLUID,ONLY:UFAR1,VFAR1,HFAR1
C     
      IMPLICIT NONE
C     
      REAL(8),PARAMETER::PI=3.141592653589793D0
      INTEGER IFLAG,N,M,NA,SIZ1,SIZ2
      INTEGER::NN(3,12)
      REAL(8)::XN(12),YN(12),ZN(12),XX(8),YY(8),ZZ(8),ZBTM,DELTX,DELTY
      CHARACTER(28):: FILENAME,FORM1
C     
      DATA NN /1,2,3,1,3,4,7,6,5,7,5,8,6,3,2,6,7,3,
     $   5,1,4,5,4,8,8,4,3,8,3,7,2,1,5,2,5,6/
      DATA XN /1,1,-1,-1,0,0,0,0,0,0,0,0/
      DATA YN /0,0,0,0,1,1,-1,-1,0,0,0,0/
      DATA ZN /0,0,0,0,0,0,0,0,1,1,-1,-1/
C     
      REAL(8),SAVE::TIME1,TIME2,TIME3,TIME4
      INTEGER,PARAMETER::MODE_STL=1,MODE_TXT=0 ! 修正必要
      DATA TIME1,TIME2,TIME3,TIME4 /0.0D0,0.0D0,0.0D0,0.0D0/ ! 修正必要
C----------------------------------------
C     出力時間間隔
C     
C     DRIFT_INTERVAL       漂流物に関するリスト出力時間間隔[s]
C     BLOCK_INTERVAL       閉塞状況ファイルの出力間隔(s)
C     RESTART_INTERVAL     リスタートデータの出力時間間隔[s]
C     STL_INTERVAL         STLデータの出力時間間隔[s]
C     
C----------------------------------------
C     
C     
C----------------------------------------
C     (0) ファイルのオープン
C----------------------------------------
      IF( IFLAG.EQ.0 ) THEN
         TIME1 = DRIFT_START
         TIME2 = BLOCK_START
         TIME3 = RESTART_START
         TIME4 = STL_START
C     
         OPEN(14,FILE='./drift/out/rout.dat',STATUS='NEW',
     &      FORM='UNFORMATTED',ERR=92)
         OPEN(71,FILE='./drift/out/drift.txt',STATUS='NEW',
     &      FORM='FORMATTED',ERR=90)
C     DEBUG         OPEN(172,FILE='./drift/out/d1.txt',STATUS='NEW',
C     DEBUG     &                                          FORM='FORMATTED',ERR=90)
C     DEBUG         OPEN(173,FILE='./drift/out/d2.txt',STATUS='NEW',
C     DEBUG     &                                          FORM='FORMATTED',ERR=90)
C     DEBUG         OPEN(174,FILE='./drift/out/d3.txt',STATUS='NEW',
C     DEBUG     &                                          FORM='FORMATTED',ERR=90)
         WRITE(71,800)
  800    FORMAT('#',4X,'N',6X,',TIME',10X,',X',10X,',Y',6X,',HZD',3X,
     $          ',THETA',6X,',U',6X,',V',4X,',WSD',2X,',OMEGA',6X,
     $          ',HFD',6X,',HTD',1X,',LD ,IS ,ID ,AR')
         IF( MODE_STL.EQ.0 ) THEN
            OPEN(72,FILE='./drift/out/drift.stl',STATUS='NEW',
     &         FORM='FORMATTED',ERR=91)
            WRITE(72,'(A)') 'solid ascii'
         ENDIF
C     
         IF( NFC.GT.0 ) THEN
            OPEN(81,FILE='./drift/out/fence.stl',STATUS='NEW',
     &         FORM='FORMATTED',ERR=94)
            WRITE(81,'(A)') 'solid ascii'
            ZBTM=-20.0D0
            DO N=1,NFC
C     ............ 横からみたときの厚さのないフェンスを出力
               WRITE(81,820) YFC2(N)-YFC1(N),XFC1(N)-XFC2(N),0.0D0
               WRITE(81,821)
               WRITE(81,822) XFC1(N),YFC1(N),ZFC(N)
               WRITE(81,822) XFC2(N),YFC2(N),ZFC(N)
               WRITE(81,822) XFC2(N),YFC2(N),ZBTM
               WRITE(81,823)
               WRITE(81,824)
C     
               WRITE(81,820) YFC2(N)-YFC1(N),XFC1(N)-XFC2(N),0.0D0
               WRITE(81,821)
               WRITE(81,822) XFC1(N),YFC1(N),ZFC(N)
               WRITE(81,822) XFC2(N),YFC2(N),ZBTM
               WRITE(81,822) XFC1(N),YFC1(N),ZBTM
               WRITE(81,823)
               WRITE(81,824)
C     
C     ............ 上からみて幅をもった線になるように出力
               DELTX=0.05D0*(YFC2(N)-YFC1(N))
     $            /SQRT((XFC2(N)-XFC1(N))**2+(YFC2(N)-YFC1(N))**2)
               DELTY=0.05D0*(XFC1(N)-XFC2(N))
     $            /SQRT((XFC2(N)-XFC1(N))**2+(YFC2(N)-YFC1(N))**2)
               WRITE(81,820) 0.0D0,0.0D0,1.0D0
               WRITE(81,821)
               WRITE(81,822) XFC1(N)+DELTX,YFC1(N)+DELTY,ZFC(N)
               WRITE(81,822) XFC2(N)+DELTX,YFC2(N)+DELTY,ZFC(N)
               WRITE(81,822) XFC2(N)-DELTX,YFC2(N)-DELTY,ZFC(N)
               WRITE(81,823)
               WRITE(81,824)
C     
               WRITE(81,820) 0.0D0,0.0D0,1.0D0
               WRITE(81,821)
               WRITE(81,822) XFC1(N)+DELTX,YFC1(N)+DELTY,ZFC(N)
               WRITE(81,822) XFC2(N)-DELTX,YFC2(N)-DELTY,ZFC(N)
               WRITE(81,822) XFC1(N)-DELTX,YFC1(N)-DELTY,ZFC(N)
               WRITE(81,823)
               WRITE(81,824)
            ENDDO
            WRITE(81,'(A)') 'end solid'
            CLOSE(81)
         ENDIF
C     
C<<<<<(START) STOC-BLC VERSION  <<<<<<<
         OPEN(75,FILE='./drift/out/blockage.dat',STATUS='NEW',
     &      FORM='FORMATTED',ERR=93)
C<<<<<(END)  STOC-BLC VERSION  <<<<<<<
      ENDIF
C     
C     
C----------------------------------------
C     (1.1) リスタート用データの出力
C----------------------------------------
      IF( TIME.GT.TIME3-0.5*DT .OR. IFLAG.EQ.2 ) THEN
         WRITE(14) TIME,NS,DT,ND,TIMFL1,DTFL,TIMWIN1,TIMWIN0
         WRITE(14) (XD1(N),YD1(N),TD1(N),UD1(N),VD1(N),OD1(N),N=1,ND)
         WRITE(14) (AM(N),AI(N),AD(N),BD(N),N=1,ND)
         WRITE(14) (WSD(N),HZD(N),HAZ(N),AHIN(N),N=1,ND)
         WRITE(14) (LD(N),IFLAG_S(N),IFLAG_D(N),N=1,ND)
         DO NA=1,MXAREA
          SIZ1=NI(NA)*NJ(NA)*NK(NA)
          SIZ2=NI(NA)*NJ(NA)
          write(14) UUAR1(NA,1:SIZ1),VVAR1(NA,1:SIZ1),HHAR1(NA,1:SIZ2)
          write(14) UUAR0(NA,1:SIZ1),VVAR0(NA,1:SIZ1),HHAR0(NA,1:SIZ2)
          write(14) UFAR1(NA,1:SIZ1),VFAR1(NA,1:SIZ1),HFAR1(NA,1:SIZ2)
         ENDDO
         WRITE(14) (XD_INIT(N),YD_INIT(N),ZD_INIT(N),N=1,ND)
         IF( L_RAND==1 ) THEN
            CALL RANDOM_SEED(GET=I_RAND_SEED)
         ENDIF
         WRITE(14) I_RAND_SEED
C     
         TIME3 = TIME3 + RESTART_INTERVAL
      ENDIF
C     
C     
C----------------------------------------
C     (1.2) リスト出力
C----------------------------------------
      IF( TIME.GT.TIME1-0.5*DT .OR. IFLAG.EQ.2 ) THEN
         DO N=1,ND
            IF( MODE_TXT.EQ.0 ) THEN
               WRITE(71,810) N,TIME,XD1(N),YD1(N),HZD(N),
     $            TD1(N)*180.0D0/PI,
     $            UD1(N),VD1(N),WSD(N),OD1(N)*180.0D0/PI,
     $            HFD(N),HTD(N),
     $            LD(N),IFLAG_S(N),IFLAG_D(N),INAR(N)
  810          FORMAT(I6.6,1X,F10.2,F12.3,F12.3,F10.3,F9.3,F8.3,F8.3,
     $            F8.3,F8.4,F10.3,F10.3,I4,I4,I4,I4)
            ELSE
               WRITE(71,815) N,TYPED(N),TIME,
     $            XD_INIT(N),YD_INIT(N),ZD_INIT(N),
     $            XD1(N),YD1(N),HZD(N)+BD(N),
     $            TD1(N)*180.0D0/PI
  815          FORMAT(I6.6,A4,1X,F10.2,F12.3,F12.3,F8.4,
     $            F12.3,F12.3,F8.4,F9.3)
            ENDIF
         ENDDO
C     
         TIME1 = TIME1 + DRIFT_INTERVAL
      ENDIF
C     
C     
C----------------------------------------
C     (1.3) STLデータ出力
C----------------------------------------
      IF( TIME.GT.TIME4-0.5*DT ) THEN
         IF( MODE_STL.EQ.1 ) THEN
            FILENAME = './drift/out/drift0000000.stl'
            WRITE(FILENAME,111) NINT(TIME)
  111       format('./drift/out/drift',I7.7,'.stl')
            OPEN(72,FILE=FILENAME,STATUS='NEW',FORM='FORMATTED',ERR=95)
            WRITE(72,'(A)') 'solid ascii'
         ENDIF
C     
         DO N=1,ND
            CALL VERTEX2(XX,YY,ZZ,XD1(N),YD1(N),TD1(N),
     $         BL(N),BB(N),AZ1(N),HZD(N))
C     
            DO M=1,12
               WRITE(72,820) XN(M),YN(M),ZN(M)
               WRITE(72,821)
               WRITE(72,822) XX(NN(1,M)),YY(NN(1,M)),ZZ(NN(1,M))
               WRITE(72,822) XX(NN(2,M)),YY(NN(2,M)),ZZ(NN(2,M))
               WRITE(72,822) XX(NN(3,M)),YY(NN(3,M)),ZZ(NN(3,M))
               WRITE(72,823)
               WRITE(72,824)
  820          FORMAT(' facet normal ',1PE14.6,1X,E14.6,1X,E14.6)
  821          FORMAT('    outer loop')
  822          FORMAT('      vertex  ',1PE13.6,1X,E13.6,1X,E13.6)
  823          FORMAT('    endloop')
  824          FORMAT(' endfacet')
            ENDDO
         ENDDO
C     
         TIME4 = TIME4 + STL_INTERVAL
C     
         IF( MODE_STL.EQ.1 ) THEN
            WRITE(72,'(A)') 'end solid'
            CLOSE(72)
         ENDIF
      ENDIF
C     
C     
C<<<<<(START) STOC-BLC VERSION  <<<<<<<
C----------------------------------------
C     (1.4) 閉塞状況の出力
C----------------------------------------
C     
      IF( TIME.GT.TIME2-0.5*DT .OR. IFLAG.EQ.2 ) THEN
         FORM1='(F10.2,      I3)'
         WRITE(FORM1(8:13),'(I6)') ND
         WRITE(75,FORM1) TIME,(LBLC(N),N=1,ND)
C
         TIME2 = TIME2 + BLOCK_INTERVAL
      ENDIF
C
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
C
C----------------------------------------
C     (2) ファイルのクローズ
C----------------------------------------
      IF( IFLAG.EQ.2 ) THEN
         CLOSE(14)
C
         CLOSE(71)
         IF( MODE_STL.EQ.0 ) THEN
            WRITE(72,'(A)') 'end solid'
            CLOSE(72)
         ENDIF
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
         CLOSE(75)
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
C
      ENDIF
C
      RETURN
   90 CONTINUE
      WRITE(*,*) 'ERROR: cannot open drift.txt'
      CALL ERRMSG('OUTPUT',-1)
   91 CONTINUE
      WRITE(*,*) 'ERROR: cannot open drift.stl'
      CALL ERRMSG('OUTPUT',-2)
   92 CONTINUE
      WRITE(*,*) 'ERROR: cannot open rout.dat'
      CALL ERRMSG('OUTPUT',-3)
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
   93 CONTINUE
      WRITE(*,*) 'ERROR: cannot open blockage.dat'
      CALL ERRMSG('OUTPUT',-4)
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
   94 CONTINUE
      WRITE(*,*) 'ERROR: cannot open fence.stl'
      CALL ERRMSG('OUTPUT',-5)
   95 CONTINUE
      WRITE(*,*) 'ERROR: cannot open ',FILENAME
      CALL ERRMSG('OUTPUT',-6)
      END
