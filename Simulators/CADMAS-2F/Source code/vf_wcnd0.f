C     作成者  磯部雅彦, 東京大学教授, 1998年4月現在
C     このプログラムは，磯部先生の許可を得て榊山　勉(電力中央研究所)が
C     「数値波動水路の耐波設計への適用に関する研究会」でのプログラム開発
C     で使用するために譲与したものです．

      SUBROUTINE VF_CND0(N1,D1,T,H,QL1,E1,RK,RL,C1,CC01)
      DIMENSION A(4),B(3,4),BB(3,4)
C     ##### ADD BY AKIYAMA(FUJI-RIC):1998/8/18 #####
      SAVE
C     ##############################################
      DATA G,PI/980.,3.141593/, EPS/1.E-5/
C
      X=1.E10
      N=N1
      NN=N+1
      NN1=NN+1
      D=D1
      GD=G*D
      C0=SQRT(GD)
      CC01=C0
      E=H/D
      E1=E
      E2=E*E
      E3=E2*E
      UR=G*H*T*T/(D*D)
      IF(UR.LT.10.) GO TO 26
   11 UR34=UR*0.75
      QQL=SQRT(UR34)
      C00=0.
      C01=0.
      C10=0.
      C11=0.
      C20=0.
      GO TO (12,13,14),N
   14 C00=E2*0.2
      C01=E2*(-0.25)
      C10=E2*0.825
      C11=E2*(-0.5)
      C20=E2*0.825
   13 C00=C00+E*(-0.25)
      C10=C10+E*(-0.5)
   12 C00=C00+1.
C
      DO 10 I=1,20
      QQ=EXP(-QQL)
      Q2=QQ*QQ
      Q4=Q2*Q2
      T2=(1.+2.*(-QQ+Q4))**4
      T3=(1.+2.*( QQ+Q4))**4
      T4=(1.+Q2*(1.+Q4))**4
      S=1.+8.*Q2*(1.-Q2)
      R=16.*QQ*T4/T2
      O=(2./QQL+T3-S)/T2
      QQLL=QQL
      QQL=SQRT(UR34*(C00+R*(C10+R*C20)+O*(C01+R*C11))/T2)
      IF(ABS((QQLL-QQL)/QQL).LT.EPS) GO TO 20
   10 CONTINUE
      I=21
      GO TO 27
C
   26 QQL=PI
      QQLL=0.
      I=0
C     ##### MOD BY AKIYAMA(FUJI-RIC):1998/8/18 #####
   27 CONTINUE
CMOD  27 WRITE(6,600) I,UR,QQL,QQLL
C     ##############################################
  600 FORMAT(1H ,'/CND0/',4X,I5,3G13.4)
      IF(I.NE.0) GO TO 20
      UR=10.
      GO TO 11
C
   20 DO 24 I=1,NN
      A(I)=0.
      DO 24 J=1,N
   24 B(J,I)=0.
      P0=0.
      C=0.
      RR=R*R
      RO=R*O
      OO=O*O
      RRR=RR*R
      RRO=RR*O
      ROO=RO*O
      OOO=OO*O
C
      GO TO (21,22,23),N
   23 A(1)  =E3*(133.*R-16.*O+399.*RR-466.*RO+100.*OO+266.*RRR
     #           -466.*RRO+200.*ROO)/400.
      A(2)  =E3*(50.-R-60.*O)/80.
      A(3)  =E3*(-151.+R+60.*O)/80.
      A(4)  =E3*101./80.
      B(1,1)=E3*(-71.*R+47.*O-23.*RR+97.*RO-50.*OO+153.*RRR-153.*RRO
     #           -25.*ROO+25.*OOO)/200.
      B(2,1)=E3*(6.*R+24.*RR-21.*RO)/8.
      B(3,1)=E3*(3.*R-3.*RR)/16.
      B(1,2)=E3*(-19.-27.*R+10.*O+101.*RR-100.*RO+15.*OO)/40.
      B(2,2)=E3*(6.+36.*R-21.*O-24.*RR+21.*RO)/4.
      B(3,2)=E3*(6.-39.*R+6.*RR)/16.
      B(1,3)=E3*(-2.+32.*R-15.*O)/10.
      B(2,3)=E3*(30.-120.*R+63.*O)/8.
      B(3,3)=E3*(-45.+45.*R)/16.
      B(1,4)=E3*6./5.
      B(2,4)=E3*(-15.)/2.
      B(3,4)=E3*45./16.
      P0    =E3*(3.*R-6.*O+9.*RR-26.*RO+5.*OO+6.*RRR-26.*RRO+10.*ROO
     #           +10.*OOO)/20.
      C     =E3*(150.+1079.*R-203.*O+2337.*RR-2653.*RO+350.*OO
     #           +1558.*RRR-2653.*RRO+700.*ROO+175.*OOO)/2800.
   22 A(1)  =A(1)  +E2*(-2.*R+O-2.*RR+2.*RO)/4.
      A(2)  =A(2)  +E2*(-0.75)
      A(3)  =A(3)  +E2*(0.75)
      B(1,1)=B(1,1)+E2*(R-O-2.*RR+2.*OO)/4.
      B(2,1)=B(2,1)+E2*(-0.75*R)
      B(1,2)=B(1,2)+E2*(1.-6.*R+2.*O)/4.
      B(2,2)=B(2,2)+E2*(-3.+3.*R)/2.
      B(1,3)=B(1,3)+E2*(-1.)
      B(2,3)=B(2,3)+E2*2.25
      P0    =P0    +E2*(-R+2.*O-RR+4.*RO-3.*OO)/2.
      C     =C     +E2*(-6.-16.*R+5.*O-16.*RR+10.*RO+15.*OO)/40.
   21 A(1)  =A(1)  +E *(R-O)
      A(2)  =A(2)  +E
      B(1,1)=B(1,1)+E *(R-O)
      B(1,2)=B(1,2)+E
      C     =C     +E*(1.+2.*R-3.*O)/2. +1.
      CC    =C     *C0
      C1    =CC
      P0    =P0    *C0*C0/G
C
      RL=T*CC
      DL=D/RL
      S=2.*SQRT(T3)*QQL*DL
      DO 25 I=1,NN
      A(I)=A(I)*D
      DO 25 J=1,N
      B(J,I)=B(J,I)*C0
   25 BB(J,I)=B(J,I)*S*FLOAT(I-1)/FLOAT(2*J-1)
      QL=PI*PI/QQL
      QL1=QL
      RK=S
      RETURN
C
C
      ENTRY VF_CND1(X1,ZS)
C
C
      ISUB=1
      IF(X.NE.X1) GO TO 210
  110 ZS=0.
      DO 111 I=1,NN
      L=NN1-I
  111 ZS=ZS*CN2+A(L)
      RETURN
C
C
      ENTRY VF_CND2(X1,Z1,U1,W1,P1)
C
C
      ISUB=2
      IF(X.EQ.X1) GO TO 220
  210 X=X1
      CALL VF_ELFN(QL,X/2.,CN,SN,DN,ZE)
      CN2=CN*CN
      CSD=CN*SN*DN
      IF(ISUB.EQ.1) GO TO 110
C
  220 Z=1.+Z1/D
      ZZ=Z*Z
      U=0.
      W=0.
      DO 221 I=1,NN
      K=NN1-I
      UUU=0.
      WWW=0.
      DO 222 J=1,N
      L=NN-J
      UUU=UUU*ZZ+B(L,K)
  222 WWW=WWW*ZZ+BB(L,K)
      U=U*CN2+UUU
      IF(I.EQ.NN) GO TO 221
      W=W*CN2+WWW
  221 CONTINUE
      W=W*CSD*Z
      P=P0+(CC*U-(U*U+W*W)/2.)/G
      U1=U
      W1=W
      P1=P
      RETURN
      END
      SUBROUTINE VF_ELINT(QL1,RK1,RKK1,SK1,SKK1,SE1,SEE1)
C     ##### ADD BY AKIYAMA(FUJI-RIC):1998/8/18 #####
      SAVE
C     ##############################################
      DATA PI,PISQ,PI2/3.141593,9.869604,1.570796/, QL/1.E10/
C
      ISUB=1
      QL=QL1
C
 1000 IQ=(12.*QL/PI-7.)/5.
      QQL=PISQ/QL
      Q =EXP(- QL)
      QQ=EXP(-QQL)
      IF(IQ) 100,200,300
C
  100 Q2=2.*QQ
      RK=1.
      RKK=4.*SQRT(QQ)
      SKK=PI2
      SK=QQL/2.
      SEE=SKK
      SE=1.
      P=2.*QQL
      FZZ=-1./SK
      GO TO 310
C
  200 Q2=QQ*QQ
      TQ2=3.*Q2
      Q4=Q2*Q2
      Q6=Q4*Q2
      FQ6=5.*Q6
      T02=1.+2.*(-QQ+Q4)
      T03=1.+2.*( QQ+Q4)
      T04=1.+Q2+Q6
      RK=(T02/T03)**2
      RKK=4.*SQRT(QQ)*(T04/T03)**2
      SKK=PI2*T03*T03
      SK =SKK*QQL/PI
      SEE=PISQ*(1.+8.*(Q2-Q4))/(4.*SKK)
      SE =PI2/SKK-(SEE/SKK-1.)*SK
      P=2.*QQL
      FZ =PI2/SKK
      FZZ=-1./SK
      GO TO 320
C
  300 Q2=Q*Q
      Q4=Q2*Q2
      Q6=Q4*Q2
      T02=1.+Q2+Q6
      T03=1.+2.*( Q+Q4)
      T04=1.+2.*(-Q+Q4)
      RK=4.*SQRT(Q)*(T02/T03)**2
      RKK=(T04/T03)**2
      SK=PI2*T03*T03
      SKK=SK*QL/PI
      SE=PISQ*(1.+8.*(Q2-Q4))/(4.*SK)
      SEE=PI2/SK-(SE/SK-1.)*SKK
      P=2.*PI
      FZ=4.*PI/SK
  320 FC=T04/T02
      FS=T03/T02
      FD=T04/T03
  310 IF(ISUB.EQ.2) GO TO 2000
      RK1=RK
      RKK1=RKK
      SK1=SK
      SKK1=SKK
      SE1=SE
      SEE1=SEE
      RETURN
C
C
      ENTRY VF_ELFN(QL2,X,CN,SN,DN,Z)
C
C
      ISUB=2
      IF(QL2.EQ.QL) GO TO 2000
      QL=QL2
      GO TO 1000
 2000 CONTINUE
      IF(MOD(X,0.25).EQ.0.) GO TO 450
      X1=MOD(X,1.0)
      IF(ABS(X1).GT.0.5) X1=X1-SIGN(1.0,X1)
      SIG=SIGN(1.0,0.25-ABS(X1))
      IF(SIG.LT.0.) X1=X1-SIGN(0.5,X1)
      IF(IQ) 150,150,350
C
  150 CONTINUE
      XX=P*X1
      C=COSH(XX)
      S=SINH(XX)
      IF(IQ.EQ.0) GO TO 250
      T=S/C
      C2=Q2*(2.*C*C-1.)
      CN=(1.-C2)/C*SIG
      SN=T        *SIG
      DN=(1.+C2)/C
      Z =T+FZZ*XX
      RETURN
C
  250 CONTINUE
      C2=4.*C*C
      S2=4.*S*S
      T4=C*(T04+S2*(Q2+Q6*(C2-1.)))
      CN=FC/T4  *(T03+C2*(-QQ+Q4* S2    ))*SIG
      SN=FS/T4*S*(T04+C2*(-Q2+Q6*(S2+1.)))*SIG
      DN=FD/T4  *(T02+C2*( QQ+Q4* S2    ))
      Z =FZ/T4*S*(1.+TQ2*(S2+3.)+FQ6*((S2+5.)*S2+5.))+FZZ*XX
      RETURN
C
  350 CONTINUE
      XX=P*X1
      C=COS(XX)
      S=SIN(XX)
      C2=4.*C*C
      S2=4.*S*S
      T4=T03-C2*(Q+Q4*S2)
      CN=FC/T4*C*(T02-S2*(Q2+Q6*(C2-1.)))*SIG
      SN=FS/T4*S*(T02-C2*(Q2+Q6*(S2-1.)))*SIG
      DN=FD/T4  *(T04+C2*(Q -Q4* S2    ))
      Z =FZ/T4*C*S*(Q-Q4*(C2-2.))
      RETURN
C
  450 M=X/0.25
      IF(MOD(M,2).NE.0) GO TO 451
      CN=1.
      SN=0.
      DN=1.
      Z=0.
      IF(MOD(M,4).NE.0) CN=-1.
      RETURN
  451 CN=0.
      SN=1.
      DN=RKK
      Z=0.
      IF(MOD(M-1,4).NE.0) SN=-1.
      RETURN
      END
