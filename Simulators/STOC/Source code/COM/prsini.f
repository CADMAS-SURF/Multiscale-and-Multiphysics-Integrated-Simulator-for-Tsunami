C-----------------------------------------------------------
      SUBROUTINE PRSINI(PATM,XC,YC)
C-----------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TYPHOI.h'
      INCLUDE 'TYPHOR.h'
C
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY)
      REAL(8),INTENT(INOUT)::PATM(MX,MY)
C
      REAL(8)::PP,PPP,PS1,PS1O,PS2O,PS3O,PS4O
      REAL(8)::RR1,RR2,RR3,RR4,TPP,TRR,TXX,TYY
      REAL(8)::XO,XT,XX1,XX2,XX3,XX4,XXX,XXX1,XXX2,XXX3,XXX4
      REAL(8)::YO,YT,YY1,YY2,YY3,YY4
      INTEGER::I,J
C
C-----------------------------------------------------------
      PP = 3.14159265358979D0
      PPP=PP/180.D0
C-----------------------------------------------------------
      TXX=TX(1)
      TYY=TY(1)
      TPP=TP(1)
      TRR=TR(1)
C-----------------------------------------------------------
      TRR=1000.D0*TRR
C
      CALL CISTN (TXX,TYY,XLN,YLT,XT,YT)
C
      XO=XT
      YO=YT
      DO 2200 J=2,MYM
      DO 2200 I=2,MXM
        PS1=0.0D0
        XX1=XO+XC(1,I-1,J)
        YY1=YO+YC(1,J-1)
        XX2=XO+XC(1,I  ,J)
        YY2=YO+YC(1,J-1)
        XX3=XO+XC(1,I-1,J)
        YY3=YO+YC(1,J  )
        XX4=XO+XC(1,I  ,J)
        YY4=YO+YC(1,J  )
        RR1=SQRT(XX1**2+YY1**2)
        RR2=SQRT(XX2**2+YY2**2)
        RR3=SQRT(XX3**2+YY3**2)
        RR4=SQRT(XX4**2+YY4**2)
        XXX=RR1/TRR
        PS1=PS1+(1013.D0-TPP)+TPP*EXP(-1.0D0/XXX)
            xxx1=xxx
            ps1o=ps1
        XXX=RR2/TRR
        PS1=PS1+(1013.D0-TPP)+TPP*EXP(-1.0D0/XXX)
            xxx2=xxx
            ps2o=ps1
        XXX=RR3/TRR
        PS1=PS1+(1013.D0-TPP)+TPP*EXP(-1.0D0/XXX)
            xxx3=xxx
            ps3o=ps1
        XXX=RR4/TRR
        PS1=PS1+(1013.D0-TPP)+TPP*EXP(-1.0D0/XXX)
            xxx4=xxx
            ps4o=ps1
        PATM(I,J)=-1013.D0+PS1*0.25D0
 2200 CONTINUE
C-----------------------------------------------------------
      RETURN
      END
