      SUBROUTINE CP_BCVVML(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                     INDU_NS,INDV_NS,INDW_NS,INDP_NS,
     2                     XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     3                     GX_ML,GY_ML,GZ_ML,GX_NS,GY_NS,GZ_NS,GV_NS,
     4                     I_ML,J_ML,K_ML,KF_ML,KG_ML,KF_NS,KG_NS,
     5                     UU_NS,VV_NS,WW_NS,TT_NS,CC_NS,HH_NS,HDEP_NS,
     6                     UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                     CSD_NS,ZBD_NS,CSD_ML,ZBD_ML,
     $                     X1_NS,X2_NS,X1_ML,X2_ML,
     7                     IS,IE,JS,JE,NESTXM,NESTXP,NESTYM,NESTYP,
     8                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     9                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
C-----------------------------------------------------------------------
C     境界流速をMLのエリアに直接セットする
C        LTURB=3 : X1=Q2,X2=QL
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'TIMEI.h'
      INCLUDE 'CONNEC.h'
C
      INTEGER,INTENT(INOUT)::IS,IE,JS,JE,NESTXM,NESTXP,NESTYM,NESTYP
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR,KBOT,KTOP
C
      INTEGER,INTENT(INOUT)::
     $   INDU_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDV_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDW_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDU_NS(MX_NS,MY_NS,MZ_NS),INDV_NS(MX_NS,MY_NS,MZ_NS),
     $   INDW_NS(MX_NS,MY_NS,MZ_NS),INDP_NS(MX_NS,MY_NS,MZ_NS)
C
      REAL(8),INTENT(INOUT)::
     $   XC_ML(8,MX_ML),YC_ML(8,MY_ML),ZC_ML(8,MZ_ML),
     $   XC_NS(8,MX_NS),YC_NS(8,MY_NS),ZC_NS(8,MZ_NS)
      REAL(8),INTENT(INOUT)::
     $   GX_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GY_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GZ_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GX_NS(MX_NS,MY_NS,MZ_NS),GY_NS(MX_NS,MY_NS,MZ_NS),
     $   GZ_NS(MX_NS,MY_NS,MZ_NS),GV_NS(MX_NS,MY_NS,MZ_NS)
C
      INTEGER,INTENT(INOUT)::
     $   I_ML(2,MX_ML),J_ML(2,MY_ML),K_ML(2,MZ_ML)
      INTEGER,INTENT(INOUT)::
     $   KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   KG_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   KF_NS(MX_NS,MY_NS),KG_NS(MX_NS,MY_NS)
C
      REAL(8),INTENT(INOUT)::
     $   UU_NS(MX_NS,MY_NS,MZ_NS),VV_NS(MX_NS,MY_NS,MZ_NS),
     $   WW_NS(MX_NS,MY_NS,MZ_NS),
     $   TT_NS(MX_NS,MY_NS,MZ_NS),CC_NS(MX_NS,MY_NS,MZ_NS),
     $   CSD_NS(MX_NS,MY_NS,MZ_NS),ZBD_NS(MX_NS,MY_NS),
     $   X1_NS(MX_NS,MY_NS,MZ_NS),X2_NS(MX_NS,MY_NS,MZ_NS),
     $   HH_NS(MX_NS,MY_NS),HDEP_NS(MX_NS,MY_NS)
C
      REAL(8),INTENT(INOUT)::
     $   UU_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   VV_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   WW_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   TT_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   CC_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   CSD_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   ZBD_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   X1_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   X2_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   HH_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $   HDEP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1)
C
      INTEGER::IDB=0
C
      INTEGER::I,I1,I2,J,J1,J2,IEM,ISKIP
C
C
      if(istep.eq.1) then
        write(*,*) '# CP_BCVVML is,ie,js,je=',is,ie,js,je
        write(*,*) '    nest(xm,xp,ym,yp)=',nestxm,nestxp,nestym,nestyp
      end if
C      NNN = NESTFL
C
C     <IS.NE.IE>
C
      IF(IS.NE.IE) THEN
C
C     下側境界の処理(J=JS,JE-1:I=IS,IE-1)
C
        IF(JS.NE.JE) THEN
          DO 100 J = JS,JE
            IF(J.LT.JS+NESTYM.AND.IPECON(4,NRANK+1).LT.0) GO TO 100
            IF(J.GT.JE-NESTYP) GO TO 100
            ISKIP = 0
            IF(J.EQ.JS+NESTYM.AND.IPECON(4,NRANK+1).GE.0) ISKIP=1
            J1 = J
            J2 = J-1
            DO 150 I = IS,IE
              IF(ISKIP.EQ.1.AND.I.NE.IS+NESTXM) GO TO 150
              IF(ISKIP.EQ.1.AND.I.EQ.IS+NESTXM.AND.
     &                  IPECON(5,NRANK+1).GE.0) GO TO 150
              IF(I.LT.IS+NESTXM.AND.IPECON(5,NRANK+1).LT.0) GO TO 150
              IF(J.GT.JS+NESTYM.AND.I.GT.IS+NESTXM) GO TO 150
              IF(I.GT.IE-NESTXP) GO TO 150
              IF(I.EQ.IS+NESTXM.AND.
     &           J.GT.JS+NESTYM.AND.IPECON(5,NRANK+1).GE.0) GO TO 150
              I1 = I
              I2 = I-1
      if(idb.ne.0) write(*,*) '100(u-,v-,w) i,j=',i,j
            CALL CP_AVEUVW(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                     INDU_NS,INDV_NS,INDW_NS,INDP_NS,
     2                     XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     3                     GX_ML,GY_ML,GZ_ML,GX_NS,GY_NS,GZ_NS,GV_NS,
     4                     I_ML,J_ML,K_ML,KF_ML,KG_ML,KF_NS,KG_NS,
     5                     UU_NS,VV_NS,WW_NS,TT_NS,CC_NS,HH_NS,HDEP_NS,
     6                     UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                     CSD_NS,ZBD_NS,CSD_ML,ZBD_ML,
     $                     X1_NS,X2_NS,X1_ML,X2_ML,
     7                     I1,I2,J1,J2,
     8                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     9                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
  150       CONTINUE
  100     CONTINUE
        END IF
C
C     <IS.NE.IE>,<JS.NE.JE>
C     上側境界の処理(J=JE,I=IS,IE-1)
C
        IF(IPECON(7,NRANK+1).LT.0) THEN
          J  = JE-NESTYP
          IF(JS.EQ.JE) J=JE
          J1 = J
          J2 = J
          IEM = IE-1
          IF(IPECON(6,NRANK+1).GE.0) IEM=IE
          DO 200 I = IS,IEM
            IF(I.LT.IS+NESTXM) GO TO 200
            IF(JS.EQ.JE.AND.I.GT.IS+NESTXM) GO TO 200
            IF(I.GE.IE-NESTXP+1.AND.
     &         IPECON(6,NRANK+1).LT.0) GO TO 200
            I1 = I
            I2 = I-1
      if(idb.ne.0) write(*,*) '200(u-,v ,w) i,j=',i,j
            CALL CP_AVEUVW(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                     INDU_NS,INDV_NS,INDW_NS,INDP_NS,
     2                     XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     3                     GX_ML,GY_ML,GZ_ML,GX_NS,GY_NS,GZ_NS,GV_NS,
     4                     I_ML,J_ML,K_ML,KF_ML,KG_ML,KF_NS,KG_NS,
     5                     UU_NS,VV_NS,WW_NS,TT_NS,CC_NS,HH_NS,HDEP_NS,
     6                     UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                     CSD_NS,ZBD_NS,CSD_ML,ZBD_ML,
     $                     X1_NS,X2_NS,X1_ML,X2_ML,
     7                     I1,I2,J1,J2,
     8                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     9                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
  200      CONTINUE
         END IF
C
C     <IS.NE.IE>
C     右側境界の処理(I=IE,J=JS,JE)
C
        IF(IPECON(6,NRANK+1).LT.0) THEN
        I  = IE-NESTXP
        I1 = I
        I2 = I
        DO 300 J = JS,JE
          IF(JS.NE.JE) THEN
            IF(J.LT.JS+NESTYM) GO TO 300
            IF(J.GT.JE-NESTYP) GO TO 300
          END IF
          J1 = J
          J2 = J
CC          IF(JS.NE.JE.AND.J.EQ.JE) J2=J-1
          IF(JS.NE.JE.AND.J.EQ.JE-NESTYP+1) J2=J-1
      if(idb.ne.0) write(*,*) '300(u ,v ,w) i,j=',i,j
          CALL CP_AVEUVW(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                   INDU_NS,INDV_NS,INDW_NS,INDP_NS,
     2                   XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     3                   GX_ML,GY_ML,GZ_ML,GX_NS,GY_NS,GZ_NS,GV_NS,
     4                   I_ML,J_ML,K_ML,KF_ML,KG_ML,KF_NS,KG_NS,
     5                   UU_NS,VV_NS,WW_NS,TT_NS,CC_NS,HH_NS,HDEP_NS,
     6                   UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                   CSD_NS,ZBD_NS,CSD_ML,ZBD_ML,
     $                   X1_NS,X2_NS,X1_ML,X2_ML,
     7                   I1,I2,J1,J2,
     8                   MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     9                   IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
  300   CONTINUE
        END IF
C
      ELSE
C
C     <IS.EQ.IE>
C     左(=右)側境界の処理(I=IS,J=JS,JE)
C
        I1 = IS
        I2 = IS
        DO 400 J = JS,JE
CC          IF(J.LT.JS+NESTYM-1) GO TO 400
CC          IF(J.GT.JE-NESTYP+1) GO TO 400
          IF((J.NE.JS+NESTYM).AND.(J.NE.JE-NESTYP)) GO TO 400
          J1 = J
          J2 = J
CC          IF(J.EQ.JE) J2=J-1
CC          IF(J.EQ.JE-NESTYP) J2=J-1
          IF(J.EQ.JS+NESTYM) J2=J-1
      if(idb.ne.0) write(*,*) '400(IS=IE) i,j=',i,j
          CALL CP_AVEUVW(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                   INDU_NS,INDV_NS,INDW_NS,INDP_NS,
     2                   XC_ML,YC_ML,ZC_ML,XC_NS,YC_NS,ZC_NS,
     3                   GX_ML,GY_ML,GZ_ML,GX_NS,GY_NS,GZ_NS,GV_NS,
     4                   I_ML,J_ML,K_ML,KF_ML,KG_ML,KF_NS,KG_NS,
     5                   UU_NS,VV_NS,WW_NS,TT_NS,CC_NS,HH_NS,HDEP_NS,
     6                   UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                   CSD_NS,ZBD_NS,CSD_ML,ZBD_ML,
     $                   X1_NS,X2_NS,X1_ML,X2_ML,
     7                   I1,I2,J1,J2,
     8                   MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     9                   IEAS,IWES,JSOU,JNOR,KBOT,KTOP)
  400   CONTINUE
C
      END IF
C
      RETURN
      END
