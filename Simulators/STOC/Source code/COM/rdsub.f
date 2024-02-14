C----------------------------------------------------------------------
C     EXTRACT AND READ A PART OF WHOLE ARRAY FROM FILE
C
C     1. RDSUB3IB: FOR 3D INTEGER ARRAY     ( BINARY FILE )
C     2. RDSUB3RB: FOR 3D REAL(8) ARRAY     ( BINARY FILE )
C     3. RDSUB2IB: FOR 2D(XY) INTEGER ARRAY ( BINARY FILE )
C     4. RDSUB2RB: FOR 2D(XY) REAL(8) ARRAY ( BINARY FILE )
C     5. RDSUB3IA: FOR 3D INTEGER ARRAY     (  ASCII FILE )
C     6. RDSUB3RA: FOR 3D REAL(8) ARRAY     (  ASCII FILE )
C     7. RDSUB2IA: FOR 2D(XY) INTEGER ARRAY (  ASCII FILE )
C     8. RDSUB2RA: FOR 2D(XY) REAL(8) ARRAY (  ASCII FILE )
C     9. RDSUB2RAX: SPECIAL VERSION FOR .sbt FILE ( SIMILAR TO 'RDSUB2RA' )
C    10. RDSUB2RBY: SPECIAL VERSION FOR .win FILE ( SIMILAR TO 'RDSUB3RB' )
C    11. RDSUB2RAZ: SPECIAL VERSION FOR .zbd FILE ( SIMILAR TO 'RDSUB2RA' )
C    12. RDSUB3RAW: SPECIAL VERSION FOR .ini FILE ( SIMILAR TO 'RDSUB3RA' )
C
C     <COMMON INPUT>
C     IFL   : FILE NUMBER
C     IFLAG : ARRAY TYPE
C             =0 CELL   CENTER    DEFINED ( ex. P )
C             =1 CELL X-INTERFACE DEFINED ( ex. U )
C             =2 CELL Y-INTERFACE DEFINED ( ex. V )
C             =3 CELL Z-INTERFACE DEFINED ( ex. W )
C     <COMMON OUTPUT>
C     IRTN  : READ STATUS
C             =0 NORMAL
C             =1 ERROR
C             =2 END
C----------------------------------------------------------------------
C### 1 ###
      SUBROUTINE RDSUB3IB(PHYS,IFL,IFLAG,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      INTEGER,INTENT(OUT):: PHYS(MX,MY,MZ)
      INTEGER,INTENT(IN):: IFL,IFLAG
      INTEGER,INTENT(OUT)::IRTN
C
      INTEGER:: XX
      INTEGER:: IS,IE,JS,JE,KS,KE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      KS=2
      KE=MZG-1
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
      IF(IFLAG.EQ.3) KS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,ERR=910,END=920)
     $   ((                (XX,I=IS,IE)                    ,J=JS,J1 ),
     $    ((XX,I=IS,I1),(PHYS(I,J,K),I=IS,MXM),(XX,I=I2,IE),J=JS,MYM),
     $    (                (XX,I=IS,IE)                    ,J=J2,JE ),
     $ K=KS,KE)
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 2 ###
      SUBROUTINE RDSUB3RB(PHYS,IFL,IFLAG,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(OUT):: PHYS(MX,MY,MZ)
      INTEGER,INTENT(IN):: IFL,IFLAG
      INTEGER,INTENT(OUT)::IRTN
C
      REAL(8):: XX
      INTEGER:: IS,IE,JS,JE,KS,KE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      KS=2
      KE=MZG-1
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
      IF(IFLAG.EQ.3) KS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,ERR=910,END=920)
     $   ((                (XX,I=IS,IE)                    ,J=JS,J1 ),
     $    ((XX,I=IS,I1),(PHYS(I,J,K),I=IS,MXM),(XX,I=I2,IE),J=JS,MYM),
     $    (                (XX,I=IS,IE)                    ,J=J2,JE ),
     $ K=KS,KE)
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 3 ###
      SUBROUTINE RDSUB2IB(PHYS,IFL,IFLAG,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      INTEGER,INTENT(OUT):: PHYS(MX,MY)
      INTEGER,INTENT(IN):: IFL,IFLAG
      INTEGER,INTENT(OUT)::IRTN
C
      INTEGER:: XX
      INTEGER:: IS,IE,JS,JE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,ERR=910,END=920)
     $    (              (XX,I=IS,IE)                    ,J=JS,J1 ),
     $    ((XX,I=IS,I1),(PHYS(I,J),I=IS,MXM),(XX,I=I2,IE),J=JS,MYM),
     $    (              (XX,I=IS,IE)                    ,J=J2,JE )
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 4 ###
      SUBROUTINE RDSUB2RB(PHYS,IFL,IFLAG,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(OUT):: PHYS(MX,MY)
      INTEGER,INTENT(IN):: IFL,IFLAG
      INTEGER,INTENT(OUT)::IRTN
C
      REAL(8):: XX
      INTEGER:: IS,IE,JS,JE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,ERR=910,END=920)
     $    (              (XX,I=IS,IE)                    ,J=JS,J1 ),
     $    ((XX,I=IS,I1),(PHYS(I,J),I=IS,MXM),(XX,I=I2,IE),J=JS,MYM),
     $    (              (XX,I=IS,IE)                    ,J=J2,JE )
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 5 ###
      SUBROUTINE RDSUB3IA(PHYS,IFL,IFLAG,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      INTEGER,INTENT(OUT):: PHYS(MX,MY,MZ)
      INTEGER,INTENT(IN):: IFL,IFLAG
      INTEGER,INTENT(OUT)::IRTN
C
      INTEGER:: XX
      INTEGER:: IS,IE,JS,JE,KS,KE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      KS=2
      KE=MZG-1
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
      IF(IFLAG.EQ.3) KS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,*,ERR=910,END=920)
     $   ((                (XX,I=IS,IE)                    ,J=JS,J1 ),
     $    ((XX,I=IS,I1),(PHYS(I,J,K),I=IS,MXM),(XX,I=I2,IE),J=JS,MYM),
     $    (                (XX,I=IS,IE)                    ,J=J2,JE ),
     $ K=KS,KE)
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 6 ###
      SUBROUTINE RDSUB3RA(PHYS,IFL,IFLAG,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(OUT):: PHYS(MX,MY,MZ)
      INTEGER,INTENT(IN):: IFL,IFLAG
      INTEGER,INTENT(OUT)::IRTN
C
      REAL(8):: XX
      INTEGER:: IS,IE,JS,JE,KS,KE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      KS=2
      KE=MZG-1
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
      IF(IFLAG.EQ.3) KS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,*,ERR=910,END=920)
     $   ((                (XX,I=IS,IE)                    ,J=JS,J1 ),
     $    ((XX,I=IS,I1),(PHYS(I,J,K),I=IS,MXM),(XX,I=I2,IE),J=JS,MYM),
     $    (                (XX,I=IS,IE)                    ,J=J2,JE ),
     $ K=KS,KE)
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 7 ###
      SUBROUTINE RDSUB2IA(PHYS,IFL,IFLAG,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      INTEGER,INTENT(OUT):: PHYS(MX,MY)
      INTEGER,INTENT(IN):: IFL,IFLAG
      INTEGER,INTENT(OUT)::IRTN
C
      INTEGER:: XX
      INTEGER:: IS,IE,JS,JE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,*,ERR=910,END=920)
     $    (              (XX,I=IS,IE)                    ,J=JS,J1 ),
     $    ((XX,I=IS,I1),(PHYS(I,J),I=IS,MXM),(XX,I=I2,IE),J=JS,MYM),
     $    (              (XX,I=IS,IE)                    ,J=J2,JE )
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 8 ###
      SUBROUTINE RDSUB2RA(PHYS,IFL,IFLAG,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(OUT):: PHYS(MX,MY)
      INTEGER,INTENT(IN):: IFL,IFLAG
      INTEGER,INTENT(OUT)::IRTN
C
      REAL(8):: XX
      INTEGER:: IS,IE,JS,JE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,*,ERR=910,END=920)
     $    (              (XX,I=IS,IE)                    ,J=JS,J1 ),
     $    ((XX,I=IS,I1),(PHYS(I,J),I=IS,MXM),(XX,I=I2,IE),J=JS,MYM),
     $    (              (XX,I=IS,IE)                    ,J=J2,JE )
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 9 ###
      SUBROUTINE RDSUB2RAX(PHYS,IFL,IFLAG,IS1,IE1,JS1,JE1,INSIDE,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(OUT):: PHYS(MX,MY)
      INTEGER,INTENT(IN):: IFL,IFLAG
      INTEGER,INTENT(INOUT):: IS1,IE1,JS1,JE1
      INTEGER,INTENT(OUT)::INSIDE,IRTN
C
      REAL(8):: XX
      INTEGER:: IS,IE,JS,JE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=IS1
      IE=IE1
      JS=JS1
      JE=JE1
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
      CALL MODIJ(IS1,IE1,JS1,JE1,1,INSIDE)
C
      I1=IS1+MYIS-2 -1
      I2=IE1+MYIS-2 +1
      J1=JS1+MYJS-2 -1
      J2=JE1+MYJS-2 +1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      IF( INSIDE.EQ.0 ) THEN
      READ(IFL,*,ERR=910,END=920) ((XX,I=IS,IE),J=JS,JE)
      ELSE
      READ(IFL,*,ERR=910,END=920)
     $    (              (XX,I=IS,IE)                     ,J=JS,J1  ),
     $    ((XX,I=IS,I1),(PHYS(I,J),I=IS1,IE1),(XX,I=I2,IE),J=JS1,JE1),
     $    (              (XX,I=IS,IE)                     ,J=J2,JE  )
      ENDIF
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 10 ###
      SUBROUTINE RDSUB2RBY(PHYS,IFL,IFLAG,KKMAX,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(4),INTENT(OUT):: PHYS(MX,MY,KKMAX)
      INTEGER,INTENT(IN):: IFL,IFLAG,KKMAX
      INTEGER,INTENT(OUT)::IRTN
C
      REAL(4):: XX,XX2
      INTEGER:: IS,IE,JS,JE,KS,KE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      KS=1
      KE=KKMAX
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,ERR=910,END=920)
     $    (((XX         ,K=KS,KE),I=IS,IE),J=JS,J1),
     $    (((XX         ,K=KS,KE),I=IS,I1),
     $     ((PHYS(I,J,K),K=KS,KE),I=IS,MXM),
     $     ((XX2        ,K=KS,KE),I=I2,IE),J=JS,MYM),
     $    (((XX         ,K=KS,KE),I=IS,IE),J=J2,JE)
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 11 ###
      SUBROUTINE RDSUB2RAZ(PHYS,IFL,IFLAG,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(OUT):: PHYS(MX,MY)
      INTEGER,INTENT(IN):: IFL,IFLAG
      INTEGER,INTENT(OUT)::IRTN
C
      REAL(8):: XX
      INTEGER:: IS,IE,JS,JE
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,*,ERR=910,END=920)
     $    (              (XX,I=IS,IE)                    ,J=JE,J2,-1 ),
     $    ((XX,I=IS,I1),(PHYS(I,J),I=IS,MXM),(XX,I=I2,IE),J=MYM,JS,-1),
     $    (              (XX,I=IS,IE)                    ,J=J1,JS,-1 )
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
C
C
C### 12 ###
      SUBROUTINE RDSUB3RAW(PHYS,IFL,IFLAG,KKMAX,KKMAX1,KKMAX2,IRTN)
      IMPLICIT NONE
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(OUT):: PHYS(MX,MY,KKMAX)
      INTEGER,INTENT(IN):: IFL,IFLAG,KKMAX,KKMAX1,KKMAX2
      INTEGER,INTENT(OUT)::IRTN
C
      REAL(8):: XX
      INTEGER:: IS,IE,JS,JE,KS,KE1,KE2
      INTEGER:: I,J,K, I1,I2,J1,J2
C
      IRTN=0
      IS=2
      IE=MXG-1
      JS=2
      JE=MYG-1
      KS=2
      KE1=KKMAX1
      KE2=KKMAX2
      IF(IFLAG.EQ.1) IS=1
      IF(IFLAG.EQ.2) JS=1
C
      I1=MYIS-1
      I2=MYIE+1
      J1=MYJS-1
      J2=MYJE+1
      IF(IFLAG.EQ.1) I1=I1-1
      IF(IFLAG.EQ.2) J1=J1-1
C
      READ(IFL,*,ERR=910,END=920)
     $   ((                (XX,I=IS,IE)                    ,J=JS,J1 ),
     $    ((XX,I=IS,I1),(PHYS(I,J,K),I=IS,MXM),(XX,I=I2,IE),J=JS,MYM),
     $    (                (XX,I=IS,IE)                    ,J=J2,JE ),
     $ K=KS,KE1),
     $   (((XX,I=IS,IE),J=JS,JE),K=KE1+1,KE2)
C
      RETURN
  910 CONTINUE
      IRTN=1
      RETURN
  920 CONTINUE
      IRTN=2
      RETURN
      END
