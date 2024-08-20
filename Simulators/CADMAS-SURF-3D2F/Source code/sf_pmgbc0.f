      SUBROUTINE SF_PMGBC0(BCU0,BCV0,BCW0,BCP0,BCF0,INDX0,INDY0
     &                    ,BCU,BCV,BCW,BCP,BCF,INDX,INDY)

      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION BCU0(NUMB0,3),BCV0(NUMB0,3),BCW0(NUMB0,3),BCP0(NUMB0,3)
      DIMENSION BCF0(NUMB0)
      DIMENSION INDX0(NUMI,NUMJ,NUMK),INDY0(NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB,3),BCV(NUMB,3),BCW(NUMB,3),BCP(NUMB,3)
      DIMENSION BCF(NUMB)
      DIMENSION INDX(NUMI,NUMJ,NUMK),INDY(NUMI,NUMJ,NUMK)

      IF (MGPRNK.GE.0) THEN
        IS=2
        JS=2
        IE=NUMI-1
        JE=NUMJ-1
        IF (MGPINF(4).EQ.0) THEN
          DO 410 K=2,NUMK-1
            DO 400 J=JS,JE
              L0=INDX0(IS,J,K)
              L=INDX(IS,J,K)
              IF(L0.GE.1 .AND. L.GE.1) THEN
                BCU0(L0,2:3)=BCU(L,2:3)
                BCV0(L0,2:3)=BCV(L,2:3)
                BCW0(L0,2:3)=BCW(L,2:3)
                BCP0(L0,2:3)=BCP(L,2:3)
                BCF0(L0)=BCF(L)
              ELSEIF(L0.GE.1 .OR. L.GE.1) THEN
                CALL VF_A2ERR('SF_PMGBC0','P.G ERROR.')
              ENDIF
 400        CONTINUE
 410      CONTINUE
        ENDIF
        IF (MGPINF(7).EQ.0) THEN
          DO 460 K=2,NUMK-1
            DO 450 J=JS,JE
              L0=INDX0(IE+1,J,K)
              L=INDX(IE+1,J,K)
              IF(L0.GE.1 .AND. L.GE.1) THEN
                BCU0(L0,2:3)=BCU(L,2:3)
                BCV0(L0,2:3)=BCV(L,2:3)
                BCW0(L0,2:3)=BCW(L,2:3)
                BCP0(L0,2:3)=BCP(L,2:3)
                BCF0(L0)=BCF(L)
              ELSEIF(L0.GE.1 .OR. L.GE.1) THEN
                CALL VF_A2ERR('SF_PMGBC0','P.G ERROR.')
              ENDIF
 450        CONTINUE
 460      CONTINUE
        ENDIF
        IF (MGPINF(5).EQ.0) THEN
          DO 510 K=2,NUMK-1
            DO 500 I=IS,IE
              L0=INDY0(I,JS,K)
              L=INDY(I,JS,K)
              IF(L0.GE.1 .AND. L.GE.1) THEN
                BCU0(L0,2:3)=BCU(L,2:3)
                BCV0(L0,2:3)=BCV(L,2:3)
                BCW0(L0,2:3)=BCW(L,2:3)
                BCP0(L0,2:3)=BCP(L,2:3)
                BCF0(L0)=BCF(L)
              ELSEIF(L0.GE.1 .OR. L.GE.1) THEN
                CALL VF_A2ERR('SF_PMGBC0','P.G ERROR.')
              ENDIF
 500        CONTINUE
 510      CONTINUE
        ENDIF
        IF (MGPINF(8).EQ.0) THEN
          DO 560 K=2,NUMK-1
            DO 550 I=IS,IE
              L0=INDY0(I,JE+1,K)
              L=INDY(I,JE+1,K)
              IF(L0.GE.1 .AND. L.GE.1) THEN
                BCU0(L0,2:3)=BCU(L,2:3)
                BCV0(L0,2:3)=BCV(L,2:3)
                BCW0(L0,2:3)=BCW(L,2:3)
                BCP0(L0,2:3)=BCP(L,2:3)
                BCF0(L0)=BCF(L)
              ELSEIF(L0.GE.1 .OR. L.GE.1) THEN
                CALL VF_A2ERR('SF_PMGBC0','P.G ERROR.')
              ENDIF
 550        CONTINUE
 560      CONTINUE
        ENDIF
      ENDIF

      END
