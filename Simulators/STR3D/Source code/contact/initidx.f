      SUBROUTINE INITIDX(IROW_L,ICOL_L,IROW_R,ICOL_R,NSIZ_L,NSIZ_R,IDEP
     &                  ,IDX,IRH,INDP,NIDEP,NINDP,ITO)
C
      DIMENSION IROW_L(*),ICOL_L(*),IROW_R(*),ICOL_R(*),IDEP(2,NIDEP)
     &         ,IDX(2,NIDEP),IRH(2,*),INDP(2,NINDP)
C-----------------------------------------------------------------------
      NROW=NIDEP
C
      IROW_L(1)=1
      IROW_R(1)=1
C
      DO 100 I=1,NROW
C
        NL=0
        NR=0
        IPL=IROW_L(I)
        IPR=IROW_R(I)
C
        ICOL_L(IPL)=I
        NL=NL+1
C
        IS=IDX(1,I)
        IE=IDX(2,I)
C
        DO 110 J=IS,IE
C
          NODE=IRH(1,J)
          IFR =IRH(2,J)
C
          DO 111 K=1,NIDEP
            IF( NODE .EQ. IDEP(1,K) .AND. IFR .EQ. IDEP(2,K) ) THEN
              CALL ORDER(NL,ICOL_L(IPL),K)
              GOTO 110
            ENDIF
  111     CONTINUE
C
          DO 112 K=1,NINDP
            IF( NODE .EQ. INDP(1,K) .AND. IFR .EQ. INDP(2,K) ) THEN
              CALL ORDER(NR,ICOL_R(IPR),K)
              GOTO 110
            ENDIF
  112     CONTINUE
C
          WRITE(ITO,*) 'STOP IN SUB. INITIDX!'
          CALL ERRSTP(90,ITO)
C
  110   CONTINUE
C
        IROW_L(I+1)=IROW_L(I)+NL
        IROW_R(I+1)=IROW_R(I)+NR
C
  100 CONTINUE
C
      NSIZ_L=IROW_L(NROW+1)-1
      NSIZ_R=IROW_R(NROW+1)-1
C
      RETURN
      END
