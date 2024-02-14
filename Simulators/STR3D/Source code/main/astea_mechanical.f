      SUBROUTINE ASTEA_MECHANICAL(KK,RR,IFL,FLNAME,NLEN,ICK)
C
      USE MPI_PARAM
      USE M_VAL
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*256 FLNAME
      CHARACTER*3 PNO
C
      DIMENSION KK(*),RR(*),IFL(*),ICK(*)
C
      ITO = IFL(11)
C
      IF( MYRANK > 0 ) THEN
        WRITE(PNO,'(I3.3)') MYRANK
        OPEN(ITO,FILE=FLNAME(1:NLEN)//'.log_'//PNO)
      ENDIF
C
      IF( KK(92) > 0 ) THEN
        IF( MYRANK == 0 ) THEN
          CALL CONT_TBL(KK,ITO)
        ELSE
          CALL CONT_TBL_P(KK)
        ENDIF
      ENDIF
C
      KK(30) = 1
C
      CALL NCGSET(KK,IELM,KK(37))
C
      CALL ALLOC(KK)
C
      CALL DFLT_DAMP(AMAT,KK(11),GRID,IELM,KK(37),KK(12),RR(5),ITO)
C
      I_TSTEP = ISUB(12,1)
      I_BCT   = ISUB(13,1)
C
      CALL TSTEPSET(I_TSTEP,KK)
C
      IF( ICPL == 2 ) THEN
        CALL RECV_RST(KK)
      ELSE
        CALL RST_CNTL(KK)
      ENDIF
C
      IF( MYRANK == 0 ) THEN
        IF( KK(83) > 0 ) THEN
          OPEN(IFL(16),FILE=FLNAME(1:NLEN)//'.rsto'
     &        ,FORM='UNFORMATTED')
          OPEN(IFL(18),FILE=FLNAME(1:NLEN)//'.rtm')
        ENDIF
        IF( KK(84) > 0 )
     &    OPEN(IFL(17),FILE=FLNAME(1:NLEN)//'.rsti'
     &        ,FORM='UNFORMATTED')
      ELSE
        IF( KK(83) > 0 ) 
     &    OPEN(IFL(16),FILE=FLNAME(1:NLEN)//'.rsto'//PNO
     &        ,FORM='UNFORMATTED')
        IF( KK(84) > 0 ) 
     &    OPEN(IFL(17),FILE=FLNAME(1:NLEN)//'.rsti'//PNO
     &        ,FORM='UNFORMATTED')
      ENDIF
C
      IF( I_BCT > 0 )
     &  CALL BCTSET(ICTB,FRTB,I_BCT,KK,KK(92),ICPA,NCPA,ICPR,NCPR,CPR
     &             ,ICRG,ITO)
C
      IF( ICPL == 2 ) 
     &  CALL SEND_CONT(I_BCT,KK(92),KK(93),ICRG,ICSF,ICTB)
C
      IF( ICPL == 2 ) THEN
        CALL RECV_SURF(KK,RR)
      ELSE
        CALL RDPRESS(KK,RR,IFL(14),FLNAME,NLEN)
      ENDIF
C
      IF( ISTM == 1 ) THEN
        IF( MYRANK == 0 ) THEN
          CALL RMSH_INIT(IRMSH,RMSH,KK(8),KK(81),IRF,IRND,IPFC,GRID,ITO)
        ELSE
          CALL RMSH_INITP(IRMSH,RMSH,KK(8),IRND,GRID,ITO)
        ENDIF
      ENDIF
C
      IF( ICK(3) > 0 ) THEN
        IF( MYRANK == 0 ) THEN
          CALL ENS_GEOM(KK,IFL(20),FLNAME,NLEN,ICK(3))
          CALL ENS_CASE(KK,IOUT,D_T,IFL(20),FLNAME,NLEN,ICK(3))
        ELSEIF( MYRANK == 1 ) THEN
          CALL M_MPI_SEND_I(6,1,0)  ! SEND IOP=6 TO GLB_COMM
        ENDIF
      ENDIF
C
      CALL ELEM_INIT(KK)
C
      IF( I_BCT > 0 ) THEN
        IF( MYRANK == 0 ) THEN
          CALL CONT_INIT(ISLV,RSLV,POS,POSO,EDML,IFRIC,KK,KK(92),ICBD
     &                  ,IELC,INDA,INDC,IEDA,IEDG,IELQ,IFCQ,IEDQ,IVRQ
     &                  ,ICTB,GRID,IELM,IBEL,WRK3,RR(14))
        ELSE
          CALL CONT_INIT_P(ISLV0,ISLVP,RSLV0,PSLV,ISTICK,POS,POSO,KK
     &                    ,KK(92),GRID,IELM,KK(37),IBEL,INDA0,ICBD,IELC
     &                    ,IELCB,INDA,INDC,IEDA,IEDG,IEDGB,ICTB,RR(14))
        ENDIF
      ELSE
        POS(:,:) = GRID(:,:)
      ENDIF
C
      IF( KK(1) == 0 ) THEN
C
        IF( KK(84) <= 0 ) CALL NL_STATIC(KK,RR,IFL,FLNAME,NLEN,ICK)
C
        CALL NL_TRANSIENT(KK,RR,IFL,FLNAME,NLEN,ICK)
C
      ELSE
C
        IF( KK(84) <= 0 ) CALL NL_STATIC_S(KK,RR,IFL,FLNAME,NLEN,ICK)
C
        CALL NL_TRANSIENT_S(KK,RR,IFL,FLNAME,NLEN,ICK)
C
      ENDIF
C
      IF( MYRANK > 0 ) CLOSE(ITO)
C
      IF( KK(83) > 0 ) THEN
        CLOSE(IFL(16))
        IF( MYRANK == 0 ) CLOSE(IFL(18))
      ENDIF
C
      IF( KK(84) > 0 ) CLOSE(IFL(17))
C
      IF( MYRANK == 0 ) THEN
        CALL M_MPI_BCAST_I(0,1)
      ELSEIF( MYRANK == 1 ) THEN
        CALL M_MPI_SEND_I(0,1,0)
      ENDIF
C
      END
