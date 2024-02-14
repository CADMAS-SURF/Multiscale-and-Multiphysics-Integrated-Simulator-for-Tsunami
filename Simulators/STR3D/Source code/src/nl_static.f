      SUBROUTINE NL_STATIC(KK,RR,IFL,FLNAME,NLEN,ICK)

      USE MPI_PARAM
      USE M_VAL

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*256 FLNAME
      DIMENSION KK(*),RR(*),IFL(*),FNRM(3),ICK(*)

      ITO = IFL(11)

      MAXITER = KK(6)
      MAXITER2 = 20

      I_SPC   = ISUB(2,1)
      I_DLOAD = ISUB(7,1)
      I_BCT   = ISUB(13,1)

      INDOF0(:,:) = 0

      CALL SPCSET(I_SPC,INDOF0,KK,IELM,KK(37),ISPA,NSPA,ISP1,NSP1,ISPC
     &           ,NSPC,SPC,DUM,1,0,ITO)

      CALL TSPCDSETI(INDOF0,I_DLOAD,KK,IDLD,LIDL,ITL1,ISPD,NSPD,0)

      IF( KK(25) > 0 ) 
     &  CALL PCNSTRI(INDOP0,IELM,KK(8),KK(12),KK(37))

      IF( I_BCT == 0 ) 
     &  CALL MKINDEX(KK,I_BCT,FLNAME,NLEN,IFL(12),IFL(13),ITO)

      UG3(:,:) = 0.

      IF( KK(25) > 0 ) PG3(:) = 0.

      FCP(:,:,:) = 0.

      EPSG(:,:) = 0.
      SIGG(:,:) = 0.

      IF( ISTM == 1 ) SUMZ(:) = 0.

      IF( ICPL == 1 ) THEN
        CALL STPRESS(PPND,KK(8),KK(82),PTIM,PND,0.D0)
      ELSEIF( ICPL == 2 ) THEN
        CALL RECV_PRES(AFC,IPND,PPND,TNEXT,D_T,KK(7),KK(8),KK(81),0)
      ENDIF

      IF( KK(25) > 0 ) THEN
        IF( MYRANK == 0 ) THEN
          CALL MDPRESS(IPND,PPND,KK(8),KK(98),GRID,POS,PG3,IELC,GELC
     &                ,IELG,0.D0,RR(8),RR(9),KK(1))
        ELSE
          CALL MDPRESSP(IPND,PPND,KK(8),KK(26),KK(98),GRID,POS,PG3,IELC
     &                 ,IELCB,GELC,IELG,0.D0,RR(8),RR(9),KK(1))
        ENDIF
      ENDIF

      CALL TSPCDSETU(DUG,I_DLOAD,KK,IDLD,SDLD,SIDL,LIDL,ITL1,ITD1,TBD1
     &              ,ISPD,NSPD,SPCD,UG1,UG3,0.D0,DT1,DT2,0)

      IF( KK(25) > 0 ) CALL PCNSTR(DPG,KK(8),INDOP0,IPND,PPND,PG3)

      CALL TLOADSET(FTO,FCO,WRK1,WRK2,KK(8),0.D0,RR(4),I_DLOAD,KK,ICRD
     &             ,TRNS,GRID,IELM,AMAT,RODA,BARD,IDLD,SDLD,SIDL,LIDL
     &             ,ITL1,ITD1,TBD1,LOAD,SLOD,SILD,LILD,IFC,NFC,FC,IPL4
     &             ,NPL4,PLD4,IGRV,GRAV,IRFC,RFRC,IPFC,AFC,IPND,PPND,POS
     &             ,0,ICPL,ITO)

      FNRM(:) = 0.

      DO ITER2 = 1, MAXITER2

        IF( MYRANK > 0 .AND. I_BCT > 0 ) THEN
          CALL RECV_TBLX(KK)
          CALL COMM_TBLX(KK)
          CALL FRICINT(IFRIC,KK(26),KK(95)+KK(103),ISLV,INDC,IELC,IEDG
     &                ,IELQ,IFCQ,IEDQ)
        ENDIF

        IF( I_BCT > 0 ) 
     &    CALL MKINDEX(KK,I_BCT,FLNAME,NLEN,IFL(12),IFL(13),ITO)

        DO ITER = 1, MAXITER + 1

          IF( I_BCT > 0 ) THEN

            CALL MPCCORR(UG3,POS,ISLV,RSLV,KK(95)+KK(103),KK(86),INDOF
     &                  ,IELC,INDC,IEDG,IRANK)

            IF( KK(87) > 0 )
     &        CALL RMPCCAL(RMPC,KK(95)+KK(103),ISLV,INDC,IEDG,IELC,IVRQ
     &                    ,IEDQ,IFCQ,POS,ITO)

            IF( KK(96) > 0 .AND. ITER == 1 )
     &        CALL FRICSET(U0,RL0,IFRIC,KK(95)+KK(103),ISLV,INDC,IEDG
     &                    ,IELC,UG3,POS,0,ITER2,0)

          ENDIF

          IF( MYRANK > 0 .AND. I_BCT > 0 ) THEN
            N4 = KK(8) + KK(28) + KK(94) + KK(108) + KK(31)
          ELSE
            N4 = KK(8)
          ENDIF

          CALL NPFORCE(FTI,FCK,FCD,FCM,FCMD,FCP(1,1,2),EPSG,SIGG,KK(36)
     &                ,KK,N4,KK(33),RR,GRID,IELM,KK(37),AMAT,RODA,BARD
     &                ,BVEC,DMT,UG3,VG,PPND,DT1,DT2,ITER,ITER2,0,ITO)

          IF( KK(25) > 0 )
     &      CALL NPFLOW(FTI,FLI,FLO,FCP,VELG,KK(36),VELE,KK,N4,RR,GRID
     &                 ,IELM,KK(37),AMAT,UG1,UG2,UG3,PG1,PG2,PG3,DT1
     &                 ,DT2,0,ITO)

          IF( KK(96) > 0 )
     &      CALL NPFRIC(IFRIC,FRIC,FRCI,FCK(1,1,3),FTI,WRK1,N4
     &                 ,KK(95)+KK(103),KK(86),INDG,ISLV,INDC,IEDG,IELC
     &                 ,IELQ,IEDQ,IFCQ,IRANK,U0,RL0,UG3,POS,FTO,FTID
     &                 ,KK(96),KK(111),KK(112),KK(113),RR(10),RR(11),0
     &                 ,RR(4),1.D0,0,ITER2,ITER)

          CALL RESFRC(RHV,RFCO,RFCI,ICONV,KK,RR,INDG,INDOF,INDMPC,MPCF
     &               ,RMPC,FTO,FTI,INDOP,FLO,FLI,FNRM,0.D0,0,ITER-1
     &               ,ITER2,ITO)

          IF( ICONV == 1 ) THEN
            EXIT
          ELSEIF( ITER == MAXITER + 1 ) THEN
            WRITE(ITO,*) 'NONLINEAR ITERATION LOOP DID NOT CONVERGE.'
            CALL ERRSTP(40,ITO)
          ENDIF

          CALL GLBSTF(STF,RHV,KK,KK(33),KK(34),KK(35),RR,GRID,IELM
     &               ,KK(37),AMAT,RODA,BARD,BVEC,INDOF,INDMPC,MPCF,RMPC
     &               ,IDSK,IDCG,DUG,UG3,VG,DMT,SIGG,KK(36),DT1,DT2
     &               ,ITER,ITER2,I_BCT,0,ITO)

          IF( KK(25) > 0 )
     &      CALL GLBSTFG(STF,RHV,KK,KK(34),KK(35),RR,GRID,IELM,KK(37)
     &                  ,AMAT,INDOF,INDMPC,MPCF,RMPC,INDOP,IDSK,IDCG
     &                  ,DUG,DPG,DT1,DT2,ITER,ITER2,I_BCT,0,ITO)

          IF( KK(96) > 0 )
     &      CALL FRICSTF(STF,RHV,KK(33),KK(34),KK(35),KK(95)+KK(103)
     &                  ,ISLV,INDC,IEDG,IELC,IELQ,IEDQ,IFCQ,IFRIC,U0,RL0
     &                  ,FRIC,UG3,DUG,INDOF,INDMPC,MPCF,RMPC,IDSK,IDCG
     &                  ,KK(21),KK(96),KK(110),0,RR(4),1.D0,ITER,ITER2
     &                  ,ITO)

          IF( ITER2 == 1 .AND. ITER == 1 ) 
     &      CALL FNORM(FNRM,KK,INDOF,INDOP,RHV,KK(21))

          SELECT CASE( KK(21) )
          CASE( 1 )
            CALL ICCG(STF,LOW,RHV,X,CGWK,KK(27),KK(19),KK(20),IDSK,IDCG
     &               ,INDOF,6,KK(22),RR(1),ITO,KK(23),I_BCT)
          CASE( 11 )
            CALL BCGSTB(STF,RHV,X,CGWK,KK(27),KK(19),IDSK,IDCG,INDOF
     &                 ,INDOP,KK(22),RR(1),I_BCT,ITO)
          CASE( 2 )
C            CALL ASESLV(KK(19),STF,X,ASEWK,RHV,SYM,NUM,IDSK,IDCG
C     &                 ,IASEWK,IFL(12),IFL(13),0.D0,0,0,0,0,ITO)
          CASE( 3, 13 )
            CALL PARSLV(STF,RHV,X,KK(19),KK(20),IDSK,IDCG,PT,IPARM
     &                 ,KK(21),ITER,ITO)
          CASE( 4, 14 )
            CALL MUMPS(X,KK(19),ITO)
          END SELECT

          CALL MERGDU(UGP,VGP,UG3,VG,DUG,X,VG0,INDOF,INDMPC,MPCF,RMPC
     &               ,POS,IELM,KK(37),IBEL,IELQ,I_BCT,PG3,DPG,INDOP,KK
     &               ,ITER,ITER2,KK(2))

          IF( KK(96) > 0 ) THEN
            CALL NPFORCED(FTID,KK,N4,RR,GRID,IELM,KK(37),AMAT,RODA
     &                   ,BARD,BVEC,DUG,UGP,VGP,FTI,DMT,SIGG,KK(36)
     &                   ,DT1,DT2,0,ITO)
            IF( KK(25) > 0 )
     &        CALL NPFLOWD(FTID,KK,RR,GRID,IELM,KK(37),AMAT,DPG,0,ITO)
            CALL NPFRICD(FTID,KK(95)+KK(103),ISLV,INDC,IEDG,IELC,IELQ
     &                  ,IEDQ,IFCQ,IFRIC,U0,RL0,FRIC,UGP,DUG,KK(96)
     &                  ,KK(110),0,RR(4),1.D0,ITER)
          ENDIF

        ENDDO

        IF( I_BCT > 0 ) THEN

          CALL CNTRFC(RFCI,N4,KK(95)+KK(103),KK(87),KK(86),FTI,FTO,INDC
     &               ,IRANK,WRK1)

          IF( MYRANK == 0 ) THEN
            CALL CONTACT(ICONV2,ISLV,ISLVO,RSLV,IFRIC,FRIC,ISTK,KK(8)
     &                  ,KK(92),KK(97),KK(95),KK(94),KK(96),INDG,ICBD
     &                  ,IELC,INDA,INDC,IEDA,IEDG,IELA,ICELA,ICEL,IBTE
     &                  ,ICEDA,ICED,ICFCA,ICFC,GELC,IELQ,IFCQ,IEDQ,IVRQ
     &                  ,ICTB,FRTB,EDML,RR(12),RR(13),RR(15),POS,POSO
     &                  ,RFCI,WRK1,WRK2,WRK3,0,ITER2,MAXITER2,0,ITO)
          ELSE
            CALL CONTACTP(ICONV2,ISLV0,ISLVP,RSLV0,PSLV,ISTICK,KK(26)
     &                   ,KK(8),KK(28),KK(12),KK(29),KK(92),KK(94)
     &                   ,KK(108),KK(102),KK(95),KK(96),IELM,KK(37),IBEL
     &                   ,IBTE,ICBD,IELC,IELCB,INDA0,IEDG,IELA,ICELA
     &                   ,ICEL,ICEDA,ICED,ICFCA,ICFC,GELC,ICTB,IFRIC
     &                   ,FRIC,U0,RL0,RR(13),RR(15),POS,POSO,RFCI,0
     &                   ,ITER2,MAXITER2,0,ITO)
          ENDIF

          CALL CLSINDEX(KK,IFL(12),IFL(13))

        ELSE

          ICONV2 = 1
 
        ENDIF

        IF( ICONV2 == 1 ) EXIT

      ENDDO

      CALL FEMAP_OUT(KK,INDG,GRID,IELM,DT1,DT2,UG1,UG2,UG3,SUMZ,WRK2
     &              ,WRK1,WRK3,RFCO,PPND,PG3,EPSG,SIGG,VELG,VELE,0,0.D0
     &              ,0,ITO,IFL(15))

      IF( ICK(3) > 0 ) THEN
        IF( MYRANK == 0 ) THEN
          CALL ENS_CASE_T(KK,IFL(20),FLNAME,NLEN,ICK(3))
        ELSEIF( MYRANK == 1 ) THEN
          CALL M_MPI_SEND_I(7,1,0)  ! SEND IOP=7 TO GLB_COMM
        ENDIF
        CALL OUTPUT(KK,KK(8),KK(94),KK(36),GRID,IELM,IELQ,IVRQ,DT1,DT2
     &             ,UG1,UG2,UG3,SUMZ,WRK2,WRK1,WRK3,RFCO,PG3,PPND,AFC
     &             ,EPSG,SIGG,VELG,VELE,N_PART,NP_ENS,NE_ENS,IE_ENS
     &             ,NG_ENS,IG_ENS,NG_ENSW,IG_ENSW,NE_ENSW,IE_ENSW,0,ITO
     &             ,IFL,FLNAME,NLEN,ICK(3))
      ENDIF

      IF( ICPL == 2 ) 
     &  CALL SEND_POS(ISEND,KK(25),KK(8),KK(26),KK(10),POS,VELE,0.D0
     &               ,TNEXT)

      IF( I_BCT == 0 ) CALL CLSINDEX(KK,IFL(12),IFL(13))

      END
