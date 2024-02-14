      SUBROUTINE NPFRIC(IFRIC,FRIC,FRCI,FCK,FTI,RFCI,NNOD,NINDC,NRANK
     &                 ,INDG,ISLV,INDC,IEDG,IELC,IELQ,IEDQ,IFCQ,IRANK,U0
     &                 ,RL0,UG,POS,FTO,FTID,IFMDL,MITER1,MITERD0,MITERD1
     &                 ,EPS0,EPSD,IDYN,BETA,DT,ISTEP,ITER2,ITER)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U0(3,4,NINDC),RL0(3,NINDC),ISLV(2,NINDC),INDC(NINDC)
     &         ,IEDG(6,*),IELC(3,*),UG(6,*),POS(3,*),IFRIC(10,NINDC)
     &         ,FRIC(10,NINDC),FRCI(3,NNOD),IELQ(4,*),IFCQ(*),IEDQ(*)
     &         ,RFC(3),FTI(6,NNOD),FTO(6,NNOD),FCK(6,NNOD),FTID(6,NNOD)
     &         ,INDG(*),VR(3),FT(3),IRANK(NINDC),RFCI(3,NNOD)
      DATA ICHK / 0 /
C-----------------------------------------------------------------------
      IF( ISTEP == 0 .AND. ITER2 == 1 ) RETURN
C
      IF( ICHK == 1 ) THEN
        WRITE(52,'(A,I6,2I3)') 'STEP, ITER2, ITER = ',ISTEP,ITER2,ITER
        WRITE(52,'(3A)')
     &    '    INDG ISTK  MDL MODE IBDY JBDY    RMU0      RMUD  '
     &   ,'     FN      RMU*FN     AVR      VR(1)     VR(2)     VR(3)  '
     &   ,'   FT(1)     FT(2)     FT(3)'
      ENDIF
C
      IF( ITER == 1 ) THEN
        DO I = 1, NINDC
          IF( IFRIC(8,I) == 1 ) THEN
            NS = INDC(I)
            FTID(:,NS) = FTI(:,NS)
          ENDIF
        ENDDO
      ENDIF
C
      FRCI(:,:) = 0.
      RFCI(:,:) = 0.
C
      DO IR = NRANK, 1, -1
C
        DO I = 1, NINDC
C
          IF( IRANK(I) /= IR ) CYCLE
C
          IF( ISLV(1,I) == 0 ) CYCLE
C
          NS = INDC(I)
          IST = ISLV(1,I)
          MA  = ISLV(2,I)
C
          RFC(:) = FTID(1:3,NS) - FTO(1:3,NS) - RFCI(:,NS)
C
          IF( IST == 1 ) THEN
            RFCI(:,MA) = RFCI(:,MA) - RFC(:)
          ELSEIF( IST == 2 ) THEN
            CALL RFCEDGE(RFCI,NS,IEDG(1,MA),IEDQ(MA),IELQ,POS,RFC)
          ELSEIF( IST == 3 ) THEN
            CALL RFCFACE(RFCI,NS,IELC(1,MA),IFCQ(MA),IELQ,POS,RFC)
          ENDIF
C
          IF( IFRIC(1,I) == 0 ) CYCLE
C
          IF( IST == 2 ) THEN
            CALL NPFEDGE(IFRIC(2,I),FRIC(3,I),FRIC(6,I),FRCI,NS
     &                  ,IEDG(1,MA),IEDQ(MA),IELQ,U0(1,1,I),RL0(1,I)
     &                  ,FRIC(1,I),FRIC(2,I),FRIC(4,I),UG,POS,RFC,DT
     &                  ,IFMDL,EPSD,IDYN)
          ELSEIF( IST == 3 ) THEN
            CALL NPFFACE(IFRIC(2,I),FRIC(3,I),FRIC(5,I),FRIC(6,I)
     &                  ,FRIC(7,I),FRCI,VR,FT,NS,IELC(1,MA),IFCQ(MA)
     &                  ,IELQ,U0(1,1,I),RL0(1,I),IFRIC(3,I),IFRIC(7,I)
     &                  ,IFRIC(9,I),FRIC(1,I),FRIC(2,I),FRIC(4,I),UG,POS
     &                  ,RFC,DT,IFMDL,MITER1,MITERD0,MITERD1,EPS0,EPSD
     &                  ,IDYN,ITER)
          ENDIF
C
          IF( ICHK == 1 .AND. IST == 3 ) THEN
            IF( IFRIC(3,I) == 1 ) THEN
              RMU = FRIC(1,I)
            ELSE
              RMU = FRIC(4,I)
            ENDIF
            WRITE(52,'(I8,5I5,1P11E10.2)') INDG(NS),IFRIC(3,I)
     &       ,IFRIC(9,I),IFRIC(2,I),IFRIC(5,I),IFRIC(6,I),FRIC(1,I),
     &       FRIC(4,I),FRIC(3,I),RMU*FRIC(3,I),FRIC(5,I),VR(:),FT(:)
          ENDIF
C
        ENDDO
C
      ENDDO
C
      FCK(1:3,:) = FCK(1:3,:) + FRCI(:,:)
C
      IF( IDYN == 0 ) THEN
        FTI(1:3,:) = FTI(1:3,:) + FRCI(:,:)
      ELSE
        FTI(1:3,:) = FTI(1:3,:) + BETA * FRCI(:,:)
      ENDIF
C
      END
