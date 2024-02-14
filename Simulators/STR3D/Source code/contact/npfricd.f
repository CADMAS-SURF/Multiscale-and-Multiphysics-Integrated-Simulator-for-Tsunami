      SUBROUTINE NPFRICD(FTI,NINDC,ISLV,INDC,IEDG,IELC,IELQ,IEDQ,IFCQ
     &                  ,IFRIC,U0,RL0,FRIC,UG,DUG,IFMDL,MITER0,IDYN,BETA
     &                  ,DT,ITER)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U0(3,4,NINDC),RL0(3,NINDC),ISLV(2,NINDC),INDC(NINDC)
     &         ,IEDG(6,*),IELC(3,*),UG(6,*),IFRIC(10,NINDC)
     &         ,FRIC(10,NINDC),KN(5),ESTF(120),DUG(6,*),IELQ(4,*)
     &         ,IFCQ(*),IEDQ(*),FTI(6,*),DFC(3,20)
C-----------------------------------------------------------------------
      DO I = 1, NINDC
C
        IF( IFRIC(1,I) == 0 ) CYCLE
C
        NS = INDC(I)
        IST = ISLV(1,I)
        MA  = ISLV(2,I)
C
        IF( IST == 2 ) THEN
          CALL ESTFEDGE(ND,KN,ESTF,NS,IEDG(1,MA),IEDQ(MA),IELQ,U0(1,1,I)
     &                 ,RL0(1,I),FRIC(1,I),FRIC(2,I),FRIC(3,I),FRIC(6,I)
     &                 ,UG,DT,IFMDL)
        ELSEIF( IST == 3 ) THEN
          CALL ESTFFACE(ND,KN,ESTF,NS,IELC(1,MA),IFCQ(MA),IELQ,U0(1,1,I)
     &                 ,RL0(1,I),IFRIC(2,I),IFRIC(3,I),IFRIC(9,I)
     &                 ,FRIC(1,I),FRIC(2,I),FRIC(3,I),FRIC(6,I),UG,DT
     &                 ,IFMDL,MITER0,IDYN,ITER)
        ENDIF
C
        IF( IDYN == 1 ) THEN
          N = 3*ND * ( 3*ND + 1 ) / 2
          ESTF(1:N) = BETA * ESTF(1:N)
        ENDIF
C
        CALL ENPFRCD(DFC,ESTF,DUG,KN,3,ND)
C
        FTI(1:3,KN(1:ND)) = FTI(1:3,KN(1:ND)) + DFC(:,1:ND)
C
      ENDDO
C
      END
