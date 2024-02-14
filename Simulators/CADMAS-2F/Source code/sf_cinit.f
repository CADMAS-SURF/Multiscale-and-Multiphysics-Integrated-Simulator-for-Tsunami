      SUBROUTINE SF_CINIT(XX,YY,ZZ,UU,VV,WW,PP,FF,FX,FY,FZ,ANU,GGV,GGX
     &                   ,GGY,GGZ,BCU,BCV,BCW,BCP,BCF,BCVI,DMTBTT,DMTBZZ
     &                   ,DMTBHH,DMTBUN,DMTBUT,DBUF,WK01,WK02,WK03,NF
     &                   ,INDX,INDY,INDZ,INDB,INDS,IBUF,NFP,INDXP,INDYP
     &                   ,INDZP,GGXP,GGYP,GGZP)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
     &         ,UU(NUMI,NUMJ,NUMK),VV(NUMI,NUMJ,NUMK),WW(NUMI,NUMJ,NUMK)
     &         ,PP(NUMI,NUMJ,NUMK),FF(NUMI,NUMJ,NUMK),FX(NUMI,NUMJ,NUMK)
     &         ,FY(NUMI,NUMJ,NUMK),FZ(NUMI,NUMJ,NUMK),BCU(NUMB,3)
     &         ,BCV(NUMB,3),BCW(NUMB,3),BCP(NUMB),BCF(NUMB),BCVI(NUMB)
     &         ,DMTBTT(MTBTT),DMTBZZ(MTBZZ),DMTBHH(MTBTT)
     &         ,DMTBUN(MTBZZ,MTBTT),DMTBUT(MTBZZ,MTBTT),DBUF(*),IBUF(*)
     &         ,WK01(NUMI,NUMJ,NUMK),WK02(NUMI,NUMJ,NUMK)
     &         ,WK03(NUMI,NUMJ,NUMK),NF(NUMI,NUMJ,NUMK)
     &         ,INDX(NUMI,NUMJ,NUMK),INDY(NUMI,NUMJ,NUMK)
     &         ,INDZ(NUMI,NUMJ,NUMK),INDS(NUMI*NUMJ*NUMK)
     &         ,INDB(MAXB1,NUMB),NFP(NUMI,NUMJ,NUMK)
     &         ,INDXP(NUMI,NUMJ,NUMK),INDYP(NUMI,NUMJ,NUMK)
     &         ,INDZP(NUMI,NUMJ,NUMK),ANU(NUMI,NUMJ,NUMK)
     &         ,GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
     &         ,GGZ(NUMI,NUMJ,NUMK),GGV(NUMI,NUMJ,NUMK)
     &         ,GGXP(NUMI,NUMJ,NUMK),GGYP(NUMI,NUMJ,NUMK)
     &         ,GGZP(NUMI,NUMJ,NUMK)

      CALL SF_RESET_UU(UU,INDXP,INDX,1,DBUF)
      CALL SF_RESET_UU(VV,INDYP,INDY,2,DBUF)
      CALL SF_RESET_UU(WW,INDZP,INDZ,3,DBUF)

      CALL SF_RESET_FF(PP,NFP,NF,DBUF)
      CALL SF_RESET_FF(FF,NFP,NF,DBUF)

      CALL VF_BWFF(FF,BCF,INDB)
      CALL VF_FNFINI(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)
      CALL VF_FNFPRV(FF,BCF,NF,INDX,INDY,INDZ,INDS,IBUF)
      CALL VF_CFXYZ(XX,YY,ZZ,FF,FX,FY,FZ,GGV,DBUF,NF,INDX,INDY,INDZ)

      CALL SF_MDFXYZ(FX,FY,FZ,INDX,INDY,INDZ,GGX,GGY,GGZ,GGXP,GGYP,GGZP
     &              ,DBUF)

      CALL VF_BWUWN(XX,YY,ZZ,UU,VV,WW,FF,BCU,BCV,BCW,BCF
     &             ,DMTBTT,DMTBZZ,DMTBHH,DMTBUN,DMTBUT,DBUF
     &             ,WK01,WK02,WK03,NF,INDX,INDY,INDB)
      
      CALL VF_BWUWT(XX,YY,ZZ,UU,VV,WW,ANU,BCU,BCV,BCW,BCF,BCVI,INDB)

      CALL VF_BWPP(PP,BCP,INDB)

      END
