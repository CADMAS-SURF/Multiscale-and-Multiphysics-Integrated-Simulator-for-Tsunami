cmod20160729(s)
c      SUBROUTINE VF_PMGGPT(GGV,GGX,GGY,GGZ,NF,IC,IS,JS,KS,IE,JE,KE)
      SUBROUTINE VF_PMGGPT(GWRKF,GWRKU,GWRKV,GWRKW,NNF,NNU,NNV,NNW,
     $   IC,IS,JS,KS,IE,JE,KE,NGWRKF,NGWRKU,NGWRKV,NGWRKW)
cmod20160729(e)
C----------------------------------------------------------2011.04 end

CD=== 概要 ===========================================================

CDT   VF_PMGGPT:マルチグリッド環境の親のポーラス値の子への送信

C==== 宣言 ===========================================================

C     -- 大域型 --
c      use mod_apara,only: 
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    GGV(@FOR-3D@)    : I/O : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : I/O : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : I/O : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : I/O : R*8 : z方向面積透過率
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
cmod20160729(s)
c      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
c      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
c      DIMENSION NF(NUMI,NUMJ,NUMK)
      DOUBLE PRECISION GWRKF(NGWRKF),GWRKU(NGWRKU)
      DOUBLE PRECISION GWRKV(NGWRKV),GWRKW(NGWRKW)
      INTEGER NNF,NNU,NNV,NNW
cmod20160729(e)

CD    -- 局所変数 --
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: GWRK
cdebug
      integer, allocatable:: iwrk(:,:)
cdebug

C==== 実行 ===========================================================

      NX = IE - IS + 1
      NY = JE - JS + 1
      NZ = KE - KS + 1

CD    -- GGV & NV --
      NGWRK = (NX*NY - MAX((NX-2)*(NY-2),0))*NZ *2
      ALLOCATE(GWRK(NGWRK))
cdebug
      ngtmp = ngwrk/nz
      allocate(iwrk(2,ngtmp))
cdebug
      IGWRK = 0
      DO 100 K=KS,KE
        DO 110 J=JS,JE
          DO 120 I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
cmod20160729(s)
              GWRK(IGWRK+1) = GWRKF(NNF+1)
              GWRK(IGWRK+2) = GWRKF(NNF+2)
cdebug
              if(k.eq.ks) then
              iwrk(1,IGWRK+1) = i
              iwrk(2,IGWRK+1) = j
              iwrk(1,IGWRK+2) = i
              iwrk(2,IGWRK+2) = j
              endif
cdebug
c              GWRK(IGWRK+1) = GGV(I,J,K)
c              GWRK(IGWRK+2) = NF (I,J,K)
              NNF=NNF+2
cmod20160729(e)
              IGWRK = IGWRK + 2
            END IF
 120      CONTINUE
 110    CONTINUE
 100  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGPT','PROGRAM ERROR(GGV).')
      END IF
      CALL VF_ZXMG_ISENDD(GWRK,NGWRK,IC,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
cdebug
c      write(500+mgrank,*) 'vf_pmggpt:is,ie=',is,ie
c      write(500+mgrank,*) 'vf_pmggpt:js,je=',js,je
c      write(500+mgrank,*) 'vf_pmggpt:ks,ke=',ks,ke
c      write(500+mgrank,*) 'vf_pmggpt:ic,ngwrk1=',ic,ngwrk
c      write(600+mgrank,'(f8.6,f4.0)') gwrk
c      write(600+mgrank,'(2i5)') iwrk
      deallocate(iwrk)     
cdebug
      DEALLOCATE(GWRK)

CD    -- GGX --
      NGWRK = ((NX+1)*NY - MAX((NX-3)*(NY-2),0))*NZ
      ALLOCATE(GWRK(NGWRK))
cdebug
      ngtmp = ngwrk/nz
      allocate(iwrk(2,ngtmp))
cdebug
      IGWRK = 0
      DO 200 K=KS,KE
        DO 210 J=JS,JE
          DO 220 I=IS,IE+1
            IF(I.LE.IS+1 .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
cmod20160729(s)
               GWRK(IGWRK+1) = GWRKU(NNU+1)
cdebug
              if(k.eq.ks) then
              iwrk(1,IGWRK+1) = i
              iwrk(2,IGWRK+1) = j
              endif
cdebug
c              GWRK(IGWRK+1) = GGX(I,J,K)
              NNU=NNU+1
cmod20160729(e)
              IGWRK = IGWRK + 1
            END IF
 220      CONTINUE
 210    CONTINUE
 200  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGPT','PROGRAM ERROR(GGX).')
      END IF
      CALL VF_ZXMG_ISENDD(GWRK,NGWRK,IC,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
cdebug
c      write(500+mgrank,*) 'vf_pmggpt:ic,ngwrk2=',ic,ngwrk
c      write(700+mgrank,'(f8.6)') gwrk
c      write(700+mgrank,'(2i5)') iwrk
      deallocate(iwrk)
cdebug
      DEALLOCATE(GWRK)

CD    -- GGY --
      NGWRK = (NX*(NY+1) - MAX((NX-2)*(NY-3),0))*NZ
      ALLOCATE(GWRK(NGWRK))
cdebug
      ngtmp = ngwrk/nz
      allocate(iwrk(2,ngtmp))
cdebug
      IGWRK = 0
      DO 300 K=KS,KE
        DO 310 J=JS,JE+1
          DO 320 I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS+1 .OR. J.GE.JE) THEN
cmod20160729(s)
               GWRK(IGWRK+1) = GWRKV(NNV+1)
cdebug
              if(k.eq.ks) then
              iwrk(1,IGWRK+1) = i
              iwrk(2,IGWRK+1) = j
              endif
cdebug
c              GWRK(IGWRK+1) = GGY(I,J,K)
              NNV=NNV+1
cmod20160729(e)
              IGWRK = IGWRK + 1
            END IF
 320      CONTINUE
 310    CONTINUE
 300  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGPT','PROGRAM ERROR(GGY).')
      END IF
      CALL VF_ZXMG_ISENDD(GWRK,NGWRK,IC,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
cdebug
c      write(500+mgrank,*) 'vf_pmggpt:ic,ngwrk3=',ic,ngwrk
c      write(800+mgrank,'(f8.6)') gwrk
c      write(800+mgrank,'(2i5)') iwrk
      deallocate(iwrk)
cdebug
      DEALLOCATE(GWRK)

CD    -- GGZ --
      NGWRK = (NX*NY - MAX((NX-2)*(NY-2),0))*(NZ+1)
      ALLOCATE(GWRK(NGWRK))
cdebug
      ngtmp = ngwrk/(nz+1)
      allocate(iwrk(2,ngtmp))
cdebug
      IGWRK = 0
      DO 400 K=KS,KE+1
        DO 410 J=JS,JE
          DO 420 I=IS,IE
            IF(I.LE.IS .OR. I.GE.IE .OR. J.LE.JS .OR. J.GE.JE) THEN
cmod20160729(s)
              GWRK(IGWRK+1) = GWRKW(NNW+1)
cdebug
              if(k.eq.ks) then
              iwrk(1,IGWRK+1) = i
              iwrk(2,IGWRK+1) = j
              endif
cdebug
c              GWRK(IGWRK+1) = GGZ(I,J,K)
              NNW=NNW+1
cmod20160729(e)
              IGWRK = IGWRK + 1
            END IF
 420      CONTINUE
 410    CONTINUE
 400  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGPT','PROGRAM ERROR(GGZ).')
      END IF
      CALL VF_ZXMG_ISENDD(GWRK,NGWRK,IC,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
cdebug
c      write(500+mgrank,*) 'vf_pmggpt:ic,ngwrk4=',ic,ngwrk
c      write(900+mgrank,'(f8.6)') gwrk
c      write(900+mgrank,'(2i5)') iwrk
      deallocate(iwrk)
cdebug
      DEALLOCATE(GWRK)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
