      SUBROUTINE DATA_CHECK( KK, RR )
C
      USE MPI_PARAM
      USE M_VAL
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*3 PNO
      DIMENSION KK(200), RR(50)
C-----------------------------------------------------------------------
      IF( MYRANK == 0 ) THEN
        OPEN(41,FILE='data.chk')
      ELSE
        WRITE(PNO,'(I3.3)') MYRANK
        OPEN(41,FILE='data.chk_'//PNO)
      ENDIF
C
      WRITE(41,'(2X,A4)') 'KK  '
      WRITE(41,'(10I10)') KK(:)
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'RR  '
      WRITE(41,'(1P5E15.7)') RR(:)
      WRITE(41,*)
C      
      WRITE(41,'(2X,A4)') 'ISUB'
      DO I=1,KK(4)
        WRITE(41,'(13I5)') ISUB(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'INDG','GRID'
      DO I=1,KK(8)
        WRITE(41,'(2I8,1P3E15.7)') I,INDG(I),GRID(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A5)') 'IELM1'
      DO I=1,KK(12)
        WRITE(41,'(8I8)') I,IELM(1:7,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A5)') 'IELM2'
      DO I=1,KK(12)
        WRITE(41,'(11I8/8X,10I8)') I,IELM(8:KK(37),I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'THK '
      DO I=1,KK(13)
        WRITE(41,'(I8,1PE15.7)') I,THK(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'RODA'
      DO I=1,KK(15)
        WRITE(41,'(I8,1PE15.7)') I,RODA(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'BARD'
      DO I=1,KK(17)
        WRITE(41,'(I8,1P6E15.7)') I,BARD(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'BVEC'
      DO I=1,KK(16)
        WRITE(41,'(I8,1P3E15.7)') I,BVEC(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'JELS','PELS'
      DO I=1,KK(18)
        WRITE(41,'(3I8,1P3E15.7)') I,JELS(:,I),PELS(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'MAT '
      DO I=1,KK(11)
        WRITE(41,'(3I8)') I,MAT(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'AMAT'
      DO I=1,KK(11)
        WRITE(41,'(I3)') I
        WRITE(41,'(1P6E15.7)') AMAT(1:6,I)
        WRITE(41,'(1P6E15.7/15X,5E15.7/30X,4E15.7/45X,3E15.7/60X,2E15.7
     &            /75X,E15.7)') AMAT(7:27,I)
        WRITE(41,'(1P6E15.7)') AMAT(28:33,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'MATT'
      DO I=1,KK(11)
        WRITE(41,'(I3)') I
        WRITE(41,'(5I3)') MATT(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ITM1'
      DO I=1,KK(72)
        WRITE(41,'(2I8)') I,ITM1(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'TBM1'
      DO I=1,KK(73)
        WRITE(41,'(I8,1P2E15.7)') I,TBM1(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ISPD'
      DO I=1,KK(38)
        WRITE(41,'(3I8)') I,ISPD(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'NSPD','SPCD'
      DO I=1,KK(39)
        WRITE(41,'(2I8,6I12)') I,NSPD(:,I)
        WRITE(41,'(16X,1P6E12.4)') SPCD(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'LOAD','SLOD'
      DO I=1,KK(40)
        WRITE(41,'(3I8,1PE15.7)') I,LOAD(:,I),SLOD(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'SLID','LILD'
      DO I=1,KK(41)
        WRITE(41,'(I8,1PE15.7,I8)') I,SILD(I),LILD(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'IFC '
      DO I=1,KK(42)
        WRITE(41,'(3I8)') I,IFC(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'NFC ','FC  '
      DO I=1,KK(43)
        WRITE(41,'(3I8,1P6E15.7)') I,NFC(:,I),FC(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'IPL4'
      DO I=1,KK(44)
        WRITE(41,'(3I8)') I,IPL4(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'NPL4','PLD4'
      DO I=1,KK(45)
        WRITE(41,'(5I8,1P4E15.7)') I,NPL4(:,I),PLD4(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'IPL1'
      DO I=1,KK(74)
        WRITE(41,'(3I8)') I,IPL1(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'NPL1','PLD1'
      DO I=1,KK(75)
        WRITE(41,'(2I8,1P3E15.7)') I,NPL1(I),PLD1(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'IGRV','GRAV'
      DO I=1,KK(60)
        WRITE(41,'(2I8,1P3E15.7)') I,IGRV(I),GRAV(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'IRFC','RFRC'
      DO I=1,KK(61)
        WRITE(41,'(3I8,1P3E15.7)') I,IRFC(:,I),RFRC(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ITMP'
      DO I=1,KK(58)
        WRITE(41,'(2I8)') I,ITMP(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'TEMP'
      IF(KK(58) .GT. 0) THEN
        DO I=1,KK(8)
          WRITE(41,'(2I8,10F8.3)') I,INDG(I),TEMP(I,:)
        ENDDO
      ENDIF
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ITP1'
      DO I=1,KK(59)
        WRITE(41,'(2I8)') I,ITP1(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'ITPF','TMP1'
      DO J=1,KK(59)
        WRITE(41,'(I8)') J
        DO I=1,KK(12)
          WRITE(41,'(3I8,2F8.3)') I,IELM(1,I),ITPF(I,J),TMP1(:,I,J)
        ENDDO
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ISPA'
      DO I=1,KK(46)
        WRITE(41,'(3I8)') I,ISPA(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'NSPA'
      DO I=1,KK(47)
        WRITE(41,'(2I8)') I,NSPA(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ISP1'
      DO I=1,KK(48)
        WRITE(41,'(3I8)') I,ISP1(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'NSP1'
      DO I=1,KK(49)
        WRITE(41,'(8I8)') I,NSP1(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ISPC'
      DO I=1,KK(56)
        WRITE(41,'(3I8)') I,ISPC(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'NSPC','SPC '
      DO I=1,KK(57)
        WRITE(41,'(2I8,6I12)') I,NSPC(:,I)
        WRITE(41,'(16X,1P6E12.4)') SPC(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'IMPA'
      DO I=1,KK(50)
        WRITE(41,'(3I8)') I,IMPA(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'NMPA'
      DO I=1,KK(51)
        WRITE(41,'(2I8)') I,NMPA(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'IMPC'
      DO I=1,KK(52)
        WRITE(41,'(3I8)') I,IMPC(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'JMPC'
      DO I=1,KK(53)
        WRITE(41,'(2I8)') I,JMPC(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'NMPC','AMPC'
      DO I=1,KK(54)
        WRITE(41,'(3I8,1PE15.7)') I,NMPC(:,I),AMPC(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'NEIG'
      DO I=1,KK(55)
        WRITE(41,'(3I8)') I,NEIG(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'IFR1','FRQ1'
      DO I=1,KK(65)
        WRITE(41,'(3I8,1P2E15.7)') I,IFR1(:,I),FRQ1(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'IDLD','SDLD'
      DO I=1,KK(66)
        WRITE(41,'(3I8,1PE15.7)') I,IDLD(:,I),SDLD(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'SIDL','LIDL'
      DO I=1,KK(67)
        WRITE(41,'(I8,1PE15.7,I8)') I,SIDL(I),LIDL(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'IRL1'
      DO I=1,KK(68)
        WRITE(41,'(8I8)') I,IRL1(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'LSEQ'
      DO I=1,KK(69)
        WRITE(41,'(5I8)') I,LSEQ(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ITD1'
      DO I=1,KK(70)
        WRITE(41,'(2I8)') I,ITD1(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'TBD1'
      DO I=1,KK(71)
        WRITE(41,'(I8,1P2E15.7)') I,TBD1(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'NLP ','RNLP'
      DO I=1,KK(76)
        WRITE(41,'(5I8,1P2E15.7)') I,NLP(:,I),RNLP(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ISTP'
      DO I=1,KK(78)
        WRITE(41,'(3I8)') I,ISTP(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4,2X,A4)') 'NSTP','DELT'
      DO I=1,KK(79)
        WRITE(41,'(3I8,1PE15.7)') I,NSTP(:,I),DELT(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ITL1'
      DO I=1,KK(77)
        WRITE(41,'(5I8)') I,ITL1(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ICPA'
      DO I=1,KK(88)
        WRITE(41,'(3I8)') I,ICPA(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'NCPA'
      DO I=1,KK(89)
        WRITE(41,'(2I8)') I,NCPA(I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ICPR'
      DO I=1,KK(90)
        WRITE(41,'(3I8)') I,ICPR(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'NCPR'
      DO I=1,KK(91)
        WRITE(41,'(3I8)') I,NCPR(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'CPR '
      DO I=1,KK(90)
        WRITE(41,'(I8,1P2E12.4)') I,CPR(:,I)
      ENDDO
      WRITE(41,*)
C
      WRITE(41,'(2X,A4)') 'ICRG'
      DO I=1,KK(92)
        WRITE(41,'(3I8)') I,ICRG(:,I)
      ENDDO
      WRITE(41,*)
C
      IF( MYRANK == 0 ) THEN
        WRITE(41,'(2X,A4)') 'NCRG'
        DO I=1,KK(93)
          WRITE(41,'(5I8)') I,NCRG(:,I)
        ENDDO
        WRITE(41,*)
      ENDIF
C
      CLOSE(41)
C
      END
