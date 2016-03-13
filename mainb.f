      program main
      implicit none

      integer i,j,k
      double precision func
      external func

      include 'cparam.inc'
      include 'hmunit.inc'
      include 'bsfile.inc'
      include 'creslt.inc'

      integer icf,lun,seed
      character*5 iicf,sseed
      common /icf/ icf
      character*17 filename1
      character*18 filename2
      character*19 filename3
      character*20 filename4
      character*21 filename5
      integer IP(NEXTERNAL),IPFLAG
      common /IP/ IP

      lun = 1

      call getarg(1,iicf)
      read(iicf,*) icf
      if (icf.lt.10) then
         filename1 = "data/"//"result"//"_"//iicf(1:1)//".dat"
         open(lun,file=filename1,status="unknown")
      elseif (icf.lt.100) then
         filename2 = "data/"//"result"//"_"//iicf(1:2)//".dat"
         open(lun,file=filename2,status="unknown")
      elseif (icf.lt.1000) then
         filename3 = "data/"//"result"//"_"//iicf(1:3)//".dat"
         open(lun,file=filename3,status="unknown")
      elseif (icf.lt.10000) then
         filename4 = "data/"//"result"//"_"//iicf(1:4)//".dat"
         open(lun,file=filename4,status="unknown")
      elseif (icf.lt.100000) then
         filename5 = "data/"//"result"//"_"//iicf(1:5)//".dat"
         open(lun,file=filename5,status="unknown")
      else
         write(*,*) "ncolor should be less than 100000"
         stop
      endif

      DO I = 1,NEXTERNAL
         IP(I) = I
      ENDDO
      DO i = 1,icf
         IF (i.NE.1) THEN
            CALL IPNEXT2(IP,NEXTERNAL,I,IPFLAG)
         ENDIF
      ENDDO

      call getarg(2,sseed)
      read(sseed,*) seed
      call bsinit(seed)
      call hmsetf('bases')
      call userin
      call bases(func, estim,sigma,ctime,it1,it2)
      call bsinfo(lustdo)
      call bhplot(lustdo)
      call bswrit(lubsf)
      call xhsave(luhist,0)
      call usrout
      write(lun,*) estim,sigma
      close(lun)

      end
