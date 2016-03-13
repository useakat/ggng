      program get_sum
      implicitnone

c      include 'cparam.inc'

      integer i,j,k
      integer lun,line,icf,njobs
      character*6 iicf
      character*17 filename1
      character*18 filename2
      character*19 filename3
      character*20 filename4
      character*21 filename5
      real*8 estim,sigma
      real*8 invsigmatot2,estimtottmp,sigmatot2
      real*8 sigmatot,estimtot,estim2tot
      real*8 estimsum,estim2sum

      lun = 1

      sigmatot = 0d0
      estimtot = 0d0
      call getarg(1,iicf)
      read(iicf,*) njobs

      open(2,file="result.dat",status="unknown")
      do icf = 1,njobs
         write (iicf,*) icf
         if (icf.lt.10) then
            filename1 = "data/"//"result"//"_"//iicf(2:2)//".dat"
            open(lun,file=filename1,status="unknown")
         elseif (icf.lt.100) then
            filename2 = "data/"//"result"//"_"//iicf(2:3)//".dat"
            open(lun,file=filename2,status="unknown")
         elseif (icf.lt.1000) then
            filename3 = "data/"//"result"//"_"//iicf(2:4)//".dat"
            open(lun,file=filename3,status="unknown")
         elseif (icf.lt.10000) then
            filename4 = "data/"//"result"//"_"//iicf(2:5)//".dat"
            open(lun,file=filename4,status="unknown")
         elseif (icf.lt.100000) then
            filename5 = "data/"//"result"//"_"//iicf(2:6)//".dat"
            open(lun,file=filename5,status="unknown")
         else
            write(*,*) "ncolor should be less than 100000"
            stop
         endif
        
         read(lun,*) estim,sigma
         close(lun)
         
         write(2,'(e14.7,e14.7,f6.3)') estim,sigma,
     &        sigma/estim*100
         estimtot = estimtot +estim
         sigmatot2 = sigmatot2 +sigma**2
         sigmatot = dsqrt(sigmatot2)
c         invsigmatot2 = invsigmatot2 +1d0/sigma**2
c         sigmatot2 = 1d0/invsigmatot2
c         sigmatot = dsqrt(sigmatot2)
c         estimtottmp = estimtottmp +estim/sigma**2
c         estimtot = estimtottmp*sigmatot2 

      enddo

      write(2,*) 
      write(2,'(e14.7,e14.7,f6.3)') estimtot,sigmatot,
     &     sigmatot/estimtot*100
      write(*,'(e14.7,e14.7,f6.3)') estimtot,sigmatot,
     &     sigmatot/estimtot*100
      close(2)

      end
