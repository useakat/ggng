      subroutine spevnt(iret)
      implicit none
      
      integer iret

c      include 'gujets.inc'
c      include 'gudat1.inc'
c      include 'fvevnt.inc'

      include 'bsfile.inc'
      include 'smptab.inc'
      include 'gnbeam.inc'

      include 'cparam.inc'
      include 'cfinal.inc'

      integer ievent/ 0/
      save ievent

      integer i,j

      iret = 0
      ievent = ievent + 1
      
      write(luevtf,'(2i10,1pe20.8)') ievent,nini+nfin,1.

      do i=1,nini
         write(luevtf,'(i5,4f15.6)') ihini(i),(pini(j,i),j=0,3)
      enddo
      do i=1,nfin
         write(luevtf,'(i5,4f15.6)') ihfin(i),(pfin(j,i),j=0,3)
      enddo
      
      return
      end
