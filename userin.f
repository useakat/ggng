      subroutine userin
      implicit none
      external func

      integer maxdim
      parameter (maxdim=50)
      double precision xl(maxdim),xu(maxdim)
      integer ig(maxdim)

      include 'bsffcm.inc'
      include 'hmparm.inc'
      include 'hmunit.inc'
      include 'smptab.inc'
      include 'gnbeam.inc'
      include 'cparam.inc'
      include 'cinit.inc'
      include 'cfinal.inc'
      include 'ccuts.inc'
      include 'coupl.inc'
      
      character*80 ctit
      character*1 char
      double precision spnave,colave,stat,color
      integer nfact
      double precision pii
      parameter( pii = 3.14159265358979323846d0 )
      integer i,id,idim

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     parameters
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      pdg(1) = 0
      pdg(2) = 0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      call hminit
      call gnstbm(0.d0)
      call smprnt
      call pdfwrap

      nhtot = 2**nexternal
      spnave = 1d0/4d0
      colave = 1d0/(8d0*8d0)
      stat   = 1d0
c      do i=1,nfin
c         stat = stat*dble(i)
c      enddo
c      stat = 1d0/stat
c      phfini = spnave*colave*stat*gev2fb*dble(nhtot)
      phfini = spnave*colave*gev2fb*dble(nhtot)
c      phfini = spnave*colave*stat*gev2fb
c      phfini = gev2fb

      do idim=1,ndim
         xl(idim) = 0d0
         xu(idim) = 1d0
         ig(idim) = 1
      enddo

      whatmin = nfin * ptcut
      taulmin = 2.d0*log(whatmin/w0)
      
      call bssetd(ndim,nwild,xl,xu,ig)
      
c      do i=1,nfin
c         write(char,'(i1)') i
c         id = 10*i
c         call xhinit(id+1,-5.d0,5.d0,50,'d sigma / d eta_a'//char)
c         call xhinit(id+2,0.d0,2.d0*pi,50,'d sigma / d phi_a'//char)
c         call xhinit(id+3,0.d0,1000.d0,50,'d sigma / d pt_a'//char)
c         call xhinit(id+4,0.d0,100.d0,50,'d sigma / d pt_a'//char)
c         call xhinit(id+5,-1.d0,1.d0,50,'d sigma / d pt_cos'//char)
c      enddo
      
c      call xhinit(111,0.d0, 4.d0, 4,'helicity(ee)')

      do idim=1,ndim
         write(ctit,1010) idim,xl(idim),xu(idim)
 1010    format('x(',i2,') ; ',e12.6,' - ',e12.6)
         call xhinit(idim+1000,xl(idim),xu(idim),50,ctit)
      enddo

      return
      end
