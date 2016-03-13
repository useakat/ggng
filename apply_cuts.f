      subroutine apply_cuts(p,ierr)
      implicitnone

      include 'cparam.inc'
      include 'ccuts.inc'

      integer i,j
      real*8 p(0:3,nfin)
      integer ierr
      double precision ppt,eeta,dr,pti,ptj,ptij
      double precision psdrap,deltar,ptvec
      real*8 pt,eta,dRij
      external ptvec,psdrap,deltar
      external pt,eta,dRij

      ierr = 0

      do i = 1,nfin
         ppt = pt(p(0,i))
         if (ppt.lt.ptcut) then
            ierr = 1
            return
         endif
         eeta = eta(p(0,i))
         if (abs(eeta).gt.etacut) then
            ierr = 1
            return
         endif
      enddo
      
      do i = 1,nfin-1
         pti = pt(p(0,i))
         do j = i+1,nfin
            ptj = pt(p(0,j))
            dr = dRij(p(0,i),p(0,j))
            ptij = min(pti,ptj)*dr
            if (ptij.lt.ptcut) then
c            if (dr.lt.drcut) then
               ierr = 1
               return
            endif
         enddo
      enddo

      end
