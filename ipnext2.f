      subroutine ipnext2(ip,n,i,ipflag)
      implicitnone

      integer n
      integer ip(n),tmp,i,ipflag

      ipflag = 0
      if (i.ge.n) then
         ipflag = 1
         return
      endif

      if (i.ne.1) then
         tmp = ip(i)
         ip(i) = ip(i+1)
         ip(i+1) = tmp
      endif

      return
      end
