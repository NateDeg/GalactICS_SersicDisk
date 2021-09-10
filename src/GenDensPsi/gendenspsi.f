ccccccccccc
c
c     Generate density-psi table
c
c       This module contains routines that calculate a table of energies and
c       associated densities
c
cccccccccc

      module GenDensPsiMod
      use Globs

      implicit none


      contains
cccc
      subroutine gendenspsi(fname,DensTemp,dfRout,m)
      implicit none
      real psi
      integer m
      integer i
      real fcut
      character(100) fname
      real dfRout
      external dfRout
      real DensTemp(1:npsi)
      open(file=trim(fname),unit=50,status='replace')

      fcut=0.
c      do i=1,1
      do i=1,npsi
        psi = DF%tableE(i)
        call getGenDens(psi,DensTemp(i),fcut,dfRout,m)
        if(i.eq.npsi) DensTemp(i) = 0.
        write(50,*) DF%tableE(i),DensTemp(i)
      enddo
      close(50)
c      print*, "Gen Test", DensTemp(1), DensTemp(npsi)


      return
      end subroutine
ccccccccc

cccccccc
      subroutine getGenDens(psi,rho,fcut,dfRout,m)
      implicit none
      real psi,rho,fcut
      real dfRout
      external dfRout

      integer m,j,jj
      real vmin,vmax,dlogv,v,e,vlog
      real df_lowered

      vmin = -10.
      vmax = log(sqrt(2.*(psi-DF%psic)))
      dlogv = (vmax - vmin)/float(m-1)
      rho = 0.
      do j=1,4
        vlog = vmin + dlogv*float(j-1)
        v = exp(vlog)
        e = psi - v*v/2.
        df_lowered = dfRout(e)-fcut
        if(df_lowered.gt.0.)
     &         rho = rho + 4.*pi*dlogv*v*v*v*DFcoef(j)*df_lowered
      enddo
      do j=5,m-4
        vlog = vmin + dlogv*float(j-1)
        v = exp(vlog)
        e = psi - v*v/2.
        df_lowered = dfRout(e)-fcut
        if(df_lowered.gt.0.)
     &          rho = rho + 4.*pi*dlogv*v*v*v*df_lowered
      enddo
      do jj=4,2,-1
        j = m-jj+1
        vlog = vmin + dlogv*float(j-1)
        v = exp(vlog)
        e = psi - v*v/2.
        df_lowered = dfRout(e)-fcut
        if(df_lowered.gt.0.)
     &          rho = rho + 4.*pi*dlogv*v*v*v*DFcoef(jj)*df_lowered
      enddo
c      print*, "GendensPsi",psi,rho,fcut

      return
      end subroutine
cccccccccc




      end module
