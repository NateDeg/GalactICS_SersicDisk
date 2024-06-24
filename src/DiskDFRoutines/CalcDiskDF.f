ccccccccccccc
c
c     Calculate Disk DF
c
c       This module contains routines for calculating the disk distribution function
c
cccccccccccc
c
      module CalcDiskDFMod
      use GenCompGlobs
      use Globs
      use OmekapMod
      use DiskSigmaMod
      use SplineMod
      use diskdfInterzMod

      implicit none
      contains
c
ccccccc
      subroutine CalcDiskDF()
      implicit none
      real fom,fka
      integer ir
c

c      print*, "Calculating Disk DF"
c first call initializes the spline fitting functions by the way
      call omekap(DUse%outdisk,fom,fka)
c      print*, "Done omekap first"
      if (sqrt(sigr2(DUse%outdisk,DUse)).gt.DUse%drtrunc*fka) then
        write(0,*) '** WARNING **: you have asked for an outer vel'
        write(0,*) '  dispersion which is larger than that used in the'
        write(0,*) '  disk truncation. Strange things may happen.'
        write(0,*) sqrt(sigr2(DUse%outdisk,DUse))/(DUse%drtrunc*fka),fka
      endif
      DUse%drspl = (DUse%outdisk + 2*DUse%drtrunc)/real(DUse%nrspl)
      print*, "Spline radius",DUse%drspl,DUse%outdisk,2*DUse%drtrunc
      call DiskDFALLOCATE(DUse)

      do ir=0,DUse%nrspl
        DUse%rr(ir)=DUse%drspl*real(ir)
        DUse%fdrat(ir)=1
        DUse%fszrat(ir)=1.
      enddo
      call splined(DUse%rr(0:DUse%nrspl),DUse%fdrat(0:DUse%nrspl)
     &              ,DUse%nrspl+1
     &              ,1.e32,1.e32,DUse%drat2(0:DUse%nrspl))
      call splined(DUse%rr(0:DUse%nrspl),DUse%fszrat(0:DUse%nrspl)
     &              ,DUse%nrspl+1
     &              ,1.e32,1.e32,DUse%szrat2(0:DUse%nrspl))
c Make niter iterations on the correction functions for midplane density
c and vertical velocity dispersion.
      call DiskDFMidPlaneDensAndVelDisp()
c Finally, write out a more detailed table of residuals.

      call ResidTable()


      return
      end subroutine
cccccc

cccccccc
      subroutine DiskDFMidPlaneDensAndVelDisp()
      implicit none
      real r, fom, fka
      integer icor,ir,ivt,nrd
      real sigr,sigz,vc,vt
      real d0,dz,dz2,dz3,dsz,dvr,dff1,dff2
      real c

      real T,T2


      open(10,file="cordbh_Int.txt", status='replace')
c      print*, "DiskDF MidPlane"
c      do icor=1,1
c        do ir=10,10
      do icor=1, niter
        do ir=0, DUse%nrspl
            r=DUse%rr(ir)
c            print*, icor,ir, r
            call omekap(r,fom,fka)
            sigr=sqrt(sigr2(r,DUse))
            sigz=sqrt(sigz2(r,DUse))
            vc=r*fom
            d0=0
            dz=0
            dz2=0
            dz3=0
            dsz=0
c     integrate analytically over vr and vz, numerically over vphi
            dvr=0.1*sigr
c            dvr=0.1*sqrt(exp(-(r/DUse%disksr)**(1./DUse%sig_n)))
c            print*, "dvr check",0.1*sigr,dvr
            do ivt=1,101
c            do ivt=1,1001
cc                print*, "Before Int",icor,ir,ivt,r,vc,dvr,vt
c                vt=vc+(ivt-501)*dvr
                vt=vc+(ivt-51)*dvr
                dff1=diskdf5intez(vt,r,0.0,DUse)
                c=0.333333333333*(mod(ivt,2)*2+2)

                d0=d0+c*dvr*dff1
                dff2=diskdf5intez(vt,r,DUse%zdisk,DUse)
c                dff2=1.
                dz2=dz2+c*dvr*dff2
c                print*, "midplance loop", ivt, vt, dff1,dff2
                if(icor .eq. 1 .and. ir .eq. 10) then
                    write(10,*) r,sigr,sigz,vc,dvr,vt,dff1,c,d0
     &                  ,dff2,dz2
     &                  ,c*dvr*dff1,c*dvr*dff2

                endif

            enddo
            T=diskdensf(r,0.,DUse)
            T2=diskdensf(r,DUse%zdisk,DUse)
            DUse%drat(ir)=d0/diskdensf(r,0.,DUse)
            DUse%dz2rat(ir)=dz2/diskdensf(r,DUse%zdisk,DUse)
c            DUse%fzrat(ir)=
c     &          log(diskdensf(r,0.,DUse)
c     &          /diskdensf(r,DUse%zdisk,DUse))/log(d0/dz2)

            DUse%fzrat(ir)=
     &          log(T
     &          /T2)/log(d0/dz2)

            if(ir .eq. 7) then
c            print*, "Checking", icor,ir,r,T,DUse%drat(ir)
c            print*, "Second Check", icor, ir,r, T,T2,d0,dz2,DUse%zdisk
c            print*, "Results check", icor, ir, r,DUse%drat(ir)
c     *                  ,DUse%dz2rat(ir),DUse%fzrat(ir)
            endif

        enddo
        close(10)

        write(*,*)
        nrd=max(1,DUse%nrspl/10)
        write(0,'(''      r: '',12f6.3)') (DUse%rr(ir),ir=0,DUse%nrspl,nrd)
        write(0,'(f6.3,''  d'',12f6.3)') 0.,(DUse%drat(ir),ir=0,DUse%nrspl,nrd)
        write(0,'(f6.3,''  d'',12f6.3)') DUse%zdisk,
     &              (DUse%dz2rat(ir),ir=0,DUse%nrspl,nrd)

        do ir=0,DUse%nrspl
            DUse%fdrat(ir)=DUse%fdrat(ir)/DUse%drat(ir)
            DUse%fszrat(ir)=DUse%fszrat(ir)/DUse%fzrat(ir)
c            print*, "DiskMidplaneDF Ini", ir,DUse%rr(ir),DUse%fdrat(ir)
c     &              ,DUse%fszrat(ir)
c     &              ,DUse%fzrat(ir),DUse%szrat2(ir)
        enddo
        call splined(DUse%rr(0:DUse%nrspl),DUse%fdrat(0:DUse%nrspl)
     &          ,DUse%nrspl+1,1.e32,1.e32,DUse%drat2(0:DUse%nrspl))
        call splined(DUse%rr(0:DUse%nrspl),DUse%fszrat(0:DUse%nrspl)
     &          ,DUse%nrspl+1,1.e32,1.e32,DUse%szrat2(0:DUse%nrspl))

      enddo

      print*, "Done Mid Disk DF"
      return
      end subroutine
cccccccc

ccccccc
      subroutine ResidTable()
      implicit none

      integer ir,ivt,nrd
      real r
      real fom,fka
      real sigr,sigz
      real vc
      real c,d0,d1,d2,d3,d4
      real dvr,vt,dff

      do ir=0,DUse%nrspl
        r=DUse%rr(ir)
        call omekap(r,fom,fka)
        sigr=sqrt(sigr2(r,DUse))
        sigz=sqrt(sigz2(r,DUse))
        vc=r*fom
        d0=0
        d1=0
        d2=0
        d3=0
        d4=0
c     integrate analytically over vr and vz, numerically over vphi
        dvr=0.1*sigr
        do ivt=1,101
            vt=vc+(ivt-51)*dvr
            dff=diskdf5intez(vt,r,0.,DUse)
            c=0.333333333333*(mod(ivt,2)*2+2)
            d0=d0+c*dvr*dff
            dff=diskdf5intez(vt,r,0.5*DUse%zdisk,DUse)
            d1=d1+c*dvr*dff
            dff=diskdf5intez(vt,r,DUse%zdisk,DUse)
            d2=d2+c*dvr*dff
            dff=diskdf5intez(vt,r,1.5*DUse%zdisk,DUse)
            d3=d3+c*dvr*dff
            dff=diskdf5intez(vt,r,2*DUse%zdisk,DUse)
            d4=d4+c*dvr*dff
        enddo
        DUse%d0rat(ir)=d0/diskdensf(r,0.,DUse)
        DUse%d1rat(ir)=d1/diskdensf(r,0.5*DUse%zdisk,DUse)
        DUse%d2rat(ir)=d2/diskdensf(r,DUse%zdisk,DUse)
        DUse%d3rat(ir)=d3/diskdensf(r,1.5*DUse%zdisk,DUse)
        DUse%d4rat(ir)=d4/diskdensf(r,2*DUse%zdisk,DUse)
      enddo
      write(*,*)
      nrd=max(1,(DUse%nrspl-1)/10+1)
      write(*,'(''      r: '',11f6.3)') (DUse%rr(ir),ir=0,DUse%nrspl,nrd)
      write(*,'(f6.3,''  d'',11f6.3)') 0.,
     &              (DUse%d0rat(ir),ir=0,DUse%nrspl,nrd)
      write(*,'(f6.3,''  d'',11f6.3)')
     &              0.5*DUse%zdisk,(DUse%d1rat(ir),ir=0,DUse%nrspl,nrd)
      write(*,'(f6.3,''  d'',11f6.3)') DUse%zdisk
     &              ,(DUse%d2rat(ir),ir=0,DUse%nrspl,nrd)
      write(*,'(f6.3,''  d'',11f6.3)')
     &              1.5*DUse%zdisk,(DUse%d3rat(ir),ir=0,DUse%nrspl,nrd)
      write(*,'(f6.3,''  d'',11f6.3)')
     &              2*DUse%zdisk,(DUse%d4rat(ir),ir=0,DUse%nrspl,nrd)

      return
      end subroutine
cccccccc

ccccccc
      subroutine OutputDiskDFCor()
      implicit none
      integer i

      open(12,file=OutputFileName,status='replace')
c      write(12,'(''#'', 2g17.7,1x,i5,2g17.7)') DUse%sigr0,Duse%disksr,DUse%nrspl
c     &              ,DUse%sig_n
      write(12,'(''#'', 2g17.7,1x,i5,2g17.7)') DUse%sigr01,Duse%disksr1
     &              ,DUse%nrspl
     &              ,DUse%sigr02,Duse%disksr2
      do i=0,DUse%nrspl
        write(12,'(1x,3g17.7)') DUse%rr(i),DUse%fdrat(i),DUse%fszrat(i)
      enddo
      close(12)
      return
      end subroutine
ccccccccc



      end module
