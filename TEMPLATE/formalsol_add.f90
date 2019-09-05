module version_formalsol_add
!
! additional routines for FORMALSOL, when calculating spectrum in predefined
! range (only if OPTCMF_ALL = .true.), with keyword 'UV'
!
! NOTES for further modifications
!
!
! WRITTEN: 
!        07/15 by JP
!
! HISTORY:
!         comletely new
!
! version 0.0 July 2015

character*5 :: ver_formalsol_add='0.0' 
end module version_formalsol_add
!
!--------------------------------------------------------------------------- &
!
module formalsol_add_var
!
Use nlte_type      
implicit none

integer(i4b) :: ntot, icount_hhe

integer(i4b), dimension(:), allocatable:: id1
real(sp), dimension(:), allocatable :: gf1, xlam1

integer(i4b), dimension(:), allocatable :: k_range, inverted_range, index_hhe, &
                                           l23, lstore
real(dp), dimension(:,:), allocatable :: opal_range, sline_range

real(dp), dimension(:), allocatable :: lamgrid, dlam_max_stark,beta_p, qstore

integer(i4b), parameter :: natom = 30
!maximum width of Doppler-broadening (in vth, used for non-Stark lines in prepray_range)
real(dp), parameter :: xmax = 4.d0

!control parameter for Stark-broadening (see subr. calcxmax_range and calcxmax)
real(dp), parameter :: eps_broad = 5.d-3

!maximum integration depth
real(dp), parameter :: taumax=15.d0 
!neglect lines with tau_sob < tauweak in range 1,lwion-1
!to included all lines, set tauweak to very low value (1.d-10 or lower)
real(dp), parameter :: tauweak=1.d-7 !compromise; for tests, set to 0.01
real(dp), parameter :: tauout=1.d0 

!maximum width of Stark-broadening (max(vbroad) calculated in calcxmax_range),
!to be used in prepray_range
real(dp), parameter :: vmax_broad_safety = 3000.d5 !(for boundaries in calc_lines)
real(dp) :: vmax_broad 

character*2, dimension(natom) :: names1 
real(dp), dimension(natom) :: aweight


! for info and checks
data names1 /'H ','HE','LI','BE','B ','C ', &
&            'N ','O ','F ','NE','NA','MG', &
&            'AL','SI','P ','S ','CL','AR',&
&            'K ','CA','SC','TI','V ','CR',&
&            'MN','FE','CO','NI','CU','ZN'/

data aweight / 1.008,  4.003,  6.941,  9.012, 10.811, 12.011, & 
&             14.007, 16.000, 18.998, 20.180, 22.990, 24.305,&
&             26.982, 28.085, 30.974, 32.066, 35.453, 39.948,&
&             39.098, 40.078, 44.956, 47.88,  50.941, 51.996,&
&             54.938, 55.847, 58.933, 58.69,  63.546, 65.39/  


end module formalsol_add_var
!
!--------------------------------------------------------------------------- &
!
subroutine calc_lines(nd,xne,te,clfac,r,velo,dvdr,sr,srvmax, &
&                     lam_blu,lam_red,iblu,ired, &
&                     vturb,yhe,balmer_lemke,maxncomp)
!
! calculates line opacities/source-functions for line-list provided from
! package cmf_all and corresponding occupation numbers
!
! OPAL corresponds to pi e2/me c * 1.d-8 *SR/vmax * lam * gf * nl/gl / clfac 
!  = true (mean) opacity * lambda * SR / vmax
! calculated opacity for prepray_range =  ln(true opac *SR*lambda/sqrt(pi)/clight)
!                                      =  ln (OPAL* vmax/clight /sqpi)
!
USE nlte_type      
USE nlte_dim, only: id_ndept, id_llevs, id_nttrd, id_frec1
USE fund_const, only: clight,hc2,sqpi
USE princesa_var, only: gl,le,labat,labl
USE formalsol_var, only: modnam=>file,nstark,nlevl,nlevu
USE formalsol_add_var, only: ntot,id1,xlam1,gf1,names1, &
&   opal_range,sline_range,k_range,inverted_range, &
&   index_hhe,icount_hhe,vmax_broad_safety,tauweak,l23,beta_p

implicit none

integer(i4b), parameter :: nrecex=id_llevs
integer(i4b), parameter :: nd1=id_ndept
integer(i4b), parameter :: ifretot=id_frec1

integer(i4b), parameter :: natom=30, nion=9, nlev=50, nrec=149

real(dp), parameter:: c_vanreg= 0.5 * 2.055d-23
real(dp), parameter:: c_forbid= 8.63d-6/(0.5*1.3707d-7)
real(dp), parameter :: gfcut=1.d-5 ! see sumopal and opacity_cmf

real(dp), parameter :: c1=1.4388354967334d8,c2=3.972970127d8
! for Planck function, identical with function bnue

real(dp), parameter :: safety=vmax_broad_safety/clight ! extend interval to allow for red/blue-shifted lines

integer(i4b), intent(in) :: nd,maxncomp

real(dp), dimension(nd1), intent(in) :: xne,clfac,r,velo,dvdr
real(dp), dimension(nd1), intent(out) :: te

real(dp), intent(in) :: sr,srvmax,lam_blu,lam_red,vturb,yhe
integer(i4b), intent(out) :: iblu,ired
logical, intent(in) :: balmer_lemke

integer(i4b), dimension(nd1) :: lll

real(dp), dimension(nd1) ::  vr, tau0, collfac,opal, sline, occlow, occup, &
                             xj1, xk1, mustar, eddf, &
&                            u0, vanreg, vanreg1, eps!, eps1

integer(i4b), dimension(ifretot) :: lthin

real(dp), dimension(ifretot) :: fre
real(dp), dimension(nd1,ifretot) :: xj, xxk
real(dp), dimension(nd1,ifretot) :: opac, scont, thomson

real(dp), dimension(nrecex,nd1) :: occexpl

integer(i4b), dimension(natom) :: jatom_full
integer(i4b), dimension(natom,nion) :: indrec
integer(i4b), dimension(natom,nd1) :: met_imin,met_imax
real(dp), dimension(nrec,nlev,nd1) :: occng

integer(i4b), dimension(:), allocatable :: lineno

character, dimension(:), allocatable :: level*6, leveu*6

real(dp), parameter :: c_tau=1.d-8*0.02654, hc2_8=hc2*1.d24 !conversion to Angstrom
real(dp), parameter :: const_opal_stark=sqpi*clight*1.d8

real(dp) :: c_tau1, const, lam, lam3, occlowi, occupi, ener, q, q1, eddtest, &
&           taubi, taui, betacic, beta, bni, epsi, eps1i, lamb1, lamr1, vmax, &
&           const_opal, maxtau,xlamb,gll,glu,gf,weight,tauc

integer(i4b) :: lwion,lwion1,ifre,i,j,ll,ii,index,k,kk,irest, &
&               ml,mu,low,up, &
&               icount_expl,icount_bg,icount_bg_noup, &
&               irec,ifstart,lth,n_inverted,n_weak,ncomp,nst

logical :: almost_converged

if(nd.ne.nd1) stop' nd ne nd1 in calc_lines'

!read merged line list (as calculated and saved in CMF_ALL
open(1,file=trim(modnam)//'/LINE_LIST_MERGED',form='UNFORMATTED')
read(1) ntot,lwion,lwion1,indrec
print*,'lwion:',lwion,' lwion1',lwion1

allocate(id1(ntot),xlam1(ntot),gf1(ntot))

read(1) id1,xlam1,gf1
close(1)


open (1,file=trim(modnam)//'/CONT_FORMAL_ALL',status='unknown', &
& form='unformatted')

read (1) ifre, (fre(i),i=1,ifretot), &
&  ((opac(i,j),i=1,nd),j=1,ifretot), &
&  ((thomson(i,j),i=1,nd),j=1,ifretot), &
&  ((scont(i,j),i=1,nd),j=1,ifretot), &  !actually, strue, but not needed
&  ((xj(i,j),i=1,nd),j=1,ifretot), &
&  (te(i),i=1,nd)
close (1)  

open (1,file=trim(modnam)//'/CONT_FORMAL_CMF',status='unknown', &
&  form='unformatted')

read (1) ((scont(i,j),i=1,nd),j=1,ifretot), & !total source function (CMF)
&        ((opac(i,j),i=1,nd),j=1,ifretot), &  !actually, opac_nolines (will be used to calculate tauc)
&        ((xxk(i,j),i=1,nd),j=1,ifretot), &
&        (lthin(j),j=1,ifretot)
close (1)  

do kk=1,ifre 
  if(fre(kk).ne.0.) then
    fre(kk)=log10(1.d8/fre(kk))
    xj(:,kk)=log10(xj(:,kk))
    xxk(:,kk)=log10(xxk(:,kk))
  endif
enddo

c_tau1=c_tau*srvmax

vr=velo/r
tau0=1.d0/(dvdr/3.+2./3.*vr)  ! at mue^2 = 1/3
collfac=c_vanreg*xne/sqrt(te)

!extend interval
vmax=sr/srvmax
lamb1=lam_blu*(1.-(vmax/clight+safety))
lamr1=lam_red*(1.+(vmax/clight+safety))

if(xlam1(1).gt.lamr1) stop' first line redwards from interval (calc_lines)'
if(xlam1(ntot).lt.lamb1) stop' last line bluewards from interval (calc_lines)' 

const_opal=vmax/(sqpi*clight) !for opal_range; division by c, since vthray=vth/c

!define range for lines in considered range
do i=1,ntot
  if(xlam1(i).ge.lamb1) exit
enddo

iblu=max(1,i-1)

do i=iblu+1,ntot
  if(xlam1(i).gt.lamr1) exit
enddo

ired=i
print*,' total number of lines in considered range:',ired-iblu+1

allocate(opal_range(nd,iblu:ired),sline_range(nd,iblu:ired))
allocate(k_range(iblu:ired),inverted_range(iblu:ired))

inverted_range=0

! read NLTE occupation numbers (expl. elements) at all depth points
open (unit=1,file=trim(modnam)//'/NLTE_POP',status='UNKNOWN',recl=nrecex*8, &
& access='DIRECT')

do ll=1,nd
  read (1,rec=ll) (occexpl(i,ll),i=1,nrecex)  
  lll(ll)=ll !auxiliary vector
enddo
close (1)

! read NLTE occupation numbers (bg elements) and considered ions
! at all depth points
open (1,file=trim(modnam)//'/OCCNG',status='unknown',form='unformatted')
read(1) met_imin,met_imax,occng
read(1) almost_converged,jatom_full
close(1)

icount_expl=0 !counter for lines from explicit elements
icount_bg =0 !counter for lines from background elements
icount_bg_noup =0 !counter for lines from explicit elements without upper level
n_inverted=0 !counter for inverted lines (inverted_range=1)
n_weak=0 !counter for weak lines (inverted_range=3)

icount_hhe=0 ! counter for H/He lines

! loop over all considered lines, to calculate opal and etal
all_lines: do ii=iblu,ired
  index=id1(ii)
  if (index.ge.1d8) then
! explicit element
    kk = index/1d8
    irest = index - kk*1d8
    ml = irest/10000
    mu = irest - ml*10000
    if(kk.ne.le(ml).or.kk.ne.le(mu)) stop' problems with kk (calc_lines)'    

    do i = 1,natom
       if(labat(kk).eq.names1(i)) then
         k_range(ii)=i
         exit
       endif  
    enddo

    lam=xlam1(ii)
    lam3=lam**3
!    print*,lam,ml,mu
    const=c_tau1*lam*gf1(ii)
    occlow=occexpl(ml,:)/gl(ml)
    occup=occexpl(mu,:)/gl(mu)
    if(occlow(1)*occup(1).eq.0.) then
! this should never happen (at least for GLOBAL = .true.)
!JO: check for GLOBAL = .false.
      print*,index,' ',lam
      stop' occlow or occup eq 0 (expl. elements) in calc_lines'
    endif
! pi e2/me c * 1.d-8 *SR/vmax * lam * gf * (nl/gl - nu/gu) /clfac
    opal=const*(occlow-occup)/clfac
    sline=hc2_8/lam3/(occlow/occup-1.d0)

!    print*,lam,'explicit'
!    do ll=1,nd
!      print*,opal(ll),sline(ll)
!    enddo

    icount_expl=icount_expl+1
    
  else
! background element
    k = index/1000000
    k_range(ii)=k
    irest = index - k*1000000
    j = irest/100000
    irest = irest - j*100000
    low = irest/100
    up  = irest-low*100
    lam=xlam1(ii)
    lam3=lam**3
    irec=indrec(k,j)
    const=c_tau1*lam*gf1(ii)
    if(up.ne.0) then
!
! lower and upper level  present
!
      if(jatom_full(k).eq.1) then
! selected elements, division lwion1
        do  ll=1,lwion1-1
!outside
          occlowi=occng(irec,low,ll) ! occng = n/g
          if(j.lt.met_imin(k,ll).or.j.gt.met_imax(k,ll).or.occlowi.eq.0.) then
            opal(ll)=0.
            sline(ll)=0.
          else
            occupi=occng(irec,up,ll)
            opal(ll)=const*(occlowi-occupi)/clfac(ll)
            sline(ll)=hc2_8/lam3/(occlowi/occupi-1.d0)
          endif
        enddo 

        do ll=lwion1,nd 
!inside
          occlowi=occng(irec,low,ll) ! occng = n/g
          occupi=occng(irec,up,ll)
          if(occlowi*occupi.eq.0) then
            print*,index,' ',lam
            stop' occlow or occup eq 0 (inner selected elements) in calc_lines'
          endif
          opal(ll)=const*(occlowi-occupi)/clfac(ll)
          sline(ll)=c2/(exp(c1/lam/te(ll))-1.d0)/lam3  
        enddo

!        print*,'selected with upper level'
!        print*,lam,k,j,low,up
!        do ll=1,nd
!          print*,opal(ll),sline(ll),met_imin(k,ll),met_imax(k,ll)
!        enddo
        
      else
! other elements (approximate), division lwion
        do  ll=1,lwion-1
          occlowi=occng(irec,low,ll) ! occng = n/g
          occupi=occng(irec,up,ll)
          if(occlowi*occupi.eq.0) then !change of ionization
           print*,index,' ',lam
           stop' occlow or occup eq 0 (outer approx. elements) in calc_lines'
          endif
          opal(ll)=const*(occlowi-occupi)/clfac(ll)
          sline(ll)=hc2_8/lam3/(occlowi/occupi-1.d0)
        enddo 
        do ll=lwion,nd 
!inside
          occlowi=occng(irec,low,ll) ! occng = n/g
          occupi=occng(irec,up,ll)
          if(occlowi*occupi.eq.0) then
           print*,index,' ',lam
           stop' occlow or occup eq 0 (inner approx.  elements) in calc_lines'
          endif
          opal(ll)=const*(occlowi-occupi)/clfac(ll)
          sline(ll)=c2/(exp(c1/lam/te(ll))-1.d0)/lam3  
        enddo

!        print*,'bg approx. with upper level'
!        print*,lam,k,j,low,up
!        do ll=1,nd
!          print*,opal(ll),sline(ll),met_imin(k,ll),met_imax(k,ll)
!        enddo
        
      endif !jatom_full
      icount_bg=icount_bg+1

!
! only lower level  present
!
    else
!
! prepare two-level approach
! sline approximated by two level atom; see subr. netmat_met

! calculate frequency dependent quantities (interpolate)
      ener=log10(lam)
      ifstart=ifre
      do kk=ifstart,2,-1
        if(fre(kk).le.ener .and. fre(kk-1).gt.ener) goto 10
      enddo
      stop' ener not found in fre (calc_lines)'
10    ifstart=kk
      q=(ener-fre(kk))/(fre(kk-1)-fre(kk))
      q1=1.d0-q
      xj1=q1*xj(:,kk)+q*xj(:,kk-1)
      xk1=q1*xxk(:,kk)+q*xxk(:,kk-1)
      xj1=10.d0**xj1
      xk1=10.d0**xk1
      eddtest=xk1(nd)/xj1(nd)
      if(eddtest.lt.0.31 .or. eddtest.gt.0.35) &
&        stop' inconsistent Edd.factor in calc_lines'

      lth=lthin(kk)
      where(lll.lt.lth)
        mustar=1.d0-(r(lth)/r)**2 !correction for lthin (checked)
        mustar=sqrt(mustar)
        eddf=(1.d0-mustar**3)/3.d0/(1.d0-mustar)! (checked)
      elsewhere          
        eddf=xk1/xj1
      endwhere
! comment: eddf checked in opacity_cmf,
! smooth transition between outer and inner region,
! goes to unity for large radii 

! prepare calculation of eps' = Cul/Aul (1-exp(-u0)) 
! (see also subroutine opacitl and sumopal)
      u0=1.4388d8/lam*te
      vanreg=collfac*lam**3*(1.d0-exp(-u0)) !should be OK, but not expl. checked
! vanreg1 propto lam^2 needs to be finally divided by gf 
      vanreg1=vanreg*c_forbid/lam  !should be OK 
      vanreg=vanreg/(1.+vanreg)
!      print*
!      print*,lam,vanreg(1),vanreg1(1)
!      print*,lam,vanreg(43),vanreg1(43)
!      print*,lam,vanreg(nd),vanreg1(nd)
!
! line specific quantities, at all depth-points
!
      if(gf1(ii).le.gfcut) then
        eps=vanreg1/gf1(ii)
        eps=eps/(1.+eps)           
      else
        eps=vanreg
      endif
!
      if(jatom_full(k).eq.1) then
! selected elements, division lwion1
        do  ll=1,lwion1-1
!outside
          occlowi=occng(irec,low,ll) ! occng = n/g
          if(j.lt.met_imin(k,ll).or.j.gt.met_imax(k,ll).or.occlowi.eq.0.) then
            opal(ll)=0.
            sline(ll)=0.
          else
            opal(ll)=const*occlowi/clfac(ll) ! includes gstat, ind. emission negl.
            taubi=opal(ll)/(eddf(ll)*dvdr(ll)+(1.-eddf(ll))*vr(ll)) ! at mue^2 = eddf
! pi e2/me c * 1.d-8 *SR/vmax * lam * gf * nl/gl / clfac / (eddf*dvdr + (1-eddf*v/r))
        
            if(taubi.lt.1.d-5) then
            betacic=xj1(ll)
            else
            betacic=(1.-exp(-taubi))/taubi*xj1(ll)
            endif
 
            taui=opal(ll)*tau0(ll)  ! at mue^2 = 1/3
! pi e2/me c * 1.d-8 *SR/vmax * lam * gf * nl/gl / clfac / (dvdr/3. + 2./3.*v/r)
            if(taui.lt.1.d-5) then
            beta=1.d0
            else
            beta=(1.-exp(-taui))/taui
            endif
!
! now, we can calculate the two-level source-function. We inline Bnue, &
! and evaluate at actual frequency, to ensure correct thermalization
!
            epsi=eps(ll)
            eps1i=1.d0-epsi
            bni = c2/ (exp(c1/lam/te(ll))-1.d0)/lam3  
            sline(ll)=(eps1i*betacic+epsi*bni)/(epsi+beta*eps1i)
!        
          endif
        enddo
        do ll=lwion1,nd 
!inside
          occlowi=occng(irec,low,ll) ! occng = n/g
          if(occlowi.eq.0) then
           print*,index,' ',lam
           stop' occlow eq 0 (inner selected elements) in opacity_cmf'
          endif
          opal(ll)=const*occlowi/clfac(ll)
          sline(ll)=c2/(exp(c1/lam/te(ll))-1.d0)/lam3  
        enddo

!        print*,'selected with lower level only'
!        print*,lam,k,j,low,up
!        do ll=1,nd
!          print*,opal(ll),sline(ll),met_imin(k,ll),met_imax(k,ll)
!        enddo

      else
! other elements (approximate), division lwion
        do  ll=1,lwion-1
            occlowi=occng(irec,low,ll) ! occng = n/g
            if(occlowi.eq.0) then
             print*,index,' ',lam
             stop' occlow eq 0 (outer approx. elements) in opacity_cmf'
            endif
            opal(ll)=const*occlowi/clfac(ll) ! includes gstat, ind. emission negl.
            taubi=opal(ll)/(eddf(ll)*dvdr(ll)+(1.-eddf(ll))*vr(ll)) ! at mue^2 = eddf
! pi e2/me c * 1.d-8 *SR/vmax * lam * gf * nl/gl / clfac / (eddf*dvdr + (1-eddf*v/r))
        
            if(taubi.lt.1.d-5) then
            betacic=xj1(ll)
            else
            betacic=(1.-exp(-taubi))/taubi*xj1(ll)
            endif
 
            taui=opal(ll)*tau0(ll)  ! at mue^2 = 1/3
! pi e2/me c * 1.d-8 *SR/vmax * lam * gf * nl/gl / clfac / (dvdr/3. + 2./3.*v/r)
            if(taui.lt.1.d-5) then
            beta=1.d0
            else
            beta=(1.-exp(-taui))/taui
            endif
!
! now, we can calculate the two-level source-function. We inline Bnue, &
! and evaluate at actual frequency, to ensure correct thermalization
!
            epsi=eps(ll)
            eps1i=1.d0-epsi
            bni = c2/ (exp(c1/lam/te(ll))-1.d0)/lam3  
            sline(ll)=(eps1i*betacic+epsi*bni)/(epsi+beta*eps1i)
!        
        enddo
        do ll=lwion,nd 
!inside
          occlowi=occng(irec,low,ll) ! occng = n/g
          if(occlowi.eq.0) then
           print*,index,' ',lam
           stop' occlow eq 0 (inner approx. elements) in opacity_cmf'
          endif
          opal(ll)=const*occlowi/clfac(ll)
          sline(ll)=c2/(exp(c1/lam/te(ll))-1.d0)/lam3  
        enddo

!        print*,'bg approx. with lower level only'
!        print*,lam,k,j,low,up
!        do ll=1,nd
!          print*,opal(ll),sline(ll),met_imin(k,ll),met_imax(k,ll)
!        enddo

      endif
!        
      icount_bg_noup=icount_bg_noup+1
      
    endif!up ne 0
  endif! foreground or background
  
  do ll=1,nd
    if(opal(ll).lt.0.) then
      if(sline(ll).gt.0.) stop' negative OPAL and positive SLINE'
      inverted_range(ii)=1
      n_inverted=n_inverted+1
      exit     
    endif
  enddo  

  do ll=1,nd
    if(opal(ll).eq.0.) then
      if(sline(ll).ne.0.) stop' OPAL = 0 and SLINE ne 0'
      inverted_range(ii)=2
      exit     
    endif
  enddo  

  maxtau=0.
  do ll=1,lwion-1
    maxtau=max(maxtau,abs(opal(ll))*tau0(ll))
  enddo  
  if(maxtau.lt.tauweak) then
    inverted_range(ii)=3
    n_weak=n_weak+1
  endif  

!  if(lam.gt.4471. .and. lam.lt.4473. .and. inverted_range(ii).ne.3) &
!& print*,index,lam,gf1(ii)    
  
!WARNING WARNING WARNING
!lines with inverted_range=2 (opal/sline=0 at some points)
!can be inverted at different points!!!! Take care!
  
  if(inverted_range(ii).eq.0) then
! prepare for interpolation
    opal_range(:,ii)=log(opal(:)*const_opal)
    sline_range(:,ii)=log(sline(:))
  else if (inverted_range(ii).eq.1) then
! special treatment
!JO so far, we neglect a correct treat, and use abs(opal)  and abs(sline),
!since (i) eta = opal*sline is positive and correct for inversion, and
!     (ii) the difference in tau should be negligible in the affected regions.
!nevertheless, this has to be tested carefully later on 
    opal_range(:,ii)=log(abs(opal(:))*const_opal)
    sline_range(:,ii)=log(abs(sline(:)))
  else if (inverted_range(ii).eq.2) then
!keep opal/sline 0 when 0, will be caught in subr. prepray_range
    do ll=1,nd
      if(opal(ll).eq.0) then
        opal_range(ll,ii)=0.
        sline_range(ll,ii)=0.
      else
        opal_range(ll,ii)=log(abs(opal(ll))*const_opal)
        sline_range(ll,ii)=log(abs(sline(ll)))
      endif
    enddo   
  endif    

  if(k_range(ii).lt.3) icount_hhe=icount_hhe+1
   
enddo all_lines

!create specific index_file for H/He lines
allocate(index_hhe(icount_hhe),l23(icount_hhe),beta_p(icount_hhe))

icount_hhe=0
do ii=iblu,ired
  if(k_range(ii).lt.3) then
    icount_hhe=icount_hhe+1
    index_hhe(icount_hhe)=ii
  endif
enddo  

print*
print*,icount_expl+icount_bg+icount_bg_noup,' line opacities/emissivites calculated'
print*,icount_expl,' lines from explicit elements' 
print*,icount_hhe,' lines from H/He' 
print*,icount_bg,' lines from background elements with upper level' 
print*,icount_bg_noup,' lines from background elements with no upper level' 
print*,n_weak,' weak lines with tau_sob < ',tauweak
print*,n_inverted,' inverted lines'

! calculation of Stark broadening profiles etc.
if (icount_hhe .gt. maxncomp) then
  print*,'NO OF H/HE LINES:',icount_hhe
  stop' TOO MANY H/HE LINES FOR STARK BROADENING (INCREASE MAXNCOMP)'
endif
  
allocate(level(icount_hhe), leveu(icount_hhe),lineno(icount_hhe), &
         nlevl(icount_hhe), nlevu(icount_hhe),nstark(icount_hhe))

nstark=1
lineno=0

do i=1,icount_hhe
    ii=index_hhe(i)
    index=id1(ii)
    if (index.lt.1d8) stop' problems with index (HHe)'
    kk = index/1d8
    irest = index - kk*1d8
    ml = irest/10000
    mu = irest - ml*10000
    level(i)=labl(ml)
    leveu(i)=labl(mu)
enddo  

call preformal(modnam,icount_hhe,level,leveu,lineno, &
               nstark,vturb,yhe,balmer_lemke,.true.)

!test IX.DAT file (should be sorted for frequency) and consistency
open (1,file=trim(modnam)//'/IX.DAT',status='unknown',form='formatted')  
rewind 1  

read (1,*) ncomp
if(ncomp.ne.icount_hhe) stop' problem with no of components in IX.DAT'

do i = 1,ncomp  
     ii=index_hhe(i)
     read (1,*) xlamb
     if (abs(xlamb-xlam1(ii)).gt. 0.1) &
&     print*,' check wavelengths (IX.DAT)! ',xlamb,' ',xlam1(ii)      
     read (1,*) gll,glu  
     read (1,*) gf  
     if (abs(log10(gf/gf1(ii))).gt. 0.1)&
&     print*,' check oscillator strength (IX.DAT! ',gf,' ',gf1(ii) 
     read (1,*) nlevl(i),nlevu(i)
     read (1,*) weight  
     read (1,*) nst 
end do  
close (1)  

print* 
print*,' Stark profiles prepared, all data consistent'  

deallocate(level, leveu, lineno)

!for all Stark-broadened lines, calculate OPAL/OPAC  at tauc=0.66
!further used in calcxmax_range

ifstart=ifre

do i=1,icount_hhe
    ii=index_hhe(i)
    lam=xlam1(ii)

! determine (approx) where tauc=2/3 
    ener=log10(lam)
    do kk=ifstart,2,-1
        if(fre(kk).le.ener .and. fre(kk-1).gt.ener) goto 100
    enddo
    stop' ener not found (2) in fre (calc_lines)'

100 ifstart=kk

!use kk to allow for larger opacity in case of edges, to be on the safe side
    tauc=0.
    do ll=2,nd
      tauc=tauc+0.5*(opac(ll-1,kk)+opac(ll,kk))*(r(ll-1)-r(ll))*sr
      if (tauc.gt.0.66) goto 110
    enddo
    stop' tauc = 2/3 not found (calc_lines)'

110 l23(i)=ll
    beta_p(i)=exp(opal_range(ll,ii))*const_opal_stark/lam/(opac(ll,kk)*sr)    
!    print*,i,lam,l23(i),beta_p(i)
enddo

return
end
!
!-----------------------------------------------------------------------
!
subroutine formal_range(nd,np,nc,vdop,rmax,r,p,z,v,ltot,lmax, &
&                       r1,v1,xne1,temp1,index1,iblu,ired, &
&                       lam_blu, lam_red, resol, sr, vmax, &
&                       opacon, scont, &
&                       zray,opacray,sconray,opalray,sray)

!here: nd actual length of microgrid
!      ndm = id_depfi maximum length
!      nd1 = id_ndept length of standard grid
!      variables with ending'1' refer to standard grid 

use nlte_type
use nlte_dim, ONLY: id_ndept, id_depfi, id_cores, id_nocor, &
                    id_maxww,id_maxtt,id_maxne,id_maxps
use fund_const, ONLY: pi
use formalsol_var, ONLY: nws,nts,nes,qhalf,dws,ts,es,ps, &
    modnam=>file,nstark,nlevl,nlevu
use formalsol_add_var, ONLY: lamgrid,icount_hhe,dlam_max_stark,l23,beta_p, &
&                            qstore,lstore
implicit none
!
!
!     .. parameters ..
integer(i4b), parameter :: nd1=id_ndept,ndm=id_depfi,lto1=2*ndm  
integer(i4b), parameter :: nc1=id_cores  
integer(i4b), parameter :: np1=nc1+2+4*id_nocor
!     ..
!     .. scalar arguments ..
real(dp), intent(in) :: vdop, rmax, lam_blu, lam_red, resol, sr, vmax
integer(i4b), intent(in) :: iblu, ired

integer(i4b) ::  nd,np,nc

!     ..
!     .. array arguments ..

real(dp) ::  r(ndm), v(ndm), p(np1), z(ndm,np1), &
&            r1(nd1), v1(nd1), xne1(nd1), temp1(nd1), index1(ndm)


real(dp), dimension(lto1) :: opacon, scont !note, dimension changed from (ndm,2) to 2*ndm
real(dp), dimension(lto1) :: zray,opacray,sconray,opalray,sray

integer(i4b) ::  lmax(np1),ltot(np1)
!     ..
!     .. local scalars ..

real(dp) :: delp, dd, z2, dtdr, xlambda, aic, dbdr, w, w1, ww, ww1, xicor, &
            emint, emint1, errmax, relem, &
            t_start, t_end, time

integer(i4b) :: jp, ndelt, irest, istart, ijp, l, j, nf, lm, k, lm1, ltaumax, nsum

logical core,first
!     ..
!     .. local arrays ..
real(dp) :: temp(ndm), xnefi(ndm), weight_range(ndm), &
&           tempray(lto1),xneray(lto1),taucon(lto1),fscon(lto1),tauray(lto1)  
!     ..
real(dp),dimension(:), allocatable :: tau, s1pabs, s1pem, s2pem
real(dp),dimension(:), allocatable :: conabs, conem1, conem2, &
&                                     profcon, profabs, profem, &
&                                     profile, profred 
integer(i4b),dimension(:), allocatable :: nl,nu
real(dp),dimension(:,:), allocatable :: zg

!     .. external functions ..
real(dp) ::  bnue,dbdt  
external bnue,dbdt  

if (nd.gt.ndm) stop ' nd > ndm in formal_range'  

!note: until here, vdop=vturb/vinf
!
!-----sets up grid variables
!
!     calculation of the p-grid
!
do jp = 1,nc  
     p(jp) = .99d0*dble(jp-1)/dble(nc-1)  
end do  

p(nc+1) = 1.d0  
p(nc+2) = 1.d0 + 1.d-4  

ndelt = (np- (nc+2))/4  

irest = (4*ndelt+1-nd1)  
if (irest.eq.0) then  
     istart = 1  

else if (irest.eq.2) then  
     istart = -1  

else  
     stop ' something wrong with number of p-rays'  
end if  

do jp = 1,ndelt  
     ijp = nc + 2 + 4*jp  
     p(ijp) = r1(nd1+1- (istart+4*jp))  
     delp = p(ijp) - p(ijp-4)  
     dd = delp/4.d0  
     do j = 1,3  
          p(ijp-j) = p(ijp) - j*dd  
     end do
end do  

p(np) = rmax  
!
!     calculation of the z-grid (fine)
!
do jp = 1,np - 1  
     do l = 1,nd  
          z2 = r(l)**2 - p(jp)**2  

          if (z2.ge.-1.d-10) then  
               if (z2.lt.0.d0) z2 = 0.d0  
               z(l,jp) = sqrt(z2)  
               lmax(jp) = l  
          else  
               exit  
          end if  
     end do
end do

z(1,np) = 0.d0  
lmax(np) = 1  

!vplus = 0.d0  
!do i = 1,nb  
!     vplus = max(vplus,xmax(i))  
!end do  

!print *  
!print *,' stark/doppler broadening effective for xmax = ',vplus  
!print *  

call xgrid_range(lam_blu,lam_red,resol,nf)

call interpol_range(r,temp,temp1,xnefi,xne1,index1,weight_range)

allocate(tau(nf),s1pabs(nf),s1pem(nf),s2pem(nf), &
&        profabs(nf),profem(nf),profile(nf),profred(nf))
allocate(conabs(nf),conem1(nf),conem2(nf),profcon(nf))
allocate(zg(np1,nf))

errmax=0.d0

tau=0.d0
! so far, tau not used; if needed, check formalsol how to implement
s1pabs=0.d0
s1pem=0.d0
s2pem=0.d0
conabs=0.d0
conem1=0.d0
conem2=0.d0

dtdr=(temp1(nd1)-temp1(nd1-1)) / (r1(nd1-1)-r1(nd1))  

!read Stark-profiles
nsum=sum(nstark)

if (nsum.gt.0) then 
!nstar, nlevl, nlevu already allocated in calc_lines
     allocate(nws(icount_hhe),nts(icount_hhe),nes(icount_hhe),qhalf(icount_hhe))
     allocate(nl(icount_hhe),nu(icount_hhe),dlam_max_stark(icount_hhe))
     allocate(dws(id_maxww,icount_hhe))
     allocate(ts(id_maxtt,icount_hhe))
     allocate(es(id_maxne,icount_hhe))
     allocate(ps(id_maxps,icount_hhe))
     nl=nlevl
     nu=nlevu
     call staread(modnam,icount_hhe,nsum,nl,nu)
     call calcxmax_range(temp1,xne1)
endif

time=0.

jploop: do jp = 1,np - 1  
!jploop: do jp = 20,25
  lm = lmax(jp)  
  core = lm .eq. nd
  first=.true.
  
kloop: do k = 1,nf
!kloop: do k = 4,4  
    xlambda=lamgrid(k)
    aic=bnue(xlambda,temp1(nd1))
    dbdr=dbdt(xlambda,temp1(nd1))*dtdr

    zg(jp,k)=-10.*rmax
    
!    print*,k,jp
    call cpu_time(t_start)
    call prepray_range(first,xlambda,k,jp,z(1,jp),p,ltot,np,lm,core, &
&                      w,w1,vdop,iblu,ired, &
&                      r,v,temp,xnefi, &
&                      r1,index1,sr,vmax, &
&                      opacon,scont,weight_range, &
&                      zray,tempray,xneray, &
&                      opacray,sconray,opalray,sray,tauray)
    call cpu_time(t_end)
    time=time+t_end-t_start
    first=.false.
    
    ww = w*pi/ (r(1)*r(1))  
    ww1 = w1*pi/ (r(1)*r(1))  

    lm1=ltot(jp)
  
    call formacon_range(taucon,ltaumax,fscon,opacray,sconray,zray,lm1)
! ltaumax index where tau_cont > taumax
    
    if (core) xicor=aic+z(lm1,jp)*dbdr/opalray(lm1) !for lines + cont
    
    call obsfram_range(lm1,core,zray,opalray,sray,tauray,xicor,zg(jp,k), &
                       emint,emint1)

    !zg not tested so far
    
    s1pabs(k) = s1pabs(k) + ww*emint1  
    s1pem(k) = s1pem(k) + ww*emint  
    if (core) then
      xicor=aic+z(lm1,jp)*dbdr/opacray(lm1) !for cont
      conabs(k) = conabs(k) + ww*xicor*exp(-taucon(lm1))
    endif
    conem1(k) = conem1(k) + ww*fscon(1)
          
    if (jp.ge.nc+2) then  
        s2pem(k) = s2pem(k) + ww1*emint  
        conem2(k) = conem2(k) + ww1*fscon(1)
    end if  
    
   end do kloop
   print*,jp,' from a total of ',np-1,' rays done'

end do jploop  

do k = 1,nf
     profcon(k) = conem1(k) + conem2(k) + conabs(k)  
     profabs(k) = s1pabs(k)  
     profem(k) = s1pem(k) + s2pem(k)  

     if (s2pem(k).eq.0. .and. profem(k).eq.0.) then  
          relem = 0.d0  
     else  
          relem = abs(s2pem(k)/profem(k))  
     end if  

     errmax = max(errmax,relem)  
     profile(k) = profabs(k) + profem(k)  
     profcon(k) = conem1(k) + conem2(k) + conabs(k)  
     profred(k) = profile(k)/profcon(k)  
end do  

do k = 1,nf  
    xlambda=lamgrid(k)
    write (*,fmt=9000) k,xlambda,profcon(k),profred(k),profile(k)
    write (2,fmt=9000) k,xlambda,profcon(k),profred(k),profile(k)
end do  

errmax = 15.d0*errmax  
if (errmax.gt..03) print*,' WARNING!!! WARNING!!! WARNING!!! WARNING!!!'

print*  
print*,' MAXIMUM ERROR IN ANGULAR INTEGRATION ',errmax  
print*  

if (errmax.gt..03) then  
     print*,' WARNING!!! WARNING!!! WARNING!!! WARNING!!!'  
     print *  
end if  

deallocate(tau,s1pabs,s1pem,s2pem, &
&          profabs,profem,profile,profred)
deallocate(conabs,conem1,conem2,profcon)
deallocate(zg)
deallocate(l23,beta_p,nlevl,nlevu)
deallocate(qstore,lstore)

if (nsum.gt.0) then
deallocate(nstark,dlam_max_stark,nl,nu)
deallocate(nws,nts,nes,qhalf)
deallocate(dws,ts,es,ps)
endif

print*,'time=',time

return  

9000 format (1x,i5,4(2x,g14.6))  

end
!
!-----------------------------------------------------------------------
!

subroutine xgrid_range(lam_blu,lam_red,resol,nf)
!
use nlte_type
use nlte_dim, ONLY: id_ndept, id_depfi
use formalsol_add_var, ONLY: lamgrid

implicit none
!
!     .. parameters ..
integer(i4b), parameter :: nd1=id_ndept,ndm=id_depfi  

real(dp), intent(in) :: lam_blu, lam_red, resol
integer(i4b), intent(out) :: nf

integer(i4b) :: i

nf=int((lam_red-lam_blu)/resol)+1

allocate(lamgrid(nf))

do i=1,nf
  lamgrid(i)=lam_blu+(i-1)*resol
enddo
  if(abs(lamgrid(nf)-lam_red).gt.resol) stop' error in xgrid_range'
print*,' ACTUAL lambda_red = ',lamgrid(nf)

return
end
!
!-----------------------------------------------------------------------
!
subroutine interpol_range(r,temp,t,xnefi,xne,index1,weight_range)

use nlte_type
use nlte_dim, ONLY: id_ndept, id_depfi

implicit none

integer(i4b), parameter :: nd1=id_ndept,ndm=id_depfi

real(dp), dimension(ndm), intent(in) :: r
real(dp), dimension(nd1), intent(in) :: t,xne
real(dp), dimension(ndm), intent(out) :: temp, xnefi, weight_range

integer(i4b), dimension(nd1), intent(in) :: index1

integer(i4b) :: i, j, j1, j2

real(dp) :: rlj, rlj1, rlj2,tl, tl1, tl2, factor

! calculating interpolation weights, and
! interpolation of temperature and n_e (and other quantities when needed) 
weight_range = 0.
  
temp(1) = t(1)
xnefi(1) = xne(1)
iloop: do i = 2,nd1  
     j1 = index1(i-1)  
     j2 = index1(i)  
     temp(j2) = t(i)
     xnefi(j2) = xne(i)
     
jloop: do j = j1 + 1,j2 - 1  
          rlj = log(r(j))  
          rlj1 = log(r(j1))  
          rlj2 = log(r(j2))  
          factor = (rlj-rlj1)/ (rlj2-rlj1)  
          tl1 = log(temp(j1))  
          tl2 = log(temp(j2))  
          tl = tl1 + factor* (tl2-tl1)  
          temp(j) = exp(tl)
          tl1 = log(xnefi(j1))  
          tl2 = log(xnefi(j2))  
          tl = tl1 + factor* (tl2-tl1)  
          xnefi(j) = exp(tl)
          weight_range(j) = factor
     end do jloop     
end do iloop

return
end
!
!-----------------------------------------------------------------------
!
subroutine prepray_range(first,xlamobs,k,jp,z,p,ltot,np,lmax,core, &
                         w,w1,vvdop,iblu,ired, &
&                        r,v,temp,xne, &
&                        r1,index1,sr,vmax, &
&                        opacon,scont,weight_range, &
&                        zray,tempray,xneray, &
&                        opacray,sconray,opalray,sray,tauray)
!JO: if elements between He and C are implemented, change first 'heavy'
!   element (first_hel = 6 so far)  

!vvdop is vturb/vinf
!remember: lines with inverted_range=2 (opal/sline=0 at some points)
!          can be inverted at different points!!!! 
!might be few weak lines (inverted_range=3) which have an influence on the
!          spectrum, when strong in intermediate/lower photosphere  
  
use nlte_type
use fund_const, ONLY: clight, amh, akb, sqpi
use nlte_dim
use formalsol_var, ONLY: vzrp, xcmfp, wp, wp1, modnam=>file, nstark
use formalsol_add_var, ONLY: taumax, natom, aweight, &
&    id1, xlam1, gf1, k_range, inverted_range, &
&    index_hhe, icount_hhe, vmax_broad, xmax, dlam_max_stark, &
&    opal_range, sline_range, qstore, lstore

implicit none
!
!     defining whole rays including backward half-sphere
!
!     .. parameters ..
integer(i4b), parameter :: ndm=id_depfi,lto1=2*ndm,nc=id_cores  
integer(i4b), parameter :: nd1=id_ndept, ifretot=id_frec1  
integer(i4b), parameter :: np1=nc+2+4*id_nocor,nfmax=id_nfesc  

integer(i4b), parameter :: first_hel=6

real(dp), parameter :: xnemin=10.d0**10.5
real(dp), parameter :: const_vth=2.d0*akb/amh
real(dp), parameter :: const_opal_stark=sqpi*clight*1.d8

!     ..
!     .. scalar arguments ..
real(dp), intent(in) ::  vvdop, xlamobs, sr, vmax
real(dp), intent(out) :: w,w1

integer(i4b), intent(in) ::  k,jp,np,lmax,iblu,ired

logical, intent(in) :: first, core  
!     ..
!     .. array arguments ..
real(dp), dimension(ndm), intent(in) :: z, r, v, temp, xne, weight_range
real(dp), dimension(np1), intent(in) :: p

real(dp), dimension(lto1) :: opacon, scont, opacray, sconray, opalray, sray
real(dp), dimension(lto1) :: zray, tempray, xneray, tauray

real(dp), intent(in) :: r1(nd1)
integer(i4b), intent(in) :: index1(nd1)
integer(i4b), intent(out) ::  ltot(np1)  
!     ..
!     .. local scalars ..
real(dp) :: vzr, vzr1, delv, zi, z1, deltaz, delz, &
&           t1, delt1, xn1, delxn1, delzi, dellin, &  
&           ddp, dpp3, dpold, dpp3old, sum1, sum2, sum3, sumex, &
&           enerobs, enercmf, opacon_last, scont_last, &
&           q, tl1, tl2, factor, vturb, vturb2, vth, &
&           lammin, lammax, xl, xk, rmin, p2, logr, q1, opal, sl, &
&           xlamcmf, perf, t_start, t_end

real(dp) :: perfil2

real(dp), save :: vc, vdopmin, vdopmax        
  

integer(i4b) :: i, j, l, ll, lm1, l1, ndel, &
&               ip, irest, kkstart, kk, j1, j2, &
&               lmax1, ltot1, jj, jj1, jj2, kel, lxne, &
&               indstart, indend, nlines, &
&               indstart_hhe, indend_hhe, nlines_hhe, loop_count, &
&               lmz0, irstart


integer(i4b), save :: ifre

!     .. local arrays ..
real(dp), dimension(nd1), save :: logr1

real(dp), dimension(lto1), save :: zcon, qarr
integer(i4b), dimension(lto1), save :: indexz

real(dp), dimension(ifretot), save :: fre
real(dp), dimension(nd1,ifretot), save :: opac, scon

real(dp), allocatable, dimension(:,:), save :: vthray

logical start, optz0, flag
!     ..

data start /.true./

if(start) then

  logr1=log(r1)
  vc=vmax/clight
  vdopmin = vvdop/5.d0  
  vdopmax = vdopmin*2.d0  

  open (1,file=trim(modnam)//'/CONT_FORMAL_ALL',status='unknown', &
&         form='unformatted')

  read (1) ifre, (fre(i),i=1,ifretot)
  close(1)
  
  open (1,file=trim(modnam)//'/CONT_FORMAL_CMF',status='unknown', &
&         form='unformatted')

  read (1) ((scon(i,j),i=1,nd1),j=1,ifretot), & !cont source function (CMF)
&          ((opac(i,j),i=1,nd1),j=1,ifretot)    !opac_nolines
  close (1)  

! for interpolation below
  do i=1,nd1
    do j=1,ifre
      scon(i,j)=log(scon(i,j)) 
      opac(i,j)=log(opac(i,j)) 
    enddo
  enddo  
      
  start=.false.
endif  

if (first) then  
! calculate frequency independent quantities along ray
! (so far, z, temp, xne)
!
optz0 = .false.  
!
!-----core rays and first point of non-core rays without interpolation
!
!NOTE: in contrast to standard approach, xcmpf now is +mu(r)*v(r)
!don't fiddle around with vzrp, since this is  an auxillary array needed
!(as it is) to do a correct interpolation!!!

     if (core) then  
          ltot(jp) = lmax  
          lm1 = lmax
          ltot1 = lmax
     else  
          lm1 = 1  
     end if  

     do l = 1,lm1  
          vzr = v(l)*z(l)/r(l)  
          xcmfp(l) = vzr  ! different 
          tempray(l) = temp(l)  
          xneray(l) = xne(l)  
          zray(l) = z(l)  
     end do
!
!-----non core rays with interpolation, and definition of zcon (zgrid for opacon and scont)
!     difference between zcon and zray:
!     zcon corresponds to (z,0,-z): thus dim(zcon) = ltot1=2*lm1-1
!          with lm1=lmax (optz0) or lm1=lmax+1 (.not.optz0) before interpol.
!          note that after interpolation lm1 changes value,
!          becomes total number of points including interpolated ones
!     zray includes interpolated values if delv too large; dim(zray)=ltot(jp)
!
     if (.not.core) then  
! consistent with calculation of opacon and scont below
          zcon(1:lmax)=z(1:lmax)       

          if (z(lmax).eq.0.d0) then  
               optz0 = .true.  
               lm1 = lmax  
          else  
               lm1 = lmax + 1  
               zcon(lm1)=0.d0
          end if  

          ltot1=2*lm1-1

          do l=lm1+1,ltot1
            jj=2*lm1-l
            zcon(l)=-z(jj)
          enddo  

          do l = 1,lmax  
               vzrp(l) = v(l)*z(l)/r(l)  
          end do            
          
          if (.not.optz0) vzrp(lm1) = 0.d0  
          
          i = 1  

lloop1:   do l = 1,lm1 - 1  
               l1 = l + 1  
               vzr = vzrp(l)  
               vzr1 = vzrp(l1)  
               delv = vzr - vzr1  
               if (delv.lt.vdopmax) then  
!
!     no interpolation necessary
!
                    i = i + 1  
                    if (i.gt.lto1) stop &
&                    ' more than lto1 points used in interpolation!'
                    xcmfp(i) = vzr1 !different  
                    
                    if (optz0 .or. l1.ne.lm1) then  

                         tempray(i) = temp(l1)  
                         xneray(i) = xne(l1)  
                         zray(i) = z(l1)  
!
!     different treatment for last point with optz0 = false, assuming
!     constant source function and opacity from z(lmax) to z=0.
!
                    else  
                         tempray(i) = temp(l)  
                         xneray(i) = xne(l)  
                         zray(i) = 0.d0  
                    end if  
!
!     interpolation linearly with z
!
               else  

                    ndel = int(delv/vdopmin) + 1  
                    if (ndel.lt.2) stop ' ndel < 2'  
                    zi = z(l)  

                    if (optz0 .or. l1.ne.lm1) then  
                         z1 = z(l1)  
                         deltaz = zi - z1  
                         delz = deltaz/dble(ndel)  
                         deltaz = 1.d0/deltaz  
                         t1 = temp(l)  
                         delt1 = temp(l1) - temp(l)  
                         xn1 = xne(l)  
                         delxn1 = xne(l1) - xne(l)  
!
                    else  
!
!     different treatment for last point with optz0 = false, assuming
!     constant source function and opacity from z(lmax) to z=0.
!
                         z1 = 0.d0  
                         deltaz = zi - z1  
                         delz = deltaz/dble(ndel)  
                         deltaz = 1.d0/deltaz  
                         t1 = temp(l)  
                         delt1 = 0.d0  
                         xn1 = xne(l)  
                         delxn1 = 0.d0  

                    end if  

                    kloop: do kk = 1,ndel  
                         i = i + 1  
                         if (i.gt.lto1) stop &
&                         ' more than lto1 points used in interpolation!'
                         delzi = kk*delz  
                         dellin = delzi*deltaz  
                         zray(i) = zi - delzi  
!
!     attention!! explicitly accounted for sign of delv
!
                         xcmfp(i) =  vzr - delv*dellin !different 
                         if(kk.eq.ndel) then
!can happen due to calculation, and leads to inconsistent grids
                           if(abs(zray(i)-z1).gt.1.d-10) stop' error in zray(ndel) -- prepray-range'
                           zray(i)=z1
                           xcmfp(i)=vzr1
                         endif  
                         tempray(i) = t1 + delt1*dellin  
                         xneray(i) = xn1 + delxn1*dellin  
                    end do kloop

               end if  

          end do lloop1  
!
!     defining quantities on the back hemisphere
!
          lm1 = 2*i - 1  
          if (lm1.gt.lto1) stop &
&                   ' more than lto1 points used in interpolation!'
          ltot(jp) = lm1  

          do l = 1,i - 1  
               ll = 2*i - l  
               vzr = -xcmfp(l)  ! change of sign remains 
               xcmfp(ll) = vzr  
               tempray(ll) = tempray(l)  
               xneray(ll) = xneray(l)  
               zray(ll) = -zray(l)  
          end do
!
!     end of non-core ray treatment
!
     end if

!for tests
!     print*,'jp=',jp
!     do ll=1,lm1
!       write(*,fmt='(i4,2(2x,f10.4),2x,e10.4)') ll,zray(ll),log10(xneray(ll)),xcmfp(ll)
!     enddo  
     
! calculation of vtherm (in units of c)
     if(lm1.ne.ltot(jp)) stop' problem with lm1'

! now, xcmfp is mu*v(r)/c with v(r) in actual units 
     xcmfp(1:lm1)=xcmfp(1:lm1)*vc
     
     if(allocated(vthray)) deallocate(vthray)
     allocate(vthray(lm1,natom))
     vturb=vvdop*vmax 
     vturb2=vturb*vturb
     do ll=1,lm1
        vth=const_vth*tempray(ll) 
        do kel=1,natom
          vthray(ll,kel)=sqrt(vturb2+vth/aweight(kel))/clight
        enddo
     enddo   

! set up index array for interpolation of opacon to opacray etc. 
     if(lm1.ne.ltot1) then
       kkstart=1
       indexz=0
       qarr=1.d0
       do i=1,ltot1
          do l=kkstart,lm1
            if(zray(l).eq.zcon(i)) then
!              print*,jp,l,i,zray(l)
              indexz(l)=i
              kkstart=l+1
              goto 60
            endif
            indexz(l)=-i
            qarr(l)=(zray(l)-zcon(i))/(zcon(i-1)-zcon(i)) 
          enddo
          print*,'not found',jp,l,i,zcon(i)
          stop'zray not found in zcon (prepray_range)'  
60        continue
       enddo  
!for tests
!       do l=1,lm1
!         i=indexz(l)
!         if(i.ne.0) print*,jp,l,zray(l),i,zcon(abs(i))
!       enddo   
     endif
!
!     calculate w = weights for flux integral
!
     if (jp.eq.1) then  
          w = p(2)*p(2)/3.d0  
          w1 = 0.d0  

     else if (jp.lt.nc+2) then  
          w = (p(jp-1)+p(jp)+p(jp+1))* (p(jp+1)-p(jp-1))/3.d0  
          w1 = 0.d0  

     else if (jp.eq.nc+2) then  
          w = (p(jp)-p(jp-1))* (2*p(jp)+p(jp-1))/3.d0  
          ddp = p(jp+1) - p(jp)  
          dpp3 = p(jp)*ddp*2.d0/3.d0  
          w = w + dpp3  
          w1 = -dpp3/15.d0  

     else  
          ip = jp - (nc+2)  
          irest = mod(ip,4)  
          ddp = p(jp+1) - p(jp)  
          dpp3 = p(jp)*ddp*2.d0/3.d0  

          if (irest.eq.0) then  
               dpold = p(jp) - p(jp-1)  
               dpp3old = p(jp)*dpold*2.d0/3.d0  
               w = dpp3 + dpp3old  
               w1 = -w/15.d0  

          else if (irest.eq.1 .or. irest.eq.3) then  
               w = 4.d0*dpp3  
               w1 = w/15.d0  
          else  
               w = 2.d0*dpp3  
               w1 = -3.d0*w/15.d0  
          end if  

     end if  

     wp(jp) = w  
     wp1(jp) = w1  

     if (jp.eq.np-1) then  
          ddp = p(jp+1) - p(jp)  
          dpp3 = p(jp+1)*ddp*2.d0/3.d0  
          wp(np) = dpp3  
          wp1(np) = -dpp3/15.d0  
!
!-----test of integration weights
!
          sum1 = 0.d0  
          sum2 = 0.d0  
          sum3 = 0.d0  
          do ip = 1,np  
               sum1 = sum1 + wp(ip)  
               sum3 = sum3 + wp1(ip)  
               if (ip.ge.nc+2) sum2 = sum2 + wp1(ip)/p(ip)  
          end do

          if (abs(sum2).gt.1.d-5) stop 'error in sum2'  
          sumex = r(1)**2  

          if (abs(sum1+sum3-sumex).gt.1.d-2) then  
               print *,sum1 + sum3 - sumex  
               stop 'error in sum1'  
          end if  
     end if  
!
! if not first 
!
else  
!
!     w = weights for flux integral
!
     w = wp(jp)  
     w1 = wp1(jp)  

end if  
!
!-------------------------------------------------------------------------
!
! calculation of continuum quantities (w.r.t. CMF-freq) on micro-grid
! note: so far, continua 'only' corrected for micro-clumping

! interpolation in frequency and space;
! frequential interpolation only at macro-grid points with respect to CMF-freq.
! spatial interpolation to micro-grid only from freq.-interpolated macro-grid variables
! more exact method (commented out) also tested, almost identical results

! note: thus far, inversion treated in approx. way (by using abs)
!       thus: inverted_range=1 needs no special attention and can be calculated
!             in parallel with inverted_range=0 

! for both core and non-core rays
! front hemisphere
enerobs=1.d8/xlamobs

kkstart=1

j2=1
enercmf=enerobs*(1.d0-z(j2)*v(j2)*vc/r(j2))

do kk=kkstart,ifre-1
  if(fre(kk).le.enercmf .and. fre(kk+1).gt.enercmf) goto 10
enddo
stop' enercmf not found (1) in fre (prepray_range)'

10 kkstart=kk
q=(enercmf-fre(kk))/(fre(kk+1)-fre(kk))
q1=1.d0-q
opacon(j2)=q1*opac(1,kk)+q*opac(1,kk+1)
scont(j2)= q1*scon(1,kk)+q*scon(1,kk+1)

iloop: do i = 2,nd1  
     j1 = index1(i-1)  
     j2 = index1(i)  
     
! for interpolation below, opacon(j1) and opacon(j2) need to be known
     if(j2.le.lmax) then
        enercmf=enerobs*(1.d0-z(j2)*v(j2)*vc/r(j2))
     else
        enercmf=enerobs  !approximate assuming z=0 (for .not. optz0)
     endif   
          
     do kk=kkstart,ifre-1
       if(fre(kk).le.enercmf .and. fre(kk+1).gt.enercmf) goto 20
     enddo
     stop' enercmf not found (2) in fre (prepray_range)'

!20   kkstart=1! since next loop starts !(more exact method, commented out)
20   kkstart=kk
     q=(enercmf-fre(kk))/(fre(kk+1)-fre(kk))
     q1=1.d0-q
! interpolation w.r.t. index i, even if j2 out of range
! (accounting for radial interpolation in next loop) 
     opacon_last=q1*opac(i,kk)+q*opac(i,kk+1)
     scont_last= q1*scon(i,kk)+q*scon(i,kk+1)
     if(j2.le.lmax) then
       opacon(j2)=opacon_last
       scont(j2)=scont_last
! otherwise, index j2 out of range
     endif
     
jloop: do j = j1 + 1,j2 - 1  

         if(j.gt.lmax) exit iloop !for non-core rays

!more exact method commented out
!         enercmf=enerobs*(1.d0-z(j)*v(j)*vc/r(j))

!         do kk=kkstart,ifre-1
!           print*,'two',kk,enercmf,fre(kk),fre(kk+1)
!           if(fre(kk).le.enercmf .and. fre(kk+1).gt.enercmf) goto 30
!         enddo
!         stop' enercmf not found (2) in fre (prepray)'
!
!30       kkstart=kk
!         q=(enercmf-fre(kk))/(fre(kk+1)-fre(kk))
!         tl1=(1.d0-q)*opac(i-1,kk)+q*opac(i-1,kk+1)
!         tl2=(1.d0-q)*opac(i,kk)+q*opac(i,kk+1)
         factor=weight_range(j)
         if(factor.eq.0.d0) stop' problems with factor (prepray_range)'
         tl1=opacon(j1)
         tl2=opacon_last
         opacon(j) = tl1 + factor * (tl2-tl1)  
         tl1=scont(j1)
         tl2=scont_last
         scont(j) = tl1 + factor * (tl2-tl1)  
       end do jloop     
end do iloop

lmax1=lmax
ltot1=lmax

! back hemisphere for non-core rays
if(.not. core) then

   optz0 = .false.  
   if(z(lmax).eq.0.d0) optz0 = .true.

   if(.not.optz0) then
! assuming constant opacities/source-functions between z(lmax) and z=0.
! (consistent with standard approach) 
      lmax1=lmax+1
      opacon(lmax1)=opacon(lmax)
      scont(lmax1)=scont(lmax)
      if(zcon(lmax1).ne.0.) stop' problem with zcon = 0!'
   endif

   ltot1=2*lmax1-1
   
   kkstart=ifre

   j2=1
   jj2=2*lmax1-j2
   enercmf=enerobs*(1.d0+z(j2)*v(j2)*vc/r(j2)) !negative mu gives plus sign

   do kk=kkstart,2,-1
     if(fre(kk).ge.enercmf .and. fre(kk-1).lt.enercmf) goto 40
   enddo
   stop' enercmf not found (3) in fre (prepray_range)'

40 kkstart=kk
   q=(enercmf-fre(kk))/(fre(kk-1)-fre(kk))
   q1=1.d0-q
   opacon(jj2)=q1*opac(1,kk)+q*opac(1,kk-1)
   scont(jj2)= q1*scon(1,kk)+q*scon(1,kk-1)

iloop1: do i = 2,nd1  
     j1 = index1(i-1)  
     j2 = index1(i)  
     jj1=2*lmax1-j1
     jj2=2*lmax1-j2
     
! for interpolation below, opacon(j1) and opacon(j2) need to be known
     if(j2.le.lmax) then
        enercmf=enerobs*(1.d0+z(j2)*v(j2)*vc/r(j2)) !negative mu gives plus sign
     else
        enercmf=enerobs  !approximate assuming z=0 (for .not. optz0)
     endif   
     
     do kk=kkstart,2,-1
       if(fre(kk).ge.enercmf .and. fre(kk-1).lt.enercmf) goto 50
     enddo
     stop' enercmf not found (4) in fre (prepray)_range'

50   kkstart=kk
     q=(enercmf-fre(kk))/(fre(kk-1)-fre(kk))
     q1=1.d0-q
! interpolation w.r.t. index i, even if out of range
! (accounting for radial interpolation in next loop) 
     opacon_last=q1*opac(i,kk)+q*opac(i,kk-1)
     scont_last= q1*scon(i,kk)+q*scon(i,kk-1)
     if(j2.le.lmax) then
! consistency test 
       if(j2.eq.lmax.and.optz0) then
         if(jj2.ne.j2) stop' index problems at z=0 (prepray_range)'
         if(abs(opacon_last-opacon(j2)).gt.1.d-10) stop' problems at z=0 (prepray_range)'
       endif
       opacon(jj2)=opacon_last
       scont(jj2)=scont_last
! otherwise, index j2 out of range
     endif
     
jloop1: do j = j1 + 1,j2 - 1  

         jj=2*lmax1-j
         if(j.gt.lmax) exit iloop1

         factor=weight_range(j)
         tl1=opacon(jj1)
         tl2=opacon_last
         opacon(jj) = tl1 + factor * (tl2-tl1)  
         tl1=scont(jj1)
         tl2=scont_last
         scont(jj) = tl1 + factor * (tl2-tl1)  
       end do jloop1     
end do iloop1

endif !non core

opacon(1:ltot1)=exp(opacon(1:ltot1))*sr
scont(1:ltot1)=exp(scont(1:ltot1))

!for tests
!do l=1,ltot1
!  if(k.eq.1) print*,jp,lmax,lmax1,l,scont(l)
!enddo
!
!-------------------------------------------------------------------------
!
! now, for all frequencies and angles, we set up opacray and sconray,
! in analogy to procedure from above [if(first) ...]
!
!-----no interpolation, when number of grid points (zray vs. zcon) equal
!
lm1=ltot(jp)
if(lm1.eq.ltot1) then
!typically, until jp = 30 ... 40
  
  opacray(1:lm1)=opacon(1:ltot1)
  sconray(1:lm1)=scont(1:ltot1)

else 
  do l=1,lm1
    i=indexz(l)
    q=qarr(l)
    if(i.gt.0.) then
! consistency tests, can be skipped later
      if(zray(l).ne.zcon(i)) stop' error(1) in indexz (prepray_range)'
      if(q.ne.1.d0) stop' error(1) in qarr (prepray_range)'
      opacray(l)=opacon(i)
      sconray(l)=scont(i)
    else
       i=abs(i)
! consistency test, can be skipped later
       if(zray(l).lt.zcon(i-1).and.zray(l).gt.zcon(i)) then
         continue
       else
         stop' error(2) in indexz (prepray_range)'
       endif    
! finally, the linear interpolation
       q1=1.d0-q
       opacray(l)=q*opacon(i-1)+q1*opacon(i)
       sconray(l)=q*scont(i-1) +q1*scont(i)
     endif
  enddo  

endif
!
!for tests
!if(k.eq.10) then
!     do l=1,lm1
!       write(*,fmt='(3(i4,2x),4(F11.3,2x),2(E10.4,2x))')  &
!&       jp,lm1,l,zray(l),xcmfp(l),tempray(l),log10(xneray(l)),opacray(l),sconray(l)
!     enddo
!     print*
!endif

!now the most time-consuming loop to calculate the total opacity and
!source-function for a given obs. frame freq. and ray

indstart=ired
indstart_hhe=icount_hhe

p2=p(jp)**2
!some final consistency tests

if (core) then
  rmin=sqrt(zray(lm1)**2+p2)
  if (abs(rmin-1.d0).gt.1.d-15) then
    print*,jp,rmin
    stop' rmin(core) ne 1'
  endif
else  
  lmz0=(lm1+1)/2.
  if(zray(lmz0).ne.0.d0) then
    print*,jp,zray(lmz0)
    stop' z(lmz0) ne 0'
  endif 
endif  

do l=1,lm1
  if(xneray(l).gt.xnemin) goto 90
enddo
if(core) stop' xnemin not found in xneray for core-rays'
l=1  !dummy for non-core rays with low xne
90 lxne=l
!JO: in present version, lxne not required

!continuum values (not symmetric)
opalray(1:lm1)=opacray(1:lm1)
sray(1:lm1)=opacray(1:lm1)*sconray(1:lm1) ! cont. emissivity
tauray(1:lm1)=0.d0

!previous approach to interpolate opacities and source-functions first
!whenever jp is changing ('first') did not work, since time to store
!interpolated variables much too long. Thus, only conventional approach.

if (first) then
! calculate interpolation weights only once per jp 
  if (allocated(qstore)) deallocate(qstore,lstore)
  allocate(qstore(lm1),lstore(lm1))

  irstart=1
  do l=1,lm1

   logr=0.5*log(zray(l)**2+p2)
! to avoid numerical problems (e.g., model d8v, where logr-logr1(1)=8.d-16)
   if(abs(logr-logr1(1)).lt.1.d-15) logr=logr1(1)
   if(core.and.abs(logr.lt.1.d-15)) logr=0.d0
   if(core) then
     do ll=irstart,nd1-1
       if(logr.le.logr1(ll) .and. logr.ge.logr1(ll+1)) goto 100
     enddo  
     print*,l,irstart,logr
     print*,logr1
     stop' logr not found in logr1 (1)'
100  irstart=ll

   else if(l.le.lmz0) then
     do ll=irstart,nd1-1
       if(logr.le.logr1(ll) .and. logr.ge.logr1(ll+1)) goto 200
     enddo  
     print*,l,irstart,logr
     print*,logr1
     stop' logr not found in logr1 (2)'
200  irstart=ll
     
   else
     if(zray(l).ge.0.) stop' error in zray'
     do ll=irstart,1,-1
!     do ll=ND1-1,1,-1
       if(logr.le.logr1(ll) .and. logr.ge.logr1(ll+1)) goto 300
     enddo  
     print*,l,irstart,logr,zray(l)
     print*,logr1
     stop' logr not found in logr1 (3)'
300  irstart=ll
        
   endif

!save interpolation weights
   qstore(l)=(logr-logr1(ll+1))/(logr1(ll)-logr1(ll+1))
   lstore(l)=ll
 enddo
endif

lines_l1: do l=1,lm1
! calculate range of cmf-frequencies which affect the given obs. frame freq.
! at first for non-HHe lines, because of the lower vtherm
   lammax=xlamobs*(1.d0+xcmfp(l)+xmax*vthray(l,first_hel))
   lammin=xlamobs*(1.d0+xcmfp(l)-xmax*vthray(l,first_hel))
! since lammin and lammax decrease with height, index-search from red to blue

   loop_count=0
   do i=indstart,iblu,-1
     xl=xlam1(i)
     
     if(xl.le.lammax.and.xl.ge.lammin) then
       if(loop_count.eq.0) indstart=i
       indend=i
       loop_count=loop_count+1
     endif
     if(xl.lt.lammin) exit
   enddo
   if(loop_count.ne.0) then
       nlines=indstart-indend+1
   else       
       indend=indstart+1! hack to ensure that loop below is not executed
       nlines=0
   endif    

!   if(l.eq.lxne.and.jp.eq.1) then
!      write(*,fmt='(a3,2x,i4,2x,3(f10.4,2x),i3)'),'met',l,xlamobs,lammin,lammax,nlines
!   endif

! restore interpolation weights     
   q=qstore(l)
   q1=1.d0-q
   ll=lstore(l)
   
!remember once more: lines with inverted_range=2 (opal/sline=0 at some points)
!                    can be inverted at different points!!!! Take care!
   
   do i=indend,indstart !works also for nlines = 0, since then indend > indstart
     kel=k_range(i)
     if(inverted_range(i).eq.3) cycle !weak lines with tau_sob < tauweak
     if(kel.lt.3) cycle               ! special treatment for H/He
     xk=(xlam1(i)/xlamobs-1.d0-xcmfp(l))/vthray(l,kel) ! v/c / vth/c = v/vth
!     if(l.eq.lxne.and.jp.eq.1) then
!        write(*,fmt='(a3,2x,i4,2x,f10.4,2x,i3,2x,f10.4)'),'met',l,xlamobs,kel,xk
!     endif     
!
      if(abs(xk).gt.4.) cycle !only for Doppler
      if(inverted_range(i).eq.2) then
! no interpolation, opal set to zero
        if(opal_range(ll,i).eq.0. .or. opal_range(ll+1,i).eq.0.) cycle
      endif  
! opal_range already multiplied by sr*lam*1.d-8/sqpi/c (subr. calc_lines) 
! opal_range, sline_range are log values (even for inversion) 
      opal=q*opal_range(ll,i)+q1*opal_range(ll+1,i)
        sl=q*sline_range(ll,i)+q1*sline_range(ll+1,i)
!JO so far, simple Doppler profile. Might be improved by Voigt profile
      opal=exp(opal-xk*xk)/vthray(l,kel)
!compact form of exp(opal)*exp(-xk*xk)
      sl=exp(sl)
      opalray(l)=opalray(l)+opal        
         sray(l)=   sray(l)+opal*sl        
   enddo  

   if(icount_hhe.ne.0) then
! special treatment of H/He lines (Stark-broadened in future times)  
! calculate range of cmf-frequencies which affect the given obs. frame freq.
     if(xneray(l).lt.xnemin) then
!use Doppler-with for hydrogen
       lammax=xlamobs*(1.d0+xcmfp(l)+xmax*vthray(l,1)) 
       lammin=xlamobs*(1.d0+xcmfp(l)-xmax*vthray(l,1))
     else
!reset of indstart (because of broader profile)
!note also that xnemin can be met twice (when clumping included), thus
       indstart_hhe=icount_hhe       
!use maximum Stark-width
       lammax=xlamobs*(1.d0+xcmfp(l)+vmax_broad/clight) 
       lammin=xlamobs*(1.d0+xcmfp(l)-vmax_broad/clight)
     endif
! since lammin and lammax decrease with height, index-search from red to blue
   
     loop_count=0
     do i=indstart_hhe,1,-1 
       j=index_hhe(i)
       xl=xlam1(j)
       if(xl.le.lammax.and.xl.ge.lammin) then
         if(loop_count.eq.0) indstart_hhe=i
         indend_hhe=i
         loop_count=loop_count+1
       endif
       if(xl.lt.lammin) exit
     enddo
     if(loop_count.ne.0) then
         nlines_hhe=indstart_hhe-indend_hhe+1
     else
         nlines_hhe=0
     endif    

!     if(l.eq.lxne.and.jp.eq.1) then
!       write(*,fmt='(a3,2x,i4,2x,3(f10.4,2x),i3)'),'HHe',l,xlamobs,lammin,lammax,nlines_hhe
!     endif

     if (nlines_hhe.ne.0) then
       do i=indend_hhe,indstart_hhe
         j=index_hhe(i)
         kel=k_range(j)
         if(inverted_range(j).eq.3) cycle !weak lines with tau_sob < tauweak
         if(inverted_range(j).eq.2) stop' OPAL/SLINE (H/He) = 0!'
         if(kel.ge.3) stop' problem with index_hhe'       

         if(xneray(l).lt.xnemin .or. nstark(i).eq.0) then
! consistency check
           if(nstark(i).eq.0 .and. dlam_max_stark(i).ne.0.) &
&             stop' HHe line with nstark = 0 and dlam_max_stark ne 0'
           xk=(xlam1(j)/xlamobs-1.d0-xcmfp(l))/vthray(l,kel) ! v/c / vth/c = v/vth
!           if(l.eq.lxne.and.jp.eq.1) then
!             write(*,fmt='(a3,2x,i4,2x,f10.4,2x,i3,2x,f10.4)'),'HHe',l,xlamobs,kel,xk
!           endif

!Doppler profile (when no other info available) 
           if(abs(xk).gt.4.) cycle
! opal_range already multiplied by sr*lam*1.d-8/sqpi/c (subr. calc_lines) 
! opal_range, sline_range are log values when no inversion 
           opal=q*opal_range(ll,j)+q1*opal_range(ll+1,j)
           sl=q*sline_range(ll,j)+q1*sline_range(ll+1,j)
           opal=exp(opal-xk*xk)/vthray(l,kel)
!compact form of exp(opal)*exp(-xk*xk)
           sl=exp(sl)
         else if(nstark(i).eq.1) then
           xl=xlam1(j)
           xlamcmf=xlamobs/(1.d0-xcmfp(l))
           if(abs(xl-xlamcmf).gt.dlam_max_stark(i)) cycle
! Stark broadening, normalized corresponding to exp(-xk*xk)*lambda/sqpi/vdop
           perf=perfil2(i,xl,xlamcmf,tempray(l),xneray(l),flag)
           if (flag) cycle
! opal_range already multiplied by sr*lam*1.d-8/sqpi/c (subr. calc_lines) 
! opal_range, sline_range are log values when no inversion 
           opal=q*opal_range(ll,j)+q1*opal_range(ll+1,j)
           sl=q*sline_range(ll,j)+q1*sline_range(ll+1,j)
! to account for normalization of perfil2 (see above)
! we have to multiply OPAL by clight*sqpi*1.d8 and to divide by xlam (in A)
           opal=exp(opal)*perf*const_opal_stark/xl 
           sl=exp(sl)
         else
           stop' NSTARK > 1 in prepray_range!'
         endif
         opalray(l)=opalray(l)+opal        
         sray(l)=   sray(l)+opal*sl        
       enddo  
     endif     
   endif !HHe treatment

! calculate final source-function
   sray(l)=sray(l)/opalray(l)
   if(sray(l).gt.10.) then
     print*,k,jp,l,opalray(l),sray(l)
     stop
   endif  
   if(l.gt.1) then
     tauray(l)=tauray(l-1)+0.5d0*(opalray(l-1)+opalray(l))*(zray(l-1)-zray(l))
     if(tauray(l).gt.taumax) then
!       print*,k,jp,l,lm1,' tau > taumax'
       exit lines_l1
     endif
   endif  
!   print*,l,sray(l),opalray(l),tauray(l)

enddo lines_l1

!print*,(xlam1(index_hhe(i)),i=1,icount_hhe)

return
end
!
!-----------------------------------------------------------------------
!
subroutine formacon_range(taucon,ltaumax,fscon,opacray,sconray,zray,ltotal)
!
use nlte_type
use nlte_dim
use formalsol_add_var, ONLY: taumax

implicit none
!---- calculates the pure continuum formal solution for each ray,
!---- using the opacities and source-functions evaluated at the
!---- corresponding CMF frequencies in subroutine preformal_range
!---- in case, a linear expansion of source function is performed over each
!---- subinterval
!-----
!
!     .. parameters ..
integer(i4b), parameter :: ndm=id_depfi,lto1=2*ndm  
!     ..
!     .. scalar arguments ..
integer(i4b) ::  ltaumax,ltotal  
!     ..
!     .. array arguments ..
real(dp), dimension(lto1) ::  fscon, taucon, opacray, sconray, zray
!     ..
!     .. local scalars ..
real(dp) ::  dt,e0,e1,w1,w2  
integer(i4b) :: i
!     ..

!in contrast to standard approach, opacray and sconray are already
!evalutated (for each ray and frequency) at the corresponding cmf-frequency

taucon(1) = 0.d0  

do i = 2,ltotal  
     taucon(i) = taucon(i-1) + 0.5d0* (opacray(i-1)+opacray(i))* &
      (zray(i-1)-zray(i))
!     print*,i,taucon(i),opac(i-1),opac(i),zray(i-1),zray(i)
     if (taucon(i).lt.taumax) ltaumax = i  
end do  

ltaumax = i + 1  
fscon(ltotal) = 0.d0

do i = ltotal - 1, 1 ,-1  
     dt = taucon(i+1) - taucon(i)  

     if (dt.lt.1.d-8) then  
          w1 = .5d0*dt  
          w2 = .5d0*dt  
     else  
          e0 = exp(-dt)  
          e1 = (1.d0-e0)/dt  
          w1 = 1.d0 - e1  
          w2 = e1 - e0  
     end if  

     fscon(i) = fscon(i+1) + exp(-taucon(i))* (sconray(i)*w1+sconray(i+1)*w2)
end do  

!for tests
!w1=0.
!do i=2,ltotal
!  dt=fscon(i-1)-fscon(i)
!  w1=w1+dt
!  write(*,fmt='(i4,4(2x,e10.4))') i,w1,dt,sconray(i),opacray(i)
!enddo

return  

end
!
!-----------------------------------------------------------------------
!
subroutine obsfram_range(lm1,core,zray,opalray,sray,tauray,xicor,z1, &
&                        emint,emint1)

! resonance zones of tau=tauout stored in z1

  
use nlte_type
use nlte_dim
use formalsol_add_var, ONLY: taumax, tauout

implicit none

!     .. parameters ..
integer(i4b), parameter :: ndm=id_depfi,lto1=2*ndm  

!     .. array arguments ..
real(dp), dimension(lto1) :: zray, opalray, sray, tauray

!     ..
!     .. scalar arguments ..
real(dp) :: xicor, z1, emint, emint1
integer(i4b) :: lm1

logical core

!     .. local scalars ..
real(dp) :: taured, taublu, sred, sblu, dt, sq, expuno, aux
integer(i4b) :: l
logical optout

optout = .true.  
emint = .0d0  

taured=tauray(1)
sred=sray(1)

do l=2,lm1
  taublu=tauray(l)
  if(taublu.eq.0.) stop' error in tauray'
  sblu=sray(l)
  dt=taublu-taured

  if(dt.gt.1.d0) then
    sq=sred
  else
    sq=0.5d0*(sred+sblu)
  endif
  aux=exp(-taured)*sq*expuno(dt)
  emint=emint+aux

!for tests  
!  write(*,fmt='(i4,4(2x,e10.4))') l,emint,aux,sray(l),opalray(l)
  
  if (emint.gt.10.d0) stop ' emint > 10 in obsfram_range'  

  if (optout .and. taublu.ge.tauout) then  
     z1 = zray(l)  
     optout = .false.  
  end if  

  if (taublu.gt.taumax) then  
     emint1 = 0.d0  
! here we don't reach the core, thus iplus not relevant
! if we would include the following statement, this could lead to problems
! when the source function is still low when taumax is reached.
! in this case, we could have emint1 >> emint
! THUS, don't active the following statement (unless for tests)
!     if (core) emint1 = xicor*exp(-taublu)  
     return  
  end if  

  taured=taublu
  sred=sblu

enddo

emint1 = 0.d0  
!here we reached the core, thus adding iplus
if (core) emint1 = xicor*exp(-taublu)  

!if finished, test with pure cont, and compare to formacon
return
end
!
!-----------------------------------------------------------------------
!
subroutine calcxmax_range(temp,xne)
!
! calculated maximum wdith/velocity until which Stark etc. broadening is
! required. We use here the same condition as in calcxmax. i.e.
! requiring that opal*perf(dlam)/opac < eps_broad (e.g., 5.d-3)
!  
USE nlte_type
USE nlte_dim
USE fund_const, only: clight
USE formalsol_var, only: nstark
USE formalsol_add_var, only: id1,xlam1,icount_hhe,index_hhe, &
&     eps_broad,dlam_max_stark,vmax_broad,vmax_broad_safety,l23, beta_p

implicit none

integer(i4b), parameter :: nd=id_ndept

real(dp), dimension(nd), intent(in) :: temp, xne

integer(i4b) :: i,j,l
real(dp) :: dlam, perf, perf1, vbroad, aux
real(dp) :: perfil2
logical :: flag

vbroad=0.

do i=1,icount_hhe
  if(nstark(i).eq.0) then
    dlam_max_stark(i)=0.
  else
    l=l23(i)
    dlam=2.
    aux=eps_broad/beta_p(i)
    perf=perfil2(i,0.,dlam,temp(l),xne(l),flag)
    do while (perf.gt.aux)
      dlam=dlam+2.
      perf=perfil2(i,0.,dlam,temp(l),xne(l),flag)
    enddo
    dlam=dlam+2. !safety
    dlam_max_stark(i) = dlam
    j=index_hhe(i)
    vbroad=max(vbroad,dlam/xlam1(j)*clight)
!    print*,id1(j),dlam,dlam/xlam1(j)*clight/1.d5
  endif   
enddo    

vmax_broad = vbroad !for prepray_range
print*
print*,' Maximum extent of Stark-broadened lines = ',vmax_broad/1.d5,' km/s'  
if(vmax_broad.gt.vmax_broad_safety) then
print*,' WARNING!!!'
print*,' vmax_broad > vmax_broad_safety = ',vmax_broad_safety/1.d5
print*,' might lead to problems at freq. boundaries'
endif

return
end
!
!-----------------------------------------------------------------------
!
FUNCTION PERFIL2(I,XLAMB0,XLAM,TEM,XNE,FLAG)  
!
! no action concerning clumping required, since we solve inside clumps,
! and n_e is the increased value
!  
USE nlte_type
USE nlte_dim
USE formalsol_var, ONLY: nstark,nws,nts,nes,qhalf,dws,ts,es,ps  

implicit none
!
!----calculates Stark profiles for the 'i'
!----component, at wavelength xlam, with temperature and electron density
!----'tem' and 'xne' resp.
!    In this version, vturb = const has been folded (PREFORMAL) into all
!    tabulated profiles (Stark, Iso and Griem)
!
!     .. parameters ..
integer(i4b), parameter :: maxw=id_maxww,maxt=id_maxtt,maxne=id_maxne, &
&          maxps=id_maxps
!     ..
!     .. scalar arguments ..
real(dp) ::  xlamb0,xlam,tem,xne,perfil2
integer(i4b) ::  i  
logical flag  
!     ..
!     .. local scalars ..
real(dp) ::  dw,elog,pf,pff,pfl,pl,plf, &
&            pll,tlog,wef,wtf,wwf
integer(i4b) ::  ifi,j,jeb,jtb,jwb,l1,l2,l3,l4
!     ..

flag = .false.  
if (nstark(i).ne.1) stop' perfil2 erroneously called'
!
!--- stark profile (doppler broadening -- temp+stark -- is included)
!
!--- weights for wavelength
!
     dw = xlam-xlamb0 !here in angstrom  
     if (qhalf(i)) dw = abs(dw)  
!
!--- out of freq. range in table (assuming then phi = 0.)
!
     if (dw.lt.dws(1,i) .or. dw.gt.dws(nws(i),i)) then  
          flag = .true.  
          perfil2 = 0.d0  
          return  
     end if  

     do j = 2,nws(i)  
          ifi = j  
          if (dws(j,i).ge.dw) go to 20  
     end do

     stop ' error in table freq. grid'  

   20      continue  

     jwb = ifi - 1  
     wwf = (dw-dws(ifi-1,i))/ (dws(ifi,i)-dws(ifi-1,i))  
!
!--- weights for t
!
     tlog = log10(tem)  
     tlog = max(ts(1,i),min(ts(nts(i),i),tlog))  
     do j = 2,nts(i)  
          ifi = j  
          if (ts(j,i).ge.tlog) exit  
     end do

     jtb = ifi - 1  
     wtf = (tlog-ts(ifi-1,i))/ (ts(ifi,i)-ts(ifi-1,i))  
!
     elog = log10(xne)  
     if (elog.lt.es(1,i)) then
!       if(xnemin.ne.0.) then
        print*,elog,es(1,i)
        stop ' error in xnemin or stark profile tables'
!       endif 
!       elog=es(1,i)  ! for approximate treatment (just to calculate xmax)
     endif
    
     do j = 2,nes(i)  
          ifi = j  
          if (es(j,i).ge.elog) exit  
     end do

     jeb = ifi - 1  
     wef = (elog-es(ifi-1,i))/ (es(ifi,i)-es(ifi-1,i))  
!
!--- interpolation
!
     l1 = jwb + nws(i)* (jtb+nts(i)*jeb)  
     pff = wwf* (ps(l1+1,i)-ps(l1,i)) + ps(l1,i)  
     l2 = l1 - nws(i)  
     plf = wwf* (ps(l2+1,i)-ps(l2,i)) + ps(l2,i)  
     l3 = l1 - nws(i)*nts(i)  
     pfl = wwf* (ps(l3+1,i)-ps(l3,i)) + ps(l3,i)  
     l4 = l3 - nws(i)  
     pll = wwf* (ps(l4+1,i)-ps(l4,i)) + ps(l4,i)  
     pf = wtf* (pff-plf) + plf  
     pl = wtf* (pfl-pll) + pll  

     perfil2 = 10.d0** (wef* (pf-pl)+pl)  

return  
end
!
!-----------------------------------------------------------------------
!
subroutine dealloc

use formalsol_add_var, ONLY: id1, xlam1, gf1, &
&                            k_range, inverted_range, &
&                            opal_range, sline_range, &
&                            lamgrid, index_hhe                       

implicit none

deallocate(id1, xlam1, gf1)
deallocate(k_range, inverted_range, index_hhe)
deallocate(opal_range, sline_range)
deallocate(lamgrid)

return
end
