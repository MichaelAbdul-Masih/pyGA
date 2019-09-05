      program igal_x_nvss
C*** checks igal against nvss

       parameter(nmx=200000,nmxby10=nmx/10)

       real ra1(nmx),dec1(nmx),ra2(nmx),dec2(nmx)
       integer ind1(nmx),ind2(nmx)
       integer nid1(nmx),nid2(nmx)
       integer mult1(10,nmxby10),mult2(10,nmxby10)
       real dist1(nmx),dist2(nmx)
       real distm1(10,nmxby10),distm2(10,nmxby10)
       real pa1(nmx),pa2(nmx)
       real pam1(10,nmxby10),pam2(10,nmxby10)
       
       character*132 str11,str12,str12n
       character*132 str13,str14
       character*132 str2(nmx)

       character*132 str2t(10)
       real distt(10)
       real pat(10),alkt(10)

       character*50 infile,infile2,rinfile

       write(*,*)'infile?'
       read(*,'(a)')infile

       infile2=infile
       do i=1,50
         if (infile(i:i).eq.':') then
           infile2=infile(i+1:50)
         endif
         if(infile2(i:i).eq.' ') then
           nc=i-1
	   goto 22
	 endif
       enddo
22     continue

       open(11,file=infile2(1:nc)//'_nvss_lowb.dat',status='unknown')
       open(12,file=infile2(1:nc)//'_nvss_pscz.dat',status='unknown')
       open(13,file='nvss_'//infile2(1:nc)//'_mm.dat',status='unknown')
       open(14,file=infile2(1:nc)//'_nvss_rej.dat',status='unknown')
       open(15,file=infile2(1:nc)//'_nvss_ch.dat',status='unknown')
       open(16,file=infile2(1:nc)//'_nvss_mm.dat',status='unknown')
       open(20,file=infile2(1:nc)//'_nvss_hum.dat',status='unknown')

       call qread(infile,nc,ra1,dec1,n1)
  
       call nvssread(ra2,dec2,n2,rinfile)

       if (n2.ge.nmx) write(*,*)'n2 too large, reset nmx'
    
       radius = 2.0    !arcmin
c       write(*,*)'search radius in arcmin?'
c       read(*,*),radius

       alkmin = 10.
c       write(*,*)'min likelihood (10)?'
c       read(*,*)alkmin

       call xx(ra1,dec1,n1,ra2,dec2,n2,radius,
     #          nid1,nid2,ind1,ind2,mult1,mult2,dist1,pa1,dist2,pa2,
     #          distm1,pam1,distm2,pam2)

C*** read in zcat sources again
      open(unit=10,file=rinfile,status='old')
      do j=1,n2
        read (10,'(a132)') str2(j)
      enddo
      close(10)

C***   write out nvss sources with >1 matches
       do j=1,n2
        if (nid2(j).gt.1) then
          write(13,*)str2(j),nid2(j)
        endif
      enddo


C*** write out QIGC sources
      open(unit=10,file=infile2(1:nc)//'.dat',status='old')

      do i=1,n1
        read(10,111)str11
        read(10,112)str12
        read(10,113)str13
c        read(10,114)str14
 111    format(a132)
 112    format(a132)
 113    format(a132)
 114    format(a132)
 
        j = ind1(i)
        str12n = str12
        nok = 0
        alkmax=0.
        nid = nid1(i)

        if (nid.ge.1) then   ! >= one match
        read(str11,115)ie1,ie2,ipa
 115    format(84x,3i3)
        emin = real(ie1)
        emaj = real(ie2)
        epa = real(ipa)

            do k=1,min(10,nid)
              if (nid.gt.1) then
                imult = -ind1(i)
                j = mult1(k,imult)
                distt(k) = distm1(k,imult)
                pat(k) = pam1(k,imult)
              else
                j = ind1(i)
                distt(k) = dist1(i)
                pat(k) = pa1(i)
              endif

              str2t(k) = str2(j)
              read(str2(j),116)f20
 116          format(36x,e11.3)

              call lik(distt(k),pat(k),f20,emaj,emin,epa,alkt(k))

              if (alkt(k).ge.alkmin) then
                nok = nok+1
                jj = j
		kk=k
                alkmax=max(alkmax,alkt(k))
              endif

            enddo


          if (nok.eq.0) then   !reject
            call writeout
     #      (14,nid,str11,str12,str13,str14,str2t,distt,alkt)
          elseif (nok.gt.1) then  !>1 good match
              call writeout
     #      (16,nid,str11,str12,str13,str14,str2t,distt,alkt)
          elseif (nok.eq.1) then
              call writeout1	  
     #      (15,str11,str12,str13,str14,str2t(kk),distt(kk),alkt(kk))
          endif

        endif

            if ((alkmax.ge.alkmin).and.(alkmax.lt.10*alkmin)) then
            call writeout
     #     (20,nid,str11,str12,str13,str14,str2t,distt,alkt)
            endif

        if (nok.ge.1) then
          if((str13(114:114).eq.'%').and.(str13(70:70).ne.'g'))then
            call writeout1  
     #      (11,str11,str12,str13,str14,str2t(kk),distt(kk),alkt(kk))
          elseif((str13(89:89).eq.'x').or.
     #    ((str13(89:89).eq.'-').and.(str13(70:70).ne.'d')))then
            
            call writeout1  
     #    (12,str11,str12,str13,str14,str2t(kk),distt(kk),alkt(kk))

          endif
	 endif
	 
c        write(11,111)str11
c        write(11,112)str12
c        write(11,113)str13

      enddo
      close(10)
      stop
      end
      
C-----------------------------------------------------------------------
       subroutine writeout
     #  (iunit,nid1,str11,str12,str13,str14,str2t,distt,alkt)

       character*132 str11,str12
       character*132 str13,str14
       character*132 str2t(10)
       real distt(10),alkt(10)

              read(str11,'(44x,f8.2)')f60
 
              write(iunit,111) str11
              write(iunit,112) str12
              write(iunit,113) str13
              write(iunit,114) str14
              do k=1,min(10,nid1)
	        read(str2t(k),'(36x,e11.3)')f20
		if(f60.gt.0)then
		  rat=1000*(f20+400E-6)/f60  !confusion bias
                else
              write(*,111) str11
              write(*,112) str12
              write(*,113) str13
              write(*,114) str14
		endif
		write(iunit,115) str2t(k)
		write(iunit,116)distt(k),alkt(k),rat
              enddo
              write(iunit,*)
 111    format(a132)
 112    format(a132)
 113    format(a132)
 114    format(a132)
 115    format(a132)
 116    format(f6.1,e9.2,f8.2)
         return
         end
C-----------------------------------------------------------------
       subroutine writeout1
     #  (iunit,str11,str12,str13,str14,str2,dist,alk)

       character*132 str11,str12
       character*132 str13,str14
       character*132 str2
       real dist,alk
 
              read(str11,'(44x,f8.2)')f60
	        read(str2,'(36x,e11.3)')f20
		if(f60.gt.0)then
		  rat=1000*(f20+400E-6)/f60  !confusion bias
                else
              write(*,111) str11
              write(*,112) str12
              write(*,113) str13
              write(*,114) str14
		endif

              write(iunit,111) str11
              write(iunit,112) str12
              write(iunit,113) str13
              write(iunit,114) str14
                write(iunit,115) str2
		write(iunit,116)dist,alk,rat
              write(iunit,*)
 111    format(a132)
 112    format(a132)
 113    format(a132)
 114    format(a132)
 115    format(a132)
 116    format(f6.1,e9.2,f8.2)
         return
         end
C-----------------------------------------------------------------
       subroutine decide(nid,str2,dist,alkt,kk,str2t)

C*** best likelihood

       character*132 str2(10),str2t
       integer ivel(10),iverr(10),ivref(10)
       real dist(10),amag(10),d1(10),alkt(10)
       character*1 camagr(10)


       alkmax = -10000.

       do k=1,nid
         if (alkt(k).ge.alkmax) then
           alkmax = alkt(k)
         endif
       enddo
       
       nok = 0
       do k=1,nid
         if (alkt(k).eq.alkmax) then
           kk = k
           return
         endif
       enddo

       kk = 0
       return
       end
C-----------------------------------------------------------------------  
       subroutine qread(infile,nc,ra,dec,n)
       character*1 cdecsn
       parameter(nmx=200000)
       real ra(nmx),dec(nmx)
       character*50 infile

       open(unit=10,file=infile(1:nc)//'.dat',status='old')

       n=0
       do i = 1,nmx
         read(10,101,end=1)irah,iram,iras,cdecsn,idd,idm,ids
 101     format(12x,1x,2i2,i3,1x,a1,3i2)
         read(10,*)
         read(10,*)
c         read(10,*)
         ra(i) = 15 * (irah + (iram+iras/600.)/60.)
         dec(i) = idd + (idm+ids/60.)/60.
         if (cdecsn.eq.'-') dec(i) = -dec(i)
         n = n+1
       enddo
 1     if (i.gt.nmx) write(*,*)'n1 too large, reset nmx'
       close(10)
       return
       end
C----------------------------------------------------------
       subroutine nvssread(ra,dec,n,rinfile)
       character*1 cdecsn
       parameter(nmx=200000)
       real ra(nmx),dec(nmx)
       character*50 rinfile
       real*8drad,ddecd

       write(*,*)'rinfile (incl .dat)?'
       read(*,'(a)')rinfile
       open(unit=10,file=rinfile,status='old')

       n=0
       do i = 1,nmx
         read(10,101,end=1)drad,ddecd
 101     format(8x,2d14.6)
         ra(i) = sngl(drad)
         dec(i) = sngl(ddecd)
         n = n+1
       enddo
 1     if (i.gt.nmx) write(*,*)'n2 too large, reset nmx'
       close(10)
       return
       end
C----------------------------------------------------------
       subroutine xx(ra1,dec1,n1,ra2,dec2,n2,radius,
     #          nid1,nid2,ind1,ind2,mult1,mult2,dist1,pa1,dist2,pa2,
     #          distm1,pam1,distm2,pam2)
C*** cross correlates files 1 and 2, size n1 and n2
C*** search radius = radius in arcmin
C*** input decimal degrees
C*** output two index files, one for each file. -ve number means multiple
C*** matches, look in mult1 for list.
C*** WS 6/12/91

       parameter(nmx=200000,nmxby10=nmx/10)

       real ra1(nmx),dec1(nmx),ra2(nmx),dec2(nmx)
       integer ind1(nmx),ind2(nmx)
       integer indx1(nmx),invindx1(nmx)
       integer indx2(nmx),invindx2(nmx)
       integer nid1(nmx),nid2(nmx)
       integer mult1(10,nmxby10),mult2(10,nmxby10)
       real dist1(nmx),dist2(nmx)
       real distm1(10,nmxby10),distm2(10,nmxby10)
       real pa1(nmx),pa2(nmx)
       real pam1(10,nmxby10),pam2(10,nmxby10)

       pi = 4*atan(1.)
       rdn = 180./pi

       do i=1,nmx
         nid1(i)=0
         ind1(i)=0
         nid2(i)=0
         ind2(i)=0
         dist1(i) = 10000.
         dist2(i) = 10000.
         pa1(i) = 0. 
         pa2(i) = 0. 
       enddo
       do i=1,nmxby10
         do k=1,10
           mult1(k,i)=0
           mult2(k,i)=0
           distm1(k,i) = 10000.
           distm2(k,i) = 10000.
           pam1(k,i) = 0. 
           pam2(k,i) = 0. 
         enddo
       enddo

       if (n1.gt.nmx) write(*,*)'n1 too large, reset n'
       if (n2.gt.nmx) write(*,*)'n2 too large, reset n'

       call decsort(ra1,dec1,indx1,invindx1,n1)
       call decsort(ra2,dec2,indx2,invindx2,n2)

       del = radius/60.     !radius in degrees
       del2 = del**2

       do i=1,50
         write(9,*) i,ra1(i),dec1(i)
       enddo
       do j=1,10
         write(9,*) j,ra2(j),dec2(j)
       enddo

       j = 1
       do i = 1,n1
c         write(*,*) i,ra1(i),dec1(i)
         dec = dec1(i)
         cdec = max(1e-6,cos(dec/rdn))
         delracrit = del/cdec

C*** (1) get to first source in 2 within del of dec.

         if (dec2(j).gt.dec+del) then
C*** forward to first within del
           do while ((dec2(j).gt.dec+del).and.(j.ne.n2))
             j = j+1
           enddo
         else
C*** backstep to first within del
           do while ((dec2(j).le.dec+del).and.(j.ne.1))
             j = j-1
           enddo
           if (j.ne.1) j = j + 1
         endif

C*** should now have j as first source within del of dec
C*** (2) count forward to dec-del, assigning matches

         do while (dec2(j).gt.dec-del)
c         write(*,*)j,ra2(j),dec2(j)

           dra = ra2(j)-ra1(i)
           dra = min(dra,360-dra)

           if (abs(dra).gt.delracrit) goto 1
           ddec = dec2(j)-dec
           drad = dra*cdec   ! delta ra in degrees
           if (drad**2+ddec**2.gt.del2) goto 1
c         write(*,*)j,ra2(j),dec2(j)

           nid1(i) = nid1(i)+1
           nid2(j) = nid2(j)+1

           dist = 60 * sqrt(drad**2+ddec**2)  !minutes
           pa = 0.
           if (abs(ddec)+abs(drad).ne.0.) then
             pa = mod(270. + (rdn * atan2(ddec,-drad)), 360.)   !degrees E of N
           endif
           if (nid1(i).eq.1) then
             ind1(i) = j
             dist1(i) = dist
             pa1(i) = pa
           elseif (nid1(i).eq.2) then
             nmult1=nmult1+1
             mult1(1,nmult1) = ind1(i)
             mult1(2,nmult1) = j
             ind1(i) = -nmult1
             distm1(1,nmult1) = dist1(i)
             distm1(2,nmult1) = dist
             pam1(1,nmult1) = pa1(i)
             pam1(2,nmult1) = pa
             dist1(i) = 0.
             pa1(i) = 0.
           elseif (nid1(i).le.10) then
             mult1(nid1(i),-ind1(i)) = j
             distm1(nid1(i),-ind1(i)) = dist
             pam1(nid1(i),-ind1(i)) = pa
           else
             call dd2hms(ra1(i),iqlon)
             call dd2dms(ra1(i),iqlat)
             write(*,*)'warning: more than 10 matches for i =',
     #           i,iqlon,iqlat
           endif

           if (nid2(j).eq.1) then
             ind2(j) = i
             dist2(j) = dist
             pa2(j) = -pa
           elseif (nid2(j).eq.2) then
             nmult2=nmult2+1
             mult2(1,nmult2) = ind2(j)
             mult2(2,nmult2) = i
             ind2(j) = -nmult2
             distm2(1,nmult2) = dist2(j)
             distm2(2,nmult2) = dist
             pam2(1,nmult2) = pa2(j)
             pam2(2,nmult2) = -pa
             dist2(j) = 0.
             pa2(j) = 0.
           elseif (nid2(j).le.10) then
             mult2(nid2(j),-ind2(j)) = i
             distm2(nid2(j),-ind2(j)) = dist
             pam2(nid2(j),-ind2(j)) = -pa
           else
             write(*,*)'warning: more than 10 matches for j =',
     #         j,ra2(j),dec2(j)
           endif
 1         j=j+1
           if (j.eq.n2+1) then
             j = n2 
             goto 2  !finished with that i source
           endif
         enddo
c         write(*,*)i,nid1(i),ind1(i)
c         if(nid1(i).eq.1) write(*,*) nid2(ind1(i)),ind2(ind1(i))
         
 2       continue
       enddo   !end of main loop
       write(*,*)i-1

       call unscramble(ra1,dec1,n1,invindx1,indx2,nid1,ind1,
     #   mult1,nmult1,dist1,pa1)
       call unscramble(ra2,dec2,n2,invindx2,indx1,nid2,ind2,
     #   mult2,nmult2,dist2,pa2)


       return
       end
C----------------------------------------------------------
       subroutine unscramble(ra,dec,n,invindx,indxb,nid,ind,
     #    mult,nmult,dist,pa)
       parameter(nmx=200000,nmxby10=nmx/10)

       real ra(nmx),dec(nmx)
       integer ind(nmx)
       integer indxb(nmx),invindx(nmx)
       integer nid(nmx)
       integer mult(10,nmxby10)
       real dist(nmx),pa(nmx)

       real rat(nmx),dect(nmx)
       integer indt(nmx)
       integer nidt(nmx)
       integer multt(10,nmxby10)
       real distt(nmx),pat(nmx)

       do i=1,n
         ii = invindx(i)
         rat(i) = ra(ii)
         dect(i) = dec(ii)
         nidt(i) = nid(ii)
         distt(i) = dist(ii)
         pat(i) = pa(ii)
         if (ind(ii).gt.0) then
           indt(i) = indxb(ind(ii))
         else
           indt(i) = ind(ii)
         endif  
       enddo

       do i = 1,nmult
         do k = 1,10
           if (mult(k,i).eq.0) goto 1
           multt(k,i) = indxb(mult(k,i))
         enddo
 1       continue
       enddo

       do i=1,n
         ra(i) = rat(i)
         dec(i) = dect(i)
         nid(i) = nidt(i)
         ind(i) = indt(i)
         dist(i) = distt(i)
         pa(i) = pat(i)
       enddo

       do i = 1,nmult
         do k = 1,10
           mult(k,i) = multt(k,i)
         enddo
       enddo
       return
       end
C------------------------------------------------------
       subroutine decsort(ra,dec,indx,invindx,n)
C*** sorts into descending dec
       parameter(nmx=200000)
       real ra(nmx),dec(nmx)
       real mindec(nmx)
       real rat(nmx),dect(nmx)
       integer indx(nmx),invindx(nmx)
       if (n.gt.nmx) write(*,*)'reset nmx in decsort'

       do i = 1,n
         mindec(i) = -dec(i)
       enddo
       write(*,*)'n=',n
       call indexx(n,mindec,indx)

       do i = 1,n
         rat(i) = ra(indx(i))
         dect(i) = dec(indx(i))
         invindx(indx(i)) = i
       enddo
       do i = 1,n
         ra(i) = rat(i)
         dec(i) = dect(i)
       enddo

       return
       end
C--------------------------------------------------------
       subroutine lik(dr,pa,f20,emaj,emin,epa,al)
C*** pseudo-likelihood for NVSS
C*** assumes exp x gaussian IRAS posn errors

       data sd1/0.025/    !rough sd/arcmin^2 at 1mJy

       data rdn/57.29578/
       data sqrt2pi/2.5066283/


       sr=5/60.   !5" posn error on radio positions

       sd = sd1 * (1000*f20)**-1    !assumes source counts like S^-1

       smaj = emaj/120.   ! one sig in '
       smin = emin/120.
       smaj = sqrt(smaj**2 + sr**2)
       smin = sqrt(smin**2 + sr**2)
       par = pa/rdn   !rdn
       epar = epa/rdn   !rdn
       cdpa = cos(par-epar)
       sdpa = sin(par-epar)

       dmaj = dr*cdpa
       dmin = dr*sdpa

       al = 1/ (sqrt2pi*2*sd*smaj*smin * 
     #  exp(min(((dmin/smin)**2)/2,75.)) * exp(min(abs(dmaj/smaj),75.)))

c       write(*,*)al
       return
       end
      SUBROUTINE HMS2DD(IHMS,DD)
C double precision subroutine
C input position as signed seven digit integer hour min sec i.e. +/-HHMMSSS
C output decimal degrees +/-DDD.DDDDD...

       IMPLICIT NONE
C input; integer position, assumed always positive
      INTEGER IHMS
C output; decimal position
      REAL*8 DD
C internal; real versions of hr.minsec, min.sec, sec.; contbns to decimal hrs
      REAL*8 RHMS,RMS,RS,RH1,RH2,RH3

      RHMS=DBLE(IHMS)/1.0D5
      RH1=AINT(RHMS+1.0D-10)
      RMS=(RHMS-RH1)*1.0D2
      RH2=AINT(RMS+1.0D-10)/6.0D1
      RS=(RMS-AINT(RMS+1.0D-10))*1.0D2
      RH3=RS/3.6D3

      DD=(RH1+RH2+RH3)*1.5D1
    
      RETURN
      END

C-----------------------------------------------------------------------
       SUBROUTINE DD2DMS(DD,IDMS)
C double precision subroutine
C input decimal degrees +/-DDD.DDDDD...
C output position as signed seven digit integer deg,min,sec i.e. +/-DDDMMSS

       IMPLICIT NONE
C input; decimal position
      REAL*8 DD
C output; integer position
      INTEGER IDMS
C internal; real versions of min.min, sec.,dummy
      REAL*8 RMM,RSS,DDUM
    
C contbns to IDMS
      INTEGER IDD,IMM,ISS
C sign fixer
      INTEGER SIGN

      SIGN=1
      IF(DD.LT.0.) SIGN=-1
      DDUM=ABS(DD)
      IDD=INT(DDUM)
      RMM=(DDUM-DBLE(IDD))*6.0D1
      IMM=INT(RMM)
      RSS=(RMM-DBLE(IMM))*6.0D1
C for seconds use NINT in case of rounding errors
      ISS=NINT(RSS)
      IF(ISS.EQ.60) THEN
       ISS=0
       IMM=IMM+1
      ENDIF
      IF(IMM.EQ.60) THEN
       IMM=0
       IDD=IDD+1
      ENDIF
      IDMS=(IDD*10000 + IMM*100 +ISS)*SIGN

      RETURN
      END


        SUBROUTINE INDEXX(N,ARRIN,INDX)
C*** private version of NRECIPES routine. Doesn't fall over when n=0 or 1 
        DIMENSION ARRIN(N),INDX(N)
        DO J=1,N
                INDX(J)=J
        ENDDO

        if (n.le.1) return
     
        L=N/2+1
        IR=N
10      CONTINUE
                IF(L.GT.1)THEN
                        L=L-1
                        INDXT=INDX(L)
                        Q=ARRIN(INDXT)
                ELSE
                        INDXT=INDX(IR)
                        Q=ARRIN(INDXT)
                        INDX(IR)=INDX(1)
                        IR=IR-1
                        IF(IR.EQ.1)THEN
                                INDX(1)=INDXT
                                RETURN
                        ENDIF
                ENDIF
                I=L
                J=L+L
20              IF(J.LE.IR)THEN
                        IF(J.LT.IR)THEN
                                IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))
     #                             J=J+1
                        ENDIF
                        IF(Q.LT.ARRIN(INDX(J)))THEN
                                INDX(I)=INDX(J)
                                I=J
                                J=J+J
                        ELSE
                                J=IR+1
                        ENDIF
                GO TO 20
                ENDIF
                INDX(I)=INDXT
        GO TO 10
        END

       SUBROUTINE DD2HMS(DD,IHMS)
C double precision subroutine
C input decimal degrees +/-DDD.DDDDD...
C output position as signed seven digit integer hr,min,sec i.e. +/-HHMMSSS

       IMPLICIT NONE
C input; decimal position
      REAL*8 DD
C output; integer position, assumed always positive
      INTEGER IHMS
C internal; real versions of hr.hr, min.min, sec.x10
      REAL*8 RHH,RMM,RSSS
C contbns to IHMS
      INTEGER IHH,IMM,ISSS

      RHH=DD/1.5D1
      IHH=INT(RHH)
      RMM=(RHH-DBLE(IHH))*6.0D1
      IMM=INT(RMM)
      RSSS=(RMM-DBLE(IMM))*6.0D2
C for seconds use NINT in case of rounding errors
      ISSS=NINT(RSSS)
      IF(ISSS.EQ.600) THEN
       ISSS=0
       IMM=IMM+1
      ENDIF
      IF(IMM.EQ.60) THEN
       IMM=0
       IHH=IHH+1
      ENDIF
      IHMS=(IHH*100000 + IMM*1000 +ISSS)

      RETURN
      END

