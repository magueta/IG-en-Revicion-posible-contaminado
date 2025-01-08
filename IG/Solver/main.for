cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      program main
c********************************************************************
    
      implicit double precision (a-h,o-z)
      logical lstop 
      common/cntl/lstop 
c******************************************************************** 
c      open(unit=13,file='error.el',status='unknown')

c      open(unit=16,file='salida.el',status='unknown')
      call grid
      call setup1 
      call start
   10 call dense
      call bound
      call output 
      if(lstop)then
c        close(16,status='keep') 
c        close(13,status='keep')
        call Dar
      endif
      if(lstop .eqv. .true.) goto 11
      call setup2
      go to 10
   11 end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
      subroutine diflow 
c******************************************************************** 
      implicit double precision (a-h,o-z)
      common/coef/flow,diff,acof
c****************************************************************** 
      acof=diff 
      if(flow.eq.0.) return 
      temp=diff-dabs(flow)*0.1 
      acof=0. 
      if(temp.le.0.) return 
      temp=temp/diff
      acof=diff*temp**5 
      return
      end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
      subroutine solve
c****************************************************************** 
      implicit double precision (a-h,o-z)
      character title*10
      logical  lsolve,lprint,lanis,lstop
      common f(50,50,9),p(50,50),rho(50,50),gam(50,50),
     1 gamx(50,50),gamy(50,50),con(50,50),
     2 aip(50,50),aim(50,50),ajp(50,50),ajm(50,50),ap(50,50), 
     3 x(50),xu(50),xdif(50),xcv(50),xcvs(50),
     4 y(50),yv(50),ydif(50),ycv(50),ycvs(50),
     5 ycvr(50),ycvrs(50),arx(50),arxj(50),arxjp(50), 
     6 r(50),rmn(50),sx(50),sxmn(50),xcvi(50),xcvip(50) 
      common  du(50,50),dv(50,50), fv(50),fvp(50),
     1 fx(50),fxm(50),fy(50),fym(50),pt(50),qt(50)
      common /indx/ nf,nfmax,np,nrho,ngam,l1,l2,l3,m1,m2,m3,
     1ist,jst,iter,last,lanis(11),
     2ipref,jpref,lsolve(11),lprint(13),mode,ntimes(11),rhocon
      common/jndx/title(13)
      common/rndx/relax(13),time,dt,xl,yl
      dimension d(50),var(50),varm(50),varp(50),phibar(50)
c****************************************************************** 
      istf=ist-1
      jstf=jst-1
      it1=l2+ist
      it2=l3+ist
      jt1=m2+jst
      jt2=m3+jst
c------------------------------------------------------------------ 
      do 999 nt=1,ntimes(nf)
      do 391 n=nf,nf
come here to do block correction
c*********************************************************
c summing in i direction
c---------------------------------------------------------
      do 22 j=jst,m2
      var(j)=0. 
      varp(j)=0.
      varm(j)=0.
      d(j)=0. 
      do 33 i=ist,l2
      var(j)=var(j)+ap(i,j) 
      if(i.ne.ist) var(j)=var(j)-aim(i,j) 
      if(i.ne.l2) var(j)=var(j)-aip(i,j)
      varm(j)=varm(j)+ajm(i,j)
      varp(j)=varp(j)+ajp(i,j)
      d(j)=d(j)+con(i,j)+aip(i,j)*f(i+1,j,n)+aim(i,j)*
     1f(i-1,j,n)+ajp(i,j)*f(i,j+1,n)+ajm(i,j)*f(i,j-1,n)- 
     2ap(i,j)*f(i,j,n)
   33 continue
   22 continue
      if((nf.eq.3).or.(nf.eq.np)) var(4)=1. 
      if((nf.eq.3).or.(nf.eq.np)) varp(4)=0.
      if((nf.eq.3).or.(nf.eq.np)) varm(4)=0.
      if((nf.eq.3).or.(nf.eq.np)) d(4)=0. 
      phibar(m1)=0. 
      phibar(jstf)=0. 
      pt(jstf)=0. 
      qt(jstf)=phibar(jstf) 
      do 44 j=jst,m2
      denom=var(j)-pt(j-1)*varm(j)
      pt(j)=varp(j)/denom 
      temp=d(j) 
      qt(j)=(temp+varm(j)*qt(j-1))/denom
   44 continue
      do 45 jj=jst,m2 
      j=jt1-jj
   45 phibar(j)=phibar(j+1)*pt(j)+qt(j) 
      do 47 i=ist,l2
      do 47 j=jst,m2
   47 f(i,j,n)=f(i,j,n)+phibar(j) 
c---------------------------------------------------------
c summing in j direction
c---------------------------------------------------------- 
c 
      do 51 i=ist,l2
      var(i)=0. 
      varp(i)=0.
      varm(i)=0.
      d(i)=0. 
      do 53 j=jst,m2
      var(i)=var(i)+ap(i,j) 
      if(j.ne.jst) var(i)=var(i)-ajm(i,j) 
      if(j.ne.m2) var(i)=var(i)-ajp(i,j)
      varp(i)=varp(i)+aip(i,j)
      varm(i)=varm(i)+aim(i,j)
      d(i)=d(i)+con(i,j)+aip(i,j)*f(i+1,j,n)+ 
     1aim(i,j)*f(i-1,j,n)+ajp(i,j)*f(i,j+1,n)+ajm(i,j)* 
     2f(i,j-1,n)-ap(i,j)*f(i,j,n) 
   53 continue
   51 continue
      if((nf.eq.3).or.(nf.eq.np)) var(4)=1. 
      if((nf.eq.3).or.(nf.eq.np)) varp(4)=0.
      if((nf.eq.3).or.(nf.eq.np)) varm(4)=0.
      if((nf.eq.3).or.(nf.eq.np)) d(4)=0. 
      phibar(l1)=0. 
      phibar(istf)=0. 
      pt(istf)=0. 
      qt(istf)=phibar(istf) 
      do 57 i=ist,l2
      denom=var(i)-pt(i-1)*varm(i)
      pt(i)=varp(i)/denom 
      temp=d(i) 
      qt(i)=(temp+qt(i-1)*varm(i))/denom
   57 continue
      do 58 ii=ist,l2 
      i=it1-ii
   58 phibar(i)=phibar(i+1)*pt(i)+qt(i) 
      do 59 i=ist,l2
      do 59 j=jst,m2
   59 f(i,j,n)=f(i,j,n)+phibar(i) 
c************************************************************ 
      do 90 j=jst,m2
      pt(istf)=0. 
      qt(istf)=f(istf,j,n)
      do 70 i=ist,l2
      denom=ap(i,j)-pt(i-1)*aim(i,j)
      pt(i)=aip(i,j)/denom
      temp=con(i,j)+ajp(i,j)*f(i,j+1,n)+ajm(i,j)*f(i,j-1,n) 
      qt(i)=(temp+aim(i,j)*qt(i-1))/denom 
   70 continue
      do 80 ii=ist,l2 
      i=it1-ii
   80 f(i,j,n)=f(i+1,j,n)*pt(i)+qt(i) 
   90 continue
c------------------------------------------------------------------ 
      do 190 jj=jst,m3
      j=jt2-jj
      pt(istf)=0. 
      qt(istf)=f(istf,j,n)
      do 170 i=ist,l2 
      denom=ap(i,j)-pt(i-1)*aim(i,j)
      pt(i)=aip(i,j)/denom
      temp=con(i,j)+ajp(i,j)*f(i,j+1,n)+ajm(i,j)*f(i,j-1,n) 
      qt(i)=(temp+aim(i,j)*qt(i-1))/denom 
  170 continue
      do 180 ii=ist,l2
      i=it1-ii
  180 f(i,j,n)=f(i+1,j,n)*pt(i)+qt(i) 
  190 continue
c------------------------------------------------------------------ 
      do 290 i=ist,l2 
      pt(jstf)=0. 
      qt(jstf)=f(i,jstf,n)
      do 270 j=jst,m2 
      denom=ap(i,j)-pt(j-1)*ajm(i,j)
      pt(j)=ajp(i,j)/denom
      temp=con(i,j)+aip(i,j)*f(i+1,j,n)+aim(i,j)*f(i-1,j,n) 
      qt(j)=(temp+ajm(i,j)*qt(j-1))/denom 
  270 continue
      do 280 jj=jst,m2
      j=jt1-jj
  280 f(i,j,n)=f(i,j+1,n)*pt(j)+qt(j) 
  290 continue
c-------------------------------------------------------------------
      do 390 ii=ist,l3
      i=it2-ii
      pt(jstf)=0. 
      qt(jstf)=f(i,jstf,n)
      do 370 j=jst,m2 
      denom=ap(i,j)-pt(j-1)*ajm(i,j)
      pt(j)=ajp(i,j)/denom
      temp=con(i,j)+aip(i,j)*f(i+1,j,n)+aim(i,j)*f(i-1,j,n) 
      qt(j)=(temp+ajm(i,j)*qt(j-1))/denom 
  370 continue
      do 380 jj=jst,m2
      j=jt1-jj
  380 f(i,j,n)=f(i,j+1,n)*pt(j)+qt(j) 
  390 continue
  391 continue
c*******************************************
  999 continue
      entry reset 
      do 400 j=2,m2 
      do 400 i=2,l2 
      con(i,j)=0. 
      ap(i,j)=0.
  400 continue
      return
      end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
      subroutine setup
c****************************************************************** 
      implicit double precision (a-h,o-z)
      character title*10
      logical  lsolve,lprint,lanis,lstop
      common f(50,50,9),p(50,50),rho(50,50),gam(50,50),
     1 gamx(50,50),gamy(50,50),con(50,50),
     2 aip(50,50),aim(50,50),ajp(50,50),ajm(50,50),ap(50,50), 
     3 x(50),xu(50),xdif(50),xcv(50),xcvs(50),
     4 y(50),yv(50),ydif(50),ycv(50),ycvs(50),
     5 ycvr(50),ycvrs(50),arx(50),arxj(50),arxjp(50), 
     6 r(50),rmn(50),sx(50),sxmn(50),xcvi(50),xcvip(50) 
      common  du(50,50),dv(50,50), fv(50),fvp(50),
     1 fx(50),fxm(50),fy(50),fym(50),pt(50),qt(50)
      common /indx/ nf,nfmax,np,nrho,ngam,l1,l2,l3,m1,m2,m3,
     1ist,jst,iter,last,lanis(11),
     2ipref,jpref,lsolve(11),lprint(13),mode,ntimes(11),rhocon
      common/jndx/title(13)
      common/rndx/relax(13),time,dt,xl,yl
      common/cntl/lstop 
      common/sorc/smax,ssum,RT 
      common/coef/flow,diff,acof
      dimension u(50,50),v(50,50),pc(50,50) 
      equivalence(f(1,1,1),u(1,1)),(f(1,1,2),v(1,1)),(f(1,1,3),pc(1,1)) 
      dimension cof(50,50,5)
      equivalence(cof(1,1,1),aip(1,1))
      dimension coff1(50,50,5),coff2(50,50,5),coff3(50,50,5),
     &          conn1(50,50),conn2(50,50)
c****************************************************************** 
c    1 format(15x,'computation  in  cartesian  coordinates') 
c    2 format(15x,'computation for axisymmetric situation')
c    3 format(15x,'computation   in   polar   coordinates')
c    4 format(14x,40(1h*),//)
      data nfmax,np,nrho,ngam/9,10,11,12/ 
      data lstop,lsolve,lprint,lanis/36*.false./
      data mode,last,time,iter/1,5,0.,0/
      data relax,ntimes/13*1.,11*1/ 
      data dt,ipref,jpref,rhocon/1.d+10,1,1,1./ 
c-------------------------------------------------------------------
      entry setup1
      l2=l1-1 
      l3=l2-1 
      m2=m1-1 
      m3=m2-1 
      x(1)=xu(2)
      do 5 i=2,l2 
    5 x(i)=0.5*(xu(i+1)+xu(i))
      x(l1)=xu(l1)
      y(1)=yv(2)
      do 10 j=2,m2
   10 y(j)=0.5*(yv(j+1)+yv(j))
      y(m1)=yv(m1)
      do 15 i=2,l1
   15 xdif(i)=x(i)-x(i-1) 
      do 18 i=2,l2
   18 xcv(i)=xu(i+1)-xu(i)
      do 20 i=3,l2
   20 xcvs(i)=xdif(i) 
      xcvs(3)=xcvs(3)+xdif(2) 
      xcvs(l2)=xcvs(l2)+xdif(l1)
      do 22 i=3,l3
      xcvi(i)=0.5*xcv(i)
   22 xcvip(i)=xcvi(i)
      xcvip(2)=xcv(2) 
      xcvi(l2)=xcv(l2)
      do 35 j=2,m1
   35 ydif(j)=y(j)-y(j-1) 
      do 40 j=2,m2
   40 ycv(j)=yv(j+1)-yv(j)
      do 45 j=3,m2
   45 ycvs(j)=ydif(j) 
      ycvs(3)=ycvs(3)+ydif(2) 
      ycvs(m2)=ycvs(m2)+ydif(m1)
      if(mode.ne.1) go to 55
      do 52 j=1,m1
      rmn(j)=1.0
   52 r(j)=1.0
      go to 56
   55 do 50 j=2,m1
   50 r(j)=r(j-1)+ydif(j) 
      rmn(2)=r(1) 
      do 60 j=3,m2
   60 rmn(j)=rmn(j-1)+ycv(j-1)
      rmn(m1)=r(m1) 
   56 continue
      do 57 j=1,m1
      sx(j)=1.
      sxmn(j)=1.
      if(mode.ne.3) go to 57
      sx(j)=r(j)
      if(j.ne.1) sxmn(j)=rmn(j) 
   57 continue
      do 62 j=2,m2
      ycvr(j)=r(j)*ycv(j) 
      arx(j)=ycvr(j)
      if(mode.ne.3) go to 62
      arx(j)=ycv(j) 
   62 continue
      do 64 j=4,m3
   64 ycvrs(j)=0.5*(r(j)+r(j-1))*ydif(j)
      ycvrs(3)=0.5*(r(3)+r(1))*ycvs(3)
      ycvrs(m2)=0.5*(r(m1)+r(m3))*ycvs(m2)
      if(mode.ne.2) go to 67
      do 65 j=3,m3
      arxj(j)=0.25*(1.+rmn(j)/r(j))*arx(j)
   65 arxjp(j)=arx(j)-arxj(j) 
      go to 68
   67 do 66 j=3,m3
      arxj(j)=0.5*arx(j)
   66 arxjp(j)=arxj(j)
   68 arxjp(2)=arx(2) 
      arxj(m2)=arx(m2)
      do 70 j=3,m3
      fv(j)=arxjp(j)/arx(j) 
   70 fvp(j)=1.-fv(j) 
      do 85 i=3,l2
      fx(i)=0.5*xcv(i-1)/xdif(i)
   85 fxm(i)=1.-fx(i) 
      fx(2)=0.
      fxm(2)=1. 
      fx(l1)=1. 
      fxm(l1)=0.
      do 90 j=3,m2
      fy(j)=0.5*ycv(j-1)/ydif(j)
   90 fym(j)=1.-fy(j) 
      fy(2)=0.
      fym(2)=1. 
      fy(m1)=1. 
      fym(m1)=0.
con,ap,u,v,rho,pc and p arrays are initialized here 
      do 95 j=1,m1
      do 95 i=1,l1
      pc(i,j)=0.
      u(i,j)=0. 
      v(i,j)=0. 
      con(i,j)=0. 
      ap(i,j)=0.
      rho(i,j)=rhocon 
      p(i,j)=0. 
   95 continue
c      if(mode.eq.1) print 1 
c      if(mode.eq.2) print 2 
c      if(mode.eq.3) print 3 
c      print 4 
      return
c---------------------------------------------------------------
      entry setup2
coefficients for the u equation 
      call reset
      nf=1
      if(.not.lsolve(nf)) go to 100 
      ist=3 
      jst=2 
      call gamsor 
      rel=1.-relax(nf)
      do 102 i=3,l2 
      flow=r(1)*xcvi(i)*v(i,2)*rho(i,1) 
      diff=r(1)*xcvi(i)*gam(i,1)/ydif(2)
      call diflow 
      ajmp=acof+dmax1(0.0d+00,flow)
      flow=r(1)*xcvip(i-1)*v(i-1,2)*rho(i-1,1)
      diff=r(1)*xcvip(i-1)*gam(i-1,1)/ydif(2) 
      call diflow 
      ajmm=acof+dmax1(0.0d+00,flow)
  102 ajm(i,2)=ajmp+ajmm
      do 103 j=2,m2 
      flow=arx(j)*u(2,j)*rho(1,j) 
      diff=arx(j)*gam(1,j)/(xcv(2)*sx(j)) 
      call diflow 
      aim(3,j)=acof+dmax1(0.0d+00,flow)
      do 103 i=3,l2 
      if(i.eq.l2) go to 104 
      fl=u(i,j)*(fx(i)*rho(i,j)+fxm(i)*rho(i-1,j))
      flp=u(i+1,j)*(fx(i+1)*rho(i+1,j)+fxm(i+1)*rho(i,j)) 
      flow=arx(j)*0.5*(fl+flp)
      diff=arx(j)*gam(i,j)/(xcv(i)*sx(j)) 
      go to 105 
  104 flow=arx(j)*u(l1,j)*rho(l1,j) 
      diff=arx(j)*gam(l1,j)/(xcv(l2)*sx(j)) 
  105 call diflow 
      aim(i+1,j)=acof+dmax1(0.0d+00,flow)
      aip(i,j)=aim(i+1,j)-flow
      if(j.eq.m2) go to 106 
      flow=xcvi(i)*v(i,j+1)*(fy(j+1)*rho(i,j+1)+fym(j+1)*rho(i,j))* 
     1rmn(j+1)
      diff=gam(i,j)*gam(i,j+1)/(ycv(j)*gam(i,j+1)+ycv(j+1)*gam(i,j)+
     1 1.0d-30)*xcvi(i)*2.*rmn(j+1) 
      call diflow 
      ajmp=acof+dmax1(0.0d+00,flow)
      ajpp=ajmp-flow
      flow=xcvip(i-1)*v(i-1,j+1)*(fy(j+1)*rho(i-1,j+1)+fym(j+1)*
     1rho(i-1,j))*rmn(j+1)
      diff=gam(i-1,j)*gam(i-1,j+1)/(ycv(j)*gam(i-1,j+1)+ycv(j+1)* 
     1 gam(i-1,j)+1.d-30)*xcvip(i-1)*2.*rmn(j+1)
      call diflow 
      ajmm=acof+dmax1(0.0d+00,flow)
      ajpm=ajmm-flow
      go to 107 
  106 flow=xcvi(i)*v(i,m1)*rho(i,m1)*r(m1)
      diff=r(m1)*xcvi(i)*gam(i,m1)/ydif(m1) 
      call diflow 
      ajmp=acof+dmax1(0.0d+00,flow)
      ajpp=ajmp-flow
      flow=r(m1)*xcvip(i-1)*v(i-1,m1)*rho(i-1,m1) 
      diff=r(m1)*xcvip(i-1)*gam(i-1,m1)/ydif(m1)
      call diflow 
      ajmm=acof+dmax1(0.0d+00,flow)
      ajpm=ajmm-flow
  107 ajm(i,j+1)=ajmp+ajmm
      ajp(i,j)=ajpp+ajpm
      vol=ycvr(j)*xcvs(i) 
      apt=(rho(i,j)*xcvi(i)+rho(i-1,j)*xcvip(i-1))
     1/(xcvs(i)*dt) 
      ap(i,j)=ap(i,j)-apt 
      con(i,j)=con(i,j)+apt*u(i,j)
      ap(i,j)=(-ap(i,j)*vol+aip(i,j)+aim(i,j)+ajp(i,j)+ajm(i,j))
     1/relax(nf)
      con(i,j)=con(i,j)*vol+rel*ap(i,j)*u(i,j)
      du(i,j)=vol/(xdif(i)*sx(j)) 
      du(i,j)=du(i,j)/ap(i,j) 
  103 continue
c
      do 800 n=1,5
      do 800 j=1,m1
      do 800 i=1,l1
  800 coff1(i,j,n)=cof(i,j,n)
      do 801 j=1,m1
      do 801 i=1,l1
  801 conn1(i,j)=con(i,j)
c
c################temporary use of pc(i,j) to store uhat#######################
      do 151 j=2,m2 
      do 151 i=3,l2 
  151 pc(i,j)=(aip(i,j)*u(i+1,j)+aim(i,j)*u(i-1,j)+ajp(i,j)*u(i,j+1)+ 
     1ajm(i,j)*u(i,j-1)+con(i,j))/ap(i,j) 
  100 continue
coefficients for the  v  equation---------------------------------- 
      call reset
      nf=2
      if(.not.lsolve(nf)) go to 200 
      ist=2 
      jst=3 
      call gamsor 
      rel=1.-relax(nf)
      do 202 i=2,l2 
      area=r(1)*xcv(i)
      flow=area*v(i,2)*rho(i,1) 
      diff=area*gam(i,1)/ycv(2) 
      call diflow 
  202 ajm(i,3)=acof+dmax1(0.0d+00,flow)
      do 203 j=3,m2 
      flow=arxj(j)*u(2,j)*rho(1,j)
      diff=arxj(j)*gam(1,j)/(xdif(2)*(sxmn(j)+sx(j))*0.5) 
      call diflow 
      aimp=acof+dmax1(0.0d+00,flow)
      flow=arxjp(j-1)*u(2,j-1)*rho(1,j-1) 
      diff=arxjp(j-1)*gam(1,j-1)/(xdif(2)*(sxmn(j)+sx(j-1))*0.5)
      call diflow 
      aimm=acof+dmax1(0.0d+00,flow)
      aim(2,j)=aimp+aimm
      do 203 i=2,l2 
      if(i.eq.l2) go to 204 
      flow=arxj(j)*u(i+1,j)*(fx(i+1)*rho(i+1,j)+fxm(i+1)*rho(i,j))
      diff=gam(i,j)*gam(i+1,j)/(xcv(i)*gam(i+1,j)+xcv(i+1)*gam(i,j)+
     1 1.d-30)*arxj(j)*2./(0.5*(sxmn(j)+sx(j))) 
      call diflow 
      aimp=acof+dmax1(0.0d+00,flow)
      aipp=aimp-flow
      flow=arxjp(j-1)*u(i+1,j-1)*(fx(i+1)*rho(i+1,j-1)+fxm(i+1)*
     1 rho(i,j-1))
      diff=gam(i,j-1)*gam(i+1,j-1)/(xcv(i)*gam(i+1,j-1)+xcv(i+1)* 
     1 gam(i,j-1)+1.0d-30)*arxjp(j-1)*2./(0.5*(sxmn(j)+sx(j-1)))
      call diflow 
      aimm=acof+dmax1(0.0d+00,flow)
      aipm=aimm-flow
      go to 205 
  204 flow=arxj(j)*u(l1,j)*rho(l1,j)
      diff=arxj(j)*gam(l1,j)/(xdif(l1)*(sxmn(j)+sx(j))*0.5) 
      call diflow 
      aimp=acof+dmax1(0.0d+00,flow)
      aipp=aimp-flow
      flow=arxjp(j-1)*u(l1,j-1)*rho(l1,j-1) 
      diff=arxjp(j-1)*gam(l1,j-1)/(xdif(l1)*(sxmn(j)+sx(j-1))*0.5)
      call diflow 
      aimm=acof+dmax1(0.0d+00,flow)
      aipm=aimm-flow
  205 aim(i+1,j)=aimp+aimm
      aip(i,j)=aipp+aipm
      if(j.eq.m2) go to 206 
      area=r(j)*xcv(i)
      fl=v(i,j)*(fy(j)*rho(i,j)+fym(j)*rho(i,j-1))*rmn(j) 
      flp=v(i,j+1)*(fy(j+1)*rho(i,j+1)+fym(j+1)*rho(i,j))*rmn(j+1)
      flow=(fv(j)*fl+fvp(j)*flp)*xcv(i) 
      diff=area*gam(i,j)/ycv(j) 
      go to 207 
  206 area=r(m1)*xcv(i) 
      flow=area*v(i,m1)*rho(i,m1) 
      diff=area*gam(i,m1)/ycv(m2) 
  207 call diflow 
      ajm(i,j+1)=acof+dmax1(0.0d+00,flow)
      ajp(i,j)=ajm(i,j+1)-flow
      vol=ycvrs(j)*xcv(i) 
      apt=(arxj(j)*rho(i,j)*0.5*(sx(j)+sxmn(j))+arxjp(j-1)*rho(i,j-1)*
     10.5*(sx(j-1)+sxmn(j)))/(ycvrs(j)*dt)
      ap(i,j)=ap(i,j)-apt 
      con(i,j)=con(i,j)+apt*v(i,j)
      ap(i,j)=(-ap(i,j)*vol+aip(i,j)+aim(i,j)+ajp(i,j)+ajm(i,j))
     1/relax(nf)
      con(i,j)=con(i,j)*vol+rel*ap(i,j)*v(i,j)
      dv(i,j)=vol/ydif(j) 
      dv(i,j)=dv(i,j)/ap(i,j) 
  203 continue
c
      do 802 n=1,5
      do 802 j=1,m1
      do 802 i=1,l1
  802 coff2(i,j,n)=cof(i,j,n)
      do 803 j=1,m1
      do 803 i=1,l1
  803 conn2(i,j)=con(i,j)
c
  200 continue
coefficients for the pressure equation################### 
      nf=np 
      if(.not.lsolve(nf)) go to 500 
      ist=2 
      jst=2 
      call gamsor
      do 402 i=2,l2 
      arho=r(1)*xcv(i)*rho(i,1) 
      con(i,2)=arho*v(i,2)
  402 ajm(i,2)=0. 
      do 403 j=2,m2 
      arho=arx(j)*rho(1,j)
      con(2,j)=con(2,j)+arho*u(2,j) 
      aim(2,j)=0. 
      do 403 i=2,l2 
      if(i.eq.l2) go to 404 
      arho=arx(j)*(fx(i+1)*rho(i+1,j)+fxm(i+1)*rho(i,j))
      flow=arho*pc(i+1,j) 
      con(i,j)=con(i,j)-flow
      con(i+1,j)=con(i+1,j)+flow
      aip(i,j)=arho*du(i+1,j) 
      aim(i+1,j)=aip(i,j) 
      go to 405 
  404 arho=arx(j)*rho(l1,j) 
      con(i,j)=con(i,j)-arho*u(l1,j)
      aip(i,j)=0. 
  405 if(j.eq.m2) go to 406 
      arho=rmn(j+1)*xcv(i)*(fy(j+1)*rho(i,j+1)+fym(j+1)*rho(i,j)) 
      vhat=(aip(i,j+1)*v(i+1,j+1)+aim(i,j+1)*v(i-1,j+1)+ajp(i,j+1)* 
     1v(i,j+2)+ajm(i,j+1)*v(i,j)+con(i,j+1))/ap(i,j+1)
      flow=arho*vhat
      con(i,j)=con(i,j)-flow
      con(i,j+1)=flow 
      ajp(i,j)=arho*dv(i,j+1) 
      ajm(i,j+1)=ajp(i,j) 
      go to 407 
  406 arho=rmn(m1)*xcv(i)*rho(i,m1) 
      con(i,j)=con(i,j)-arho*v(i,m1)
      ajp(i,j)=0. 
  407 ap(i,j)=aip(i,j)+aim(i,j)+ajp(i,j)+ajm(i,j) 
  403 continue
c
      do 804 n=1,5
      do 804 j=1,m1
      do 804 i=1,l1
  804 coff3(i,j,n)=cof(i,j,n)
c
      if(iter.le.1) go to 409
      do 408 j=2,m2 
      do 408 i=2,l2 
      ap(i,j)=ap(i,j)/relax(np) 
      con(i,j)=con(i,j)+(1.-relax(np))*ap(i,j)*p(i,j) 
  408 continue
  409 continue
      call solve
c
c
c
      nf=1
      ist=3 
      jst=2 
c
      do 805 n=1,5
      do 805 j=1,m1
      do 805 i=1,l1
  805 cof(i,j,n)=coff1(i,j,n)
      do 806 j=1,m1
      do 806 i=1,l1
  806 con(i,j)=conn1(i,j)
c
      do 413 j=2,m2 
      do 413 i=3,l2 
  413 con(i,j)=con(i,j)+du(i,j)*ap(i,j)*(p(i-1,j)-p(i,j)) 
      call solve
c
      nf=2
      ist=2 
      jst=3 
c
      do 807 n=1,5
      do 807 j=1,m1
      do 807 i=1,l1
  807 cof(i,j,n)=coff2(i,j,n)
      do 808 j=1,m1
      do 808 i=1,l1
  808 con(i,j)=conn2(i,j)
c
      do 414 j=3,m2 
      do 414 i=2,l2 
  414 con(i,j)=con(i,j)+dv(i,j)*ap(i,j)*(p(i,j-1)-p(i,j)) 
      call solve
c
c
c
coefficients for the pressure correction equation-------------------
c
      do 809 n=1,5
      do 809 j=1,m1
      do 809 i=1,l1
  809 cof(i,j,n)=coff3(i,j,n)
c
      nf=3
      if(.not.lsolve(nf)) go to 500 
      ist=2 
      jst=2 
      call gamsor 
      smax=0. 
      ssum=0. 
      do 410 j=2,m2 
      do 410 i=2,l2 
      vol=ycvr(j)*xcv(i)
  410 con(i,j)=con(i,j)*vol 
      do 474 i=2,l2 
      arho=r(1)*xcv(i)*rho(i,1) 
  474 con(i,2)=con(i,2)+arho*v(i,2) 
      do 475 j=2,m2 
      arho=arx(j)*rho(1,j)
      con(2,j)=con(2,j)+arho*u(2,j) 
      do 475 i=2,l2 
      if(i.eq.l2) go to 476 
      arho=arx(j)*(fx(i+1)*rho(i+1,j)+fxm(i+1)*rho(i,j))
      flow=arho*u(i+1,j)
      con(i,j)=con(i,j)-flow
      con(i+1,j)=con(i+1,j)+flow
      go to 477 
  476 arho=arx(j)*rho(l1,j) 
      con(i,j)=con(i,j)-arho*u(l1,j)
  477 if(j.eq.m2) go to 478 
      arho=rmn(j+1)*xcv(i)*(fy(j+1)*rho(i,j+1)+fym(j+1)*rho(i,j)) 
      flow=arho*v(i,j+1)
      con(i,j)=con(i,j)-flow
      con(i,j+1)=con(i,j+1)+flow
      go to 479 
  478 arho=rmn(m1)*xcv(i)*rho(i,m1) 
      con(i,j)=con(i,j)-arho*v(i,m1)
  479 pc(i,j)=0.
      smax=dmax1(smax,dabs(con(i,j)))
      ssum=ssum+con(i,j)
  475 continue
      call solve
come here to correct the velocities-------------------
      do 501 j=2,m2 
      do 501 i=2,l2 
      if(i.ne.2) u(i,j)=u(i,j)+du(i,j)*(pc(i-1,j)-pc(i,j))
      if(j.ne.2) v(i,j)=v(i,j)+dv(i,j)*(pc(i,j-1)-pc(i,j))
  501 continue
  500 continue
coefficients for other equations----------------------------------- 
      ist=2 
      jst=2 
      do 600 nf=4,nfmax 
      if(.not.lsolve(nf)) go to 600 
      call gamsor 
	
      rel=1.-relax(nf)
	RT=0.
c
      if(.not.lanis(nf)) then
      do 601 j=1,m1
      do 601 i=1,l1
      gamx(i,j)=gam(i,j)
  601 gamy(i,j)=gam(i,j)
      endif
c
      do 602 i=2,l2 
      area=r(1)*xcv(i)
      flow=area*v(i,2)*rho(i,1) 
      diff=area*gamy(i,1)/ydif(2)
      call diflow 
  602 ajm(i,2)=acof+dmax1(0.0d+00,flow)
      do 603 j=2,m2 
      flow=arx(j)*u(2,j)*rho(1,j) 
      diff=arx(j)*gamx(1,j)/(xdif(2)*sx(j))
      call diflow 
      aim(2,j)=acof+dmax1(0.0d+00,flow)
      do 603 i=2,l2 
      if(i.eq.l2) go to 604 
      flow=arx(j)*u(i+1,j)*(fx(i+1)*rho(i+1,j)+fxm(i+1)*rho(i,j)) 
      diff=arx(j)*2.*gamx(i,j)*gamx(i+1,j)/((xcv(i)*gamx(i+1,j)+ 
     1 xcv(i+1)*gamx(i,j)+1.0d-30)*sx(j))
      go to 605 
  604 flow=arx(j)*u(l1,j)*rho(l1,j) 
      diff=arx(j)*gamx(l1,j)/(xdif(l1)*sx(j))
  605 call diflow 
      aim(i+1,j)=acof+dmax1(0.0d+00,flow)
      aip(i,j)=aim(i+1,j)-flow
      area=rmn(j+1)*xcv(i)
      if(j.eq.m2) go to 606 
      flow=area*v(i,j+1)*(fy(j+1)*rho(i,j+1)+fym(j+1)*rho(i,j)) 
      diff=area*2.*gamy(i,j)*gamy(i,j+1)/(ycv(j)*gamy(i,j+1)+
     1 ycv(j+1)*gamy(i,j)+1.0d-30) 
      go to 607 
  606 flow=area*v(i,m1)*rho(i,m1) 
      diff=area*gamy(i,m1)/ydif(m1)
  607 call diflow 
      ajm(i,j+1)=acof+dmax1(0.0d+00,flow)
      ajp(i,j)=ajm(i,j+1)-flow
      vol=ycvr(j)*xcv(i)
      apt=rho(i,j)/dt 
      ap(i,j)=ap(i,j)-apt 
      con(i,j)=con(i,j)+apt*f(i,j,nf) 
      ap(i,j)=(-ap(i,j)*vol+aip(i,j)+aim(i,j)+ajp(i,j)+ajm(i,j))
     1/relax(nf)
      con(i,j)=con(i,j)*vol+rel*ap(i,j)*f(i,j,nf) 
	RT=dmax1(RT,dabs(con(i,j)+aip(i,j)*f(i+1,j,nf)+ 
     1aim(i,j)*f(i-1,j,nf)+ajp(i,j)*f(i,j+1,nf)+ajm(i,j)* 
     2f(i,j-1,nf)-ap(i,j)*f(i,j,nf))) 

  603 continue
      call solve
  600 continue
c Modificacion por Juan Carlos Magueta
c      write (13,*) iter,smax,ssum   
c      open(unit=13,file='error.el',status='unknown')  
c      if (iter+1 .eq. last) then
c      write(13) 'fin'
c      else
c      write(13) iter
c      write (13) smax 
c      end if
c     write (13,*) iter,smax,ssum   
c      close(13,status='keep')
c Fin de la modificacion
      time=time+dt
      iter=iter+1 
      if(iter.ge.last) lstop=.true. 
      return
      end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
      subroutine supply 
c****************************************************************** 
      implicit double precision (a-h,o-z)
      character title*10
      logical  lsolve,lprint,lanis,lstop
      common f(50,50,9),p(50,50),rho(50,50),gam(50,50),
     1 gamx(50,50),gamy(50,50),con(50,50),
     2 aip(50,50),aim(50,50),ajp(50,50),ajm(50,50),ap(50,50), 
     3 x(50),xu(50),xdif(50),xcv(50),xcvs(50),
     4 y(50),yv(50),ydif(50),ycv(50),ycvs(50),
     5 ycvr(50),ycvrs(50),arx(50),arxj(50),arxjp(50), 
     6 r(50),rmn(50),sx(50),sxmn(50),xcvi(50),xcvip(50) 
      common  du(50,50),dv(50,50), fv(50),fvp(50),
     1 fx(50),fxm(50),fy(50),fym(50),pt(50),qt(50)
      common /indx/ nf,nfmax,np,nrho,ngam,l1,l2,l3,m1,m2,m3,
     1ist,jst,iter,last,lanis(11),
     2ipref,jpref,lsolve(11),lprint(13),mode,ntimes(11),rhocon
      common/jndx/title(13)
      common/rndx/relax(13),time,dt,xl,yl
      dimension u(50,50),v(50,50),pc(50,50) 
      equivalence(f(1,1,1),u(1,1)),(f(1,1,2),v(1,1)),(f(1,1,3),pc(1,1)) 
c****************************************************************** 
c   10 format(1x,26(1h*),3x,a10,3x,26(1h*))
c   11 format(a10)
c   20 format(1x,4h i =,i6,6i9)
c   30 format(1x,1hj)
c   40 format(1x,i2,3x,1p7e9.2)
c salto de linea
c   50 format(1x,1h )
c 
c   51 format(1x,'i =',2x,7(i4,5x))
c   61 format(bn,i4,';',bn,i4,';',bn,i4,';',bn,i4,';',bn,i4,';'
c     1 ,bn,i4,';',bn,i4,';')
c
c   52 format(1x,'x =',1p7e9.2)
c   62 format(1x,e9.2,';',e9.3,';',e9.3,';',e9.3,';',e9.3,';',e9.3,
c     1';',e9.3,';')
c   53 format('th =',1p7e9.2)
c   54 format(1x,'j =',2x,7(i4,5x))
c   55 format(1x,'y =',1p7e9.2)
c****************************************************************** 
c------------------------------------------------------------------ 
      entry ugrid 
      xu(2)=0.
      dx=xl/float(l1-2) 
      do 1 i=3,l1 
    1 xu(i)=xu(i-1)+dx
      yv(2)=0.
      dy=yl/float(m1-2) 
      do 2 j=3,m1 
    2 yv(j)=yv(j-1)+dy
      return
c************************************************ 
      entry print 
c     if(.not.lprint(3)) go to 80 
calculate the stream function---------------------------------------
      f(2,2,3)=0. 
      do 82 i=2,l1
      if(i.ne.2) f(i,2,3)=f(i-1,2,3)-rho(i-1,1)*v(i-1,2)
     1*r(1)*xcv(i-1)
      do 82 j=3,m1
      rhom=fx(i)*rho(i,j-1)+fxm(i)*rho(i-1,j-1) 
   82 f(i,j,3)=f(i,j-1,3)+rhom*u(i,j-1)*arx(j-1)
   80 continue
c 
      if(.not.lprint(np)) go to 90
c 
construct boundary pressures by extrapolation 
      do 91 j=2,m2
      p(1,j)=(p(2,j)*xcvs(3)-p(3,j)*xdif(2))/xdif(3)
   91 p(l1,j)=(p(l2,j)*xcvs(l2)-p(l3,j)*xdif(l1))/xdif(l2)
      do 92 i=2,l2
      p(i,1)=(p(i,2)*ycvs(3)-p(i,3)*ydif(2))/ydif(3)
   92 p(i,m1)=(p(i,m2)*ycvs(m2)-p(i,m3)*ydif(m1))/ydif(m2)
      p(1,1)=p(2,1)+p(1,2)-p(2,2) 
      p(l1,1)=p(l2,1)+p(l1,2)-p(l2,2) 
      p(1,m1)=p(2,m1)+p(1,m2)-p(2,m2) 
      p(l1,m1)=p(l2,m1)+p(l1,m2)-p(l2,m2) 
      pref=p(ipref,jpref) 
      do 93 j=1,m1
      do 93 i=1,l1
   93 p(i,j)=p(i,j)-pref
   90 continue
c 
      
c      print 50            
      iend=0
  301 if(iend.eq.l1) go to 310
      ibeg=iend+1 
      iend=iend+7 
      iend=min0(iend,l1)
c      print 50
c      print 51,(i,i=ibeg,iend)
      if(mode.eq.3) go to 302 
c      print 52,(x(i),i=ibeg,iend) 
      go to 303 
c  302 print 53,(x(i),i=ibeg,iend) 
302	continue 
  303 go to 301 
  310 jend=0
c      print 50
  311 if(jend.eq.m1) go to 320
      jbeg=jend+1 
      jend=jend+7 
      jend=min0(jend,m1)
c      print 50
c      print 54,(j,j=jbeg,jend)
c      print 55,(y(j),j=jbeg,jend) 
      go to 311 
  320 continue
c 
      do 999 nf=1,ngam
      if(.not.lprint(nf)) go to 999 
c      print 50
c      print 10,title(nf)
      ifst=1
      jfst=1
      if(nf.eq.1.or.nf.eq.3) ifst=2 
      if(nf.eq.2.or.nf.eq.3) jfst=2 
      ibeg=ifst-7 
  110 continue
      ibeg=ibeg+7 
      iend=ibeg+6 
      iend=min0(iend,l1)
c      print 50
c      print 20,(i,i=ibeg,iend)
c      print 30
      jfl=jfst+m1 
      do 115 jj=jfst,m1 
      j=jfl-jj
c      print 40,j,(f(i,j,nf),i=ibeg,iend)
  115 continue
      if(iend.lt.l1) go to 110
  999 continue
      return															 
      end