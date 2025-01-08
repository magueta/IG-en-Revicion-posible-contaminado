cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine user 
********************************************************************
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
	dimension th(50),thu(50),thdif(50),thcv(50),thcvs(50)
	equivalence(x,th),(xu,thu),(xdif,thdif),(xcv,thcv)
	1  ,(xcvs,thcvs),(xl,thl)
****************************************************************
*															 *
* ---  Do not alter the program above this line !			---- *
* ---														---- *
* ---  User												---- *
* ---														---- *
* ---  C.Beckermann 5/30/1992								---- *
* ---  Do not alter this copy of the program !			---- *
* ---														---- *
* ---  Modificacion hecha por Juan C. Magueta B.			---- *
* ---  10/25/1998			 						---- *
* ---														---- *
* ---  Para que trabaje con archivos de intercambios 		---- *
* ---  para las interfaces graficas Pre-Post 				---- *
* ---  hechas por Juan C. Magueta B.						---- *
* 														     *
****************************************************************
	logical lrc
	dimension t(50,50), rK(50,50),amu(50,50),gbx(50,50),gby(50,50)
	dimension tSp(50,50),tSc(50,50),cp(50,50),rho1(50,50),rho2(50,50)
	dimension xi(2,10),yi(2,10),xs(2,10),yd(2,10)
	dimension txi(3,10),tyi(3,10),txs(3,10),tyd(3,10)
	dimension uyi(10),uyd(10)
	dimension vxi(10),vxs(10)
	equivalence (f(1,1,4),t(1,1))
	

c-------------------------------------------------------------
	entry grid
	open(unit=18,file='geom.el',status='old')
	read (18,*) mode
	read (18,*) xl,yl,r(1)
	read (18,*) l1,m1
	read (18,*) lsolve(1),lsolve(2),lsolve(4)
	lsolve(3)=(lsolve(1) .or. lsolve(2))
	lsolve(10)=lsolve(3)
	do i=1,4
	 lprint(i)=lsolve(i)
	end do
	lprint(10)=lsolve(10)
	do 10 i=2,l1
	 read(18,*) xu(i)
   10 continue    
	 do 15 i=2,m1
	 read(18,*) yv(i)
   15 continue 
	read(18,*) lsim  
	close(18,status='keep')
c-------------------------------------------------------------
	open(unit=18,file='param.el',status='old')	
	read(18,*) lrc
	read(18,*) last
	read(18,*) ntimes(1),ntimes(2),ntimes(4),ntimes(10)
	ntimes(3)=ntimes(10)
	read (18,*) relax(1),relax(2),relax(4),relax(10)
	relax(3)=relax(10)		
	if (lrc ) then
		read(18,*) last
	end if
	close(18,status='keep')
c-------------------------------------------------------------
	title(1)='Velocidad u'
	title(2)='Velocidad v'
	title(3)='F. Corriente'
	title(4)='Temperatura'
	title(10)='Presion'
	return
c-------------------------------------------------------------
	entry start  
	open(unit=18,file='t.el',status='old')
	do j=1 , m1
	 do i=1 , l1
	  read(18,*) t(i,j)
	 end do
	end do
	close(18,status='keep')
	open(unit=18,file='u.el',status='old')
	do j=1 , m1
	 do i=2 , l1
	  read(18,*) u(i,j)
	 end do
	end do
	close(18,status='keep')
	open(unit=18,file='p.el',status='old')
	do j=1 , m1
	 do i=1 , l1
	  read(18,*) p(i,j)
	 end do
	end do
	close(18,status='keep')

c-------------------------------------------------------------
	open(unit=18,file='v.el',status='old')
	do j=2 , m1
	 do i=1 , l1
	  read(18,*) v(i,j)
	 end do
	end do
	close(18,status='keep')
	open(unit=18,file='gama.el',status='old')
	do j=1 , m1
	 do i=1 , l1
	  read(18,*) rK(i,j),cp(i,j)
	 end do
	end do
	do j=1 , m1
	 do i=1 , l1
	  read(18,*) amu(i,j)
	 end do
	end do	  
	do j=1 , m1
	 do i=1 , l1
	  read(18,*) gbx(i,j)
	 end do
	end do
	do j=1 , m1
	 do i=1 , l1
	  read(18,*) gby(i,j)
	 end do
	end do
	close(18,status='keep')
c-------------------------------------------------------------
	open(unit=18,file='rho.el',status='old')
	do j=1 , m1
	 do i=1 , l1
	  read(18,*) rho1(i,j)
	  rho2(i,j)=rho1(i,j)*cp(i,j)
	 end do
	end do
	close(18,status='keep')

c-------------------------------------------------------------
  
	open(unit=18,file='termi.el',status='old')
	do j=1 , m1
	 do i=1 , l1
	  read(18,*) tSc(i,j),tSp(i,j)
	 end do
	end do
	close(18,status='keep')
c-------------------------------------------------------------
	open(unit=18,file='contor.el',status='old')
	read(18,*) ndxi,ndyi,ndxs,ndyd
	do j=1 , ndxi
	 do i=1 , 2
	  read(18,*) xi(i,j)
	 end do
	 do i=1 , 3
	  read(18,*) txi(i,j)
	 end do
	 read(18,*) vxi(j)
	end do
	do j=1 , ndyi
	 do i=1 , 2
	  read(18,*) yi(i,j)
	 end do
	 do i=1 , 3
	  read(18,*) tyi(i,j)
	 end do
	 read(18,*) uyi(j) 
	end do		
	do j=1 , ndxs
	 do i=1 , 2
	  read(18,*) xs(i,j)
	 end do
	 do i=1 , 3
	  read(18,*) txs(i,j)
	 end do		 
	 read(18,*) vxs(j)
	end do	   
	do j=1 , ndyd
	 do i=1 , 2
	  read(18,*) yd(i,j)		  
	 end do
	 do i=1 , 3
	  read(18,*) tyd(i,j)
	 end do 
	 read(18,*) uyd(j)
	end do
	 
	close(18,status='keep')
c-------------------------------------------------------------
	return
c-------------------------------------------------------------
	entry dense

	return
c-------------------------------------------------------------
	entry bound
c====================Prosedimiento para F. Totalmente D.==========
	alfa=0.
	flowout=0.
	flowin=0
	
	do j=1 , ndyi
	 do i=yi(1,j)+1 ,yi(2,j)+1
	  if (uyi(j) .eq. 1) then
	   flowout=flowout-u(3,i)*ycvr(i)
	  else
	   flowin=flowin+u(2,i)*ycvr(i)
	  end if
	 end do
	end do
	do j=1 , ndyd
	 do i=yd(1,j)+1 ,yd(2,j)+1
	  if (uyd(j) .eq. 1) then
	   flowout=flowout+u(l2,i)*ycvr(i)
	  else
	   flowin=flowin-u(l1,i)*ycvr(i)
	  end if
	 end do
	end do
	do j=1 , ndxi
	 do i=xi(1,j)+1 ,xi(2,j)+1
	  if (vxi(j) .eq. 1) then
	   flowout=flowout-v(i,3)*xcv(i)
	  else
	   flowin=flowin+v(i,2)*xcv(i)
	  end if
	 end do
	end do 
	do j=1 , ndxs
	 do i=xs(1,j)+1 ,xs(2,j)+1
	  if (vxs(j) .eq. 1) then
	   flowout=flowout+v(i,m2)*xcv(i)
	  else
	   flowin=flowin-v(i,m1)*xcv(i) 
	  end if
	 end do
	end do
	if ((lsolve(1) .or. lsolve(2)) .and. flowout .ne. 0)then
	 alfa= (flowin/flowout)
	end if
c===================Haya T y las Veloc. en los Contornos====================
	do j=1 , ndxi
	 do i=xi(1,j)+1 ,xi(2,j)+1
	  if (txi(1,j) .eq. 1) then
	   t(i,1)=t(i,2)+txi(2,j)*ydif(2)/rK(i,2)
	  end if
	  if (txi(1,j) .eq. 4) then
	   t(i,1)=(t(i,2)*rK(i,2)/ydif(2)+txi(3,j)*txi(2,j))/
     1   (rK(i,2)/ydif(2)+txi(3,j))
	  end if
	  if (vxi(j) .eq. 1) then
	   v(i,2)=alfa*v(i,3)
	   amu(i,1)=0.
	  end if
	 end do
	end do
	do j=1 , ndyi
	 do i=yi(1,j)+1 ,yi(2,j)+1
	  if (tyi(1,j) .eq. 1) then
	   t(1,i)=t(2,i)+tyi(2,j)*xdif(2)/rK(2,i)
	  end if
	  if (tyi(1,j) .eq. 4) then
	   t(1,i)=(t(2,i)*rK(2,i)/xdif(2)+tyi(3,j)*tyi(2,j))/
     1   (rK(2,i)/xdif(2)+tyi(3,j))
	  end if
	  if (uyi(j) .eq. 1) then
	   u(2,i)=alfa*u(3,i)
	   amu(1,i)=0.
	  end if
	 end do
	end do
	do j=1 , ndxs
	 do i=xs(1,j)+1 ,xs(2,j)+1
	  if (txs(1,j) .eq. 1) then
	   t(i,m1)=t(i,m2)+txs(2,j)*ydif(m1)/rK(i,m2)
	  end if
	  if (txs(1,j) .eq. 4) then
	   t(i,m1)=(t(i,m2)*rK(i,m2)/ydif(m1)+txs(3,j)*txs(2,j))/
     1   (rK(i,m2)/ydif(m1)+txs(3,j))
	  end if
	  if (vxs(j) .eq. 1) then
	   v(i,m1)=alfa*v(i,m2)
	   amu(i,m1)=0.
	  end if
	 end do
	end do
	do j=1 , ndyd
	 do i=yd(1,j)+1 ,yd(2,j)+1
	  if (tyd(1,j) .eq. 1) then
	   t(l1,i)=t(l2,i)+tyd(2,j)*xdif(l1)/rK(l2,i)
	  end if
	  if (tyd(1,j) .eq. 4) then
	   t(l1,i)=(t(l2,i)*rK(l2,i)/xdif(l1)+tyd(3,j)*tyd(2,j))/
     1   (rK(l2,i)/xdif(l1)+tyd(3,j))
	  end if
	  if (uyd(j) .eq. 1) then
	   u(l1,i)=alfa*u(l2,i)
	   amu(l1,i)=0.
	  end if
	 end do
	end do
c======================Contorno Simetrico================================
	if (lsim .eq. 1) then
	 do i=1 , l1
	  t(i,1)=t(i,2)
	  u(i,1)=u(i,2)
	  v(i,2)=0.
	 end do
	end if
	if (lsim .eq. 2) then
	 do j=1 , m1
	  t(1,j)=t(2,j)
	  v(1,j)=v(2,j)
	  u(2,j)=0.
	 end do 
	end if
	if (lsim .eq. 3) then
	 do i=1 , l1
	  t(i,m1)=t(i,m2)
	  u(i,m1)=u(i,m2)
	  v(i,m1)=0.
	 end do	
	end if
	if (lsim .eq. 4) then
	 do j=1 , m1
	  t(l1,j)=t(l2,j)
	  v(l1,j)=v(l2,j)
	  u(l1,j)=0.
	 end do 
	end if
c===============Hace 0 las velocidades en un solido================	
	do j=2 , m2
	 do i=2 , l2
	  if (amu(i,j) > 1.e30) then
	   u(i,j)=0.
	   u(i+1,j)=0.
	   v(i,j)=0.
	   v(i,j+1)=0.
	  end if
	 end do	
	end do 
c=============Interpola la T en los nodos extremos=============			
c=====================(solo para graficar)=====================
	t(1,1)=(t(2,1)*ydif(2)+t(1,2)*xdif(2))/(ydif(2)+xdif(2))
	t(l1,1)=(t(l2,1)*ydif(2)+t(l1,2)*xdif(l1) )/(ydif(2)+xdif(l1))
	t(1,m1)=(t(2,m1)*ydif(m1)+t(1,m2)*xdif(2))/(ydif(m1)+xdif(2))
	t(l1,m1)=(t(l2,m1)*ydif(m1)+t(l1,m2)*xdif(l1))/(ydif(m1)+xdif(l1))
c==============================================================
	return
c-------------------------------------------------------------
	entry output
	
   62 format(1p, I4, ' ', e11.4, ' ', e11.4, ' ', 1e11.4)
	if (iter .ne. 0) goto 1
	open(unit=13,file='error.el',status='unknown')
	goto 2    
    1 write(13,*) iter,ssum,smax,rt
	print 62, iter,ssum,smax,rt
    2 if(iter.ne.last)return
	print*,"Fin del calculo"
	print 62, iter,ssum,smax,rt
	close(unit=13,status='keep')
	do j=1,m1
	 do i=1,l1
	  rho(i,j)=rho1(i,j)
	 end do
	end do
	call print

	return
c-------------------------------------------------------------
	entry gamsor


c==============================================================
c==============================================================
	do j=1,m1
	 do i=1,l1
	  if (nf .eq. 4) then
	   gam(i,j)=rk(i,j)
	   rho(i,j)=rho2(i,j)
	  else
	   gam(i,j)=amu(i,j)
	   rho(i,j)=rho1(i,j)
	  end if
	 end do
	end do
c==============Evalua frontera en E. Energia===================
	if (nf .eq. 4) then
	 do j=1 , ndxi
	  do i=xi(1,j)+1 ,xi(2,j)+1
	   if (txi(1,j) .eq. 1) then
	    gam(i,1)=0.
	    con(i,2)=(txi(2,j)/ycvr(2))+con(i,2)
	   end if
	   if (txi(1,j) .eq. 4) then
	    gam(i,1)=0.
	    res=(ydif(2)/rK(i,2)+1./txi(3,j))
	1    *ycvr(2)
	    con(i,2)=(txi(2,j)/res)+con(i,2)
	    ap(i,2)=(-1./res)+ap(i,2)
	   end if
	  end do
	 end do
	 do j=1 , ndyi
	  do i=yi(1,j)+1 ,yi(2,j)+1
	   if (tyi(1,j) .eq. 1) then
	    gam(1,i)=0.
	    con(2,i)=(tyi(2,j)/xcv(2))+con(2,i)
	   end if	
	   if (tyi(1,j) .eq. 4) then
	    gam(1,i)=0.
	    res=(xdif(2)/rK(2,i)+1./tyi(3,j))
	1    *xcv(2)
    	    con(2,i)=(tyi(2,j)/res)+con(2,i)
	    ap(2,i)=(-1./res)+ap(2,i)
	   end if
	  end do
	 end do
	 do j=1 , ndxs
	  do i=xs(1,j)+1 ,xs(2,j)+1
	   if (txs(1,j) .eq. 1) then
	    gam(i,m1)=0.
	    con(i,m2)=(txs(2,j)/ycvr(m2))+con(i,m2)
	   end if
	   if (txs(1,j) .eq. 4) then
	    gam(i,m1)=0.
	    res=(ydif(m1)/rK(i,m2)+1./txs(3,j))
	1    *ycvr(m2)
	    con(i,m2)=(txs(2,j)/res)+con(i,m2)
	    ap(i,m2)=(-1./res)+ap(i,m2)
	   end if
	  end do
	 end do
	 do j=1 , ndyd
	  do i=yd(1,j)+1 ,yd(2,j)+1
	   if (tyd(1,j) .eq. 1) then
	    gam(l1,i)=0.
	    con(l2,i)=(tyd(2,j)/xcv(l2))+con(l2,i)
	   end if
	   if (tyd(1,j) .eq. 4) then
	    gam(l1,i)=0.
	    res=(xdif(l1)/rK(l2,i)+1./tyd(3,j))
	1    *xcv(l2)
    	    con(l2,i)=(tyd(2,j)/res)+con(l2,i)
	    ap(l2,i)=(-1./res)+ap(l2,i)
	   end if
	  end do
	 end do
c==================Termino generativo en le E. Energia====================	
	 do j=2 , m2
	  do i=2 , l2
	   if (tSp(i,j) .ne. 0.) ap(i,j) = tSp(i,j)+ap(i,j)
	   if (tSc(i,j) .ne. 0.) con(i,j)= tSc(i,j)+con(i,j)
	  end do
	 end do
	
	end if 
	if (nf .ne. 4) then
	 do j=1 , ndxi
	  do i=xi(1,j)+1 ,xi(2,j)+1
	   if (vxi(j) .eq. 1) then
	    amu(i,1)=0.
	    amu(1,1)=0.
	    amu(l1,1)=0.
	   end if
	  end do
	 end do
	 do j=1 , ndyi
	  do i=yi(1,j)+1 ,yi(2,j)+1
	   if (uyi(j) .eq. 1) then
	    amu(1,i)=0.
	    amu(1,1)=0.
	    amu(1,m1)=0.
	   end if
	  end do
	 end do
	 do j=1 , ndxs
	  do i=xs(1,j)+1 ,xs(2,j)+1
	   if (vxs(j) .eq. 1) then
	    amu(i,m1)=0.
	    amu(1,m1)=0.
	    amu(l1,m1)=0.
	   end if
	  end do
	 end do
	 do j=1 , ndyd
	  do i=yd(1,j)+1 ,yd(2,j)+1
	   if (uyd(j) .eq. 1) then
	    amu(l1,i)=0.
	    amu(l1,1)=0.
	    amu(l1,m1)=0.
	   end if
	  end do
	 end do
	end if
c======================Terminos generativo en las ecuaciones de momento=======================	
	if (nf .eq. 1) then
	 do j=2 , m2
	  do i=2 , l2
	  
	   if (gbx(i,j) .ne. 0.) con(i,j)=-gbx(i,j)*(fx(i)*t(i,j)
     1								  +fxm(i)*t(i-1,j))+con(i,j)
	  end do
	 end do
	end if 
	if (nf .eq. 2) then
	 do j=2 , m2
	  do i=2 , l2
	   
	   if (gby(i,j) .ne. 0.) con(i,j)=-gby(i,j)*(fy(j)*t(i,j)
     1								  +fym(j)*t(i,j-1))+con(i,j)
	  end do
	 end do
	end if
c=========================Evalua simetria=========================	
	if (lsim .eq. 2) then
	 do j=1 , m1
	 if (nf.ne.1) gam(1,j)=0.
     	 end do
	end if
	if (lsim .eq. 1) then
	 do i=1 , l1
	 if (nf.ne.2) gam(i,1)=0.
     	 end do
     	end if
	if (lsim .eq. 4) then
	 do j=1 , m1
	 if (nf.ne.1) gam(l1,j)=0.
     	 end do
     	end if
	if (lsim .eq. 3) then
	 do i=1 , l1
	 if (nf.ne.2) gam(i,m1)=0.
	 end do
	end if
c==============================================================
	return
c-------------------------------------------------------------
	entry Dar
c-------------------------------------------------------------
	open(unit=18,file='t.el',status='unknown')
	do j=1 , m1
	 do i=1 , l1
	  write(18,*) t(i,j)
	 end do
	end do
	close(18,status='keep')
c-------------------------------------------------------------
	open(unit=18,file='u.el',status='unknown')
	do j=1 , m1
	 do i=2 , l1
	  write(18,*) u(i,j)
	 end do
	end do
	close(18,status='keep')
c-------------------------------------------------------------
	open(unit=18,file='v.el',status='unknown')
	do j=2 , m1
	 do i=1 , l1
	  write(18,*) v(i,j)
	 end do
	end do
	close(18,status='keep')
c-------------------------------------------------------------
	open(unit=18,file='p.el',status='unknown')
	do j=1 , m1
	 do i=1 , l1
	  write(18,*) p(i,j)
	 end do
	end do
	close(18,status='keep')
c-------------------------------------------------------------
	open(unit=18,file='pc.el',status='unknown')
	do j=2 , m1
	 do i=2 , l1
	  write(18,*) pc(i,j)
	 end do
	end do
	close(18,status='keep')
c--------------------------------------------------------------	
	return
	end