subroutine bc()
!call non_eq_extra()
call inlet()
call bouncebk()
call outlet_pressure()
end subroutine

subroutine inlet() ! Zou-He boundary condition for velocity inlet
	use vars
	i=1
	do j=2,jed-1
		rho(i,j)=(f(0,i,j)+f(2,i,j)+f(4,i,j)+2*(f(3,i,j)+f(6,i,j)+f(7,i,j)))/(1-u(i,j))
		f(1,i,j)=f(3,i,j)+2*rho(i,j)*u(i,j)/3.0
		f(5,i,j)=f(7,i,j)-0.5*(f(2,i,j)-f(4,i,j))+rho(i,j)*u(i,j)/6.0
		f(8,i,j)=f(6,i,j)+0.5*(f(2,i,j)-f(4,i,j))+rho(i,j)*u(i,j)/6.0
	enddo
end subroutine
!----------------------------------------------------------------------------
subroutine outlet_open() ! open boundary condition
	use vars
	i=ied
	do j=1,jed
!		do k=0,Q
!			f(k,i,j)=2*f(k,i-1,j)-f(k,i-2,j)
!		enddo
		f(3,i,j)=2*f(3,i-1,j)-f(3,i-2,j)
		f(6,i,j)=2*f(6,i-1,j)-f(6,i-2,j)
		f(7,i,j)=2*f(7,i-1,j)-f(7,i-2,j)
!		v(i,j)=0.0
!		u(i,j)=2*u(i-1,j)-u(i-2,j)
	enddo
end subroutine
!----------------------------------------------------------------------------
subroutine outlet_pressure() ! pressure boundary condition
	use vars
	i=ied
	do j=1,jed
		u(i,j)=(f(0,i,j)+f(2,i,j)+f(4,i,j)+2.0*(f(1,i,j)+f(5,i,j)+f(8,i,j)))/rho_out-1.0
		f(3,i,j)=f(1,i,j)-2*rho_out*u(i,j)/3
		f(6,i,j)=f(8,i,j)-0.5*(f(2,i,j)-f(4,i,j))-rho_out*u(i,j)/6.0
		f(7,i,j)=f(5,i,j)+0.5*(f(2,i,j)-f(4,i,j))-rho_out*u(i,j)/6.0
	enddo
end subroutine
!----------------------------------------------------------------------------
subroutine bouncebk 
	use vars
	integer i,j,k
	do i=2,ied-1
	do j=2,jed-1
		if(ph(i,j)==1) then
			if(ph(i+1,j)==2) f(1,i,j)=f(3,i,j)
			if(ph(i,j+1)==2) f(2,i,j)=f(4,i,j)
			if(ph(i-1,j)==2) f(3,i,j)=f(1,i,j)
			if(ph(i,j-1)==2) f(4,i,j)=f(2,i,j)
			if(ph(i+1,j+1)==2) f(5,i,j)=f(7,i,j)
			if(ph(i-1,j+1)==2) f(6,i,j)=f(8,i,j)
			if(ph(i-1,j-1)==2) f(7,i,j)=f(5,i,j)
			if(ph(i+1,j-1)==2) f(8,i,j)=f(6,i,j)
		endif
	enddo
	enddo
	j=1
	do i=1,ied
		if(ph(i,j)==1) then
			if(ph(i,j+1)==2) f(2,i,j)=f(4,i,j)
			if(ph(i+1,j+1)==2) f(5,i,j)=f(7,i,j)
			if(ph(i-1,j+1)==2) f(6,i,j)=f(8,i,j)
		endif
	enddo
	j=jed
	do i=1,ied
		if(ph(i,j)==1) then
			if(ph(i,j-1)==2) f(4,i,j)=f(2,i,j)
			if(ph(i-1,j-1)==2) f(7,i,j)=f(5,i,j)
			if(ph(i+1,j-1)==2) f(8,i,j)=f(6,i,j)
		endif
	enddo
	i=1
	do j=1,jed
		if(ph(i,j)==1) then
			if(ph(i+1,j)==2) f(1,i,j)=f(3,i,j)
			if(ph(i,j+1)==2) f(2,i,j)=f(4,i,j)
			if(ph(i,j-1)==2) f(4,i,j)=f(2,i,j)
			if(ph(i+1,j+1)==2) f(5,i,j)=f(7,i,j)
			if(ph(i+1,j-1)==2) f(8,i,j)=f(6,i,j)
		endif
	enddo
	i=ied
	do j=1,jed
		if(ph(i,j)==1) then
			if(ph(i,j+1)==2) f(2,i,j)=f(4,i,j)
			if(ph(i-1,j)==2) f(3,i,j)=f(1,i,j)
			if(ph(i,j-1)==2) f(4,i,j)=f(2,i,j)
			if(ph(i-1,j+1)==2) f(6,i,j)=f(8,i,j)
			if(ph(i-1,j-1)==2) f(7,i,j)=f(5,i,j)
		endif
	enddo
end subroutine
