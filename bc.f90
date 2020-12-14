subroutine bc()
!call non_eq_extra()
call inlet()
call bouncebk()
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
subroutine bouncebk 
	use vars
	integer i,j,k
! lower	
	j=1
	do i=1,ied-1
		f(2,i,j)=f(4,i,j)
		f(5,i,j)=f(7,i,j)
		f(6,i,j)=f(8,i,j)
	enddo
! upper	
	j=jed
	do i=1,ied-1
		f(4,i,j)=f(2,i,j)
		f(7,i,j)=f(5,i,j)
		f(8,i,j)=f(6,i,j)
	enddo
! left	
!	i=1
!	do j=2,jed-1
!		f(1,i,j)=f(3,i,j)
!		f(5,i,j)=f(7,i,j)
!		f(8,i,j)=f(6,i,j)
!	enddo
! right
	i=ied
	do j=1,jed
		do k=0,Q
			f(k,i,j)=2*f(k,i-1,j)-f(k,i-2,j)
		enddo
!		f(1,i,j)=2*f(1,i-1,j)-f(1,i-2,j)
!		f(5,i,j)=2*f(5,i-1,j)-f(5,i-2,j)
!		f(8,i,j)=2*f(8,i-1,j)-f(8,i-2,j)
		v(i,j)=0.0
		u(i,j)=2*u(i-1,j)-u(i-2,j)
	enddo
	
! corner points, bounce back -- test
!	i=1;j=1
!	f(5,i,j)=f(7,i+1,j+1)
!	i=ied;j=1
!	f(6,i,j)=f(8,i-1,j+1)
	
end subroutine
