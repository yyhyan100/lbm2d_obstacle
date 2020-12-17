subroutine init() 
	use vars
	use io_parameters
	integer i,j
	real uv,eu
	open(10,file="control.in")
	read(10,*) ied,jed,dx,dt
	read(10,*) u0,rho_in,rho_out
	read(10,*) t_end,kstep_save,kstep_view,file_format
	read(10,*) output_filename
	close(10)
	omg=0.9
	call allocateField()
	call gengrid()
	u(:,:)=0.0
	v(:,:)=0.0
	rho(:,:)=rho_in
	u(1,2:jed-1)=u0
	ei(:,:)=0.0
	ei(1,1)=1.0
	ei(5,1)=1.0
	ei(8,1)=1.0
	ei(3,1)=-1.0
	ei(6,1)=-1.0
	ei(7,1)=-1.0
	ei(5,2)=1.0
	ei(2,2)=1.0
	ei(6,2)=1.0
	ei(4,2)=-1.0
	ei(7,2)=-1.0
	ei(8,2)=-1.0
	wi(0)=4.0/9
	wi(1:4)=1.0/9
	wi(5:8)=1.0/36
!	do j=1,jed
!		u(1,j)=-1*(y(1,j)-0.5)**2+0.25
!	enddo
	ph(:,:)=2
	
	do i=1,ied
	do j=1,jed
		if ((x(i,j)-2)**2+(y(i,j)-1)**2 <= 0.04) then
			ph(i,j)=0
			rho(i,j)=-rhon_in
		else
			ph(i,j)=2
		endif
	enddo
	enddo
call set_interface()
call init_f()
end subroutine
!-----------------------------------------------
subroutine set_interface()
use vars
integer i,j,k1,k2
do i=1,ied
do j=1,jed
	if (ph(i,j)==0) then
		do k1=-1,1,2
		do k2=-1,1,2
			if(ph(i+k1,j+k2)==2) ph(i+k1,j+k2)=1			
		enddo
		enddo
	endif
enddo
enddo
do i=1,ied
	ph(i,1)=1
	ph(i,jed)=1
enddo
end subroutine
!-----------------------------------------------
subroutine init_f()
use vars
integer i,j
real uv,eu
do i=1,ied
	do j=1,jed
		if (ph(i,j)>0) then
			uv=u(i,j)**2+v(i,j)**2
			do k=0,Q
				eu=ei(k,1)*u(i,j)+ei(k,2)*v(i,j)
				feq(k,i,j)=wi(k)*rho(i,j)*(1+3*eu+4.5*eu*eu-1.5*uv)
			enddo
		endif
	enddo
enddo
f(:,:,:)=feq(:,:,:)
end subroutine
!-----------------------------------------------
subroutine gengrid() 
use vars
do i=1,ied
do j=1,jed
	x(i,j)=(i-1)*dx
	y(i,j)=(j-1)*dx
enddo
enddo
end subroutine
