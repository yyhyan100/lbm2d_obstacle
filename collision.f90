subroutine collision 
	use vars
	integer i,j,k
	do i=1,ied
	do j=1,jed
	if (ph(i,j)>0) then 
		do k=0,Q
			f(k,i,j)=(1.0-omg)*f(k,i,j)+omg*feq(k,i,j)
		enddo
	endif
	enddo
	enddo
end subroutine
