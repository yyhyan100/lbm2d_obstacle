subroutine streaming 
	use vars
	integer i,j,k
	
! direction -> x+	
	do i=ied,2,-1
	do j=1,jed
		if(ph(i,j)*ph(i-1,j)>0)	f(1,i,j)=f(1,i-1,j)
	enddo
	enddo
! direction -> y+
	do i=1,ied
	do j=jed,2,-1
		if(ph(i,j)*ph(i,j-1)>0) f(2,i,j)=f(2,i,j-1)
	enddo
	enddo
! direction -> x-
	do i=1,ied-1
	do j=1,jed
		if(ph(i,j)*ph(i+1,j)>0) f(3,i,j)=f(3,i+1,j)
	enddo
	enddo
! direction -> y-
	do i=1,ied
	do j=1,jed-1
		if(ph(i,j)*ph(i,j+1)>0) f(4,i,j)=f(4,i,j+1)
	enddo
	enddo
! direction -> x+,y+
	do i=ied,2,-1
	do j=jed,2,-1
		if(ph(i,j)*ph(i-1,j-1)>0) f(5,i,j)=f(5,i-1,j-1)
	enddo
	enddo
! direction -> x-,y+
	do i=1,ied-1
	do j=jed,2,-1
		if(ph(i,j)*ph(i+1,j-1)>0) f(6,i,j)=f(6,i+1,j-1)
	enddo
	enddo
! direction -> x-,y-
	do i=1,ied-1
	do j=1,jed-1
		if(ph(i,j)*ph(i+1,j+1)>0) f(7,i,j)=f(7,i+1,j+1)
	enddo
	enddo
! direction -> x+,y-
	do i=ied,2,-1
	do j=1,jed-1
		if(ph(i,j)*ph(i-1,j+1)>0) f(8,i,j)=f(8,i-1,j+1)
	enddo
	enddo
end subroutine
