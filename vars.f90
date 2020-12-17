module vars
integer,parameter:: Q=8
integer ied,jed
real dx,dt,tt,t_end
real ei(0:Q,2)
real, pointer:: f(:,:,:), feq(:,:,:), u(:,:), v(:,:), rho(:,:)
integer, pointer:: ph(:,:)
real, pointer:: x(:,:), y(:,:)
real wi(0:Q),g(0:Q)
real omg,u0,rho_in,rho_out
contains

subroutine allocateField()
	allocate(f(0:Q,ied,jed))
	allocate(feq(0:Q,ied,jed))
	allocate(u(ied,jed))
	allocate(v(ied,jed))
	allocate(rho(ied,jed))
	allocate(ph(ied,jed))	
	allocate(x(ied,jed))
	allocate(y(ied,jed))
end subroutine

subroutine deallocateField()
	deallocate(f,feq,u,v,rho,ph,x,y)
end subroutine
end module

!----------------------------------------------------------------------------
module io_parameters
	character(len=50),save:: output_filename
	integer,save:: kstep_view,kstep_save,data_binary,file_format
end module io_parameters

