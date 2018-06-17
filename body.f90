module body
    type bodyData 
        real(8) :: position(3) =0.0D0
        integer :: test = 0
    end type
     
    interface
        module subroutine duplicate(bodyvar)
            type(bodyData), intent(inout) :: bodyVar
        end subroutine duplicate
    end interface
end module
