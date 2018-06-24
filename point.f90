module point

    type pointTS
        real(8) :: x = 0.0D0
        real(8) :: y = 0.0D0
        real(8) :: angle = 0.0D0
    end type
     
    type pointData
        type(pointTS) :: TS(-2:0)
        integer :: pointNr = 0

        contains

        procedure init
    end type    

    interface
        module subroutine init(point,x,y)
            class(pointData), intent(out) :: point
            real(8), intent(in) :: x,y
        end subroutine init
    end interface
end module
