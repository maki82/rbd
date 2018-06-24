submodule (point) point_sub1
        integer :: temp
contains
    module procedure init
        point%TS(:)%x = x
        point%TS(:)%y = y
    end procedure
end submodule
