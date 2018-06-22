submodule (point) point_sub1
        integer :: temp
contains
    module procedure duplicate
        write(*,*) 'temp'
    end procedure duplicate
end submodule
