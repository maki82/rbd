module matrixRoutines

contains

    subroutine solveSystem(A,x)
        real(8), intent(in) :: A(:,:)
        real(8), intent(inout) :: x(:)
        
        real(8) :: work(size(x))
        integer :: ipiv(size(x))
        integer :: n, info , i,j

        n = size(x)

        if (max(size(x),size(A,1),size(A,2)) & 
            .ne. min(size(x),size(A,1),size(A,2))) stop 'Oida die Dimensionen miasen passen.'

        call dgesv(n,1,A,n,ipiv,x,n,info)
         if(info .ne. 0)  write(*,*) 'INFO' , info

    end subroutine
          

end module
