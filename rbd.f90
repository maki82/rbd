program rbd

    use point
    use basic
    
    implicit none

    character(len=32) :: program_name, strNumBodies
    type(pointData), allocatable :: bodies(:)
    integer :: numBodies, ioError,i


    call get_command_argument(0,program_name)
    call get_command_argument(1,strNumBodies)

    call str2int(strNumBodies,numBodies,ioError)
    
    allocate(bodies(numBodies))

    forall (i=1:numBodies) bodies(i)%pointNR=i

    do i=1,numbodies
        write(*,*) 'Enter x-coordinate of point ', i
        read(*,*) bodies(i)%ts(0)%x
        write(*,*) 'Enter y-coordinate of point ', i
        read(*,*) bodies(i)%ts(0)%y
        write(*,*) 'Enter angle of point ', i
        read(*,*) bodies(i)%ts(0)%angle
    end do

    

    do concurrent(i=1:numBodies)
        write(*,*) 'Dere', bodies(i)%ts(0)%x 
        write(*,*) 'Dere', bodies(i)%ts(0)%y
        write(*,*) 'Dere', bodies(i)%ts(0)%angle 
        bodies(i)%ts(-1) = bodies(i)%ts(0)
        bodies(i)%ts(-2) = bodies(i)%ts(0)
    end do

    write(*,*) 'This is the very first version of ', program_name

   ! call duplicate(test)
end program

