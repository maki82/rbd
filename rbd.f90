program rbd

    use point
    use basic
    use joints
    use iso_fortran_env
    
    implicit none

    character(len=32) :: program_name, strNumBodies
    type(pointData), allocatable :: bodies(:)
    type(jointCon), allocatable  :: join(:)
    integer :: numBodies, ioError,i, inds,inde
    real(8) :: res(12), ddot
    real(8) :: mat(3,3), vec(3)
    integer :: a,b,c,d,ipiv(3), info
    external :: ddot

    mat=0.0D0
    mat(1,1) =2.0D0
    mat(2,2) =2.0D0
    mat(3,3) =2.0D0
    vec = 1.0D0

    a=3; b=1
    call dgetri(a,b,mat,a,ipiv,vec,a,info)

    mat(2,2)=ddot(a,vec,1,vec,1)
    write(*,*) vec
    write(*,*) compiler_version()
    write(*,*) compiler_options()

    allocate(bodies(4))
    allocate(join(6))
 
    allocate(jointLinearMove::join(1)%jo)
    allocate(jointRigid::join(2)%jo)
    allocate(jointDistance::join(3)%jo)
    allocate(jointLiner::join(4)%jo)
    allocate(jointFixedAngle::join(5)%jo)
    allocate(jointFixed::join(6)%jo)

    call bodies(1)%init(0.0D0,0.0D0)
    call bodies(2)%init(1.0D0,0.0D0)
    call bodies(3)%init(0.0D0,2.0D0)
    call bodies(4)%init(0.0D0,3.0D0)

    select type ( jj => join(1)%jo)
        type is(jointLinearMove)
            call initLinearMove(jj,0.0D0,0.0D0,1.0D0)
    end select

    call join(1)%jo%init(1,bodies(1))
    call join(2)%jo%init(1,bodies(1),bodies(2))
    call join(3)%jo%init(1,bodies(2),bodies(3))
    call join(4)%jo%init(1,bodies(3),bodies(4))
    call join(5)%jo%init(1,bodies(3))
    call join(6)%jo%init(1,bodies(4))

    res=0.0D0
    inds =1 
    inde =1 
    do i=1,6
        inde= inds + join(i)%jo%equations - 1
        write(*,*) inds,inde
        call join(i)%jo%eval(0.01D0, res(inds:inde ))
        inds= inde+1
    end do

    write(*,*) res

!    call get_command_argument(0,program_name)
!    call get_command_argument(1,strNumBodies)
!
!    call str2int(strNumBodies,numBodies,ioError)
!    
!    allocate(bodies(numBodies))
!
!    forall (i=1:numBodies) bodies(i)%pointNR=i
!
!    do i=1,numbodies
!        write(*,*) 'Enter x-coordinate of point ', i
!        read(*,*) bodies(i)%ts(0)%x
!        write(*,*) 'Enter y-coordinate of point ', i
!        read(*,*) bodies(i)%ts(0)%y
!        write(*,*) 'Enter angle of point ', i
!        read(*,*) bodies(i)%ts(0)%angle
!    end do
!
!    
!
!    do concurrent(i=1:numBodies)
!        write(*,*) 'Dere', bodies(i)%ts(0)%x 
!        write(*,*) 'Dere', bodies(i)%ts(0)%y
!        write(*,*) 'Dere', bodies(i)%ts(0)%angle 
!        bodies(i)%ts(-1) = bodies(i)%ts(0)
!        bodies(i)%ts(-2) = bodies(i)%ts(0)
!    end do
!
!    write(*,*) 'This is the very first version of ', program_name

   ! call duplicate(test)
end program

