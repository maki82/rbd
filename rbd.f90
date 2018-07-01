program rbd

    use point
    use basic
    use joints
    use matrixRoutines
    use iso_fortran_env
    
    implicit none

    character(len=32) :: program_name, strNumBodies
    type(pointData), allocatable :: bodies(:)
    type(jointCon), allocatable  :: join(:)
    integer :: numBodies, ioError,i, inds,inde,j,k
    real(8) :: res(12), ddot, angle
    real(8) :: mat(12,12), vec(12)
    real(8), parameter :: pi = 2.0D0*asin(1.0D0)
    integer :: a,b,c,d,ipiv(3), info
    external :: ddot

    mat=0.0D0


    write(*,*) compiler_version()
    write(*,*) compiler_options()

    allocate(bodies(4))

    call bodies(1)%init(0.0D0,0.0D0)
    call bodies(2)%init(1.0D0,0.0D0)
    call bodies(3)%init(0.0D0,2.0D0)
    call bodies(4)%init(0.0D0,3.0D0)
    
    forall(i=1:4) bodies(i)%pointNR=i        

    allocate(join(7))

    allocate(jointLinearMove::join(1)%jo)
    allocate(jointRigid::join(2)%jo)
    allocate(jointDistance::join(3)%jo)
    allocate(jointLiner::join(4)%jo)
    allocate(jointFixedAngle::join(5)%jo)
    allocate(jointFixed::join(6)%jo)
    allocate(jointRot::join(7)%jo)
 
 !   select type ( jj => join(1)%jo)
 !       type is(jointLinearMove)
     !       call joininitLinearMove(jj,0.0D0,0.0D0,1.0D0)
 !    call jj%init(1,bodies(1),0.0D0,0.0D0,1.0D0)
 !   end select

    call join(1)%jo%init(1,bodies(1),0.0D0,0.0D0,1.0D0)
    call join(2)%jo%init(1,bodies(1),bodies(2))
    call join(3)%jo%init(1,bodies(2),bodies(3))
    call join(4)%jo%init(1,bodies(3),bodies(4))
    call join(5)%jo%init(1,bodies(3))
    call join(6)%jo%init(1,bodies(4))
    call join(7)%jo%init(eqnr=1,point1=bodies(4),point2=bodies(5),gearRatio=2.0D0)

    res=0.0D0
    inds =1 
    inde =1 
    do i=1,6
        inde= inds + join(i)%jo%equations - 1
        join(i)%jo%eqNR=inds
        inds= inde+1
    end do

    do k=1,360,90
        angle= dble(k)/180.0*pi
        do j=1,50
            do i=1,6
                call join(i)%jo%eval(angle, res)
            end do
           
            write(*,*) k, j, norm2(res)
            if (norm2(res)<1.0D-10) then
                exit
            endif

            mat=0.0D0
            do i=1,6
                call join(i)%jo%derivative(angle, mat)
            end do

            call solveSystem(mat,res)

            forall(i=1:4) 
                bodies(i)%TS(0)%x = bodies(i)%TS(0)%x - res((i-1)*3+1)
                bodies(i)%TS(0)%y = bodies(i)%TS(0)%y - res((i-1)*3+2)
                bodies(i)%TS(0)%angle = bodies(i)%TS(0)%angle - res((i-1)*3+3)
            end forall
        enddo
    enddo

end program

