module joints

    use point

    implicit none


    type, abstract :: joint
        integer :: jointNumber = 0
        integer :: equations = 0
        integer :: eqNR = 0
        type(pointData), pointer :: point1 => null()
        type(pointData), pointer :: point2 => null()

        contains 
        
        procedure(evalGeneric), deferred, pass(inJoint) :: eval 
        procedure(derivativeGeneric), deferred, pass(inJoint) :: derivative
        procedure :: init => initGeneric

    end type

    type :: jointCon
        class(joint), allocatable :: jo
    end type    

    interface 
        subroutine evalGeneric(inJoint,time,output)
            import joint
            class(joint),intent(in) :: inJoint
            real(8), intent(in) :: time
            real(8), intent(out) :: output(:)
    
        end subroutine
        subroutine derivativeGeneric(inJoint,time,output)
            import joint
            class(joint),intent(in) :: inJoint
            real(8), intent(in) :: time
            real(8), intent(out) :: output(:,:)
    
        end subroutine
    end interface

    type, extends(joint) :: jointFixed
        contains
        procedure :: eval => evalFixed
    end type

    type, extends(joint) :: jointFixedAngle
        contains
        procedure :: eval => evalFixedAngle
    end type

    type, extends(joint) :: jointRigid
        real(8) :: initialX = 0.0D0
        real(8) :: initialY = 0.0D0

        contains

        procedure :: eval => evalRigid
    end type

    type, extends(joint) :: jointDistance
        real(8) :: Distance2 = 0.0D0

        contains

        procedure :: eval => evalDistance
    end type

    type, extends(joint) :: jointLiner
        real(8) :: dir(2) = 0.0D0

        contains

        procedure :: eval => evalLiner
    end type

    type, extends(joint) :: jointLinearMove
        real(8) :: incX = 0.0D0
        real(8) :: incY = 0.0D0
        real(8) :: incAngle = 0.0D0

        contains

        procedure :: initLinearMove
        procedure :: eval => evalLinearMove
    end type

    contains

    subroutine initGeneric(inJoint,eqNR,point1,point2)
        type(pointData), target, intent(in)           :: point1
        type(pointData), target, optional, intent(in) :: point2
        class(joint), intent(inout)                   :: injoint
        integer, intent(in)                           :: eqNR
      
        inJoint%eqNR = eqNR
        inJoint%point1 => point1
        if(present(point2)) inJoint%point2 => point2

        select type (injoint)
            type is(jointFixed)
                inJoint%equations = 3
            type is(jointFixedAngle)
                inJoint%equations = 1
            type is(jointRigid)
                inJoint%equations = 3
                inJoint%initialX = point2%TS(0)%x - point1%TS(0)%x
                inJoint%initialY = point2%TS(0)%y - point1%TS(0)%y
            type is(jointLinearMove)
                inJoint%equations = 3
            type is(jointDistance)
                inJoint%equations = 1
                inJoint%Distance2 = (point2%TS(0)%x - point1%TS(0)%x)**2.0D0 + (point2%TS(0)%y - point1%TS(0)%y)**2.0D0
            type is(jointLiner)
                inJoint%equations = 1
                inJoint%dir(1) = point2%TS(0)%x - point1%TS(0)%x
                inJoint%dir(2) = point2%TS(0)%y - point1%TS(0)%y
                if(norm2(injoint%dir) < 1.0D-14) then
                    stop 'nonono'
                else
                    injoint%dir= injoint%dir/norm2(injoint%dir)
                endif
        end select        
    end subroutine

    subroutine initLinearMove(inJoint,incX,incY,incAngle)
        class(jointLinearMove), intent(inout)          :: injoint
        real(8), intent(in)                           :: incX
        real(8), intent(in)                           :: incY
        real(8), intent(in)                           :: incAngle

        inJoint%incX = incX
        inJoint%incY = incY
        inJoint%incAngle = incAngle
    end subroutine 

    subroutine evalLinearMove(inJoint,time,output)
        class(jointLinearMove),intent(in) :: inJoint
        real(8), intent(in) :: time
        real(8), intent(out) :: output(:)

        output(inJoint%eqNR+0)= inJoint%point1%TS(0)%x - inJoint%incX * time    
        output(inJoint%eqNR+1)= inJoint%point1%TS(0)%y - inJoint%incY * time    
        output(inJoint%eqNR+2)= inJoint%point1%TS(0)%angle - inJoint%incAngle * time    
    end subroutine

    subroutine derivativeLinearMove(inJoint,time,output)
        class(jointLinearMove),intent(in) :: inJoint
        real(8), intent(in) :: time
        real(8), intent(out) :: output(:,:)
        
        associate(p1=>inJoint%points1)
            output(inJoint%eqNR+0,(p1%pointNr-1)*3+1)= 1.0D0
            output(inJoint%eqNR+1,(p1%pointNr-1)*3+2)= 1.0D0
            output(inJoint%eqNR+2,(p1%pointNr-1)*3+3)= 1.0D0
        end associate
    end subroutine

    subroutine evalFixed(inJoint,time,output)
        class(jointFixed),intent(in) :: inJoint
        real(8), intent(in) :: time
        real(8), intent(out) :: output(:)

        output(inJoint%eqNR+0)= inJoint%point1%TS(0)%x - inJoint%point1%TS(-1)%x    
        output(inJoint%eqNR+1)= inJoint%point1%TS(0)%y - inJoint%point1%TS(-1)%y    
        output(inJoint%eqNR+2)= inJoint%point1%TS(0)%angle - inJoint%point1%TS(-1)%angle    
    end subroutine

    subroutine derivativeFixed(inJoint,time,output)
        class(jointLinearMove),intent(in) :: inJoint
        real(8), intent(in) :: time
        real(8), intent(out) :: output(:,:)
        
        associate(p1=>inJoint%points1)
            output(inJoint%eqNR+0,(p1%pointNr-1)*3+1)= 1.0D0
            output(inJoint%eqNR+1,(p1%pointNr-1)*3+2)= 1.0D0
            output(inJoint%eqNR+2,(p1%pointNr-1)*3+3)= 1.0D0
        end associate
    end subroutine

    subroutine evalFixedAngle(inJoint,time,output)
        class(jointFixedAngle),intent(in) :: inJoint
        real(8), intent(in) :: time
        real(8), intent(out) :: output(:)

        output(inJoint%eqNR+0)= inJoint%point1%TS(0)%angle - inJoint%point1%TS(-1)%angle    
    end subroutine

    subroutine derivativeFixedAngle(inJoint,time,output)
        class(jointLinearMove),intent(in) :: inJoint
        real(8), intent(in) :: time
        real(8), intent(out) :: output(:,:)
        
        associate(p1=>inJoint%points1)
            output(inJoint%eqNR+0,(p1%pointNr-1)*3+1)= 1.0D0
        end associate
    end subroutine

    subroutine evalRigid(inJoint,time,output)
        class(jointRigid),intent(in) :: inJoint
        real(8), intent(in) :: time
        real(8), intent(out) :: output(:)

        associate(p1 => injoint%point1%TS(0) , p2 => injoint%point2%TS(0) )
            output(inJoint%eqNR+0)= p2%x - p1%x - injoint%initialx * cos(p1%angle) - injoint%initialY * sin(p1%angle)
            output(inJoint%eqNR+1)= p2%y - p1%y - injoint%initialy * cos(p1%angle) + injoint%initialX * sin(p1%angle)
            output(inJoint%eqNR+2)= p2%angle - p1%angle    
        end associate
    end subroutine

    subroutine derivativeRigid(inJoint,time,output)
        class(jointLinearMove),intent(in) :: inJoint
        real(8), intent(in) :: time
        real(8), intent(out) :: output(:,:)
        
        associate(p1 => injoint%point1 , p2 => injoint%point2 )
            output(inJoint%eqNR+0,(p1%pointNr-1)*3+1)= -1.0D0 
            output(inJoint%eqNR+1,(p1%pointNr-1)*3+2)= -1.0D0
            output(inJoint%eqNR+2,(p1%pointNr-1)*3+3)= -1.0D0

            output(inJoint%eqNR+0,(p2%pointNr-1)*3+1)= 1.0D0 
            output(inJoint%eqNR+1,(p2%pointNr-1)*3+2)= 1.0D0
            output(inJoint%eqNR+2,(p2%pointNr-1)*3+3)= 1.0D0

            output(inJoint%eqNR+0,(p1%pointNr-1)*3+3)= inJoint%initialx*sin(p1%angle) - inJoint%initialY * cos(p1%angle)
            output(inJoint%eqNR+1,(p1%pointNr-1)*3+3)= inJoint%initialy*sin(p1%angle) + inJoint%initialX * cos(p1%angle)
        end associate
    end subroutine

    subroutine evalDistance(inJoint,time,output)
        class(jointDistance),intent(in) :: inJoint
        real(8), intent(in) :: time
        real(8), intent(out) :: output(:)

        associate(p1 => injoint%point1%TS(0) , p2 => injoint%point2%TS(0) )
            output(inJoint%eqNR+0)= (p2%x - p1%x)**2.0D0 + (p2%y - p1%y)**2.0D0 - inJoint%distance2
        end associate
    end subroutine

    subroutine evalLiner(inJoint,time,output)
        class(jointLiner),intent(in) :: inJoint
        real(8), intent(in) :: time
        real(8), intent(out) :: output(:)

        associate(p1 => injoint%point1%TS(0) , p2 => injoint%point2%TS(0) )
            output(inJoint%eqNR+0)= (p2%x - p1%x) * injoint%dir(2) - (p2%y - p1%y) * injoint%dir(1) 
        end associate
    end subroutine
        
end module  
