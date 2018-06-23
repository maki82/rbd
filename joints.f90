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
        procedure :: init => initGeneric

    end type

    interface 
        subroutine evalGeneric(inJoint,output)
            import joint
            class(joint),intent(in) :: inJoint
            real(8), intent(out) :: output(:)
    
        end subroutine
    end interface

    type, extends(joint) :: jointFixed
        contains
        procedure :: eval => evalFixed
        ! procedure :: init => initFixed
    end type

    type, extends(joint) :: jointRigid
        real(8) :: initialX = 0.0D0
        real(8) :: initialY = 0.0D0

        contains

        procedure :: eval => evalRigid
        ! procedure :: init => initRigid
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
            type is(jointRigid)
                inJoint%equations = 3
                inJoint%initialX = point2%TS(0)%x - point1%TS(0)%x
                inJoint%initialX = point2%TS(0)%y - point1%TS(0)%y
        end select        
    end subroutine

    subroutine evalfixed(inJoint,output)
        class(jointFixed),intent(in) :: inJoint
        real(8), intent(out) :: output(:)

        if(size(output) .ne. 3 ) stop  'Dere'

        output(1)= inJoint%point1%TS(0)%x - inJoint%point1%TS(-1)%x    
        output(2)= inJoint%point1%TS(0)%y - inJoint%point1%TS(-1)%y    
        output(3)= inJoint%point1%TS(0)%angle - inJoint%point1%TS(-1)%angle    
    end subroutine

    subroutine evalRigid(inJoint,output)
        class(jointRigid),intent(in) :: inJoint
        real(8), intent(out) :: output(:)

        if(size(output) .ne. 3 ) stop  'Dere'

        associate(p1 => injoint%point1%TS(0) , p2 => injoint%point2%TS(0) )
            output(1)= p2%x - p1%x - injoint%initialx * cos(p1%angle) - injoint%initialY * sin(p1%angle)
            output(2)= p2%y - p1%y - injoint%initialy * cos(p1%angle) + injoint%initialX * sin(p1%angle)
            output(3)= p2%angle - p1%angle    
        end associate
    end subroutine
        
end module  
