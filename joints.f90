module joints

    use point

    implicit none

    type, abstract :: joint
        integer :: jointNumber = 0
        integer :: equations = 0
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
      !  procedure :: init => initFixed
    end type

    contains

    subroutine initGeneric(inJoint,point1,point2)
        type(pointData), target, intent(in)           :: point1
        type(pointData), target, optional, intent(in) :: point2
        class(joint), intent(inout)                   :: injoint
      
        inJoint%point1 => point1
        if(present(point2)) inJoint%point2 => point2

    end subroutine

    subroutine evalfixed(inJoint,output)
        class(jointFixed),intent(in) :: inJoint
        real(8), intent(out) :: output(:)

        if(size(output) .ne. 3 ) stop  'Dere'

        output(1)= inJoint%point1%TS(0)%x - inJoint%point1%TS(-1)%x    
        output(2)= inJoint%point1%TS(0)%y - inJoint%point1%TS(-1)%y    
        output(3)= inJoint%point1%TS(0)%angle - inJoint%point1%TS(-1)%angle    

    end subroutine
        
end module  
