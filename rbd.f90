program rbd

    use body
        
    character(len=32) :: program_name
    type(bodyData) test

    call get_command_argument(0,program_name)

    write(*,*) test%position
     
    do concurrent(int =1:10)
        write(*,*) 'Dere' 
    end do

    write(*,*) 'This is the very first version of ', program_name

    call duplicate(test)
    write(*,*) test%position
end program

