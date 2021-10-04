program question3
    implicit none
    integer::s, r
    logical::valid
    character(25)::name
    print *, "Please enter your full name (maximum 25 letters) using quotations, and your student number like the below example:"
    print *, '"John Smith",123456'
    read (*,*) name, s
    valid = (s >= 215001 .and. s<= 221999)
    if (valid) then
        r = mod(s, 100)
        select case (r)
            case(0:33)
                print *, "Dear ", name, "your exam will be held in Hall-A"
            case(34:66)
                print *, "Dear ", name, "your exam will be held in Hall-B"
            case(67:99)
                print *, "Dear ", name, "your exam will be held in Hall-C"
        end select
    else
        do while (.not. valid)
            print *, "Your student number is not valid. Please enter it again."
            print *, "If you are not sure, press Control+C on your keyboard and try again later."
            read (*,*) s
            valid = (s >= 215001 .and. s<= 221999)
        end do
        r = mod(s, 100)
        select case (r)
            case(0:33)
                print *, "Dear ", name, "your exam will be held in Hall-A"
            case(34:66)
                print *, "Dear ", name, "your exam will be held in Hall-B"
            case(67:99)
                print *, "Dear ", name, "your exam will be held in Hall-C"
        end select
    end if
end program question3
