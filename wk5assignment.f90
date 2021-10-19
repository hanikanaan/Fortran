program week5
    implicit none
    integer:: count, length
    real::start, finish, inc, curr, diff, temp
    real, dimension (:,:), allocatable::resu
    logical::valid

    print *, "Enter the starting and ending temperatures of your table (separated with a comma):"
    read (*,*) start, finish
    valid = ((start >= -10) .and. (finish <= 210))

    do while (.not. valid)
        print *, "Please enter start and stop values between -10 and 210:"
        read (*,*) start, finish
        valid = ((start >= -10) .and. (finish <= 210))
    end do
    diff = finish - start

    print *, "Enter the increment of your table, a value between 1 and 185:"
    read (*,*) inc
    valid = ((inc >= 1) .and. (inc <= 185))
    do while (.not. valid)
        print *, "Please enter increment value between 1 and 185:"
        read (*,*) inc
        valid = ((inc >= 1) .and. (inc <= 185))
    end do

    temp = diff / inc
    length = ceiling(temp) + 1

    curr = start
    print *, '  Centigrade       ', 'Fahrenheit       ', 'Kelvin'
    count = 0
    allocate (resu(3,length))
    do while (curr <= finish)
        resu(1, count) = curr
        resu(2, count) = curr * 1.8 + 32
        resu(3, count) = curr + 273.15
        print *, resu(1, count), resu(2, count), resu(3, count)
        count = count + 1
        curr = curr + inc
        if (curr > finish) then
            curr = finish
            resu(1, count) = curr
            resu(2, count) = curr * 1.8 + 32
            resu(3, count) = curr + 273.15
            print *, resu(1, count), resu(2, count), resu(3, count)
            exit
        end if
    end do

end program week5

