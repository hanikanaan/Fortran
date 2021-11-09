program problem3
    implicit none
    real ,dimension(:), allocatable::u, v
    real::curr, sum, largest, smallest, dot
    integer::n, count, index, i, more_than_4, less_than_neg4
    
    open(1, file='Problem3_input.txt')
    open(2, file='Problem3_output.txt')

    print *, "This program creates two n-element vectors from  question3_input.txt"
    print *, "Enter n (an integer between 6 and 16):"
    read (*,*) n

    write (2, *) "Vector V:     Vector U:"

    allocate(u(n))
    allocate(v(n))
    index = 1
    count = 0
    ! positives (vector v)
    do while (count < n)
        read (1, *) curr
        if (curr > 0) then
            v(index) = curr
            index = index + 1
            count = count + 1
        end if
    end do

    index = 1
    count = 0
    ! negatives (vector u)
    do while (count < n)
        read (1, *) curr
        if (curr < 0) then
            u(index) = curr
            index = index + 1
            count = count + 1
        end if
    end do

    do i = 1, n
        write (2, 200) v(i), u(i)
    end do
    
    largest = maxval(v)
    smallest = minval(u)

    write (2, 600) "The multiplication of the largest element of V and the smallest element of U is:", largest * smallest

    more_than_4 = 0
    less_than_neg4 = 0
    do i = 1, n
        if (v(i) > 4) then
            more_than_4 = more_than_4 + 1
        end if
        if (u(i) < -4) then
            less_than_neg4 = less_than_neg4 + 1
        end if
    end do

    write (2, 300) "V has ", more_than_4, " elements greater than 4, and U has ", less_than_neg4, " elements smaller than -4."
    write (2, 400) "The dot product of V and U is:", dot_product(v, u)
    write (2, 400) "The sum of all the V elements plus the sum of all the U elements is: ", sum(u) + sum(v)

    200 format (1x, ES8.2, 6x, ES9.2)
    300 format (a6, i1, a36, i1, a26)
    400 format (a30, f10.3)
    500 format (a69, f10.3)
    600 format (a80, f10.3)

end program problem3