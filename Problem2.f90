program problem2
    implicit none
    integer::n
    Print*, 'Enter an integer number >1'
    read*, n
    print '(a,i3,a,i4)', 'hanoi number of ',n,' is:',TOH(n)


    contains
    recursive function TOH(n) result(s)
        integer, intent(in)::n
        integer::s
        if (n == 1) then
            s = 1
        else if (n > 1) then
            s = 2 * TOH(n - 1) + 1
        end if
    end function TOH
    
end program problem2