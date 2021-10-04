program question1
    implicit none
    real::a, y
    real, dimension(4)::x
    integer::i
    x=(/1.3,2.6,3.9,5.2/)
    do i =  1, 4
        a = 1 / (3 + (27.1 * (x(i) ** 2)))
        y = ((x(i)**3.2 - 1) ** (4 * cos(x(i)))) / (log(x(i) - 1) - a)
        print *, "For  x=", x(i), "y is equal to: ", y
    end do
end program question1
