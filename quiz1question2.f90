program question2
    implicit none
    integer::n, i, j
    real, dimension(3,2)::m
    
    print *, "Enter n (an integer between 0 to 6:"
    read (*,*) n

    if (0 <= n .and. 6 >= n) then
        do i=1,3
            do j=1,2
                m(i,j)= (2 * i + j) ** n
                print *, "i=", i, "j=", j, "Mij=", m(i,j)
            end do
        end do
    end if
end program question2