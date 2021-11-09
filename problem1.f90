program problem1
    implicit none
    character(len=10)::str
    character(len=1)::currletter
    integer::i, j, k, l, index
    character(len = 10), dimension(20)::repeated
    
    open(1, file='Problem1_input.txt')
    read(1, *)

    index = 1
    do i = 1, 19
        read(1, *) str
        do j = 1, len_trim(str)
            currletter = str(j:j)
            do k = j + 1, len_trim(str)
                if (str(k:k) == currletter) then
                    if (.not.(any(repeated == str))) then
                        repeated(index) = str
                        index = index + 1
                    end if
                end if
            end do
        end do
    end do

    do i = 1, index + 1
        print *, repeated(i)
    end do
end program problem1