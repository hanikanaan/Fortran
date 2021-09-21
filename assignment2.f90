program assignment2
    implicit none
    character(len=65)::name
    integer::num, curr_odd_num, odd_sum
    curr_odd_num=1
    odd_sum=0
    print *, "This program prints all odd numbers between 0 and your desired integer number (less than 1000) on"
    print *, "the screen and represents the sum of the odd numbers at the end."
    print *, "Please enter your name (maximum 65 letters)"
    read(*,*) name
    print *, "Please enter your desired integer number (your number should be between 0 to 1000)"
    read (*,*) num
    IF (num > 1000 .OR. num < 0) THEN 
        DO WHILE (num > 1000 .OR. num < 0)
            print *, "Your input is out of range, please enter another number."
            read(*,*) num
        END DO        
    END IF
    IF (num > 0 .AND. num <= 1000) THEN
        print *, "The odd numbers between 0 to ", num, "are:"
        DO WHILE (curr_odd_num <= num)
            print *, curr_odd_num
            curr_odd_num = curr_odd_num + 2
            odd_sum = odd_sum + curr_odd_num
        END DO
        print *, "The sum of these odd numbers is: ", odd_sum
    END IF
    IF (num == 0) THEN
        print *, "Your input is 0 and there are no odd numbers."
    END IF
    
    print *, "Thank you ", name, " for using this code."
end program assignment2

