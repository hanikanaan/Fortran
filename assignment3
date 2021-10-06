program assignment3
    !Initialization
    implicit none
    real(kind=4)::minPrice, maxPrice, discount, min, max, exchange_rate, curr, PLACEHOLDER
    real::priceAfterTax, priceAfterConversion, priceAfterDiscount
    integer:: min_valid, max_valid
    logical :: valid
    character (len=85)::title
    character (len = 4)::CAD, USD
    CAD = 'CAD'
    USD = 'USD'

    !Print statements and input for title
    print *, "This program creates a discount chart for the store. HST+GST is accounted 13%." 
    print *, "You can copy and paste the chart from screen to print and post. You should provide the store price range, and the" 
    print *, "most current CAD to USD exchange rate." 
    print *, "If you are not ready yet press control+C to exit." 
    print *, "Enter the title (maximum 85 characters between quotations) of the discount chart."
    read (*,*) title


    !To check validity  of min/max prices and store them
    print *, "Please enter the minimum and maximum price tags in store using comma."
    print *, "Note: price tags must end in .49 or .99. For example, 1.99, 14.49."
    read (*,*) minPrice, maxPrice
    min = real(minPrice)
    max = real(maxPrice)
    min_valid = INT(min * 100)
    max_valid = INT(max * 100)
    valid = ((mod(min_valid, 100)==49).or.(mod(min_valid, 100)==99)).and.(((mod(max_valid, 100)==49).or.(mod(max_valid, 100)==99)))
    valid = valid .and. ((min > 0) .and. (max > 0))
    IF (min > max) THEN
        PLACEHOLDER = min
        min = max
        max = PLACEHOLDER
    END IF
    do while (.not. valid)
        print *, "One or both price tags are not valid values."
        print *, "Note that price tags must be positive and end in .49 or .99."
        print *, "Please re enter the minimum and maximum price tags in store."
        read (*,*) minPrice, maxPrice
        min = real(minPrice)
        max = real(maxPrice)
        min_valid = INT(min * 100)
        max_valid = INT(max * 100)
        valid=((mod(min_valid,100)==49).or.(mod(min_valid,100)==99)).and.(((mod(max_valid,100)==49).or.(mod(max_valid,100)==99)))
        valid = valid .and. ((min > 0) .and. (max > 0))
        IF (min > max) THEN
            PLACEHOLDER = min
            min = max
            max = PLACEHOLDER
        END IF
    end do


    !To check validity of discount and store it
    print *, "Please enter the  discount percentage (a value  between 0 to 100."
    print *, "Note: do not use % symbol, otherwise execution exits."
    read (*,*) discount
    valid = ((discount <= 100) .and. (discount >= 0))
    do while (.not. valid)
      print *, "The discount  percentage is not a valid value."
      print *, "Please re-enter a value between 0 and 100."
      read (*,*) discount
      valid = ((discount <= 100) .and. (discount >= 0))
    end do


    !To check validity of exchange rate and store it
    print *, "Please enter the most current CAD to USD exchange rate."
    read (*,*) exchange_rate
    valid = exchange_rate > 0
    do while (.not. valid)
        print *, "The exchange rate is not a valid value."
        print *, "Note that the exchange rate must be a positive value."
        print *, "Please re-enter the most current CAD to USD conversion rate."
        read (*,*) exchange_rate
        valid = (exchange_rate > 0)
    end do

    !Print final chart
    print *, "copy & paste the chart (provided between the dash lines.)"
    print *, "----------------------------------------------------------------"
    print *, title
    print *, "Original Price Tag Before Tax", "    Discounted Price After Tax"
    print *, CAD, "                              ", CAD, "               ", USD
    curr = min
    do while (curr <= max)
        priceAfterDiscount = curr * (1 - discount / 100)
        priceAfterTax = priceAfterDiscount * 1.13
        priceAfterConversion = priceAfterTax * exchange_rate
        print *, curr, "               ", priceAfterTax , priceAfterConversion
        curr = curr + 0.50
    end do
    print *, "----------------------------------------------------------------"

end program assignment3
