program solution
    character(len=100) :: passport
    character(len=3), dimension(7) :: requiredFields
    integer :: validPassports, i
    character(len=100) :: line
    character(len=100), dimension(1000) :: passports
    integer :: numPassports
    open(unit=10, file='input.txt')
    
    numPassports = 0
    passport = ""
    do
        read(10, '(A)', end=100) line
        if (line == "") then
            numPassports = numPassports + 1
            passports(numPassports) = trim(passport)
            passport = ""
        else
            passport = trim(passport) // " " // trim(line)
        end if
    end do
100 continue

    if (trim(passport) /= "") then
        numPassports = numPassports + 1
        passports(numPassports) = trim(passport)
    end if

    requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    validPassports = 0
    do i = 1, numPassports
        if (isValid(passports(i), requiredFields)) then
            validPassports = validPassports + 1
        end if
    end do

    print *, validPassports
    close(10)

contains

    logical function isValid(passport, requiredFields)
        character(len=100), intent(in) :: passport
        character(len=3), dimension(7), intent(in) :: requiredFields
        integer :: i
        
        isValid = .true.
        do i = 1, 7
            if (index(passport, requiredFields(i)//":") == 0) then
                isValid = .false.
                exit
            end if
        end do
    end function isValid

end program solution