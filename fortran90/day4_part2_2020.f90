
program solution
    character(1000) :: line, passport
    character(1000), allocatable :: passports(:)
    integer :: i, validPassports
    character(1000) :: field, key, value
    character(1000), allocatable :: fields(:)
    character(1000), allocatable :: validEcl(:)
    logical :: isValidPassport

    open(unit=10, file='input.txt', status='old')

    i = 1
    allocate(passports(i))
    passport = ""

    do
        read(10, '(A)', iostat=iostat) line
        if (iostat /= 0) then
            exit
        end if

        if (line == "") then
            passports(i) = trim(passport)
            i = i + 1
            allocate(passports(i))
            passport = ""
        else
            passport = trim(passport) // " " // line
        end if
    end do

    if (len_trim(passport) /= 0) then
        passports(i) = trim(passport)
    end if

    close(10)

    validPassports = 0

    do i = 1, size(passports)
        if (isValidPassport(passports(i))) then
            validPassports = validPassports + 1
        end if
    end do

    print *, validPassports

contains

    logical function isValidPassport(passport)
        character(1000), intent(in) :: passport
        character(1000) :: field, key, value
        character(1000), allocatable :: fieldMap(:)
        logical :: validateByr, validateIyr, validateEyr, validateHgt, validateHcl, validateEcl, validatePid

        allocate(fieldMap(size(split(passport, " "))))

        fieldMap = split(passport, " ")

        do i = 1, size(fieldMap)
            field = fieldMap(i)
            key = trim(field(1:index(field, ":")-1))
            value = trim(field(index(field, ":")+1:))
            fieldMap(i) = key // ":" // value
        end do

        if (.not. validateByr(value=fieldMap(index(fieldMap, "byr:")))) then
            isValidPassport = .false.
            return
        end if

        if (.not. validateIyr(value=fieldMap(index(fieldMap, "iyr:")))) then
            isValidPassport = .false.
            return
        end if

        if (.not. validateEyr(value=fieldMap(index(fieldMap, "eyr:")))) then
            isValidPassport = .false.
            return
        end if

        if (.not. validateHgt(value=fieldMap(index(fieldMap, "hgt:")))) then
            isValidPassport = .false.
            return
        end if

        if (.not. validateHcl(value=fieldMap(index(fieldMap, "hcl:")))) then
            isValidPassport = .false.
            return
        end if

        if (.not. validateEcl(value=fieldMap(index(fieldMap, "ecl:")))) then
            isValidPassport = .false.
            return
        end if

        if (.not. validatePid(value=fieldMap(index(fieldMap, "pid:")))) then
            isValidPassport = .false.
            return
        end if

        isValidPassport = .true.
    end function isValidPassport

    logical function validateByr(value)
        character(1000), intent(in) :: value
        integer :: year

        read(value, '(I5)', iostat=iostat) year

        if (iostat /= 0) then
            validateByr = .false.
            return
        end if

        validateByr = year >= 1920 .and. year <= 2002
    end function validateByr

    logical function validateIyr(value)
        character(1000), intent(in) :: value
        integer :: year

        read(value, '(I5)', iostat=iostat) year

        if (iostat /= 0) then
            validateIyr = .false.
            return
        end if

        validateIyr = year >= 2010 .and. year <= 2020
    end function validateIyr

    logical function validateEyr(value)
        character(1000), intent(in) :: value
        integer :: year

        read(value, '(I5)', iostat=iostat) year

        if (iostat /= 0) then
            validateEyr = .false.
            return
        end if

        validateEyr = year >= 2020 .and. year <= 2030
    end function validateEyr

    logical function validateHgt(value)
        character(1000), intent(in) :: value
        integer :: hgt

        if (index(value, "cm") > 0) then
            read(value, '(I3)', iostat=iostat) hgt

            if (iostat /= 0) then
                validateHgt = .false.
                return
            end if

            validateHgt = hgt >= 150 .and. hgt <= 193
        else if (index(value, "in") > 0) then
            read(value, '(I2)', iostat=iostat) hgt

            if (iostat /= 0) then
                validateHgt = .false.
                return
            end if

            validateHgt = hgt >= 59 .and. hgt <= 76
        else
            validateHgt = .false.
        end if
    end function validateHgt

    logical function validateHcl(value)
        character(1000), intent(in) :: value
        logical :: matched

        matched = .false.
        call execute_command_line("echo " // value // " | grep -P '^#[0-9a-f]{6}$' > /dev/null")
        if (system('echo $?') == 0) then
            matched = .true.
        end if

        validateHcl = matched
    end function validateHcl

    logical function validateEcl(value)
        character(1000), intent(in) :: value
        character(1000), allocatable :: validEcl(:)
        logical :: valid

        allocate(validEcl(7))
        validEcl = (/"amb", "blu", "brn", "gry", "grn", "hzl", "oth"/)

        valid = .false.
        do i = 1, size(validEcl)
            if (trim(value) == validEcl(i)) then
                valid = .true.
                exit
            end if
        end do

        validateEcl = valid
    end function validateEcl

    logical function validatePid(value)
        character(1000), intent(in) :: value
        logical :: matched

        matched = .false.
        call execute_command_line("echo " // value // " | grep -P '^[0-9]{9}$' > /dev/null")
        if (system('echo $?') == 0) then
            matched = .true.
        end if

        validatePid = matched
    end function validatePid

end program solution
