
program password_generator
    implicit none
    character(len=8) :: current_password, first_new_password, second_new_password
    integer :: i, unit = 10

    open(unit, file="input.txt", status="old", action="read")
    read(unit, '(A)') current_password
    close(unit)

    first_new_password = find_next_password(current_password)
    second_new_password = find_next_password(first_new_password)

    print *, second_new_password

contains

    function find_next_password(password) result(next_password)
        character(len=8), intent(in) :: password
        character(len=8) :: next_password
        logical :: is_valid

        next_password = password
        do
            next_password = increment_password(next_password)
            is_valid = is_valid_password(next_password)
            if (is_valid) exit
        end do
    end function find_next_password

    function increment_password(password) result(incremented_password)
        character(len=8), intent(in) :: password
        character(len=8) :: incremented_password
        integer :: i
        character(len=1) :: carry

        incremented_password = password
        carry = '1'
        do i = 8, 1, -1
            if (carry == '1') then
                if (incremented_password(i:i) == 'z') then
                    incremented_password(i:i) = 'a'
                    carry = '1'
                else
                    incremented_password(i:i) = char(ichar(incremented_password(i:i)) + 1)
                    carry = '0'
                end if
            else
                exit
            end if
        end do
    end function increment_password

    function is_valid_password(password) result(valid)
        character(len=8), intent(in) :: password
        logical :: valid

        valid = has_straight(password) .and. .not. contains_invalid_letters(password) .and. has_two_pairs(password)
    end function is_valid_password

    function has_straight(password) result(straight)
        character(len=8), intent(in) :: password
        logical :: straight
        integer :: i

        straight = .false.
        do i = 1, 6
            if (ichar(password(i:i)) + 1 == ichar(password(i+1:i+1)) .and. ichar(password(i:i)) + 2 == ichar(password(i+2:i+2))) then
                straight = .true.
                return
            end if
        end do
    end function has_straight

    function contains_invalid_letters(password) result(invalid)
        character(len=8), intent(in) :: password
        logical :: invalid
        integer :: i

        invalid = .false.
        do i = 1, 8
            if (password(i:i) == 'i' .or. password(i:i) == 'o' .or. password(i:i) == 'l') then
                invalid = .true.
                return
            end if
        end do
    end function contains_invalid_letters

    function has_two_pairs(password) result(two_pairs)
        character(len=8), intent(in) :: password
        logical :: two_pairs
        integer :: i, count

        count = 0
        i = 1
        do while (i < 8)
            if (password(i:i) == password(i+1:i+1)) then
                count = count + 1
                i = i + 2
            else
                i = i + 1
            end if
        end do
        two_pairs = count >= 2
    end function has_two_pairs

end program password_generator
