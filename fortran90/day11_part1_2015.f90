
program next_password
  implicit none
  character(len=8) :: current_password, new_password
  integer :: i, ios

  open(unit=10, file="input.txt", status="old", action="read", iostat=ios)
  if (ios /= 0) then
    print *, "Error opening input file"
    stop
  end if
  read(10, '(A)') current_password
  close(10)

  new_password = find_next_password(current_password)
  print *, new_password

contains

  function find_next_password(password) result(next_pass)
    character(len=*), intent(in) :: password
    character(len=8) :: next_pass
    character(len=8) :: temp_pass

    temp_pass = password
    do
      temp_pass = increment_password(temp_pass)
      if (is_valid_password(temp_pass)) then
        exit
      end if
    end do
    next_pass = temp_pass
  end function find_next_password

  function increment_password(password) result(incremented_pass)
    character(len=*), intent(in) :: password
    character(len=8) :: incremented_pass
    integer :: i

    incremented_pass = password
    do i = 8, 1, -1
      incremented_pass(i:i) = char(ichar(incremented_pass(i:i)) + 1)
      if (incremented_pass(i:i) > 'z') then
        incremented_pass(i:i) = 'a'
      else
        exit
      end if
    end do
  end function increment_password

  function is_valid_password(password) result(valid)
    character(len=*), intent(in) :: password
    logical :: valid

    valid = has_straight(password) .and. .not. contains_invalid_letters(password) .and. has_two_pairs(password)
  end function is_valid_password

  function has_straight(password) result(straight)
    character(len=*), intent(in) :: password
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
    character(len=*), intent(in) :: password
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
    character(len=*), intent(in) :: password
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

end program next_password
