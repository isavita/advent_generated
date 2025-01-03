
program polymer_reaction
  implicit none
  character(len=:), allocatable :: polymer, temp_polymer, reacted_polymer
  integer :: i, j, min_length, polymer_len, temp_len
  character(len=1) :: unit
  integer, parameter :: BUF_SIZE = 100000
  character(len=BUF_SIZE) :: buffer
  integer :: ios

  open(unit=10, file="input.txt", status="old", action="read", iostat=ios)
  if (ios /= 0) then
    print *, "Error opening input.txt"
    stop
  end if
  read(10, '(A)', iostat=ios) buffer
  close(10)
  if (ios /= 0) then
    print *, "Error reading input.txt"
    stop
  end if
  polymer = trim(buffer)
  polymer_len = len(polymer)

  min_length = polymer_len

  do i = ichar('a'), ichar('z')
    unit = char(i)
    temp_polymer = ""
    do j = 1, polymer_len
      if (polymer(j:j) /= unit .and. polymer(j:j) /= char(i-32)) then
        temp_polymer = temp_polymer // polymer(j:j)
      end if
    end do
    temp_len = len(temp_polymer)
    reacted_polymer = react_polymer(temp_polymer)
    if (len(reacted_polymer) < min_length) then
      min_length = len(reacted_polymer)
    end if
  end do

  print *, min_length

contains

  function react_polymer(polymer) result(reacted)
    implicit none
    character(len=:), allocatable, intent(in) :: polymer
    character(len=:), allocatable :: reacted
    integer :: i, polymer_len, j
    logical :: reaction_occurred
    character(len=1), allocatable :: stack(:)
    integer :: stack_ptr

    polymer_len = len(polymer)
    allocate(stack(polymer_len))
    stack_ptr = 0

    do i = 1, polymer_len
      if (stack_ptr > 0) then
        if (stack(stack_ptr) /= polymer(i:i) .and. &
            (achar(iachar(stack(stack_ptr)) - 32) == polymer(i:i) .or. &
             achar(iachar(stack(stack_ptr)) + 32) == polymer(i:i))) then
          stack_ptr = stack_ptr - 1
        else
          stack_ptr = stack_ptr + 1
          stack(stack_ptr) = polymer(i:i)
        end if
      else
        stack_ptr = stack_ptr + 1
        stack(stack_ptr) = polymer(i:i)
      end if
    end do

    reacted = ""
    do j = 1, stack_ptr
      reacted = reacted // stack(j)
    end do
    deallocate(stack)
  end function react_polymer

end program polymer_reaction
