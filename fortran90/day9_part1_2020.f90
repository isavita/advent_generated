program xmas
  implicit none
  integer, parameter :: preamble_length = 25
  integer, dimension(1000) :: numbers
  integer :: i, n, io
  logical :: found
  open(unit=10, file='input.txt', status='old', action='read')
  i = 0
  do
    read(10, *, iostat=io) n
    if (io /= 0) exit
    i = i + 1
    numbers(i) = n
  end do
  close(10)
  do i = preamble_length, i
    found = .false.
    do n = i - preamble_length, i - 1
      if (any(numbers(n) + numbers(n:i-1) == numbers(i))) then
        found = .true.
        exit
      end if
    end do
    if (.not. found) then
      print *, numbers(i)
      stop
    end if
  end do
end program xmas