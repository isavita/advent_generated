
program maze
  implicit none
  integer, allocatable :: offsets(:)
  integer :: index, steps, jump, n, i, ios
  character(len=20) :: line
  
  open(unit=10, file="input.txt", status="old", action="read", iostat=ios)
  if (ios /= 0) then
    print *, "Error opening input.txt"
    stop
  end if

  n = 0
  do
    read(10, '(a)', iostat=ios) line
    if (ios /= 0) exit
    n = n + 1
  end do
  rewind(10)

  allocate(offsets(n))
  do i = 1, n
    read(10, *) offsets(i)
  end do
  close(10)

  index = 1
  steps = 0
  do while (index >= 1 .and. index <= n)
    jump = offsets(index)
    offsets(index) = offsets(index) + 1
    index = index + jump
    steps = steps + 1
  end do

  print *, steps
end program maze
