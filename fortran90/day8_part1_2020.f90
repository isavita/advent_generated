
program bootcode
  implicit none
  integer, allocatable :: instructions(:,:)
  integer :: acc, i, n, result
  character(len=10) :: line
  character(len=3) :: op
  integer :: arg
  logical, allocatable :: visited(:)
  
  open(unit=10, file="input.txt", status="old", action="read")
  n = 0
  do
    read(10, '(a)', iostat=result) line
    if (result /= 0) exit
    n = n + 1
  end do
  rewind(10)
  allocate(instructions(n,2))
  allocate(visited(n))
  visited = .false.
  
  do i = 1, n
    read(10, '(a)') line
    read(line, '(a3,i5)') op, arg
    if (op == "acc") then
      instructions(i,1) = 1
    else if (op == "jmp") then
      instructions(i,1) = 2
    else
      instructions(i,1) = 3
    end if
    instructions(i,2) = arg
  end do
  close(10)

  acc = 0
  i = 1
  do
    if (i > n) exit
    if (visited(i)) exit
    visited(i) = .true.
    select case (instructions(i,1))
    case (1)
      acc = acc + instructions(i,2)
      i = i + 1
    case (2)
      i = i + instructions(i,2)
    case (3)
      i = i + 1
    end select
  end do
  
  print *, acc
  
  deallocate(instructions)
  deallocate(visited)
end program bootcode
