program solution
  integer :: steps, currentPos, i
  integer, allocatable :: buffer(:)
  character(len=10) :: line

  open(unit=10, file='input.txt', status='old')
  read(10, '(A)') line
  close(10)

  read(line, '(I6)') steps
  allocate(buffer(1))

  buffer(1) = 0
  currentPos = 0

  do i = 1, 2017
    currentPos = mod(currentPos + steps, size(buffer))
    buffer = [buffer(1:currentPos+1), i, buffer(currentPos+2:)]
    currentPos = currentPos + 1
  end do

  do i = 1, size(buffer)
    if (buffer(i) == 2017) then
      print *, buffer(mod(i+1, size(buffer)))
      exit
    end if
  end do

end program solution