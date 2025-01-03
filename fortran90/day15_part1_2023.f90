
program main
  implicit none
  integer, parameter :: hashTableSize = 256
  character(len=100000) :: line
  character(len=100), allocatable :: steps(:)
  integer :: i, j, res, numSteps, start, finish
  integer :: hashValue
  
  open(unit=10, file="input.txt", status="old", action="read")
  read(10, '(A)') line
  close(10)

  numSteps = 0
  start = 1
  do i = 1, len(line)
    if (line(i:i) == ',' .or. i == len(line)) then
      numSteps = numSteps + 1
    end if
  end do
  allocate(steps(numSteps))

  numSteps = 0
  start = 1
  do i = 1, len(line)
    if (line(i:i) == ',' .or. i == len(line)) then
      if (i == len(line)) then
        finish = i
      else
        finish = i - 1
      end if
      numSteps = numSteps + 1
      steps(numSteps) = line(start:finish)
      start = i + 1
    end if
  end do

  res = 0
  do i = 1, numSteps
    hashValue = 0
    do j = 1, len(trim(steps(i)))
      hashValue = mod((hashValue + ichar(steps(i)(j:j))) * 17, hashTableSize)
    end do
    res = res + hashValue
  end do

  print *, res

end program main
