program solution
  implicit none
  integer :: target, elf, house, houseNumber, presents
  character(len=100) :: input
  integer, allocatable :: houses(:)
  integer :: i, ierr

  open(unit=10, file='input.txt', status='old', action='read')
  read(10, '(A)') input
  close(10)

  read(input, '(I10)') target
  target = target / 11

  allocate(houses(0:target))

  do elf = 1, target
    do house = elf, min(elf*50, target), elf
      houses(house) = houses(house) + elf
    end do
  end do

  do i = 0, size(houses)
    if (houses(i) >= target) then
      houseNumber = i
      exit
    end if
  end do

  print *, houseNumber

end program solution