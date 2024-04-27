program toboggan_trajectory
  implicit none
  character(len=1024) :: line
  integer :: i, j, trees, width, height
  logical, allocatable :: map(:, :)
  open(unit=10, file='input.txt', status='old')
  read(10, '(A)') line
  width = len_trim(line)
  height = 0
  do
    read(10, '(A)', end=10) line
    height = height + 1
  end do
  10 rewind(10)
  allocate(map(height, width))
  do i = 1, height
    read(10, '(A)') line
    do j = 1, width
      if (line(j:j) == '#') then
        map(i, j) = .true.
      else
        map(i, j) = .false.
      end if
    end do
  end do
  close(10)
  trees = 0
  j = 1
  do i = 1, height - 1
    j = mod(j + 2, width) + 1
    if (map(i + 1, j)) then
      trees = trees + 1
    end if
  end do
  print *, 'Trees encountered:', trees
end program toboggan_trajectory