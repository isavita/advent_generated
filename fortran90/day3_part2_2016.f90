
program triangles
  implicit none
  integer, allocatable :: numbers(:,:)
  integer :: i, j, n, valid_triangles, a, b, c
  character(len=256) :: line
  integer, dimension(:), allocatable :: row
  integer :: ios

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

  allocate(numbers(n, 3))
  
  do i = 1, n
    read(10, *) numbers(i, :)
  end do

  valid_triangles = 0
  do i = 1, size(numbers,2)
    do j = 1, size(numbers,1), 3
      if (j+2 <= size(numbers,1)) then
        a = numbers(j,i)
        b = numbers(j+1,i)
        c = numbers(j+2,i)
        if (a+b > c .and. a+c > b .and. b+c > a) then
          valid_triangles = valid_triangles + 1
        end if
      end if
    end do
  end do

  print *, valid_triangles

  close(10)
end program triangles
