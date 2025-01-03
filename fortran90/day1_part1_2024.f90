
program solve
  implicit none
  integer, parameter :: max_size = 100000
  integer :: left(max_size), right(max_size)
  integer :: n, i, totalDistance, l, r
  character(len=100) :: line
  
  open(unit=10, file="input.txt", status="old", action="read")
  n = 0
  do
    read(10, '(A)', iostat=i) line
    if (i /= 0) exit
    n = n + 1
    read(line, *) l, r
    left(n) = l
    right(n) = r
  end do
  close(10)

  call quicksort(left, 1, n)
  call quicksort(right, 1, n)

  totalDistance = 0
  do i = 1, n
    totalDistance = totalDistance + abs(left(i) - right(i))
  end do

  print *, totalDistance

contains

  recursive subroutine quicksort(arr, low, high)
    integer, intent(in out) :: arr(:)
    integer, intent(in) :: low, high
    integer :: pivot, i, j, temp

    if (low < high) then
      pivot = low
      i = low
      j = high
      do while (i < j)
        do while (arr(i) <= arr(pivot) .and. i < high)
          i = i + 1
        end do
        do while (arr(j) > arr(pivot))
          j = j - 1
        end do
        if (i < j) then
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
        end if
      end do
      temp = arr(pivot)
      arr(pivot) = arr(j)
      arr(j) = temp
      call quicksort(arr, low, j - 1)
      call quicksort(arr, j + 1, high)
    end if
  end subroutine quicksort
end program solve
