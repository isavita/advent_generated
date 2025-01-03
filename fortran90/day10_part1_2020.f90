
program adapter_array
  implicit none
  integer, allocatable :: adapters(:)
  integer :: joltage, diff, prev_joltage, i, n, count1, count3
  character(len=20) :: line
  integer, parameter :: BUF_SIZE = 1024
  character(len=BUF_SIZE) :: buffer
  integer :: ios

  open(unit=10, file="input.txt", status="old", action="read", iostat=ios)
  if (ios /= 0) then
    print *, "Error opening file"
    stop
  end if

  n = 0
  do
    read(10, '(A)', iostat=ios) line
    if (ios /= 0) exit
    read(line, *) joltage
    n = n + 1
    if (allocated(adapters)) then
      adapters = [adapters, joltage]
    else
      allocate(adapters(1))
      adapters(1) = joltage
    end if
  end do
  close(10)

  call quicksort(adapters, 1, n)

  count1 = 0
  count3 = 1
  prev_joltage = 0

  do i = 1, n
    diff = adapters(i) - prev_joltage
    if (diff == 1) then
      count1 = count1 + 1
    else if (diff == 3) then
      count3 = count3 + 1
    end if
    prev_joltage = adapters(i)
  end do

  print *, count1 * count3

contains

  recursive subroutine quicksort(arr, low, high)
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: low, high
    integer :: pivot_index

    if (low < high) then
      call partition(arr, low, high, pivot_index)
      call quicksort(arr, low, pivot_index - 1)
      call quicksort(arr, pivot_index + 1, high)
    end if
  end subroutine quicksort

  subroutine partition(arr, low, high, pivot_index)
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: low, high
    integer, intent(out) :: pivot_index
    integer :: pivot, i, j, temp

    pivot = arr(high)
    i = low - 1

    do j = low, high - 1
      if (arr(j) <= pivot) then
        i = i + 1
        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
      end if
    end do

    temp = arr(i + 1)
    arr(i + 1) = arr(high)
    arr(high) = temp
    pivot_index = i + 1
  end subroutine partition

end program adapter_array
