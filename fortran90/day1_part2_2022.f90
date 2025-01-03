
program calorie_count
  implicit none
  integer, allocatable :: calories(:)
  integer :: current_calories, total_calories, i, n, unit, status
  character(len=20) :: line
  
  open(newunit=unit, file="input.txt", status="old", action="read", iostat=status)
  if (status /= 0) then
    print *, "Error opening input.txt"
    stop
  end if

  allocate(calories(10000))
  n = 0
  current_calories = 0

  do
    read(unit, '(a)', iostat=status) line
    if (status /= 0) exit
    if (len_trim(line) == 0) then
      n = n + 1
      calories(n) = current_calories
      current_calories = 0
    else
      read(line, *) total_calories
      current_calories = current_calories + total_calories
    end if
  end do
  
  if (current_calories > 0) then
    n = n + 1
    calories(n) = current_calories
  end if

  call quicksort(calories, 1, n)

  total_calories = 0
  do i = 1, min(3, n)
    total_calories = total_calories + calories(i)
  end do

  print *, total_calories

  deallocate(calories)
  close(unit)

contains

  recursive subroutine quicksort(arr, low, high)
    integer, intent(inout) :: arr(:)
    integer, intent(in) :: low, high
    integer :: pivot, i, j, temp

    if (low < high) then
      pivot = arr(high)
      i = low - 1
      do j = low, high - 1
        if (arr(j) > pivot) then
          i = i + 1
          temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
        end if
      end do
      temp = arr(i+1)
      arr(i+1) = arr(high)
      arr(high) = temp
      call quicksort(arr, low, i)
      call quicksort(arr, i+2, high)
    end if
  end subroutine quicksort

end program calorie_count
