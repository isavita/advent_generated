
program solve
  implicit none
  integer, parameter :: max_lines = 10000
  character(len=256), dimension(max_lines) :: lines
  integer :: num_lines, i, j, k, size, total, want, available, min_size
  character(len=256) :: line, path, dir_name
  integer, dimension(:), allocatable :: sizes
  integer, dimension(:), allocatable :: dir_sizes
  character(len=256), dimension(:), allocatable :: dir_names
  integer :: dir_count, file_size
  character(len=256), dimension(:), allocatable :: file_paths
  integer, dimension(:), allocatable :: file_sizes
  integer :: file_count
  character(len=256), dimension(100) :: curr
  integer :: curr_len
  
  open(unit=10, file="input.txt", status="old", action="read")
  num_lines = 0
  do
    read(10, '(a)', iostat=i) line
    if (i /= 0) exit
    num_lines = num_lines + 1
    lines(num_lines) = line
  end do
  close(10)

  allocate(dir_names(1000))
  allocate(dir_sizes(1000))
  allocate(file_paths(10000))
  allocate(file_sizes(10000))
  dir_count = 0
  file_count = 0
  curr_len = 0
  
  do i = 1, num_lines
    line = lines(i)
    if (line(1:1) == "$") then
      if (line(3:4) == "cd") then
        if (line(6:6) == "/") then
          curr_len = 0
        else if (line(6:7) == "..") then
          curr_len = curr_len - 1
        else
          curr_len = curr_len + 1
          curr(curr_len) = line(6:)
        end if
        path = ""
        do j = 1, curr_len
          path = trim(path) // "/" // trim(curr(j))
        end do
        dir_name = path
        do k = 1, dir_count
          if (dir_name == dir_names(k)) exit
        end do
        if (k > dir_count) then
          dir_count = dir_count + 1
          dir_names(dir_count) = dir_name
          dir_sizes(dir_count) = 0
        end if
      end if
    else if (line(1:3) /= "dir") then
      read(line, *) file_size
      file_count = file_count + 1
      file_sizes(file_count) = file_size
      path = ""
      do j = 1, curr_len
        path = trim(path) // "/" // trim(curr(j))
      end do
      file_paths(file_count) = trim(path) // "/" // trim(line(index(line, " ")+1:))
    end if
  end do

  do i = 1, file_count
    path = file_paths(i)
    size = file_sizes(i)
    do j = 1, len(path)
      if (path(j:j) == "/") then
        dir_name = path(1:j-1)
        do k = 1, dir_count
          if (dir_name == dir_names(k)) then
            dir_sizes(k) = dir_sizes(k) + size
            exit
          end if
        end do
      end if
    end do
    do k = 1, dir_count
      if (path == dir_names(k)) then
        dir_sizes(k) = dir_sizes(k) + size
        exit
      end if
    end do
  end do

  allocate(sizes(dir_count))
  sizes = dir_sizes(1:dir_count)
  call quicksort(sizes, 1, dir_count)

  total = 70000000
  want = 30000000
  available = total - dir_sizes(1)
  
  do i = 1, dir_count
    if (sizes(i) >= want - available) then
      min_size = sizes(i)
      exit
    end if
  end do
  
  print *, min_size
  
  deallocate(sizes)
  deallocate(dir_sizes)
  deallocate(dir_names)
  deallocate(file_paths)
  deallocate(file_sizes)
  
contains

  recursive subroutine quicksort(arr, low, high)
    integer, dimension(:), intent(inout) :: arr
    integer, intent(in) :: low, high
    integer :: i, j, pivot, temp
    if (low < high) then
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
      call quicksort(arr, low, i)
      call quicksort(arr, i + 2, high)
    end if
  end subroutine quicksort

end program solve
