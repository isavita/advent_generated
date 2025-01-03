
program conway_cubes
  implicit none
  integer, parameter :: max_size = 50
  logical, dimension(:,:,:), allocatable :: cubes, next_cubes
  integer :: x, y, z, cycle, active_count, i, j, k, dx, dy, dz, count
  character(len=max_size) :: line
  integer :: x_size, y_size
  
  open(unit=10, file="input.txt", status="old")
  
  y_size = 0
  do
    read(10, '(A)', iostat=i) line
    if (i /= 0) exit
    y_size = y_size + 1
  end do
  rewind(10)
  
  x_size = len_trim(line)
  allocate(cubes(0:max_size, 0:max_size, 0:max_size))
  allocate(next_cubes(0:max_size, 0:max_size, 0:max_size))
  cubes = .false.
  
  do y = 0, y_size - 1
    read(10, '(A)') line
    do x = 0, x_size - 1
      if (line(x+1:x+1) == '#') then
        cubes(x+max_size/2-x_size/2, y+max_size/2-y_size/2, max_size/2) = .true.
      end if
    end do
  end do
  close(10)
  
  do cycle = 1, 6
    next_cubes = .false.
    do z = 1, max_size-1
      do y = 1, max_size-1
        do x = 1, max_size-1
          count = 0
          do dz = -1, 1
            do dy = -1, 1
              do dx = -1, 1
                if (dx == 0 .and. dy == 0 .and. dz == 0) cycle
                if (cubes(x+dx, y+dy, z+dz)) count = count + 1
              end do
            end do
          end do
          if (count == 3 .or. (count == 2 .and. cubes(x,y,z))) then
            next_cubes(x,y,z) = .true.
          end if
        end do
      end do
    end do
    cubes = next_cubes
  end do
  
  active_count = 0
  do z = 0, max_size
    do y = 0, max_size
      do x = 0, max_size
        if (cubes(x,y,z)) active_count = active_count + 1
      end do
    end do
  end do
  
  print *, active_count
  
  deallocate(cubes)
  deallocate(next_cubes)
end program conway_cubes
