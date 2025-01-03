
program main
  implicit none
  integer, parameter :: max_size = 25
  integer :: x, y, z, i, j, k, surface_area
  logical, dimension(-max_size:max_size,-max_size:max_size,-max_size:max_size) :: cubes
  character(len=20) :: line
  integer, dimension(3) :: coords
  integer :: unit

  cubes = .false.
  open(newunit=unit, file="input.txt", status="old", action="read")
  do
    read(unit, '(a)', iostat=i) line
    if (i /= 0) exit
    read(line, *, iostat=i) coords(1), coords(2), coords(3)
    if (i /= 0) cycle
    x = coords(1)
    y = coords(2)
    z = coords(3)
    cubes(x, y, z) = .true.
  end do
  close(unit)

  surface_area = 0
  do x = -max_size, max_size
    do y = -max_size, max_size
      do z = -max_size, max_size
        if (cubes(x, y, z)) then
          surface_area = surface_area + calculate_exposed_sides(x, y, z, cubes)
        end if
      end do
    end do
  end do

  print *, surface_area

contains

  function calculate_exposed_sides(x, y, z, cubes) result(exposed_sides)
    implicit none
    integer, intent(in) :: x, y, z
    logical, dimension(-max_size:max_size,-max_size:max_size,-max_size:max_size), intent(in) :: cubes
    integer :: exposed_sides
    integer, dimension(3) :: directions(6,3)
    integer :: i, nx, ny, nz

    directions(1,:) = [1, 0, 0]
    directions(2,:) = [-1, 0, 0]
    directions(3,:) = [0, 1, 0]
    directions(4,:) = [0, -1, 0]
    directions(5,:) = [0, 0, 1]
    directions(6,:) = [0, 0, -1]

    exposed_sides = 6
    do i = 1, 6
      nx = x + directions(i,1)
      ny = y + directions(i,2)
      nz = z + directions(i,3)
      if (nx >= -max_size .and. nx <= max_size .and. ny >= -max_size .and. ny <= max_size .and. nz >= -max_size .and. nz <= max_size) then
        if (cubes(nx, ny, nz)) then
          exposed_sides = exposed_sides - 1
        end if
      end if
    end do
  end function calculate_exposed_sides

end program main
