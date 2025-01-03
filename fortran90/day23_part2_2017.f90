
program main
  implicit none
  integer :: b, c, h, x, i
  logical, allocatable :: is_prime(:)
  integer, parameter :: start_b = 57*100 + 100000
  integer, parameter :: step = 17
  integer, parameter :: end_c = start_b + 17000
  integer :: max_val

  b = start_b
  c = end_c
  h = 0
  max_val = c

  allocate(is_prime(2:max_val))
  is_prime = .true.

  do i = 2, int(sqrt(real(max_val)))
    if (is_prime(i)) then
      is_prime(i*i:max_val:i) = .false.
    end if
  end do

  do x = b, c, step
    if (.not. is_prime(x)) then
      h = h + 1
    end if
  end do

  print *, h

  deallocate(is_prime)
end program main
