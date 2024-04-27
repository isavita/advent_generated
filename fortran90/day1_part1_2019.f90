program fuel_counter
  implicit none

  integer, parameter :: dp = selected_real_kind(15)
  real(dp) :: total_fuel, fuel, mass
  integer :: io, i

  open(unit=10, file='input.txt', status='old', action='read')
  total_fuel = 0.0_dp

  do
    read(10, *, iostat=io) mass
    if (io /= 0) exit

    fuel = mass / 3.0_dp
    fuel = floor(fuel)
    fuel = fuel - 2.0_dp
    if (fuel < 0.0_dp) fuel = 0.0_dp

    total_fuel = total_fuel + fuel
  end do

  close(10)

  write(*, '(A,F15.2)') 'Total fuel requirement: ', total_fuel

end program fuel_counter