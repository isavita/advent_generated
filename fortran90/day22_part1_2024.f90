
program secret_sum
  implicit none
  integer, parameter :: dp = kind(0.d0)
  integer(kind=8) :: total, b, s, x
  integer :: i, ios
  character(len=100) :: line

  total = 0
  open(unit=10, file="input.txt", status="old", action="read")

  do
    read(10, "(a)", iostat=ios) line
    if (ios /= 0) exit
    if (len_trim(line) == 0) cycle

    read(line, *) b
    s = b
    do i = 1, 2000
      x = s * 64
      s = ieor(s, x)
      s = iand(s, int(z'FFFFFF', kind=8))
      x = s / 32
      s = ieor(s, x)
      s = iand(s, int(z'FFFFFF', kind=8))
      x = s * 2048
      s = ieor(s, x)
      s = iand(s, int(z'FFFFFF', kind=8))
    end do
    total = total + s
  end do

  close(10)
  print *, total
end program secret_sum
