program solution
  implicit none
  character(len=100) :: line
  integer :: sum, firstDigit, lastDigit, value, i
  character :: c

  sum = 0

  open(unit=10, file='input.txt', status='old')

  do
    read(10, '(A)', iostat=i) line
    if (i /= 0) exit

    firstDigit = -1
    lastDigit = -1

    do i = 1, len_trim(line)
      c = line(i:i)
      if (c >= '0' .and. c <= '9') then
        if (firstDigit == -1) then
          firstDigit = ichar(c) - ichar('0')
        end if
        lastDigit = ichar(c) - ichar('0')
      end if
    end do

    if (firstDigit /= -1 .and. lastDigit /= -1) then
      value = firstDigit * 10 + lastDigit
      sum = sum + value
    end if
  end do

  close(10)

  print *, sum
end program solution