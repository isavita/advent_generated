
program fft
  implicit none
  integer, parameter :: nphases = 100
  character(len=10000) :: input_str
  integer, allocatable :: digits(:), output(:)
  integer :: i, j, phase, sum, pattern_val, len_input
  integer, parameter :: base_pattern(4) = (/0, 1, 0, -1/)
  open(unit=10, file="input.txt", status="old")
  read(10, "(a)") input_str
  close(10)
  len_input = len_trim(input_str)
  allocate(digits(len_input), output(len_input))
  do i = 1, len_input
    read(input_str(i:i), "(i1)") digits(i)
  end do
  do phase = 1, nphases
    do i = 1, len_input
      sum = 0
      do j = 1, len_input
        pattern_val = base_pattern(mod((j/(i)) , 4) + 1)
        sum = sum + digits(j) * pattern_val
      end do
      output(i) = mod(abs(sum), 10)
    end do
    digits = output
  end do
  do i = 1, 8
    write(*, "(i1)", advance="no") digits(i)
  end do
  write(*,*)
  deallocate(digits, output)
end program fft
