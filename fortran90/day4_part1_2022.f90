
program main
  implicit none
  integer :: count, start1, end1, start2, end2
  character(len=20) :: line
  integer, dimension(2) :: range1, range2
  integer :: iostat
  open(unit=10, file="input.txt", status="old", action="read", iostat=iostat)
  if (iostat /= 0) then
    print *, "Error opening file"
    stop
  end if
  count = 0
  do
    read(10, '(a)', iostat=iostat) line
    if (iostat /= 0) exit
    call parse_range(line, range1, range2)
    start1 = range1(1)
    end1 = range1(2)
    start2 = range2(1)
    end2 = range2(2)
    if ((start1 <= start2 .and. end1 >= end2) .or. (start2 <= start1 .and. end2 >= end1)) then
      count = count + 1
    end if
  end do
  close(10)
  print *, count
contains
  subroutine parse_range(line, range1, range2)
    implicit none
    character(len=*), intent(in) :: line
    integer, dimension(2), intent(out) :: range1, range2
    integer :: i, j, k, dash_pos, comma_pos
    character(len=20) :: temp
    comma_pos = index(line, ",")
    temp = line(1:comma_pos-1)
    dash_pos = index(temp, "-")
    read(temp(1:dash_pos-1), *) range1(1)
    read(temp(dash_pos+1:), *) range1(2)
    temp = line(comma_pos+1:)
    dash_pos = index(temp, "-")
    read(temp(1:dash_pos-1), *) range2(1)
    read(temp(dash_pos+1:), *) range2(2)
  end subroutine parse_range
end program main
