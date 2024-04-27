program stream_processing
  implicit none
  character(len=100000) :: input
  integer :: i, score, group_level, garbage, ignore
  logical :: in_group, in_garbage

  open(unit=10, file='input.txt', status='old', action='read')
  read(10, '(A)') input
  close(10)

  score = 0
  group_level = 0
  garbage = 0
  ignore = 0
  in_group = .true.
  in_garbage = .false.

  do i = 1, len_trim(input)
    if (ignore == 1) then
      ignore = 0
      cycle
    end if

    if (in_garbage) then
      if (input(i:i) == '!') then
        ignore = 1
      else if (input(i:i) == '>') then
        in_garbage = .false.
        garbage = 0
      else
        garbage = garbage + 1
      end if
    else
      if (input(i:i) == '<') then
        in_garbage = .true.
        garbage = 0
      else if (input(i:i) == '{') then
        group_level = group_level + 1
        score = score + group_level
      else if (input(i:i) == '}') then
        group_level = group_level - 1
      end if
    end if
  end do

  print *, score
end program stream_processing