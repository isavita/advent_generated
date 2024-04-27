program day9
  implicit none
  character(len=100000) :: line
  integer :: i, score, group_score, garbage_count
  logical :: in_group, in_garbage, cancel_next
  open(unit=10, file='input.txt', status='old')
  read(10, '(A)') line
  close(10)
  score = 0
  group_score = 0
  garbage_count = 0
  in_group = .true.
  in_garbage = .false.
  cancel_next = .false.
  do i = 1, len_trim(line)
    if (cancel_next) then
      cancel_next = .false.
      cycle
    end if
    if (in_garbage) then
      if (line(i:i) == '!') then
        cancel_next = .true.
      else if (line(i:i) == '>') then
        in_garbage = .false.
      else
        garbage_count = garbage_count + 1
      end if
    else if (in_group) then
      if (line(i:i) == '{') then
        group_score = group_score + 1
        score = score + group_score
      else if (line(i:i) == '}') then
        group_score = group_score - 1
      else if (line(i:i) == '<') then
        in_garbage = .true.
      end if
    end if
  end do
  write (*, '(A,I0)') 'Total score: ', score
  write (*, '(A,I0)') 'Garbage count: ', garbage_count
end program day9