program main
  implicit none
  character(len=1024) :: line
  logical :: file_exists
  integer :: io, totalCount, i
  character :: question
  logical, allocatable :: groupAnswers(:)

  inquire(file='input.txt', exist=file_exists)
  if (.not. file_exists) then
    write (*,*) 'Error: file does not exist'
    stop
  end if

  open(unit=10, file='input.txt', status='old', action='read')
  totalCount = 0
  allocate(groupAnswers(126)) ! assuming ASCII characters

  do
    read(10, '(A)', iostat=io) line
    if (io /= 0) exit
    if (line == '') then
      totalCount = totalCount + count(groupAnswers)
      groupAnswers = .false.
    else
      do i = 1, len_trim(line)
        question = line(i:i)
        groupAnswers(ichar(question)) = .true.
      end do
    end if
  end do

  totalCount = totalCount + count(groupAnswers)
  write (*,*) totalCount
  close(10)
end program main