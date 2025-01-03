
program santa
  implicit none
  integer, allocatable :: packages(:)
  integer :: total_weight, target_weight, num_packages, i, j, weight, group_weight, group_length
  integer(kind=8) :: best_qe, qe
  integer :: best_length
  character(len=20) :: line
  integer :: ios
  
  open(unit=10, file="input.txt", status="old", action="read", iostat=ios)
  if (ios /= 0) then
    print *, "Error opening input.txt"
    stop
  end if

  num_packages = 0
  total_weight = 0
  do
    read(10, '(a)', iostat=ios) line
    if (ios /= 0) exit
    read(line, *) weight
    num_packages = num_packages + 1
    total_weight = total_weight + weight
  end do
  close(10)

  allocate(packages(num_packages))
  open(unit=10, file="input.txt", status="old", action="read")
  do i = 1, num_packages
    read(10, *) packages(i)
  end do
  close(10)

  target_weight = total_weight / 3
  best_qe = huge(best_qe)
  best_length = huge(best_length)

  do i = 1, 2**num_packages - 1
    group_weight = 0
    qe = 1
    group_length = 0
    do j = 1, num_packages
      if (btest(i-1, j-1)) then
        group_weight = group_weight + packages(j)
        qe = qe * packages(j)
        group_length = group_length + 1
      end if
    end do
    if (group_weight == target_weight .and. group_length <= best_length) then
      if (group_length < best_length .or. qe < best_qe) then
        best_length = group_length
        best_qe = qe
      end if
    end if
  end do

  print *, best_qe

  deallocate(packages)
end program santa
