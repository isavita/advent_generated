
program solve_grid
  implicit none
  integer, parameter :: max_rows = 1000
  integer, parameter :: max_cols = 1000
  character(len=max_cols) :: grid(max_rows)
  logical :: visited(max_rows, max_cols) = .false.
  integer :: rows, cols, total_price, r, c, area, perimeter
  character :: char
  integer :: queue(max_rows * max_cols, 2), head, tail

  open(unit=10, file="input.txt", status="old")
  rows = 0
  do
    rows = rows + 1
    read(10, "(A)", end=100) grid(rows)
  end do
  100 continue
  rows = rows - 1
  cols = len_trim(grid(1))
  close(10)

  total_price = 0
  do r = 1, rows
    do c = 1, cols
      if (.not. visited(r, c)) then
        area = 0
        perimeter = 0
        head = 1
        tail = 0
        
        char = grid(r)(c:c)
        tail = tail + 1
        queue(tail, :) = [r, c]
        visited(r, c) = .true.

        do while (head <= tail)
          area = area + 1
          
          if (queue(head, 1) == 1 .or. queue(head, 1) == rows .or. &
              queue(head, 2) == 1 .or. queue(head, 2) == cols) then
            
            if (queue(head, 1) > 1) then
              if (grid(queue(head, 1) - 1)(queue(head, 2):queue(head, 2)) /= char) then
                perimeter = perimeter + 1
              else if (.not. visited(queue(head, 1) - 1, queue(head, 2))) then
                tail = tail + 1
                queue(tail, :) = [queue(head, 1) - 1, queue(head, 2)]
                visited(queue(head, 1) - 1, queue(head, 2)) = .true.
              end if
            else
              perimeter = perimeter + 1
            end if
            
            if (queue(head, 1) < rows) then
              if (grid(queue(head, 1) + 1)(queue(head, 2):queue(head, 2)) /= char) then
                perimeter = perimeter + 1
              else if (.not. visited(queue(head, 1) + 1, queue(head, 2))) then
                tail = tail + 1
                queue(tail, :) = [queue(head, 1) + 1, queue(head, 2)]
                visited(queue(head, 1) + 1, queue(head, 2)) = .true.
              end if
            else
              perimeter = perimeter + 1
            end if
            
            if (queue(head, 2) > 1) then
              if (grid(queue(head, 1))(queue(head, 2) - 1:queue(head, 2) - 1) /= char) then
                perimeter = perimeter + 1
              else if (.not. visited(queue(head, 1), queue(head, 2) - 1)) then
                tail = tail + 1
                queue(tail, :) = [queue(head, 1), queue(head, 2) - 1]
                visited(queue(head, 1), queue(head, 2) - 1) = .true.
              end if
            else
              perimeter = perimeter + 1
            end if
            
            if (queue(head, 2) < cols) then
              if (grid(queue(head, 1))(queue(head, 2) + 1:queue(head, 2) + 1) /= char) then
                perimeter = perimeter + 1
              else if (.not. visited(queue(head, 1), queue(head, 2) + 1)) then
                tail = tail + 1
                queue(tail, :) = [queue(head, 1), queue(head, 2) + 1]
                visited(queue(head, 1), queue(head, 2) + 1) = .true.
              end if
            else
              perimeter = perimeter + 1
            end if
          else
            if (queue(head, 1) > 1) then
              if (grid(queue(head, 1) - 1)(queue(head, 2):queue(head, 2)) /= char) then
                perimeter = perimeter + 1
              else if (.not. visited(queue(head, 1) - 1, queue(head, 2))) then
                tail = tail + 1
                queue(tail, :) = [queue(head, 1) - 1, queue(head, 2)]
                visited(queue(head, 1) - 1, queue(head, 2)) = .true.
              end if
            end if
            
            if (queue(head, 1) < rows) then
              if (grid(queue(head, 1) + 1)(queue(head, 2):queue(head, 2)) /= char) then
                perimeter = perimeter + 1
              else if (.not. visited(queue(head, 1) + 1, queue(head, 2))) then
                tail = tail + 1
                queue(tail, :) = [queue(head, 1) + 1, queue(head, 2)]
                visited(queue(head, 1) + 1, queue(head, 2)) = .true.
              end if
            end if
            
            if (queue(head, 2) > 1) then
              if (grid(queue(head, 1))(queue(head, 2) - 1:queue(head, 2) - 1) /= char) then
                perimeter = perimeter + 1
              else if (.not. visited(queue(head, 1), queue(head, 2) - 1)) then
                tail = tail + 1
                queue(tail, :) = [queue(head, 1), queue(head, 2) - 1]
                visited(queue(head, 1), queue(head, 2) - 1) = .true.
              end if
            end if
            
            if (queue(head, 2) < cols) then
              if (grid(queue(head, 1))(queue(head, 2) + 1:queue(head, 2) + 1) /= char) then
                perimeter = perimeter + 1
              else if (.not. visited(queue(head, 1), queue(head, 2) + 1)) then
                tail = tail + 1
                queue(tail, :) = [queue(head, 1), queue(head, 2) + 1]
                visited(queue(head, 1), queue(head, 2) + 1) = .true.
              end if
            end if
          end if
          head = head + 1
        end do
        total_price = total_price + area * perimeter
      end if
    end do
  end do

  print *, total_price
end program solve_grid
