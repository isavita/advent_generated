
/* REXX */
call main

main:
  expenses.0 = 0
  filename = 'input.txt'
  do i = 1 while lines(filename) > 0
    expenses.i = linein(filename)
    expenses.0 = i
  end
  close(filename)

  do i = 1 to expenses.0
    do j = i + 1 to expenses.0
      do k = j + 1 to expenses.0
        if expenses.i + expenses.j + expenses.k = 2020 then do
          say expenses.i * expenses.j * expenses.k
          exit
        end
      end
    end
  end
return
