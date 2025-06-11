
call main

main:
  seen. = ''
  file = 'input.txt'
  do while lines(file) > 0
    n = linein(file)
    if n = '' then iterate

    complement = 2020 - n
    if seen.complement <> '' then do
      say n * complement
      exit
    end
    seen.n = 1
  end
return
