
call main
main:
  fileName = 'input.txt'
  dirSize. = 0
  dirList = ''
  isDir. = 0
  path = ''

  do while lines(fileName) > 0
    line = linein(fileName)
    parse var line p1 p2 p3

    if p1 = '$' & p2 = 'cd' then do
      select
        when p3 = '/'  then path = '/'
        when p3 = '..' then path = left(path, lastpos('/', left(path, length(path)-1)))
        otherwise           path = path || p3 || '/'
      end
    end
    else if datatype(p1, 'W') then do
      size = p1
      p = path
      do forever
        if \isDir.p then do
          isDir.p = 1
          dirList = dirList p
        end
        dirSize.p = dirSize.p + size
        if p = '/' then leave
        p = left(p, lastpos('/', left(p, length(p)-1)))
      end
    end
  end
  call stream fileName, 'c', 'close'

  totalSum = 0
  do i = 1 to words(dirList)
    dirName = word(dirList, i)
    if dirSize.dirName <= 100000 then
      totalSum = totalSum + dirSize.dirName
  end

  say totalSum
return
