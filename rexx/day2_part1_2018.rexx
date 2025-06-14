
main:
  countTwo = 0
  countThree = 0
  fileName = 'input.txt'
  alphabet = 'abcdefghijklmnopqrstuvwxyz'

  do while lines(fileName) > 0
    boxID = linein(fileName)
    charCount. = 0
    hasTwo = 0
    hasThree = 0

    do i = 1 to length(boxID)
      c = substr(boxID, i, 1)
      charCount.c = charCount.c + 1
    end

    do i = 1 to length(alphabet)
      c = substr(alphabet, i, 1)
      select
        when charCount.c = 2 then hasTwo = 1
        when charCount.c = 3 then hasThree = 1
        otherwise nop
      end
    end

    countTwo = countTwo + hasTwo
    countThree = countThree + hasThree
  end

  say countTwo * countThree
return
