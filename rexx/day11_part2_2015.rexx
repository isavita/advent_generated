
/* Rexx */
main:
  currentPassword = linein('input.txt')
  firstNewPassword = findNextPassword(currentPassword)
  secondNewPassword = findNextPassword(firstNewPassword)
  say secondNewPassword
exit

findNextPassword: procedure
  parse arg p
  do forever
    p = increment(p)
    if isValidPassword(p) then leave
  end
  return p

increment: procedure
  parse arg p
  len = length(p)
  do i = len to 1 by -1
    char = substr(p, i, 1)
    if char == 'z' then
      p = overlay('a', p, i)
    else do
      new_char = d2c(c2d(char) + 1)
      p = overlay(new_char, p, i)
      leave
    end
  end
  return p

isValidPassword: procedure
  parse arg p
  return hasStraight(p) & hasNoInvalidLetters(p) & hasTwoPairs(p)

hasStraight: procedure
  parse arg p
  len = length(p)
  do i = 1 to len - 2
    c1 = c2d(substr(p, i, 1))
    c2 = c2d(substr(p, i + 1, 1))
    c3 = c2d(substr(p, i + 2, 1))
    if c1 + 1 == c2 & c2 + 1 == c3 then return 1
  end
  return 0

hasNoInvalidLetters: procedure
  parse arg p
  return verify(p, 'abcdefghjkmnpqrstuvwxyz') == 0

hasTwoPairs: procedure
  parse arg p
  len = length(p)
  pairs = 0
  i = 1
  do while i < len
    if substr(p, i, 1) == substr(p, i + 1, 1) then do
      pairs = pairs + 1
      i = i + 2
    end; else
      i = i + 1
  end
  return pairs >= 2
