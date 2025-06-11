
main:
    validPassports = 0
    passport = ''
    fileName = 'input.txt'

    do while lines(fileName) > 0
        line = linein(fileName)
        if line = '' then do
            if isValid(passport) then
                validPassports = validPassports + 1
            passport = ''
        end
        else
            passport = space(passport line)
    end

    if passport \= '' then
        if isValid(passport) then
            validPassports = validPassports + 1

    say validPassports
exit

isValid:
    procedure
    parse arg passport
    required = 'byr: iyr: eyr: hgt: hcl: ecl: pid:'
    do i = 1 to words(required)
        if pos(word(required, i), passport) = 0 then
            return 0
    end
    return 1
