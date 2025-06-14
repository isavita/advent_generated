
/* REXX */
call main
exit

main:
  reg. = 0
  highestValue = 0
  fileName = 'input.txt'

  do while lines(fileName) > 0
    line = linein(fileName)
    parse var line target op amt 'if' condReg condOp condVal

    conditionMet = 0
    select
      when condOp = '>'  then if reg.condReg >  condVal then conditionMet = 1
      when condOp = '>=' then if reg.condReg >= condVal then conditionMet = 1
      when condOp = '<'  then if reg.condReg <  condVal then conditionMet = 1
      when condOp = '<=' then if reg.condReg <= condVal then conditionMet = 1
      when condOp = '==' then if reg.condReg =  condVal then conditionMet = 1
      when condOp = '!=' then if reg.condReg \= condVal then conditionMet = 1
    end

    if conditionMet then do
      if op = 'inc' then
        reg.target = reg.target + amt
      else
        reg.target = reg.target - amt

      highestValue = max(highestValue, reg.target)
    end
  end

  call stream fileName, 'c', 'close'
  say highestValue
return
