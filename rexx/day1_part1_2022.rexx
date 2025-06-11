
/* Rexx */
main:
  maxCalories = 0
  currentCalories = 0
  fileName = 'input.txt'

  do while lines(fileName) > 0
    line = linein(fileName)
    if line = '' then do
      maxCalories = max(maxCalories, currentCalories)
      currentCalories = 0
    end
    else
      currentCalories = currentCalories + line
  end

  maxCalories = max(maxCalories, currentCalories)
  say maxCalories
return
