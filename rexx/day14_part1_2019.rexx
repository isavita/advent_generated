
/* REXX */
call main
exit

main:
  reactions. = ''
  ingredients. = ''
  surplus. = 0
  fn = 'input.txt'

  do while lines(fn) > 0
    line = linein(fn)
    parse var line inputs_str ' => ' output_str
    parse var output_str out_amount out_name

    reactions.out_name = out_amount
    inputs_str = changestr(',', inputs_str, ' ')
    i = 0
    do while inputs_str <> ''
      i = i + 1
      parse var inputs_str in_amount in_name inputs_str
      ingredients.out_name.i.amount = in_amount
      ingredients.out_name.i.name = in_name
    end
    ingredients.out_name.0 = i
  end
  call stream fn, 'c', 'close'

  say calculateOre('FUEL', 1)
return

calculateOre:
  procedure expose reactions. ingredients. surplus.
  arg chem, amount_needed

  if chem = 'ORE' then return amount_needed

  if surplus.chem >= amount_needed then do
    surplus.chem = surplus.chem - amount_needed
    return 0
  end

  amount_needed = amount_needed - surplus.chem
  surplus.chem = 0

  reaction_amount = reactions.chem
  times = (amount_needed + reaction_amount - 1) % reaction_amount
  total_ore = 0

  do i = 1 to ingredients.chem.0
    ing_name = ingredients.chem.i.name
    ing_amount = ingredients.chem.i.amount
    required = ing_amount * times
    total_ore = total_ore + calculateOre(ing_name, required)
  end

  surplus.chem = surplus.chem + (times * reaction_amount) - amount_needed
return total_ore
