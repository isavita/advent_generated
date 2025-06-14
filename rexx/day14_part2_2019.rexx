
/* REXX */
call main
exit

main:
    call parse_input
    ore_available = 1000000000000

    surplus. = 0
    ore_for_one = calculate_ore('FUEL', 1)

    low = 1
    high = (ore_available % ore_for_one) * 2
    max_fuel = 0

    do while low <= high
        mid = low + (high - low) % 2
        if mid = 0 then do
            low = 1
            iterate
        end

        surplus. = 0
        ore_needed = calculate_ore('FUEL', mid)

        if ore_needed <= ore_available then do
            max_fuel = mid
            low = mid + 1
        end
        else do
            high = mid - 1
        end
    end

    say max_fuel
return

calculate_ore: procedure expose reaction. surplus.
    parse arg chem_name, amount_needed
    numeric digits 32

    if chem_name = 'ORE' then return amount_needed

    have = surplus.chem_name
    if have >= amount_needed then do
        surplus.chem_name = have - amount_needed
        return 0
    end

    if have > 0 then do
        amount_needed = amount_needed - have
        surplus.chem_name = 0
    end

    r_output_amount = reaction.chem_name.output.amount
    times = (amount_needed + r_output_amount - 1) % r_output_amount

    ore_total = 0
    do i = 1 to reaction.chem_name.input.0
        input_name = reaction.chem_name.input.i.name
        input_amount = reaction.chem_name.input.i.amount
        ore_total = ore_total + calculate_ore(input_name, input_amount * times)
    end

    produced = times * r_output_amount
    leftover = produced - amount_needed
    surplus.chem_name = surplus.chem_name + leftover

return ore_total

parse_input:
    reaction. = ''
    surplus. = 0
    fname = 'input.txt'

    do while lines(fname) > 0
        line = linein(fname)
        parse var line inputs_str '=>' output_str

        output_str = strip(output_str)
        parse var output_str out_amount out_name

        reaction.out_name.output.amount = out_amount
        reaction.out_name.output.name = out_name
        reaction.out_name.input.0 = 0

        inputs_str = space(translate(inputs_str, ' ', ','), 1)
        do i = 1 to words(inputs_str) / 2
            in_amount = word(inputs_str, (i-1)*2 + 1)
            in_name = word(inputs_str, (i-1)*2 + 2)
            c = reaction.out_name.input.0 + 1
            reaction.out_name.input.c.amount = in_amount
            reaction.out_name.input.c.name = in_name
            reaction.out_name.input.0 = c
        end
    end
    call stream fname, 'C', 'CLOSE'
return
