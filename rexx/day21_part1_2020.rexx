
/* REXX */
main:
  call stream 'input.txt', 'c', 'open read'
  ingredient_counts. = 0
  allergen_map. = ''
  all_allergens = ''
  all_ingredients = ''

  do while lines('input.txt') > 0
    line = linein('input.txt')
    parse var line line_ings '(contains' line_algs ')'
    line_ings = space(strip(line_ings), 1)
    line_algs = space(translate(strip(line_algs), ' ', ','), 1)

    do i = 1 to words(line_ings)
      ing = word(line_ings, i)
      ingredient_counts.ing = ingredient_counts.ing + 1
      if wordpos(ing, all_ingredients) = 0 then
        all_ingredients = all_ingredients ing
    end

    do i = 1 to words(line_algs)
      alg = word(line_algs, i)
      if wordpos(alg, all_allergens) = 0 then do
        all_allergens = all_allergens alg
        allergen_map.alg = line_ings
      end; else do
        old_candidates = allergen_map.alg
        new_candidates = ''
        do j = 1 to words(old_candidates)
          cand_ing = word(old_candidates, j)
          if wordpos(cand_ing, line_ings) > 0 then
            new_candidates = new_candidates cand_ing
        end
        allergen_map.alg = strip(new_candidates)
      end
    end
  end
  call stream 'input.txt', 'c', 'close'

  potentially_allergenic = ''
  all_allergens = space(all_allergens, 1)
  do i = 1 to words(all_allergens)
    alg = word(all_allergens, i)
    candidates = allergen_map.alg
    do j = 1 to words(candidates)
      ing = word(candidates, j)
      if wordpos(ing, potentially_allergenic) = 0 then
        potentially_allergenic = potentially_allergenic ing
    end
  end

  safe_count = 0
  all_ingredients = space(all_ingredients, 1)
  do i = 1 to words(all_ingredients)
    ing = word(all_ingredients, i)
    if wordpos(ing, potentially_allergenic) = 0 then
      safe_count = safe_count + ingredient_counts.ing
  end

  say safe_count
return
