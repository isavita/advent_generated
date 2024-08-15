require "file_utils"

class AllergenFinder
  def initialize
    @allergen_map = Hash(String, Hash(String, Bool)).new
    @ingredient_allergen = Hash(String, String).new
  end

  def find_allergens
    File.open("input.txt", "r") do |file|
      file.each_line do |line|
        parts = line.split(" (contains ")
        ingredients = parts[0].split
        allergens = parts[1]? ? parts[1][0...-1].split(", ") : Array(String).new

        allergens.each do |allergen|
          if !@allergen_map.has_key?(allergen)
            @allergen_map[allergen] = Hash(String, Bool).new
            ingredients.each { |ingredient| @allergen_map[allergen][ingredient] = true }
          else
            @allergen_map[allergen].each_key do |ingredient|
              ingredients.includes?(ingredient) || @allergen_map[allergen].delete(ingredient)
            end
          end
        end
      end
    end

    while !@allergen_map.empty?
      @allergen_map.each do |allergen, ingredients|
        if ingredients.size == 1
          @ingredient_allergen[allergen] = ingredients.keys.first
          remove_ingredient_from_all(allergen, @ingredient_allergen[allergen])
          @allergen_map.delete(allergen)
        end
      end
    end

    allergens = @ingredient_allergen.keys.sort
    result = allergens.map { |allergen| @ingredient_allergen[allergen] }
    puts result.join(",")
  end

  def remove_ingredient_from_all(allergen, ingredient)
    @allergen_map.each_value { |ingredients| ingredients.delete(ingredient) }
  end
end

finder = AllergenFinder.new
finder.find_allergens