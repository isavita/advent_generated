
input <- readLines("input.txt")
input <- input[input != ""]  # Remove empty lines

boss_hp <- as.numeric(sub("Hit Points: (\\d+)", "\\1", input[1]))
boss_damage <- as.numeric(sub("Damage: (\\d+)", "\\1", input[2]))
boss_armor <- as.numeric(sub("Armor: (\\d+)", "\\1", input[3]))

weapons <- data.frame(
  Name = c("Dagger", "Shortsword", "Warhammer", "Longsword", "Greataxe"),
  Cost = c(8, 10, 25, 40, 74),
  Damage = c(4, 5, 6, 7, 8),
  Armor = c(0, 0, 0, 0, 0)
)

armor <- data.frame(
  Name = c("None", "Leather", "Chainmail", "Splintmail", "Bandedmail", "Platemail"),
  Cost = c(0, 13, 31, 53, 75, 102),
  Damage = c(0, 0, 0, 0, 0, 0),
  Armor = c(0, 1, 2, 3, 4, 5)
)

rings <- data.frame(
  Name = c("None", "Damage +1", "Damage +2", "Damage +3", "Defense +1", "Defense +2", "Defense +3"),
  Cost = c(0, 25, 50, 100, 20, 40, 80),
  Damage = c(0, 1, 2, 3, 0, 0, 0),
  Armor = c(0, 0, 0, 0, 1, 2, 3)
)

min_gold <- Inf

for (i in 1:nrow(weapons)) {
  for (j in 1:(nrow(armor) + 1)) {
    for (k in 1:(nrow(rings) + 1)) {
      for (l in (k + 1):(nrow(rings) + 1)) {
        player_damage <- weapons$Damage[i] + armor$Damage[j] + rings$Damage[k] + rings$Damage[l]
        player_armor <- weapons$Armor[i] + armor$Armor[j] + rings$Armor[k] + rings$Armor[l]
        player_cost <- weapons$Cost[i] + armor$Cost[j] + rings$Cost[k] + rings$Cost[l]
        
        if (!anyNA(c(player_damage, player_armor, player_cost))) {  # Check for missing values
          player_turns_to_win <- ceiling(boss_hp / max(1, player_damage - boss_armor))
          boss_turns_to_win <- ceiling(100 / max(1, boss_damage - player_armor))
          
          if (player_turns_to_win <= boss_turns_to_win && player_cost < min_gold) {
            min_gold <- player_cost
          }
        }
      }
    }
  }
}

print(min_gold)
