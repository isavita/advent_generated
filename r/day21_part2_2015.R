parse_stat <- function(line) {
  value <- as.integer(gsub(".*: ", "", line))
  return(value)
}

player_wins <- function(player, boss) {
  player_damage <- pmax(1, player$damage - boss$armor)
  boss_damage <- pmax(1, boss$damage - player$armor)

  player_turns <- (boss$hit_points + player_damage - 1) %/% player_damage
  boss_turns <- (player$hit_points + boss_damage - 1) %/% boss_damage

  return(player_turns <= boss_turns)
}

input <- readLines("input.txt")

# Ensure the last line ends with a newline character
if (nchar(input[length(input)]) > 0) {
  input[length(input)] <- paste0(input[length(input)], "\n")
}

boss <- list(
  hit_points = parse_stat(input[1]),
  damage = parse_stat(input[2]),
  armor = parse_stat(input[3])
)

weapons <- list(
  list(cost = 8, damage = 4),
  list(cost = 10, damage = 5),
  list(cost = 25, damage = 6),
  list(cost = 40, damage = 7),
  list(cost = 74, damage = 8)
)

armors <- list(
  list(cost = 0, armor = 0),
  list(cost = 13, armor = 1),
  list(cost = 31, armor = 2),
  list(cost = 53, armor = 3),
  list(cost = 75, armor = 4),
  list(cost = 102, armor = 5)
)

rings <- list(
  list(cost = 0),
  list(cost = 25, damage = 1),
  list(cost = 50, damage = 2),
  list(cost = 100, damage = 3),
  list(cost = 20, armor = 1),
  list(cost = 40, armor = 2),
  list(cost = 80, armor = 3)
)

max_cost <- 0

for (w in 1:length(weapons)) {
  for (a in 1:length(armors)) {
    for (ri in 1:(length(rings) - 1)) {
      for (rj in (ri + 1):length(rings)) {
        player <- list(
          hit_points = 100,
          damage = weapons[[w]]$damage,
          armor = armors[[a]]$armor
        )

        # Handle cases where damage or armor is missing from a ring
        player$damage <- player$damage + ifelse(is.null(rings[[ri]]$damage), 0, rings[[ri]]$damage) + ifelse(is.null(rings[[rj]]$damage), 0, rings[[rj]]$damage)
        player$armor <- player$armor + ifelse(is.null(rings[[ri]]$armor), 0, rings[[ri]]$armor) + ifelse(is.null(rings[[rj]]$armor), 0, rings[[rj]]$armor)

        cost <- weapons[[w]]$cost + armors[[a]]$cost + rings[[ri]]$cost + rings[[rj]]$cost
        if (!player_wins(player, boss) & cost > max_cost) {
          max_cost <- cost
        }
      }
    }
  }
}

cat(max_cost)