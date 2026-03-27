
parse_input <- function(filename) {
  lines <- readLines(filename)
  army <- 0
  groups <- list()
  for (line in lines) {
    if (line == "Immune System:") { army <- 1; next }
    if (line == "Infection:") { army <- 2; next }
    if (line == "" || army == 0) next
    
    parts <- regmatches(line, regexec("(\\d+) units each with (\\d+) hit points (\\((.*)\\) )?with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)", line))[[1]]
    units <- as.integer(parts[2])
    hp <- as.integer(parts[3])
    mods_str <- parts[5]
    dmg <- as.integer(parts[6])
    type <- parts[7]
    init <- as.integer(parts[8])
    
    imm <- character(0)
    weak <- character(0)
    if (nchar(mods_str) > 0) {
      for (m in strsplit(mods_str, "; ")[[1]]) {
        if (startsWith(m, "immune to ")) imm <- strsplit(sub("immune to ", "", m), ", ")[[1]]
        if (startsWith(m, "weak to ")) weak <- strsplit(sub("weak to ", "", m), ", ")[[1]]
      }
    }
    groups[[length(groups) + 1]] <- list(army=army, units=units, hp=hp, dmg=dmg, type=type, init=init, imm=imm, weak=weak, id=length(groups)+1)
  }
  groups
}

calc_damage <- function(att, def) {
  if (att$type %in% def$imm) return(0)
  pwr <- att$units * att$dmg
  if (att$type %in% def$weak) return(pwr * 2)
  pwr
}

solve <- function() {
  groups <- parse_input("input.txt")
  
  while (TRUE) {
    armies <- sapply(groups, function(g) if(g$units > 0) g$army else NA)
    if (length(unique(na.omit(armies))) < 2) break
    
    groups <- groups[order(sapply(groups, function(g) g$units * g$dmg), sapply(groups, function(g) g$init), decreasing=TRUE)]
    targets <- rep(NA, length(groups))
    chosen <- rep(FALSE, length(groups))
    
    for (i in seq_along(groups)) {
      if (groups[[i]]$units <= 0) next
      best_t <- -1
      max_d <- 0
      for (j in seq_along(groups)) {
        if (groups[[j]]$army == groups[[i]]$army || groups[[j]]$units <= 0 || chosen[j]) next
        d <- calc_damage(groups[[i]], groups[[j]])
        if (d == 0) next
        if (d > max_d) {
          max_d <- d; best_t <- j
        } else if (d == max_d && max_d > 0) {
          ep_j <- groups[[j]]$units * groups[[j]]$dmg
          ep_b <- groups[[best_t]]$units * groups[[best_t]]$dmg
          if (ep_j > ep_b || (ep_j == ep_b && groups[[j]]$init > groups[[best_t]]$init)) best_t <- j
        }
      }
      if (best_t != -1) {
        targets[i] <- best_t
        chosen[best_t] <- TRUE
      }
    }
    
    order_idx <- order(sapply(groups, function(g) g$init), decreasing=TRUE)
    total_killed <- 0
    for (idx in order_idx) {
      if (groups[[idx]]$units <= 0 || is.na(targets[idx])) next
      t_idx <- targets[idx]
      dmg <- calc_damage(groups[[idx]], groups[[t_idx]])
      killed <- min(groups[[t_idx]]$units, dmg %/% groups[[t_idx]]$hp)
      groups[[t_idx]]$units <- groups[[t_idx]]$units - killed
      total_killed <- total_killed + killed
    }
    if (total_killed == 0) break
  }
  cat(sum(sapply(groups, function(g) max(0, g$units))), "\n")
}

solve()
