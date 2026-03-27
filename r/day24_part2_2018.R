
parse_input <- function() {
  lines <- readLines("input.txt")
  groups <- list()
  side <- ""
  id_count <- 1
  for (line in lines) {
    if (grepl("Immune System:", line)) { side <- "Immune"; next }
    if (grepl("Infection:", line)) { side <- "Infection"; next }
    if (!grepl("units", line)) next
    
    units <- as.numeric(sub(" units.*", "", line))
    hp <- as.numeric(sub(".*each with (\\d+) hit points.*", "\\1", line))
    init <- as.numeric(sub(".*at initiative (\\d+).*", "\\1", line))
    atk_val <- as.numeric(sub(".*does (\\d+) .*", "\\1", line))
    atk_type <- sub(".*does \\d+ (\\w+) damage.*", "\\1", line)
    
    imm <- character(0)
    weak <- character(0)
    if (grepl("\\(", line)) {
      aff <- sub(".*\\((.*)\\).*", "\\1", line)
      parts <- strsplit(aff, "; ")[[1]]
      for (p in parts) {
        types <- strsplit(sub("(weak|immune) to ", "", p), ", ")[[1]]
        if (grepl("immune", p)) imm <- types else weak <- types
      }
    }
    groups[[length(groups) + 1]] <- list(side=side, units=units, hp=hp, atk=atk_val, type=atk_type, 
                                        init=init, imm=imm, weak=weak, id=id_count)
    id_count <- id_count + 1
  }
  groups
}

calc_dmg <- function(a, d) {
  if (a$type %in% d$imm) return(0)
  p <- a$units * a$atk
  if (a$type %in% d$weak) p * 2 else p
}

simulate <- function(groups, boost) {
  for (i in seq_along(groups)) if (groups[[i]]$side == "Immune") groups[[i]]$atk <- groups[[i]]$atk + boost
  
  while (length(unique(sapply(groups, `[[`, "side"))) > 1) {
    groups <- groups[sapply(groups, function(g) g$units > 0)]
    ord <- order(sapply(groups, function(g) g$units * g$atk), sapply(groups, `[[`, "init"), decreasing=TRUE)
    
    targeted <- integer(0)
    targets <- rep(NA, length(groups))
    
    for (i in ord) {
      atk <- groups[[i]]
      best_d <- NA
      max_dmg <- 0
      
      for (j in seq_along(groups)) {
        def <- groups[[j]]
        if (atk$side == def$side || j %in% targeted) next
        dmg <- calc_dmg(atk, def)
        if (dmg == 0) next
        if (dmg > max_dmg) {
          max_dmg <- dmg; best_d <- j
        } else if (dmg == max_dmg && max_dmg > 0) {
          if (groups[[j]]$units * groups[[j]]$atk > groups[[best_d]]$units * groups[[best_d]]$atk) {
            best_d <- j
          } else if (groups[[j]]$units * groups[[j]]$atk == groups[[best_d]]$units * groups[[best_d]]$atk && groups[[j]]$init > groups[[best_d]]$init) {
            best_d <- j
          }
        }
      }
      if (!is.na(best_d)) { targets[i] <- best_d; targeted <- c(targeted, best_d) }
    }
    
    atk_ord <- order(sapply(groups, `[[`, "init"), decreasing=TRUE)
    total_killed <- 0
    for (i in atk_ord) {
      if (groups[[i]]$units <= 0 || is.na(targets[i])) next
      t_idx <- targets[i]
      dmg <- calc_dmg(groups[[i]], groups[[t_idx]])
      killed <- min(groups[[t_idx]]$units, dmg %/% groups[[t_idx]]$hp)
      groups[[t_idx]]$units <- groups[[t_idx]]$units - killed
      total_killed <- total_killed + killed
    }
    if (total_killed == 0) return(list(side="Stalemate", units=0))
    groups <- groups[sapply(groups, function(g) g$units > 0)]
  }
  list(side=groups[[1]]$side, units=sum(sapply(groups, `[[`, "units")))
}

groups <- parse_input()
boost <- 0
while (TRUE) {
  res <- simulate(groups, boost)
  if (res$side == "Immune") {
    cat(res$units, "\n")
    break
  }
  boost <- boost + 1
}
