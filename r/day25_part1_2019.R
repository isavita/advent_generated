
options(scipen = 999)

code <- as.numeric(strsplit(readLines("input.txt", warn = FALSE), ",")[[1]])
mem <- as.environment(as.list(setNames(code, seq_along(code) - 1)))

get_mem <- function(addr) {
  val <- mem[[as.character(addr)]]
  if (is.null(val)) 0 else val
}

set_mem <- function(addr, val) {
  mem[[as.character(addr)]] <- val
}

ip <- 0
rb <- 0
inputs <- numeric(0)

emulate <- function() {
  while (TRUE) {
    inst <- get_mem(ip)
    op <- inst %% 100
    mode <- function(n) (inst %/% (10^(n + 1))) %% 10
    
    get_p <- function(n) {
      m <- mode(n)
      v <- get_mem(ip + n)
      if (m == 0) get_mem(v) else if (m == 1) v else get_mem(rb + v)
    }
    
    get_a <- function(n) {
      m <- mode(n)
      v <- get_mem(ip + n)
      if (m == 0) v else rb + v
    }
    
    if (op == 1) {
      set_mem(get_a(3), get_p(1) + get_p(2))
      ip <<- ip + 4
    } else if (op == 2) {
      set_mem(get_a(3), get_p(1) * get_p(2))
      ip <<- ip + 4
    } else if (op == 3) {
      if (length(inputs) == 0) return(list(type = "WAIT"))
      set_mem(get_a(1), inputs[1])
      inputs <<- inputs[-1]
      ip <<- ip + 2
    } else if (op == 4) {
      out <- get_p(1)
      ip <<- ip + 2
      return(list(type = "OUT", val = out))
    } else if (op == 5) {
      if (get_p(1) != 0) ip <<- get_p(2) else ip <<- ip + 3
    } else if (op == 6) {
      if (get_p(1) == 0) ip <<- get_p(2) else ip <<- ip + 3
    } else if (op == 7) {
      set_mem(get_a(3), if (get_p(1) < get_p(2)) 1 else 0)
      ip <<- ip + 4
    } else if (op == 8) {
      set_mem(get_a(3), if (get_p(1) == get_p(2)) 1 else 0)
      ip <<- ip + 4
    } else if (op == 9) {
      rb <<- rb + get_p(1)
      ip <<- ip + 2
    } else if (op == 99) {
      return(list(type = "HALT"))
    }
  }
}

send <- function(s) inputs <<- c(inputs, utf8ToInt(paste0(s, "\n")))

opposite <- c(north = "south", south = "north", west = "east", east = "west")
blacklist <- c("photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet")

world <- list()
inv <- character(0)
mode <- "EXPLORE"
path <- character(0)
checkpoint <- NULL
floor_room <- NULL
test_dir <- ""
available <- character(0)
mask <- 0
curr_room_name <- ""
last_room_name <- ""
last_dir <- ""
out_buf <- ""

while (TRUE) {
  res <- emulate()
  if (res$type == "OUT") {
    out_buf <- paste0(out_buf, intToUtf8(res$val))
    next
  }
  
  if (res$type == "HALT") {
    cat(sub(".*typing ([0-9]+) on the keypad.*", "\\1", gsub("\n", " ", out_buf)), "\n")
    break
  }
  
  lines <- strsplit(out_buf, "\n")[[1]]
  out_buf <- ""
  
  room_match <- grep("^== (.+) ==$", lines, value = TRUE)
  if (length(room_match) > 0) {
    curr_room_name <- sub("^== (.+) ==$", "\\1", room_match[length(room_match)])
    if (is.null(world[[curr_room_name]])) {
      world[[curr_room_name]] <- list(name = curr_room_name, conns = list(), items = character(0))
    }
  }
  
  items_idx <- which(lines == "Items here:")
  if (length(items_idx) > 0) {
    i <- items_idx[length(items_idx)] + 1
    while (i <= length(lines) && grepl("^- ", lines[i])) {
      item <- sub("^- ", "", lines[i])
      world[[curr_room_name]]$items <- unique(c(world[[curr_room_name]]$items, item))
      i <- i + 1
    }
  }
  
  doors_idx <- which(lines == "Doors here lead:")
  if (length(doors_idx) > 0) {
    i <- doors_idx[length(doors_idx)] + 1
    while (i <= length(lines) && grepl("^- ", lines[i])) {
      d <- sub("^- ", "", lines[i])
      if (is.null(world[[curr_room_name]]$conns[[d]])) world[[curr_room_name]]$conns[[d]] <- NA
      i <- i + 1
    }
  }
  
  if (any(grepl("Alert!", lines))) {
    if (mode == "EXPLORE") {
      checkpoint <- last_room_name
      floor_room <- curr_room_name
      test_dir <- last_dir
      world[[checkpoint]]$conns[[test_dir]] <- floor_room
      path <- path[-length(path)]
    }
    curr_room_name <- last_room_name
  } else if (last_dir != "" && last_room_name != "") {
    world[[last_room_name]]$conns[[last_dir]] <- curr_room_name
    world[[curr_room_name]]$conns[[opposite[last_dir]]] <- last_room_name
  }
  
  if (mode == "EXPLORE") {
    found_item <- FALSE
    for (itm in world[[curr_room_name]]$items) {
      if (!(itm %in% blacklist)) {
        inv <- c(inv, itm)
        world[[curr_room_name]]$items <- setdiff(world[[curr_room_name]]$items, itm)
        send(paste("take", itm))
        found_item <- TRUE
        break
      }
    }
    if (found_item) next
    
    target_dir <- ""
    for (d in names(world[[curr_room_name]]$conns)) {
      if (is.na(world[[curr_room_name]]$conns[[d]])) {
        target_dir <- d
        path <- c(path, curr_room_name)
        break
      }
    }
    
    if (target_dir != "") {
      last_dir <- target_dir
      last_room_name <- curr_room_name
      send(target_dir)
    } else if (length(path) > 0) {
      prev <- path[length(path)]
      path <- path[-length(path)]
      for (d in names(world[[curr_room_name]]$conns)) {
        if (!is.na(world[[curr_room_name]]$conns[[d]]) && world[[curr_room_name]]$conns[[d]] == prev) {
          last_dir <- d
          last_room_name <- curr_room_name
          send(d)
          break
        }
      }
    } else {
      mode <- "NAVIGATE"
      q <- list(list(curr_room_name, character(0)))
      v <- curr_room_name
      while (length(q) > 0) {
        curr <- q[[1]]
        q <- q[-1]
        if (curr[[1]] == checkpoint) {
          path <- curr[[2]]
          break
        }
        for (d in names(world[[curr[[1]]]]$conns)) {
          neighbor <- world[[curr[[1]]]]$conns[[d]]
          if (!is.na(neighbor) && !(neighbor %in% v)) {
            v <- c(v, neighbor)
            q[[length(q) + 1]] <- list(neighbor, c(curr[[2]], d))
          }
        }
      }
    }
  }
  
  if (mode == "NAVIGATE") {
    if (length(path) > 0) {
      last_dir <- path[1]
      path <- path[-1]
      last_room_name <- curr_room_name
      send(last_dir)
    } else {
      mode <- "TEST"
      available <- inv
      current_inv_state <- rep(TRUE, length(available))
      mask <- 0
    }
  }
  
  if (mode == "TEST") {
    target_state <- as.logical(bitwAnd(mask, bitwShiftL(1, seq_along(available) - 1)))
    diffs <- which(current_inv_state != target_state)
    if (length(diffs) > 0) {
      idx <- diffs[1]
      action <- if (target_state[idx]) "take" else "drop"
      current_inv_state[idx] <- target_state[idx]
      send(paste(action, available[idx]))
    } else {
      mask <- mask + 1
      send(test_dir)
    }
  }
}
