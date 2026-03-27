options(warn = -1)

input <- readLines("input.txt", warn = FALSE)
while (length(input) > 0 && nchar(input[length(input)]) == 0) {
    input <- input[-length(input)]
}

h <- length(input)
w <- max(nchar(input))
grid <- matrix(" ", h, w)
for (i in 1:h) {
    chars <- strsplit(input[i], "")[[1]]
    if (length(chars) > 0) {
        grid[i, 1:length(chars)] <- chars
    }
}

portals_raw <- list()
for (r in 1:h) {
    for (c in 1:w) {
        if (grid[r, c] >= "A" && grid[r, c] <= "Z") {
            if (c < w && grid[r, c + 1] >= "A" && grid[r, c + 1] <= "Z") {
                label <- paste0(grid[r, c], grid[r, c + 1])
                dot_pos <- NULL
                if (c > 1 && grid[r, c - 1] == ".") {
                    dot_pos <- c(r, c - 1)
                } else if (c + 2 <= w && grid[r, c + 2] == ".") {
                    dot_pos <- c(r, c + 2)
                }
                if (!is.null(dot_pos)) {
                    portals_raw[[length(portals_raw) + 1]] <- list(label = label, pos = dot_pos)
                }
            }
            if (r < h && grid[r + 1, c] >= "A" && grid[r + 1, c] <= "Z") {
                label <- paste0(grid[r, c], grid[r + 1, c])
                dot_pos <- NULL
                if (r > 1 && grid[r - 1, c] == ".") {
                    dot_pos <- c(r - 1, c)
                } else if (r + 2 <= h && grid[r + 2, c] == ".") {
                    dot_pos <- c(r + 2, c)
                }
                if (!is.null(dot_pos)) {
                    portals_raw[[length(portals_raw) + 1]] <- list(label = label, pos = dot_pos)
                }
            }
        }
    }
}

label_groups <- list()
for (p in portals_raw) {
    label_groups[[p$label]] <- rbind(label_groups[[p$label]], p$pos)
}

start_pos <- label_groups[["AA"]][1, ]
end_pos <- label_groups[["ZZ"]][1, ]

is_outer <- function(r, c, h, w) {
    r <= 3 || r >= h - 2 || c <= 3 || c >= w - 2
}

portal_info <- array(0, dim = c(h, w, 3))
for (lbl in names(label_groups)) {
    if (lbl == "AA" || lbl == "ZZ") next
    pts <- label_groups[[lbl]]
    p1 <- pts[1, ]
    p2 <- pts[2, ]
    d1 <- if (is_outer(p1[1], p1[2], h, w)) -1 else 1
    portal_info[p1[1], p1[2], ] <- c(p2[1], p2[2], d1)
    d2 <- if (is_outer(p2[1], p2[2], h, w)) -1 else 1
    portal_info[p2[1], p2[2], ] <- c(p1[1], p1[2], d2)
}

MAX_LEVEL <- 100
visited <- array(FALSE, dim = c(h, w, MAX_LEVEL))
q_size <- 1000000
q_r <- integer(q_size)
q_c <- integer(q_size)
q_l <- integer(q_size)
q_s <- integer(q_size)

q_r[1] <- start_pos[1]
q_c[1] <- start_pos[2]
q_l[1] <- 1
q_s[1] <- 0
visited[start_pos[1], start_pos[2], 1] <- TRUE
head <- 1
tail <- 2

dr <- c(0, 0, 1, -1)
dc <- c(1, -1, 0, 0)
ans <- -1

while (head < tail) {
    r <- q_r[head]
    c <- q_c[head]
    l <- q_l[head]
    s <- q_s[head]
    head <- head + 1
    
    if (r == end_pos[1] && c == end_pos[2] && l == 1) {
        ans <- s
        break
    }
    
    for (i in 1:4) {
        nr <- r + dr[i]
        nc <- c + dc[i]
        if (nr >= 1 && nr <= h && nc >= 1 && nc <= w && grid[nr, nc] == ".") {
            if (!visited[nr, nc, l]) {
                visited[nr, nc, l] <- TRUE
                q_r[tail] <- nr
                q_c[tail] <- nc
                q_l[tail] <- l
                q_s[tail] <- s + 1
                tail <- tail + 1
            }
        }
    }
    
    if (portal_info[r, c, 1] != 0) {
        nl <- l + portal_info[r, c, 3]
        if (nl >= 1 && nl <= MAX_LEVEL) {
            nr <- portal_info[r, c, 1]
            nc <- portal_info[r, c, 2]
            if (!visited[nr, nc, nl]) {
                visited[nr, nc, nl] <- TRUE
                q_r[tail] <- nr
                q_c[tail] <- nc
                q_l[tail] <- nl
                q_s[tail] <- s + 1
                tail <- tail + 1
            }
        }
    }
}

cat(ans, "\n")