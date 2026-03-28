
lines <- readLines("input.txt")
m <- do.call(rbind, strsplit(lines, ""))
nr <- nrow(m); nc <- ncol(m); n_rc <- nr * nc
start_pos <- which(m == "^" | m == ">" | m == "v" | m == "<", arr.ind = TRUE)
sr <- start_pos[1,1]; sc <- start_pos[1,2]
sd <- match(m[sr, sc], c("^", ">", "v", "<"))
m[sr, sc] <- "."; m_v <- as.vector(m)
drs <- c(-1, 0, 1, 0); dcs <- c(0, 1, 0, -1)

v_path <- matrix(FALSE, nr, nc)
cr <- sr; cc <- sc; cd <- sd
while (TRUE) {
  v_path[cr, cc] <- TRUE
  tr <- cr + drs[cd]; tc <- cc + dcs[cd]
  if (tr < 1 || tr > nr || tc < 1 || tc > nc) break
  if (m_v[tr + (tc - 1) * nr] == "#") cd <- if (cd == 4) 1 else cd + 1
  else { cr <- tr; cc <- tc }
}
upath <- which(v_path, arr.ind = TRUE)

v <- integer(n_rc * 4); sid <- 0; ans <- 0
for (i in seq_len(nrow(upath))) {
  obs_r <- upath[i, 1]; obs_c <- upath[i, 2]
  if (obs_r == sr && obs_c == sc) next
  sid <- sid + 1; cr <- sr; cc <- sc; cd <- sd
  while (TRUE) {
    idx <- cr + (cc - 1) * nr + (cd - 1) * n_rc
    if (v[idx] == sid) { ans <- ans + 1; break }
    v[idx] <- sid
    tr <- cr + drs[cd]; tc <- cc + dcs[cd]
    if (tr < 1 || tr > nr || tc < 1 || tc > nc) break
    t_idx <- tr + (tc - 1) * nr
    if (m_v[t_idx] == "#" || (tr == obs_r && tc == obs_c)) {
      cd <- if (cd == 4) 1 else cd + 1
    } else {
      cr <- tr; cc <- tc
    }
  }
}
cat(ans, "\n")
