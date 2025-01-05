
input <- readLines("input.txt")

dirs <- list()
current_path <- c()
dirs[["/"]] <- list(files = list(), dirs = list())

for (line in input) {
  parts <- strsplit(line, " ")[[1]]
  if (parts[1] == "$") {
    if (parts[2] == "cd") {
      if (parts[3] == "/") {
        current_path <- c("/")
      } else if (parts[3] == "..") {
        current_path <- head(current_path, -1)
      } else {
        current_path <- c(current_path, parts[3])
        path_str <- paste(current_path, collapse = "/")
        if(is.null(dirs[[path_str]])){
          dirs[[path_str]] <- list(files = list(), dirs = list())
        }
      }
    }
  } else if (parts[1] == "dir") {
    dir_name <- parts[2]
    path_str <- paste(current_path, collapse = "/")
    dirs[[path_str]]$dirs[[dir_name]] <- list(files = list(), dirs = list())
  } else {
    size <- as.integer(parts[1])
    file_name <- parts[2]
    path_str <- paste(current_path, collapse = "/")
    dirs[[path_str]]$files[[file_name]] <- size
  }
}

calculate_size <- function(dir_path) {
  size <- 0
  if(!is.null(dirs[[dir_path]]$files)){
    size <- sum(unlist(dirs[[dir_path]]$files))
  }
  if(!is.null(dirs[[dir_path]]$dirs)){
    for (subdir in names(dirs[[dir_path]]$dirs)) {
      size <- size + calculate_size(paste(dir_path, subdir, sep = "/"))
    }
  }
  return(size)
}

total_sizes <- sapply(names(dirs), calculate_size)
result <- sum(total_sizes[total_sizes <= 100000])
cat(result, "\n")
