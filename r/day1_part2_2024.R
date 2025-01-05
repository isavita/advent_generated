
input <- read.table("input.txt", header = FALSE, col.names = c("left", "right"))
right_counts <- table(input$right)
similarity_score <- sum(input$left * right_counts[as.character(input$left)], na.rm = TRUE)
cat(similarity_score, "\n")
