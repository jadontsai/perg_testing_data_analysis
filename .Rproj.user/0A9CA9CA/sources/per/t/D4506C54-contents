scores <- read.csv("perg_scores.csv", check.names = TRUE, stringsAsFactors = FALSE)


scores$Total.distance. <- as.numeric(scores$Total.distance.)
scores$Average.power.  <- as.numeric(scores$Average.power.)

time_cols <- c(
  "X0.seconds",
  "X5.seconds",
  "X10.seconds",
  "X15.seconds",
  "X20.seconds",
  "X25.seconds",
  "X30.seconds"
)

for (col in time_cols) {
  scores[[col]] <- as.numeric(scores[[col]])
}

feedback_yes <- scores[scores$Feedback. == "Feedback", ]
feedback_no  <- scores[scores$Feedback. == "No feedback", ]


feedback_yes <- feedback_yes[order(feedback_yes$Name.nickname.), ]
feedback_no  <- feedback_no[order(feedback_no$Name.nickname.), ]


print(nrow(feedback_yes))
print(nrow(feedback_no))


cat("\n Total Distance ===========================================\n")
print(t.test(feedback_yes$Total.distance.,
             feedback_no$Total.distance.,
             paired = TRUE))


cat("\n Average Power ===========================================\n")
print(t.test(feedback_yes$Average.power.,
             feedback_no$Average.power.,
             paired = TRUE))


for (col in time_cols) {
  cat("\n", col, "===========================================\n")
  
  result <- t.test(feedback_yes[[col]],
                   feedback_no[[col]],
                   paired = TRUE)
  
  print(result)
}

