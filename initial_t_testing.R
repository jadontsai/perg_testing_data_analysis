scores <- read.csv("/Users/pranav/Desktop/perg_testing_data_analysis/perg_scores.csv", check.names = TRUE, stringsAsFactors = FALSE)

# convert string to numeric values
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

# Take average of two trials per person per feedback level
averaged_scores <- aggregate(. ~ Name.nickname. + Feedback., 
                             data = scores[, c("Name.nickname.", "Feedback.", "Total.distance.", "Average.power.", time_cols)], 
                             FUN = mean)

feedback_yes_avg <- averaged_scores[averaged_scores$Feedback. == "Feedback", ]
feedback_no_avg  <- averaged_scores[averaged_scores$Feedback. == "No feedback", ]

feedback_yes <- feedback_yes[order(feedback_yes$Name.nickname.), ]
feedback_no  <- feedback_no[order(feedback_no$Name.nickname.), ]

feedback_yes_avg <- feedback_yes_avg[order(feedback_yes_avg$Name.nickname.), ]
feedback_no_avg  <- feedback_no_avg[order(feedback_no_avg$Name.nickname.), ]

# Quality variable calculation - standard deviation of power across 30 seconds
feedback_yes_avg$SD_time <- apply(feedback_yes_avg[, time_cols], 1, sd)
feedback_no_avg$SD_time <- apply(feedback_no_avg[, time_cols], 1, sd)

# should return 24 each
print(nrow(feedback_yes_avg))
print(nrow(feedback_no_avg))

# print out all averaged distance values (sanity check)
#print("Feedback YES - Total Distance:")
#print(feedback_yes_avg$Total.distance.)

#print("Feedback YES - Average Power:")
#print(feedback_yes_avg$Average.power.)


cat("\n Total Distance ===========================================\n")
print(t.test(feedback_yes_avg$Total.distance.,
             feedback_no_avg$Total.distance.,
             paired = TRUE))


cat("\n Average Power ===========================================\n")
print(t.test(feedback_yes_avg$Average.power.,
             feedback_no_avg$Average.power.,
             paired = TRUE))


cat("\n Speed Variable ===========================================\n")
print(t.test(feedback_yes_avg$Total.distance./30,
             feedback_no_avg$Total.distance./30,
             paired = TRUE))


cat("\n Quality Variable ===========================================\n")
print(t.test(feedback_yes_avg$SD_time,
             feedback_no_avg$SD_time,
             paired = TRUE))


for (col in time_cols) {
  cat("\n", col, "===========================================\n")
  
  result <- t.test(feedback_yes_avg[[col]],
                   feedback_no_avg[[col]],
                   paired = TRUE)
  
  print(result)
}

# graph plotting function

plot_and_save_graph <- function(filename, plot_function, width=1500, height=1500, pointsize=12, res=300) 
{
  png(filename, width=width, height=height, pointsize=pointsize, res=res)
  par(mar=c(5,5,4,2))  # Adjust margins
  plot_function()
  dev.off() 
}

# GRAPHS WE NEED

# Normal QQ plots to test normality assumption
# normal if points follow a straight line
# qqline connects first and third quartiles

plot_and_save_graph("~/Desktop/perg_testing_data_analysis/Images/qqplot_quality_feedback.png", function() {
  qqnorm(feedback_yes_avg$SD_time, main="QQ Plot for Quality", col="blue", pch=19)
  qqline(feedback_yes_avg$SD_time, col="red")
})
plot_and_save_graph("~/Desktop/perg_testing_data_analysis/Images/qqplot_quality_nofeedback.png", function() {
  qqnorm(feedback_no_avg$SD_time, main="QQ Plot for Quality", col="blue", pch=19)
  qqline(feedback_no_avg$SD_time, col="red")
})

plot_and_save_graph("~/Desktop/perg_testing_data_analysis/Images/qqplot_speed_feedback.png", function() {
  qqnorm(feedback_yes_avg$Total.distance./30, main="QQ Plot for Speed", col="blue", pch=19)
  qqline(feedback_yes_avg$Total.distance./30, col="red")
})

plot_and_save_graph("~/Desktop/perg_testing_data_analysis/Images/qqplot_speed_nofeedback.png", function() {
  qqnorm(feedback_no_avg$Total.distance./30, main="QQ Plot for Speed", col="blue", pch=19)
  qqline(feedback_no_avg$Total.distance./30, col="red")
})

# GRAPHS WE MIGHT NEED 

# boxplot
plot_and_save_graph("~/Desktop/perg_testing_data_analysis/Images/boxplot_quality.png", function() {
  par(mfrow=c(1,2))
  boxplot(feedback_yes_avg$SD_time, feedback_no_avg$SD_time, names=c("Feedback", "No Feedback"), col=c("blue", "red"), main="Boxplot of Quality (SD of Power)", ylab="Quality (SD of Power)")

  boxplot(feedback_yes_avg$Total.distance./30, feedback_no_avg$Total.distance./30, names=c("Feedback", "No Feedback"), col=c("blue", "red"), main="Boxplot of Speed (Distance/30s)", ylab="Speed (m/s)")
  par(mfrow=c(1,2))
})

# copilot made this for some reason. It is a good idea but not mine :pensive:. it's scaled by z score
plot_and_save_graph("~/Desktop/perg_testing_data_analysis/Images/quality_vs_speed_zscore.png", function() {
  # Plot feedback condition (blue) - using scaled data for comparable axes
  plot(scale(feedback_yes_avg$Total.distance./30), scale(feedback_yes_avg$SD_time),
       main="Scaled Quality vs Scaled Speed", xlab="Scaled Speed (z-score)", ylab="Scaled Quality (z-score)", 
       col="blue", pch=19, xlim=range(c(scale(feedback_yes_avg$Total.distance./30), scale(feedback_no_avg$Total.distance./30)), na.rm=TRUE), 
       ylim=range(c(scale(feedback_yes_avg$SD_time), scale(feedback_no_avg$SD_time)), na.rm=TRUE))
  
  # plot no feedback condition on top in red 
  points(scale(feedback_no_avg$Total.distance./30), scale(feedback_no_avg$SD_time), col="red", pch=19)
  
  # legend
  legend("topright", legend=c("Feedback", "No Feedback"), col=c("blue", "red"), pch=19)
  
  # Add reference lines at 0
  abline(h=0, lty=2, col="gray")
  abline(v=0, lty=2, col="gray")
})

plot_and_save_graph("~/Desktop/perg_testing_data_analysis/Images/quality_vs_speed.png", function() {
  # Plot feedback condition (blue)
  plot(feedback_yes_avg$Total.distance./30, feedback_yes_avg$SD_time,
       main="Quality vs Speed", xlab="Speed (m/s)", ylab="Quality (SD of Power)", 
       col="blue", pch=19, xlim=range(c(feedback_yes_avg$Total.distance./30, feedback_no_avg$Total.distance./30), na.rm=TRUE), 
       ylim=range(c(feedback_yes_avg$SD_time, feedback_no_avg$SD_time), na.rm=TRUE))
  
  # plot no feedback condition on top in red 
  points(feedback_no_avg$Total.distance./30, feedback_no_avg$SD_time, col="red", pch=19)
  
  # legend
  legend("topright", legend=c("Feedback", "No Feedback"), col=c("blue", "red"), pch=19)
})

# two sample tests
# equal variance assumption -> use F test. assumes data is normally distribution so welp. p=0.05 is convention for rejecting null hypothesis (that variances are equal)
print(var.test(feedback_yes_avg$SD_time, feedback_no_avg$SD_time))
print(var.test(feedback_yes_avg$Total.distance./30, feedback_no_avg$Total.distance./30))

print(t.test(feedback_yes_avg$SD_time, feedback_no_avg$SD_time))
print(t.test(feedback_yes_avg$Total.distance./30, feedback_no_avg$Total.distance./30))

# correlation coefficients -> not needed but maybe useful?

print(cor.test(feedback_yes_avg$SD_time, feedback_yes_avg$Total.distance./30)) # correlation with feedback between quality and accuracy
print(cor.test(feedback_no_avg$SD_time, feedback_no_avg$Total.distance./30)) # correlation with no feedback between quality and accuracy


# to do: mann whitney u test?? it was mentioned on piazza
# shapiro wilk test for normality?? 
#print(shapiro.test(feedback_yes_avg$SD_time))
#print(shapiro.test(feedback_no_avg$SD_time))
#print(shapiro.test(feedback_yes_avg$Total.distance./30))
#print(shapiro.test(feedback_no_avg$Total.distance./30))