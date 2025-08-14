# Make sure outputs folder exists
dir.create("outputs", showWarnings = FALSE)

# Helper function to save plot to PNG
save_plot <- function(filename, plot_expr) {
  png(file.path("outputs", filename), width = 800, height = 600, res = 120)
  plot_expr
  dev.off()
}

### Exercise 2.12

# a) Regular dice
diceRoll <- replicate(10000, sum(sample(1:6 , 2, replace = TRUE)))
save_plot("dice_regular.png", barplot(table(diceRoll),
                                      main = "Regular Dice Outcomes",
                                      xlab = "Sum of Dice Rolls",
                                      ylab = "Frequency"))
barplot(table(diceRoll), main = "Regular Dice Outcomes", xlab = "Sum of Dice Rolls", ylab = "Frequency")

# b) Trick dice
trick <- replicate(10000, {
  die1 <- sample(rep(5, 6), 1)
  die2 <- sample(c(rep(2,3), rep(6,3)), 1)
  die1 + die2
})
save_plot("dice_trick.png", barplot(table(trick),
                                    main = "Trick Dice Outcomes",
                                    xlab = "Sum of Dice Rolls",
                                    ylab = "Frequency"))
barplot(table(trick), main = "Trick Dice Outcomes", xlab = "Sum of Dice Rolls", ylab = "Frequency")

# c) Sicherman dice
sicherman <- replicate(10000, {
  die1 <- sample(c(1, 2, 2, 3, 3, 4), 1)
  die2 <- sample(c(1, 3, 4, 5, 6, 8), 1)
  die1 + die2
})
save_plot("dice_sicherman.png", barplot(table(sicherman),
                                        main = "Sicherman Dice Outcomes",
                                        xlab = "Sum of Dice Rolls",
                                        ylab = "Frequency"))
barplot(table(sicherman), main = "Sicherman Dice Outcomes", xlab = "Sum of Dice Rolls", ylab = "Frequency")

### Exercise 2.20

# a) P(sum == 10)
eventAB <- replicate(10000, {
  dieRoll <- sample(1:6, 2, replace = TRUE)
  (sum(dieRoll) == 10)
})
p_exact_10 <- mean(eventAB)

# b) P(sum >= 10)
eventAB <- replicate(10000, {
  dieRoll <- sample(1:6, 2, replace = TRUE)
  (sum(dieRoll) >= 10)
})
p_at_least_10 <- mean(eventAB)

# c) P(sum == 10 | sum >= 10)
eventAB <- replicate(10000, sum(sample(1:6, 2, replace = TRUE)))
at_least_10 <- eventAB[eventAB >= 10]
exactly_10 <- (at_least_10 == 10)
p_conditional <- mean(exactly_10)

### Exercise 2.21

# a) P(sum == 10)
eventAB <- replicate(10000, {
  slips <- sample(1:6, 2, replace = FALSE)
  (sum(slips) == 10)
})
p_exact_10_no_replace <- mean(eventAB)

# b) P(sum >= 10)
eventAB <- replicate(10000, {
  slips <- sample(1:6, 2, replace = FALSE)
  (sum(slips) >= 10)
})
p_at_least_10_no_replace <- mean(eventAB)

# c) P(sum == 10 | sum >= 10)
eventAB <- replicate(10000, {
  slips <- sample(1:6, 2, replace = FALSE)
  sum(slips)
})
at_least_10 <- eventAB[eventAB >= 10]
exactly_10 <- (at_least_10 == 10)
p_conditional_no_replace <- mean(exactly_10)

# Save results to file
results <- data.frame(
  Exercise = c("2.20a", "2.20b", "2.20c", "2.21a", "2.21b", "2.21c"),
  Probability = c(p_exact_10, p_at_least_10, p_conditional,
                  p_exact_10_no_replace, p_at_least_10_no_replace, p_conditional_no_replace)
)
write.csv(results, "outputs/results.csv", row.names = FALSE)

print(results)
