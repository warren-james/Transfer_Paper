library(tidyverse)
library(psyphy)


load("scratch/df_part2")

df <- df[complete.cases(df),]

people <- c(0, 2, 3, 4, 5)

for (person in people) {

	# load in part 1 data to get performance curve
	dat <- read.csv(paste("data/results/Part_1/0_", person, "_part1.txt", sep = ""), header = FALSE)
	names(dat) <- c("block", "delta", "response")

	performance_model <- glm(
		response ~ delta, 
		data = dat, 
		family = binomial(mafc.logit(2)))
	rm(dat)
	
	# now simulate part 2
	dat <- filter(df, participant == person, is.finite(correct))

	# compute centre chance 
	centre_chance <- predict(
		performance_model, 
		type = "response",
		newdata = list(delta = dat$separation))
	
  	# same for side 
	far_chance <- predict(
	  performance_model,
	  type = "response",
	  newdata = list(delta = 2*dat$separation))
	
	side_chance <- 0.5 + 0.5 * far_chance

	# Optimal strat chance
	optimal_chance <- pmax(centre_chance, side_chance)

	# simulate 100k experiments
	 # pred_acc <- replicate(1000000, mean(optimal_chance > runif(length(optimal_chance))))
	obs_acc <- mean(dat$correct)
	pred_acc <- mean(optimal_chance)
	print(paste("obs acc of:", round(obs_acc,5), "compared to opt acc of: ", round(pred_acc,5)))
	print(mean(pred_acc) - obs_acc)
	# print(paste("prob(getting as low, or lower, acc under optimal strat) = ", round(mean(obs_acc  > pred_acc),3) ))
}