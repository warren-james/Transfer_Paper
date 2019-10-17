library(tidyverse)

###################################################################
## import data from part 1 and fit the psychometric detection curve
###################################################################
folder <- "data/results/Part_1/"
files <- dir(folder, full.names = TRUE)

# extract participant IDs from filenames
p_id <- regmatches(files, regexpr('(?<=_)[0-9]*(?=_)', files, perl = TRUE))

# read in data
dat <- map(files, read_csv, col_names = c("block", "sep", "correct"))
rm(files, folder)

fit_detection_curve <- function(df) 
{
	m <- glm(correct ~ sep, 
		data = df, 
		family = "binomial")

	return(model = m)
}

det_fits <- tibble('participant' = p_id, model = map(dat, fit_detection_curve))

rm(dat, p_id)

###################################################################
## import data from part 2 and 3
###################################################################

folder <- "data/results/Part_2-3/"
files <- dir(folder, full.names = TRUE)

# parse condition info, saved as part = 2 or 3 in filename
condition_code <- regmatches(files, regexpr('(?<=part)[23]*', files, perl = TRUE))
condition_code <- if_else(condition_code == "2", "no instruction", "instruction")

# parse participant info
p_id <- regmatches(files, regexpr('(?<=_)[0-9]*(?=_)', files, perl = TRUE))

# read in data
map_df(files, read_csv, 
		col_names = c("block", "sep", "fixated", "correct")) %>%
	mutate(
		participant = rep(p_id, each = 90),
		group = as_factor(rep(condition_code, each = 90)),
		block = if_else(block < 5, 1, 2)) %>%
	select(participant, group, block, sep, fixated, correct) -> dat

rm(files, folder, condition_code)

# simulate

# get expected accuracy for central fixations 
dat %>% 
	nest(group, group, block, sep, fixated, correct) %>%
	full_join(det_fits) -> ldat
dat$p_centre <- unlist(map2(ldat$model, ldat$data, predict, type = "response"))
rm(ldat)

# get expected accuracy for side fixations
dat %>% mutate(sep = 2*sep) %>%
	nest(group, group, block, sep, fixated, correct, p_centre) %>%
	full_join(det_fits) -> ldat
dat$p_side <- unlist(map2(ldat$model, ldat$data, predict, type = "response"))
dat$p_side <- 0.5 + 0.5 * dat$p_side

# get expected optimal strat and simulate:
dat %>% mutate(
	p_optimal =  pmax(p_centre, p_side),
	sim_acc  = if_else(p_optimal > runif(n = n()), 1, 0)) %>%
	select(participant, group, block, sep, sim_acc) %>%
	rename(correct = "sim_acc") %>%
	mutate(group = "simulated") %>%
	bind_rows(dat) %>%
	select(participant, group, block, sep, correct) -> dat

dat %>% group_by(group) %>%
summarise(mean_acc = mean(correct, na.rm = TRUE))





