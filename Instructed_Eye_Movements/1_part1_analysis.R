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

	return(tibble(intercept = m$coefficients[1], beta = m$coefficients[2]))
}

det_fits <- bind_cols('p_id' = p_id, map_df(dat, fit_detection_curve))

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
		condition = rep(condition_code, each = 90)) %>%
	select(participant, condition, block, sep, fixated, correct) -> dat

rm(files, folder)