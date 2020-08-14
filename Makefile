# Search path
VPATH = data:scripts:models

# Processed data files
DATA = ncaa.rds difficulty_points.rds win_probability_train_data.rds win_probability_model_posterior.rds tournament_sample.rds reseeded_tournament_sample.rds upsets_tournament_sample.rds upsets_reseeded_tournament_sample.rds

# Reports
REPORTS = README.md

# All targets
all : $(DATA) $(REPORTS)

# Data dependencies
win_probability_model_posterior.rds : win_probability_model_posterior.R win_probability_train_data.rds win_probability_model.stan
	Rscript scripts/win_probability_model_posterior.R

# Pattern rules
%_tournament_sample.rds : %_tournament_sample.R win_probability_model_posterior.rds
	Rscript $<

%.rds : %.R
	Rscript $<
	
%.md : %.Rmd
	Rscript -e 'rmarkdown::render(input = "$<", output_options = list(html_preview = FALSE))'
