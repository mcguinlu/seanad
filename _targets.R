library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/helpers.R")
options(tidyverse.quiet = TRUE)

# Set target-specific options such as packages.
tar_option_set(packages = "dplyr")

clean_final_data <- function(data) {
  return(data)
}

# End this file with a list of target objects.
list(
  # Raw data
  tar_target(raw_voting_data,
             get_raw_voting_data()),
  tar_target(base_data,
             create_base_data()),
  tar_target(debate_texts,
             get_debate_text(base_data)),

  # Start combining
  tar_target(filtered_data,
             apply_filters(base_data)),
  tar_target(voting_added,
             add_voting(filtered_data, raw_voting_data)),
  tar_target(attendence_added,
             add_attendance(voting_added)),
  tar_target(contributions_added,
             add_contributions(attendence_added, debate_texts)),
  tar_target(cleaned_data,
             clean_final_data(contributions_added)),

  # Create summary stats and visualisations
  tar_target(plots,
             create_plots(cleaned_data))
)
