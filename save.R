# Source required scripts
source("tool/generate_ss_s3.R")
source("tool/perform_s3.R")
source("tool/measures.R")
source("method/back_logit.R")
source("method/backward_pvalue.R")
source("method/berank.R")
source("method/lasso_exact.R")
source("method/modified_pen.R")
source("method/stepwise.R")
source("method/unilogit.R")
source("method/unirank.R")

args <- commandArgs(trailingOnly = TRUE)

task_id <- as.integer(args[1])

# Associate each task with one sample size
sample_sizes <- c(
  ndev,
  ndev1,
  ndev2
)

sample_size <- sample_sizes[task_id]

# Run the simulation
result <- perform_s3(
  sample_size,
  n.para,
  n.true,
  beta0,
  beta,
  nval
)

# Create a results directory
dir.create(
  "results",
  showWarnings = FALSE,
  recursive = TRUE
)

# Give each task a separate output filename
output_file <- file.path(
  "results",
  paste0("result_sample_size_", sample_size, ".rds")
)

# Save the result
saveRDS(
  result,
  file = output_file
)
