source("run/run_simulation.R")

args <- commandArgs(trailingOnly = TRUE)

percentage <- as.numeric(strsplit(args[1], ",")[[1]])
ndev <- as.numeric(args[2])
output_name <- args[3]
n_restrict <- args[4]

if (n_restrict == "NULL") {
  n_restrict <- NULL
} else {
  n_restrict <- as.numeric(n_restrict)
}

result <- run_simulation(
  percentage = percentage,
  ndev = ndev,
  output_name = output_name,
  n.restrict = n_restrict
)
