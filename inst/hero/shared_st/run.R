if(!require(heRomod)) {
  if(!require(devtools)) {
    install.packages('devtools')
  }
  library(devtools)
  install_github('PolicyAnalysisInc/heRomod')
  library(heRomod)
}
model <- readRDS('./model.rds')
results <- do.call(run_hero_bc, model)

