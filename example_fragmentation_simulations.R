# Simulations equivalent to those found in the jupyter notebook.
library(rcoalescence)
for(seed in seq(1, 10, 1))
{
  sim <- SpatialTreeSimulation()
  sim$setSimulationParameters(task = 1, seed=seed, 
                              min_speciation_rate = 0.000001, sigma=16,
                              deme=5, output_directory = file.path("output", "r_out"),
                              fine_map_file = file.path("maps", "fine_present.tif"),
                              coarse_map_file = file.path("maps", "coarse_present.tif"))
  sim$addHistoricalMap(historical_fine_map = file.path("maps", "fine_historical.tif"),
                       historical_coarse_map = file.path("maps", "coarse_historical.tif"), 
                       gen_since_historical = 99.9)
  sim$runSimulation()
  sim$applySpeciationRates(speciation_rates = c(0.000001, 0.000005, 0.00001))
  sim$output()
}

