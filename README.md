# rsm-thermal-daylight-optimization

This repository provides supplementary materials to reproduce the results outlined in the research preprint, **Response Surface Methodology coupled with desirability functions for multi-objective optimization: minimizing indoor overheating hours and maximizing useful daylight illuminance** (currently under review). 

All code used in this study is provided as R Markdown and R script files within the repository.

The repository is organized into the following folders:
*	`rhino-grasshopper-honeybee-files`: Contains the `.gh script file` for running thermal comfort (indoor overheating hours, IOH) and daylight (useful daylight illuminance, UDI) simulations. It also includes:
	A `.3dm file` with the 3D model of the dwelling.
.csv files that provide input data for different parts of the script (e.g., occupancy schedules, outdoor temperature).
A `.csv file` with the fractional factorial design used to feed the Colibri component for the Screening subsection.
*	`simulation-inputs`: Includes the first-order (orthogonal first-order 2k factorial design) and second-order (Central Composite Design) experimental designs used for simulating IOH and UDI.
*	`simulation-outputs`: If you prefer to skip running the simulations, this folder contains precomputed results that can be directly used with the R code provided in `RSM_thermal_daylight_optimization.R` and `RSM_thermal_daylight_optimization.md`.
*	`randomization`: Stores randomized values (following a normal distribution) for factors with negligible effects on Overall Desirability (D), as identified through Lasso and Stepwise Regression.


![](https://github.com/juan-gamero-salinas/rsm-thermal-daylight-optimization/blob/main/UDI_64runs.gif?raw=true)

## If you cite this work or repository
Gamero-Salinas, J., & López-Fidalgo, J. (2024). Response Surface Methodology coupled with desirability functions for multi-objective optimization: minimizing indoor overheating hours and maximizing useful daylight illuminance. *arXiv preprint*. DOI: [https://doi.org/10.48550/arXiv.2409.09093](https://doi.org/10.48550/arXiv.2409.09093)


