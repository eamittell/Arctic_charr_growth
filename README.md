## Spatiotemporal variation in growth rates in small isolated populations of Arctic charr (*Salvelinus alpinus*)

The code in this repository is for processing raw data from the Arctic charr study system of fish living in lava caves around Lake Myvatn, Iceland. This is a long-term individual-based study run from Hólar University, Iceland. 

The code is also for formatting the data from the study to use in and run models for the above paper.

In this study we assess the spatial, temporal and spatiotemporal variation in growth rate among isolated populations of Arctic charr. We demonstrate how to incorporate a covariate that contains missing data without data deletion or data augmentation, but via a state-space model where the covariate and its' uncertainty are incorporated into the model. This is an important consideration in ecological models. We use temperature as a covariate in this model, which is an important driver of growth rates in fishes. We also discuss the limits of missing data when including a covariate in this way. Furthermore, we find interesting biological results in our unusual study system.

C.A.L., B.K.K., K.R., & E.A.M. collected the data for this study. E.A.M. & M.B.M. analysed the data. E.A.M. is responsible for the code in this repository.

Elizabeth A. Mittell^1,2^*, Camille A. Leblanc^1^, Bjarni K. Kristjánsson^1^, Moira M. Ferguson^3^, Katja Räsänen^4,5^, Michael B. Morrissey^2^

^1^Department of Aquaculture and Fish Biology, Hólar University, Sauðárkrókur, Iceland; ^2^School of Biology, University of St. Andrews, UK; ^3^Department of Integrative Biology, University of Guelph, Guelph, ON, Canada; ^4^Department of Aquatic Ecology, EAWAG and Institute of Integrative Biology, ETH‐Zurich, Switzerland; ^5^Department of Biology and Environmental Science, University of Jyväskylä, Finland

*Corresponding author: Elizabeth A. Mittell, e.mittell@ed.ac.uk, e.mittell@gmail.com -- current address Univeristy of Edinburgh, UK.

### Scripts
Charr_env_data_processing.R and Charr_data_processing.R contain the code that was used to go from the database to the data that was formatted for the models in the Charr_data_formatting.R script.

Model_data.csv contains the processed data output from the above scripts. For each individual ID, the fork length (mm; size variable used), location (cave), year, season and measurement date are shown. The temperature is also shown where the data were available; NA is used to explicitly indicate missing information.

Charr_model.R contains the model that was used in the main manuscript, including initial model convergence checks. Further model checks were run and are described in Charr_model_checks.R. Charr_time_offset_changed.R contains a model where an alternative time-offset was used. This did not make any difference to the results.

Charr_model_checks_analyses.R contains the analyses that were run using the output from the model checks. The results of these are in the supplementary material.

Asymptotic_size_Charr_growth.R contains the code for the thought experiment about aysmptotic size that is described in the supplementary material.

The packages used within each script are shown in the scripts. These were run in various versions of R. All run and are installable in R version 4.3.2 as of 10th March 2024 on macOS Monterey version 12.7.1.
