# Growth-dependent predation and generalised transduction of antimicrobial resistance by bacteriophage: Code Repository

## About
This repository contains all the code used to run the analyses outlined in the corresponding paper available [here](https://journals.asm.org/doi/full/10.1128/msystems.00135-22).

For any questions, please contact Quentin Leclerc.

- Email: quentin.leclerc@lshtm.ac.uk
- Twitter: [QuentinLclrc](https://twitter.com/QuentinLclrc)

## Data

The "/Data" folder contains the in vitro data generated as part of this project. Please refer to the paper for further details. The files included are:
- `growth_summary.csv`: results from the growth experiment of the two single-resistant bacterial strains and the one double-resistant progeny strain in the absence of exogenous phage.
- `transduction_summary_X.csv`: results from the transduction experiment, where "X" is either "10_3", "10_4", or "10_5", indicating the starting phage concentration for that dataset.
- `varying_MOI_data.xlsx`: results from the transduction experiment conducted with a starting concentration of 10^6 bacteria per mL, with varying starting phage concentrations. 

## Model

The core model code is in the "/Model" folder. The skeletons of the growth model (no phage, bacteria only) and transduction model are respectively stored in the files `growth_model.rds` and `transduction_model.rds`, generated by the scripts `growth_fitmodel.R` and `transduction_fitmodel.R`.

These models are designed to work within the `R` package `fitR`, please refer to the package [here](https://github.com/sbfnk/fitR) for further documentation.

The `transduction_model_functions.R` script contains essential functions to work with the transduction model:
- `choose_model()`: when first loaded, the transduction model is by default incomplete. You can flexibly choose the features of the model by running it through the `choose_model()` function. Below is an example to set the model to be frequency-depedent, with a link between phage burst size and bacterial growth. Please refer to the script for detals on further options available.

``` r
#load the model skeleton:
model = readRDS(here::here("Model", "transduction_model.rds"))

#define the model:
model = choose_model(model,                   #provide model
                     frequentist = TRUE       #should the model be frequency-dependent? Otherwise, will be density-dependent
                     link_beta = FALSE,       #link adsorption rate to bacterial growth?
                     link_L = TRUE,           #link burst size to bacterial growth?
                     transduction = TRUE)     #enable transduction?
                     
#the "model" object is now ready to be used!

``` 

- `run_mcmc()`: wrapper function for the model fitting framework provided by the `fitR` package. This is used in the scripts contained in the folder "/Fitting".
- `multi_run2()`: function used to run the model multiple times, with multiple ways to introduce random variations through parameter sampling or Poisson resampling of model output.


## Fitting

The "/Fitting" folder contains the scripts used to fit the model to the in vitro data. The `mcmcMH_growth.R` script performs the fitting for the bacterial growth parameters (using the basic growth model, with no phage present), and the other scripts each perform fitting for a different model (e.g. density dependent with adsorption rate linked to bacterial growth, frequency dependent with burst size linked to bacterial growth etc...). Scripts labelled with a "b" are identical to their counterpart without the "b", but with different starting values for the model fitting - so we had two chains for the MCMC fitting process.

Note that the fitting process takes several hours! These script were run several times on parallel servers rather than a single computer as part of this work, until convergence was achieved.

The resulting full MCMC chains are present in the "/Fitting/Full_chains" subfolder as `.RDS` files. The subfolder "/Fitting/Full_chains/Best_fits" contains plots showing the single best-fitting output from the fitting process for each model and chain, for rapid visual inspection.

These full chains have then undergone burning and thinning, resulting in the `.csv` files in the "/Fitting/Fitted_params" subfolder.

Finally, these burned and thinned chains are processed into summary parameter tables (Table 1 in the paper), resulting in the `growth_params.csv` and `transduction_params.csv` files in the "/Fitting" folder.

## Analysis

The "/Analysis" folder contains various scripts used to create the figures shown in the paper. Most are explicitly named to indicate the figure they generate, except the following:
- `analyse_lab_data.R`: this generate Figure 2 and Supplementary Figure 2, looking at the in vitro data, alongside some bacteria fitness values shown in the paper.
- `analyse_fitted_models.R`: this converts the burned and thinned fitted parameter chains into summary parameter tables (see previous section), and generates the 1st part of Figure 4.
- `plot_varying_MOI.R`: this generates the 2nd part of Figure 4, and should be run after the previous script to recreate the complete Figure 4. This script also generates Supplementary Figure 4.

## Figures

The "/Figures" folder contains the figures created by the different scripts. Each figure is named according to its position in the paper (e.g. "fig2" is Figure 2 in the paper).

## License

This work is distributed under the MIT license (see LICENSE file).
