# Status and trends of California’s fishes

This is the GitHub repository for the following paper nearing submission:

* Free CM, Allen LG, Bellquist L, Cieri K, Claisse JT, Field JC, Hamilton SL, Leising A, Satterthwaite EV, Silva J, Harris J. Status and trends of California’s fishes. Near submission.

Please contact Chris Free (cfree14@gmail.com) with any questions about the repository.

## California Ocean Report Card

This project is to support the California Ocean Report Card. Read more about the Ocean Report Card here: https://opc.ca.gov/annual-reports/

## Repository structure

The repository is structured as follows:

 * **data:** data used in the paper; subfolders include:
   * **raw:** raw data
   * **processed:** processed data
   * **output:** estimated indices of relative abundance
   * **figures:** visualizations of the processed data and abundance indices
   * code to clean the raw data, fit the abundance indices, and vizualize data and results
 * **code:** code to merge the abundance indices, analyze the indices, and make figures for the paper
 * **tables:** tables for the paper
 * **figures:** figures for the paper

The **data** folder includes a subfolder for each of the monitoring surveys analyzed in the paper. Within the monitoring survey subfolders are folders to hold the raw data, the processed data, the estimated indices of relative abundance, and visualizations of the data and results. Each subfolder also includes the code used to clean the data, fit the models, and visualize the results. The scripts are numbered in the sequences in which they should be run.

There are also data subfolders for other datasets used in the study, such as the species that are managed in state and federal management plans (**mgmt_plans**) and life history traits of the evaluated species (**fish_species**).

The **code** folder contains the scripts that merge the abundance indices estimated from each of the monitoring surveys and the scripts to conduct the analysis of these abundance estimates. It also contains the scripts used to create the figures in the paper. The analysis scripts are numbereed in the sequence in which they should be run. The figure scripts are numbered based on the figure that they produce.

The **figures** folder contains the figures published in the manuscript. Please note that the data subfolders house the figures used to examine each monitoring survey individually.

The **output** folder will eventually contain the synthesized data that are the focus of the manuscript. Currently, these data are exported to the **data/merged** folder.
