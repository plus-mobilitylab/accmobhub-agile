# Assessment of bicycle accessibility to mobility hubs under different criteria for cycling network quality

This repository contains the R code to reproduce the analysis of the paper "Assessment of bicycle accessibility to mobility hubs under different criteria for cycling network quality", presented at the annual conference of the Association of Geographic Information Laboratories in Europe (AGILE), held in Glasgow on 4-7 June 2024. For more information, see [here](https://agile-gi.eu/conference-2024). The published paper can be found [here](https://doi.org/10.5194/agile-giss-5-48-2024).

## Instructions

Reproduce the analysis with the following steps.

#### 1. Clone this repository

Clone this repository with [git](https://git-scm.com/) and move into the cloned directory.

```bash
git clone https://github.com/plus-mobilitylab/accmobhub-agile.git
cd accmobhub-agile
```

#### 2. Get the source data

Download the required data from the supplementary materials of the paper, for example using [wget](https://www.gnu.org/software/wget/). The materials can be found on [Zenodo](https://zenodo.org/doi/10.5281/zenodo.10949523). The required files are:

- **salzburgerland-netascore-20240123.gpkg**: The street network of the Salzburg province including bicycle suitability indices as computed by the [NetAScore software](https://github.com/plus-mobilitylab/netascore). Note that if you also want to reproduce the index computations, follow the instructions on the Zenodo repository.
- **austria-addresses-20211001.csv**: The address location file of the Austrian address database.

Store these files in the data folder of the repository.

```bash
wget -P data https://zenodo.org/records/10949524/files/salzburgerland-netascore-20240123.gpkg?download=1
wget -P data https://zenodo.org/records/10949524/files/austria-addresses-20211001.csv?download=1
```

#### 3. Run the analysis

To run the analysis, simply execute the R script [scripts/run.R](https://github.com/plus-mobilitylab/accmobhub-agile/blob/main/scripts/run.R), from example from within the RStudio IDE or directly from the command line. It assumes your working directory is the top level of the cloned repository. Note that you need to have the following R packages installed to make it work:

- sf
- sfnetworks
- tidyverse
- tidygraph
- tidyterra
- maptiles
- here
- units

The script will store the results in a file `data/data.RData`. The figures will be stored in the `plots/` directory. 

## Acknowledgements

Parts of this research were funded by a commission from [Research Studios Austria - iSpace](https://ispace.maps.arcgis.com/home/index.html).

## License

This project is licensed under the MIT license. For details please see [LICENSE](LICENSE).
