# Machine learning prediction of nitrate in fresh and saltwater estuaries

## Overview

### Get to know the data

All raw data are downloaded from NOAA's National Estuarine Research Reserve (NERR) data repository found [here](https://cdmo.baruch.sc.edu/). For convenience, we include the downloaded raw data in the _"data_NERR"_ folder, with two sub-folders for the two sites included in this study. The "cbv" folder contains data for the Chesapeake Bay Virginia NERR site, and the "owc" folder contains data for the Old Woman Creek NERR site. Each of these folders contains sub-folders for the three types of data we downloaded: water quality, nutrient, and meteorological datasets.

### Running scripts

We organized our workflow in the 'R' folder, where scripts are ordered alphabetically (A-G) to represent the order in which the scripts should be run to prepare the data, construct models ,evaluate models, and create high frequency predictions. Please note that training the large number of random forest models takes a significant amount of time, and some scripts will generate large datasets and objects.

### General workflow organization

The general workflow is built on modularity and, thus, we split our scripts into constants, functions, and main scripts. If you intend to use the functions other than how they are used within our scripts, we cannot assure that they will perform as expected. However, if an issue does arise look within the Functions folder for code (refer to headings) and/or please feel free to contact us regarding the issue either through email or the issues tab.

## Contributions

Code edits or concerns are welcome and appreciated! Insert these in [Issues](https://github.com/COMPASS-DOE/bgc_synthesis/issues) or [Pull Request](https://github.com/COMPASS-DOE/bgc_synthesis/projects) depending on severity.

## Data sources

Data came from National Estuarine Research Reserve system ([NERR](https://cdmo.baruch.sc.edu/)) from 2000 to 2020.

## Example workflow

Please check out the presentation markdown [file](https://github.com/COMPASS-DOE/bgc_synthesis/blob/main/presentation.Rmd) for figures and a short description on their interpretations. 
