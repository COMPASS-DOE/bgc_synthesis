# Create high resolution nutrient and chlorphyll-a concentration on disturbance event and decadel timescales revealed by random forests

## Overview

### Get to know the data

All raw data is in the _"data_NERR"_ folder: "cbv" and "owc". For each location we have a folder that splits up water quality, nutrient, and meteorlogical data specied in different folder, respectively. This is how NERR gives us the data for these locations, so we suspect this procedure to be standard.

### Running scripts for high frequency data

We organized our workflow in the R folder. Due to the immense size of teh high frequency data and memory restrictions, we put raw data directly downloaded from site. Inside the R folder, there are scripts in alphabetical order A-G to represent the proper steps to go from prepping the data, constructing models, evaluating the models, and finally having the chance to evaluate high frequency predictions. We split evaluation of prediction into patterns and events to help in minimizing computational time.

Warning: overall patterns, high frequency predictions, and training random forest models need an extensive amount of time given the size of the dataset.

### General Workflow Organization

The general workflow is built on modularity and, thus, we split our scripts into constants, functions, and main scripts. If you intend to use the functions other than how they are used within our scripts, we cannot assure that they will perform as expected. However, if an issue does arise look within the Functions folder for code (refer to headings) and/or please feel free to contact us regarding the issue either through email or the issues tab.

## Contributions

Code edits or concerns are welcome and appreciated! Insert these in [Issues](https://github.com/COMPASS-DOE/bgc_synthesis/issues) or [Pull Request](https://github.com/COMPASS-DOE/bgc_synthesis/projects) depending on severity.

## Data Sources

Data came from National Estuarine Research Reserve system ([NERR](https://cdmo.baruch.sc.edu/)) from 2000 to 2020.

## Example Workflow

Extensive literature describing the procedures are currently being prepared. In the meantime, checkout the presentation markdown [file](https://github.com/COMPASS-DOE/bgc_synthesis/blob/main/presentation.Rmd) for figures and a short description on their interpretations. 
