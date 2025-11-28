[![CC0 1.0][cc-zero-shield]][cc-zero]

# SCVPM-DES
*Discrete Event Simulation implementation of the Scottish Cardiovascular Policy Model* by **[Ziyi Lin (林子义)]** and **[Andrew H Briggs]**

**Associated Paper:** Our research paper describing this model has been published in *PharmacoEconomics* and is available at: https://doi.org/10.1007/s40273-025-01560-6.

## Description
This repository contains R code implementing the **S**cottish **C**ardio**V**ascular **P**olicy **M**odel (SCVPM) using a discrete event simulation (DES) modelling approach. Consistent with the original SCVPM, which was firstly implemented as a Markov model in Excel and later in R ([available here](https://github.com/yiqiaoxin/CVDmodel)), this DES version is designed for cost-effectiveness analysis of statin treatment in cardiovascular disease prevention. The model evaluates the long-term costs and quality-adjusted life years (QALYs) associated with statin therapy compared to no treatment.

## License
This R code is dedicated to the public domain under the [CC0 1.0 Universal license][cc-zero].

[![CC0 1.0][cc-zero-image]][cc-zero]

**CC0 1.0 Universal Summary:**

* Users are free to distribute, remix, adapt, and build upon the material in any medium or format, even for commercial purposes;

* This is a public domain dedication;

* No attribution is required, though it is appreciated.

**Please note that this is a summary and does not cover all the details of the license. Users should refer to the full [CC0 1.0][cc-zero] License for complete information.**

**Please also note that the accompanying academic paper is published separately in *[PharmacoEconomics]* and is subject to the publisher's licensing terms. This license applies only to the R code in this repository.**

## Usage
To run the model, follow these steps:

1. Clone this repository to your local machine.
2. Open the `SCVPM-DES.Rproj` file in RStudio (or set your working directory to the repository folder)
3. Ensure required R packages are installed (see Prerequisites below)
4. Run the script `From_Markov_to_DES.R`.

### Prerequisites
Install required R packages:
```r
install.packages(c(
  "tidyverse", "readxl", "scales", "truncnorm", "MASS",
  "bookdown", "knitr", "kableExtra"
))
```
### Data Requirements
The `Data/CVDparameters.xlsx` file contains all model parameters and coefficients. Ensure this file is present in the `Data` folder before running the model.

### Expected Outputs
The model will generate:

* Console output: Incremental cost-effectiveness results

* Tables: Cost and QALY differences by treatment arm and demographic subgroups

* Plots: Cost-effectiveness planes and acceptability curves

## Contact
If you have any questions or feedback, please open an issue on this repository or contact [Ziyi Lin (林子义)] at [ziyi.lin@lshtm.ac.uk](mailto:ziyi.lin@lshtm.ac.uk), or [Andrew H Briggs] at [andrew.briggs@lshtm.ac.uk](mailto:andrew.briggs@lshtm.ac.uk).

[cc-zero]: http://creativecommons.org/publicdomain/zero/1.0/
[cc-zero-image]: https://licensebuttons.net/l/zero/1.0/88x31.png
[cc-zero-shield]: https://img.shields.io/badge/License-CC0%201.0-lightgrey.svg
[PharmacoEconomics]: https://doi.org/10.1007/s40273-025-01560-6
[Andrew H Briggs]: https://github.com/Akadeem
[Ziyi Lin (林子义)]: https://github.com/ZiyiIiIiIiI

