# SpainSurveyAEAT

## Overview

**SpainSurveyAEAT** is a comprehensive toolkit designed for the analysis and manipulation of microdata related to Spain's housing sector. Utilizing the power of the `data.table` and `survey` libraries, this project aims to deliver robust inference capabilities on population samples, emphasizing rental income analysis.

## Getting Started

### Prerequisites

Ensure you have R installed on your system. This project relies heavily on the `data.table` and `systemfit` R packages for performance and analysis.

### Installation

Clone this repository to your local machine using:

```bash
git clone https://github.com/iliciuv/SpainSurveyAEAT.git
```

Navigate to the project directory and install the required R packages:

```R
install.packages(c("data.table", "survey"))
```

### Usage

The main script can be run from the root folder:

```R
Rscript main.R
```

This script analyzes key statistics related to renterism in the Spanish housing sector, leveraging microdata from both the EFF and IEF datasets.

## Project Structure

- `main.R`: Entry point for the analysis.
- `merge_survey_files/`: Contains pipelines for preparing microdata files.
- `tests/`: Includes examples on handling sample and replication weights.

## Data Sources

This project is compatible with microdata from:
- EFF (Encuesta Financiera de Familias): [EFF Data Access](https://app.bde.es/efs_www/home?lang=ES)
- IEF (Instituto de Estudios Fiscales) [IEF Data Request](https://www.ief.es/docs/investigacion/estadistica/PeticionDatos.pdf)

## Contributing

Contributions to enhance **SpainSurveyAEAT** are welcome. If you experience any problem reproducing any results referencing this repository, please contact miguel.garcia.duch@ucm.es

## License

This project is licensed under the GNU General Public License v3.0. For more details, please visit [GNU General Public License](https://www.gnu.org/licenses/gpl-3.0.en.html).

## Acknowledgments

- Data providers: EFF and IEF for their invaluable datasets.
- All contributors who help in refining and advancing this analysis toolkit.

