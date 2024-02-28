## SpainSurveyAEAT
Microdata manipulation &amp; Survey Analysis for Spain´s Housing Sector

### Overview
This repository includes scripts for preparing data and performing robust inference on the population from samples with multiple replications and sample weights. It showcases examples dealing with the analysisi of rental income in the Spanish housing sector.  This project tries to combine the need of few dependencies and strong raw performance relying almost exclusively on libraries **data.table & systemfit**.


### Project structure
- **main.R**: The main entrypoint is the file main.R on root folder. It reports some key statistics on the Spanish housing sector renterism using r package survey.
- **merge_survey_files**: The folder includes pipes to prepare basic microdata files to work with main.R 
- **tests**: this folder includes an example file explaining how to deal with sample and replication weights in the context of multiple imputation.

### Sources and Scope
This repository is prepared to work both with microdata from Encuesta Financiera de Familias (EFF) and the large IRPF sample recently made public by Instituto de Estudios Fiscales using data from AEAT.
No microdata is provided for privacy reasons but both sources can be accessed for free after request to their respective provider:
 - EFF: https://app.bde.es/efs_www/home?lang=ES
 - IEF: https://www.ief.es/docs/investigacion/estadistica/PeticionDatos.pdf

### Reproductibility
Once your access to microdata has been granted, if you find any issue reproducing any of these results please contact to: miguel.garcia.duch@ucm.es

