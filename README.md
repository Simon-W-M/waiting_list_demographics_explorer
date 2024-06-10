# Waiting List Demographics Explorer
## NHS England Elective Recovery Insights Cell

### About the Project

{ADD EXPERIMENTAL STATUS BADGE IF PROJECT IS IN DEVELOPMENT}
[![status: experimental](https://github.com/GIScience/badges/raw/master/status/experimental.svg)](https://github.com/GIScience/badges#experimental)

This repository holds code for the Waiting List Explorer Tool

_**Note:** Only public or fake data are shared in this repository._

### Project Stucture

Code is split into two sections.

One is a pipeline to injest and process the Waiting list minimum dataset (WLMSDS) from UDAL and perform pre processing.

This processed data is then fed into a shiny tool for visualisation.

### Built With

{LIST SOFTWARE USED TO CREATE PROJECT}

[R Studio](http://www.rstudio.com/.)  
[R Statistical Software](https://www.R-project.org/.)  
[SQL SSMS](https://learn.microsoft.com/en-us/sql/ssms/download-sql-server-management-studio-ssms?view=sql-server-ver16)  


- {LIST OF MAIN PACKAGE VERSIONS}
Data processing
"tidyverse", "odbc",  "DBI", "glue","janitor",   "cluster", "data.table", "mltools", "cli") 
Shiny tool
shiny, tidyverse, reactable, reactablefmtr, tippy, shinythemes, shinyalert

### Getting Started

#### Installation

To get a local copy up and running follow these simple steps.

To clone the repo:

`git clone https://github.com/nhengland/waiting_list_demographics_explorer`



### Usage
data_injestion_preprossesing runs the injesting and pre processing process

Data is statsicially tested using Fisher's exact test to compare statisical differences between groups of short and long waits.  Long waits are calulated at 52, 72 amd 104 week cohorts.
Chi square tests are also run in the background to check conformity.

Where there are small groups, these are excluded from the analaysis.

Local ICB level populatons across demographic grousp are also created, this is to benchmark size of lists against other ICBs.

Cluster analsysis using Gower distance is carried out across categorical demographic features



#### Outputs
app.r
This is the shiny app that takes the processed data and turns it into an interactive visualisation



#### Datasets
This utilises the Waiting List Minimum Dataset(WLMDS)
It also uses a SUS ethnicity table to improve DQ around ethnicity


### Roadmap

Additional analysis around trend is to be introduced.

### Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any contributions you make are **greatly appreciated**.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

_See [CONTRIBUTING.md](./CONTRIBUTING.md) for detailed guidance._

### License

Unless stated otherwise, the codebase is released under [the MIT Licence][mit].
This covers both the codebase and any sample code in the documentation.

_See [LICENSE](./LICENSE) for more information._

The documentation is [© Crown copyright][copyright] and available under the terms
of the [Open Government 3.0][ogl] licence.

[mit]: LICENCE
[copyright]: http://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/
[ogl]: http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/

### Contact

To find out more get in touch at [Simon Wellesley-Miller](mailto:simon.wellesley-miller@nhs.net).

<!-- ### Acknowledgements -->

