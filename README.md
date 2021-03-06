<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Last-changedate](https://img.shields.io/badge/last%20change-2019--01--06-brightgreen.svg)](https://github.com/nevrome/IfriElBaroud/commits/master)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-brightgreen.svg)](https://cran.r-project.org/)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
[![ORCiD](https://img.shields.io/badge/ORCiD-0000--0003--3448--5715-green.svg)](http://orcid.org/0000-0003-3448-5715)

## Research compendium for a contribution to ‘Human Occupation and Environmental Change in the Western Maghreb during the Last Glacial Maximum (LGM) and the Late Glacial. New Evidence from the Iberomaurusian Site Ifri El Baroud (North-east Morocco)’

### Compendium DOI:

<http://dx.doi.org/10.17605/OSF.IO/J4VFC>

The files at the URL above will generate the results as found in the
publication. The files hosted at
<https://github.com/nevrome/IfriElBaroud> are the development versions
and may have changed since the report was published

### Author of this repository:

Clemens Schmid (<clemens@nevrome.de>)

### Published in:

Potì, A., Kehl, M., Broich, M., Carrión Marco, Y., Hutterer, R., Jentke, T., Linstädter, J., López-Sáez, J.A., Mikdad, A., Morales, J., Pérez-Díaz, S., Portillo, M., Schmid, C., Vidal-Matutano, P., Weniger, G.-C., 2019. Human occupation and environmental change in the western Maghreb during the Last Glacial Maximum (LGM) and the Late Glacial. New evidence from the Iberomaurusian site Ifri El Baroud (northeast Morocco). _Quaternary Science Reviews_ 220, 87–110. https://doi.org/10.1016/j.quascirev.2019.07.013

### Overview of contents:

This repository contains code and data for a small contribution to the
paper. Further context information can be found there.

For the trench excavated in the Ifri El Baroud during the campaign in
2015, the three-dimensional architecture of the main stratigraphic units
and their limits were inferred from a combination of field data with
information deduced a posteriori through a 3D regression analysis. Even
the archaeological macro units were at times challenging to trace during
the excavation, but they became very obviously visible in the final
profile analysis at the end of the campaign. To retrieve this
information and make it again useful to understand the stratigraphic
attribution of the artificial excavation squares, we used surface
reconstruction via kriging to extrapolate the macro-unit borders from
the profiles over the extent of the narrow trench. A semiautomatic
algorithm was employed to calculate the degree of membership of every
square to every macro-unit. This method allowed to cross-check the
correlation between units and artificial squares, and to identify the
border cases where the correlation was not completely clear. These are
of questionable value for chronotypological analysis and had to be
singled out for careful assessment. The code for the semiautomatic
square allocation is available in this repository and an R package on
CRAN (<https://CRAN.R-project.org/package=recexcavAAR>) along with a
technical description of the process in a vignette.

![](screenshot_trench_3D.png) [You can find an interactive 3D
visualisation here:
https://nevrome.github.io/IfriElBaroud](https://nevrome.github.io/IfriElBaroud/)

The `data/` directory contains elevation data measured on surfaces and
profiles within the trench (`border_* & level_*`) as well as the corner
positions of the excavation squares (`corner_excavation_squares`). All
coordinates refer to the cave’s internal grid system established for the
campaign in 2015. The `code/` directory contains the code for the
`attribution_calculation` and a 3D `visualisation` of the results. The
latter are stored in `output/`, though only in one possible solution as
not all recexcavAAR algorithms are entirely deterministic. The
`attribution.csv` table stores the degree of membership for each
artificial square to each archaeological horizon and is, therefore, the
main result of this analysis.

### How to reproduce:

As the data and code in this repository are complete and self-contained,
it can be reproduced with any R environment (\> version 3.5.0). The
necessary package dependencies are documented in the `deps.yaml` file
and can be installed manually or automatically with
`automagic::install_deps_file()`. If it’s not possible any more to
construct a working environment with these methods due to technological
progress, one can use the Docker image.

A Docker image is a lightweight GNU/Linux virtual computer that can be
run as a piece of software on Windows, Linux, and OSX. To capture the
complete computational environment used for this project we have a
Dockerfile that specifies how to make the Docker image that we developed
this project in. The Docker image includes all of the software
dependencies needed to run the code in this project, including the data
and code itself. To launch the Docker image for this project, first,
[install Docker](https://docs.docker.com/installation/) on your computer
and download the `.tar` file with the ifrielbaroud image
[here](https://osf.io/q6j8p). At the Docker prompt, you can load and run
the image with:

    docker load -i ifrielbaroud_docker_image.tar
    docker run -e PASSWORD=ifrielbaroud -dp 8787:8787 --name ifrielbaroud ifrielbaroud

This will start a server instance of RStudio. Then open your web browser
at localhost:8787 or run `docker-machine ip default` in the shell to
find the correct IP address, and log in with rstudio/ifrielbaroud. Once
logged in, use the Files pane (bottom right) to navigate to the script
files. More information about using RStudio in Docker is available at
the [Rocker](https://github.com/rocker-org)
[wiki](https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image)
pages.

We developed and tested the package on this Docker container, so this is
the only platform that We’re confident it works on. It was built and
stored with:

    docker build -t ifrielbaroud .
    docker save -o ifrielbaroud_docker_image.tar ifrielbaroud

### Licenses:

Code: MIT <http://opensource.org/licenses/MIT> year: 2018, copyright
holder: Clemens Schmid

Data: CC0 <http://creativecommons.org/publicdomain/zero/1.0/>
attribution requested in reuse
