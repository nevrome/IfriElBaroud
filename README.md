<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Last-changedate](https://img.shields.io/badge/last%20change-2018--12--27-brightgreen.svg)](https://github.com/nevrome/IfriElBaroud/commits/master)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-brightgreen.svg)](https://cran.r-project.org/)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
[![ORCiD](https://img.shields.io/badge/ORCiD-0000--0003--3448--5715-green.svg)](http://orcid.org/0000-0003-3448-5715)

Research compendium for a contribution to ‘Human Occupation and Environmental Change in the Western Maghreb during the Last Glacial Maximum (LGM) and the Late Glacial. New Evidence from the Iberomaurusian Site Ifri El Baroud (North-east Morocco)’
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### Compendium DOI:

<a href="http://dx.doi.org/" class="uri">http://dx.doi.org/</a>…

The files at the URL above will generate the results as found in the
publication. The files hosted at
<a href="https://github.com/nevrome/IfriElBaroud" class="uri">https://github.com/nevrome/IfriElBaroud</a>
are the development versions and may have changed since the report was
published

### Author of this repository:

Clemens Schmid
(<a href="mailto:clemens@nevrome.de" class="email">clemens@nevrome.de</a>)

### Published in:

Not yet published.

### Overview of contents:

This repository contains code and data for a small contribution to the
paper concerning excavation data analysis. The context is explained in
the following paragraph taken from the *Materials and methods* section:

> The three-dimensional architecture of the main stratigraphic units and
> their limits were inferred from a combination of field data with
> information deduced posteriori through a 3D regression analysis. Even
> the macro units were at times difficult to trace during the
> excavation, but they became very obviously visible in the final
> profile analysis at the end of the campaign. To retrieve this
> information and make it again useful to understand the stratigraphic
> attribution of the artificial excavation squares, we used surface
> reconstruction via kriging to extrapolate the macro-unit borders from
> the profiles over the extent of the narrow trench. A semiautomatic
> algorithm was employed to calculate the degree of membership of every
> square to every macro-unit. This method allowed to cross-check the
> correlation between units and artificial squares, and to identify the
> border cases where the correlation was not completely clear. These are
> of questionable value for chronotypological analysis and had to be
> singled out for careful assessment. The code for the semiautomatic
> square allocation is available in a R package on CRAN
> (<a href="https://CRAN.R-project.org/package=recexcavAAR" class="uri">https://CRAN.R-project.org/package=recexcavAAR</a>)
> along with a technical description of the process in a vignette.

The `data/` directory contains elevation data measured on surfaces and
profiles within the trench (`border_* & level_*`) as well as the corner
positions of the excavation squares (`corner_excavation_squares`). All
coordinates refer to the cave’s interal grid system established for the
campaign in 2015. The `code/` directory contains the code for the
`attribution_calculation` and a 3D `visualisation` of the results. The
latter are stored in `output/`, though only in one possible solution as
not all recexcavAAR algorithms are entirely deterministic. The
`attribution.csv` table stores the degree of membership for each
artificial square to each archaeological horizon and is therefore the
main result of this analysis.

### How to reproduce:

A Docker image is a lightweight GNU/Linux virtual computer that can be
run as a piece of software on Windows and OSX (and other Linux systems).
To capture the complete computational environment used for this project
we have a Dockerfile that specifies how to make the Docker image that we
developed this project in. The Docker image includes all of the software
dependencies needed to run the code in this project, as well as the R
package and other compendium files. To launch the Docker image for this
project, first, [install Docker](https://docs.docker.com/installation/)
on your computer. At the Docker prompt, enter:

    docker run -dp 8787:8787 nevrome/IfriElBaroud

This will start a server instance of RStudio. Then open your web browser
at localhost:8787 or or run `docker-machine ip default` in the shell to
find the correct IP address, and log in with rstudio/rstudio.

Once logged in, use the Files pane (bottom right) to navigate to `/`
(the root directory), then open the folder for this project, and open
the `.Rproj` file for this project. Once that’s open, you’ll see the
`analysis/paper` directory in the Files pane where you can find the R
markdown document, and knit them to produce the results in the paper.
More information about using RStudio in Docker is avaiable at the
[Rocker](https://github.com/rocker-org)
[wiki](https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image)
pages.

We developed and tested the package on this Docker container, so this is
the only platform that We’re confident it works on, and so recommend to
anyone wanting to use this package to generate the vignette, etc.

### Licenses:

Manuscript: CC-BY-4.0
<a href="http://creativecommons.org/licenses/by/4.0/" class="uri">http://creativecommons.org/licenses/by/4.0/</a>

Code: MIT
<a href="http://opensource.org/licenses/MIT" class="uri">http://opensource.org/licenses/MIT</a>
year: 2016, copyright holder: Clemens Schmid

Data: CC0
<a href="http://creativecommons.org/publicdomain/zero/1.0/" class="uri">http://creativecommons.org/publicdomain/zero/1.0/</a>
attribution requested in reuse
