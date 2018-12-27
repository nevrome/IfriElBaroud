FROM rocker/tidyverse:3.5.0

WORKDIR "home/rstudio"

COPY ./data/ ./data/
COPY ./code/ ./code/
COPY ./output/ ./output/

RUN R -e "install.packages('automagic')"
RUN R -e "automagic::automagic()"

RUN find . -type d -exec chmod 777 {} \;
RUN find . -type f -exec chmod 777 {} \;