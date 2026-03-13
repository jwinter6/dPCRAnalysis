FROM rocker/shiny:4.4.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny','shinydashboard','bslib','shinyWidgets','DT','plotly','ggplot2','dplyr','tidyr','readr','tibble','testthat','dpcR'), repos='https://cloud.r-project.org')"

COPY . /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
