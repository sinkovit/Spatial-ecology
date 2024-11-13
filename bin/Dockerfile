# Use the Rocker Project's R version 4.3 image
FROM rocker/r-ver:4.4.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libsodium-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Set the default CRAN mirror and prefer binaries
RUN echo 'options(repos = c(CRAN = "https://cloud.r-project.org"), install.packages.check.source = "no")' >>"${R_HOME}/etc/Rprofile.site"

# Install R packages via install2.r (using binaries)
RUN install2.r --error --skipinstalled -n -1 \
    keyring \
    readr \
    shiny \
    R.utils \
    shinyjs \
    shinycssloaders \
    DT \
    stringr \
    shinydashboard \
    shinyBS \
    shinyFiles \
    tools \
    mkde \ 
    move2 \
    sf 

# Install additional R packages using binaries with Rscript
RUN Rscript -e "options(install.packages.check.source = 'no'); install.packages(c('ggplot2', 'ggmap', 'terra'))"

RUN apt-get update && \
    apt-get remove -y linux-libc-dev && \
    apt-get install -y libgomp1 libproj22 libgdal30 && \
    apt-get purge -y 'mysql*' 'libmysql*' 'openjdk*' 'bslib*' 'libpixman*' && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN rm -rf /usr/local/lib/R/site-library/bslib/lib/bs4

# Set the working directory inside the Docker container
WORKDIR /srv/shiny-server/myapp

# Copy the Shiny app code into the Docker image
COPY app.R cleanUTMZones.R loadDataframe.R processDataframe.R compute.R util.R /srv/shiny-server/myapp/

# Expose port 3838 to run Shiny applications
EXPOSE 3838

# Command to run Shiny app when container starts
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/myapp', host='0.0.0.0', port=3838)"]
