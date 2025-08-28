# Spatial-ecology

An R Shiny application for analyzing animal tracking data with the following main features:
* Load data from your computer locally or from [Movebank](https://www.movebank.org/)
* Calculate and plot spatial extent of animal location data using Minimum Convex Polygon (MCP)
* Calculate and plot area dimension using Brownian Bridge Markov Model (BBMM)
* Save output files locally

[Application user documentation](https://github.com/sinkovit/Spatial-ecology/wiki/Space-Use-Estimator-Application-User-Guide) can be found on our Wiki

We aim to follow the [Google style guide](https://google.github.io/styleguide/Rguide.html) but be aware that there are legacy code that don't follow this convention. 

## Requirements
* R version 4.5.0 (may work in earlier versions)
* [Stadia Maps API key](https://stadiamaps.com) (if you want to see the plot on a map)

## How to use

There are currently 2 ways to run our application.  You can choose the method that works best for you...

### 1. Container

We have made our application available to be used in a container. This allows you to run your own container with all the required software already installed.

#### 1.1 Requirements

1. Docker version 27.2 (other versions may work)
1. Stadia Maps key - you must have a Stadia Maps key to plot.  There is a free tier. Instructions for setting up your key can be found [here](https://docs.stadiamaps.com/authentication/#api-keys).

#### 1.2 Setup

1. Download the image from ghcr.io/matthewwangg/spatial-ecology-gateway-dev:latest   
   `docker pull ghcr.io/matthewwangg/spatial-ecology-gateway-dev:latest`
1. Save your Stadmap Maps key into a file with the syntax `STADIA_MAPS_API_KEY=<your key>`
1. Create & run the container   
   `docker run --env-file <filename_from_step_above> -p 3838:3838 ghcr.io/matthewwangg/spatial-ecology-gateway-dev`
1. Enter `http://localhost:3838` into your browser and the app should appear

### 2. RStudio

#### 2.1 Requirements

1.  [RStudio](https://posit.co/products/open-source/rstudio/?sid=1) 2025-05-* (other versions will probably work also)

#### 2.2 Setup

1. Download and install RStudio on your machine
2. Download our code.  The main branch contains the last stable release and the dev branch contains work-in-progress.
3. Run the bin/app.R in your RStudio
4. RStudio should automatically pull in the packages our application needs.

### Support
This project's funding officially ended and we are providing minimum support as we look for additional funding.  If you need support, please submit a [GitHub support ticket](https://github.com/sinkovit/Spatial-ecology/issues/new) and we will get back to you as soon as possible; thanks for your patience. 
