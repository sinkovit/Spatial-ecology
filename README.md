# Spatial-ecology
This space use software is deployed in the [Spatial Ecology Gateway](https://uccommunityhub.hubzero.org/groups/spaceuseecology)

* User documentation can be found [in the gateway](https://uccommunityhub.hubzero.org/groups/spaceuseecology/space-use-estimator-doc).
* The structure of this repo matches HUBZero's tool's register structure
requirement (see [Nano Hub's template](https://github.com/nanohub-apps/hubtooljupytertemplate)).
* Original code is in the "Bob" branch.
* We aim to follow the [Google style guide](https://google.github.io/styleguide/Rguide.html) but be aware that there are legacy code that don't follow this convention. 

## Container
We are making our space use software available to be used in a container. This allows you to run your own container with all the required software already installed.

### Requirements
1. Docker version ? 
1. Stadia Maps key - you must have a Stadia Maps key to plot.  There is a free tier. Instructions for setting up your key can be found [here](https://docs.stadiamaps.com/authentication/#api-keys).

### Setup
1. Download the image from ghcr.io/matthewwangg/spatial-ecology-gateway-dev:latest
1. Save your Stadmap Maps key into a file with the syntax STADIA_MAPS_API_KEY=<your key>
1. Create & run the container docker run --env-file <filename from above> -p 3838:3838 ghcr.io/matthewwangg/spatial-ecology-gateway-dev
1. Enter "http://localhost:3838" into your browser and the app should appear

## Support
We are providing minimum support during project's funding gap.  If you need support, please submit a [support ticket](https://uccommunityhub.hubzero.org/support) and we will get back to you as soon as possible; thanks for your patience. 
