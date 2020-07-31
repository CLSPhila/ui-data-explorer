# Unemployment Insurance Data Explorer

An explorer of UI data, which can be downlaoded from here: https://ows.doleta.gov/unemploy

This data concerns the administration of the UI program in each state.

Specifically, this focuses on data regarding payment and decision timelapses, to ensure that states are holding up their end of the bargain in promptly paying claimants or at least adjudicating their cases promptly.

## Acknowledgments

This product uses the FRED® API but is not endorsed or certified by the Federal Reserve Bank of St. Louis. See the FRED API's terms of use: https://research.stlouisfed.org/docs/api/terms_of_use.html

## Viewing the app

You can view the app here: https://clsphila.shinyapps.io/ui-data-explorer/

## Getting the data: 

Data is released here: https://github.com/CLSPhila/ui-data-explorer/releases/tag/v0

## Developing 

The code in this repository produces two different things: a package of processed unemployment compensation data an


### Publishing the data


### Publishing the app


## Docker

The Docker image and Compose file described here run the data processor script and download unemployment data.

The Image downloads the data to a directory, `/data` in the running container. The docker-compose file maps this as a bind mount volume, so that tthe data ends up in your local filesystem.

Run it with:
`docker-compose run --rm datadownload`

# Configuaration

There are two different configuration files.

The config.yml file is for configuration values that aren't secrets. The configurable locations of the data directory and the project root are in here.

If you're running the data processor, or deploying the app, you'll also need to set some environment variables. The easiest way to do that is with a `.env` file, the library `dotenv`, and the command `dotenv::load_env()` to load the `.env` file into the environment. 

See `.env.example` for the secrets you need to set to process data and deploy the app.

# Running locally

There are a number of environment variables you need to set up. See `.env.example`.

# Setting up workflows

We use github actions/workflows/jobs (there are a lot of different terms!) to download the data.

# Deploying

## Shinyapps.io

We can host the app on shinyapps.io. To do this make sure the app runs locally for you (this means all the data must be local). Then deploy with

```
rsconnect::setAccountInfo(name='...',token='...',secret='...')
rsconnect::deployApp(".",appFileManifest='./filemanifest.txt')
```

Get the SetAccountInfo information from your shinyapps.io account.
