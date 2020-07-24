# Unemployment Insurance Data Explorer

An explorer of UI data, which can be downlaoded from here: https://ows.doleta.gov/unemploy

This data concerns the administration of the UI program in each state.

Specifically, this focuses on data regarding payment and decision timelapses, to ensure that states are holding up their end of the bargain in promptly paying claimants or at least adjudicating their cases promptly.

## Acknowledgments

This product uses the FRED® API but is not endorsed or certified by the Federal Reserve Bank of St. Louis. See the FRED API's terms of use: https://research.stlouisfed.org/docs/api/terms_of_use.html

## Docker

The Docker image and Compose file described here run the data processor script and download unemployment data.

The Image downloads the data to a directory, `/data` in the running container. The docker-compose file maps this as a bind mount volume, so that tthe data ends up in your local filesystem.

Run it with:
`docker-compose run --rm datadownload`

# Setting up workflows

We use github actions/workflows/jobs (there are a lot of different terms!) to download the data.
