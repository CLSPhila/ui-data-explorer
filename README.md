# Unemployment Insurance Data Explorer

An explorer of Unemployment Insurance data, which can be downlaoded from here: https://oui.doleta.gov/unemploy

This data concerns the administration of the UI program in each state.

Specifically, this focuses on data regarding payment and decision timelapses, to ensure that states are holding up their end of the bargain in promptly paying claimants or at least adjudicating their cases promptly.

## Acknowledgments

This product uses the FRED® API but is not endorsed or certified by the Federal Reserve Bank of St. Louis. See the FRED API's terms of use: https://research.stlouisfed.org/docs/api/terms_of_use.html

This project was originally created by [Michael Hollander](mailto:hollander@gmail.com) and [Community Legal Services](https://clsphila.org) in 2017. It has been updated and refreshed through a collaboration between [The Century Foundation](https://tcf.org), Community Legal Services, and Michael Hollander.

![CLS/TCF Logo](https://raw.githubusercontent.com/tcf-ui-data/ui-data-explorer/main/www/CLS-Logo_TCF.png)

## Viewing the web app.

You can explore the unemployment insurance data we've compiled though an interactive website with an array of charts and maps.

You can view the app here: https://tcf-ui-data.shinyapps.io/ui-data-explorer/ 

## Getting the data:

You can download the data for your own analysis as well.

Data is released here as a `parquet` file for use with R, python, and other programming languages. https://github.com/tcf-ui-data/ui-data-explorer/releases/tag/uiExplorerData

You can also download the data as a collection of `csv` tables. You can use these tables in your favorite statistical package as well as Excel, LibreOffice Calc, or other spreadsheet app. https://github.com/tcf-ui-data/ui-data-explorer/releases/tag/uiExplorerCSV

## Running locally and developing

This project is open source, and we value contributions.

The code in this repository produces two different things: a package of processed unemployment compensation data and a Shiny webapp for visualizing and interacting with the data.

### Downloading and processing the data

You can download and process the data on your own computer. Download this repository with `git clone`. Run `Rscripts unemploymentDataProcessor.R` to download and process data into a variety of useful tables.

You can also use docker-compose to download and process the data with `docker-compose run -rm datadownload`

### Publishing the data

We can publish the data in two ways.

**Locally**

1. Locally clone the repository and run `Rscripts unemploymentDataProcessor.R`.
2. Load a github token into your shell's environment. A `.env` file is helpful here.
3. Run `. ./updateRelease.sh` to update the released data on Github. The script accepts a few command line arguments. See the script for the details.

**Github Actions**
The Github workflow described in the file `.github/workflows/releasedata` will automatically process and publish the data.

### Running the app.

Once you've managed to download the data, you can use RStudio to run the app. The app is a Shiny application, so RStudio can help you install the necessary packages and get the app running on your computer.

### Publishing the app

We can publish the app in three ways.

**Locally in RStudio**
Once you have the app running locally, use the `rsconnect` library from Shinyapps.io. Set environment variables for `SHINYAPPS_ACCOUNT`, `SHINYAPPS_TOKEN`, and `SHINYAPPS_SECRET`. You can get these values from your shinyapps account. Then run `Rscripts deployShinyApps.R`.

**Locally with Docker-Compose**.
Running `docker-compose run --rm shinyappdeploy` will start a docker service that downloads and processes the data, and publishes the app to Shinyapps. Make sure your `SHINYAPPS_x` environment variables are set up. Docker-compose will automatically load a `.env` file if there is one.

**Github Actions**
The Github workflow `.github/workflows/deployshinyio` describes a workflow that processes the data and publishes to Shinyapps. The workflow `.github/workflows/deployshinyiofromrelease` describes a workflow that uses the released parquet data to publish to shinyapps.

To use these workflows, you'll need to add your shiny token, account, and secret as Github Secrets.
