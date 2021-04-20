# This dockerfile, when run, will by default download and process uc data into a feather format.
FROM rocker/tidyverse
RUN apt-get update && apt-get install -y libsodium-dev pkg-config
RUN mkdir /data

COPY unemploymentDataProcessor.R config.yml /Rscripts/
COPY inst /Rscripts/inst

# /data should be a volume mount, where the dataprocessor will write.

RUN install2.r RCurl zoo config googledrive googlesheets4 sodium &&\
    installGithub.r https://github.com/sboysel/fredr.git 
RUN chmod u+x /Rscripts/unemploymentDataProcessor.R
# we need to be working in the Rscripts directory
# so that the processor.R file can find the config.yml file.
WORKDIR /Rscripts
CMD ["Rscript","/Rscripts/unemploymentDataProcessor.R"]
