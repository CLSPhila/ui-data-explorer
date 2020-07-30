# This dockerfile, when run, will by default download and process uc data into a feather format.
FROM rocker/tidyverse
RUN mkdir /data

COPY unemploymentDataProcessor.R app.R filemanifest.txt helper.R deployShinyApps.R /Rscripts/
COPY data /data/

# /data should be a volume mount, where the dataprocessor will write.

RUN install2.r RCurl zoo rsconnect && \
    installGithub.r https://github.com/sboysel/fredr.git 
RUN chmod u+x /Rscripts/unemploymentDataProcessor.R && \
    chmod u+x /Rscripts/deployShinyApps.R
CMD ["Rscript","/Rscripts/unemploymentDataProcessor.R"]
