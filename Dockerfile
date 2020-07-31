# This dockerfile, when run, will by default download and process uc data into a feather format.
FROM rocker/tidyverse
RUN mkdir /data

COPY unemploymentDataProcessor.R /Rscripts/

# /data should be a volume mount, where the dataprocessor will write.

RUN install2.r RCurl zoo &&\
    installGithub.r https://github.com/sboysel/fredr.git 
RUN chmod u+x /Rscripts/unemploymentDataProcessor.R
CMD ["Rscript","/Rscripts/unemploymentDataProcessor.R"]
