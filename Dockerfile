FROM rocker/tidyverse

COPY unemploymentDataProcessor.R /Rscripts/

# R CMD INSTALL ??

# /data should be a volume mount, where the dataprocessor will write.
RUN mkdir /data

RUN install2.r RCurl zoo
RUN install2.r fredr
CMD ["Rscript","/Rscripts/unemploymentDataProcessor.R"]
