FROM rocker/tidyverse

COPY unemploymentDataProcessor.R /Rscripts/

# R CMD INSTALL ??

# /data should be a volume mount, where the dataprocessor will write.
RUN mkdir /data

CMD ["RScript","/Rscripts/unemploymentDataProcessor.R"]
