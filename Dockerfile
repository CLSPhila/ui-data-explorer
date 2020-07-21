FROM rocker/tidyverse

COPY unemploymentDataProcessor.R /Rscripts/
COPY EUC-1982-1987-USDOLData.csv /Rscripts/
# R CMD INSTALL ??

# /data should be a volume mount, where the dataprocessor will write.
RUN mkdir /data

RUN install2.r RCurl zoo
RUN installGithub.r https://github.com/sboysel/fredr.git 
CMD ["Rscript","/Rscripts/unemploymentDataProcessor.R"]
