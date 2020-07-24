FROM rocker/tidyverse
RUN mkdir /data

COPY unemploymentDataProcessor.R /Rscripts/
COPY data/EUC-1982-1987-USDOLData.csv /data/
# R CMD INSTALL ??

# /data should be a volume mount, where the dataprocessor will write.

RUN install2.r RCurl zoo
RUN installGithub.r https://github.com/sboysel/fredr.git 
CMD ["Rscript","/Rscripts/unemploymentDataProcessor.R"]
