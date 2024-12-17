FROM rocker/shiny-verse:latest

ENV OFFLINE 1

# copy necessary files
## app folder
COPY . /app
COPY ./www /app/www

RUN R -e 'install.packages("shinythemes")'
RUN R -e 'install.packages("shinyWidgets")'
RUN R -e 'install.packages("shinycssloaders")'
RUN R -e 'install.packages("DT")'
RUN R -e 'install.packages("eeptools")'
RUN R -e 'install.packages("jsonlite")'
RUN R -e 'install.packages("plotly")'
RUN R -e 'install.packages("plotly")'
RUN R -e 'install.packages("glue")'
RUN R -e 'install.packages("dotenv")'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
