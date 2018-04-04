FROM rocker/shiny

# Add application codebase
ADD ./ /srv/shiny-server/price_optimization
RUN chmod a+rwx -R /srv/shiny-server/price_optimization

# Temporal fix for RStan package
# http://discourse.mc-stan.org/t/error-when-installing-rstan-2-16-2/1730
# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux
RUN echo "\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function" >> /etc/R/Makeconf
RUN echo "\nCXXFLAGS+=-DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION" >> /etc/R/Makeconf
RUN echo "\nCPPFLAGS+=-DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION" >> /etc/R/Makeconf

# Install additional Ubuntu packages
RUN sudo apt-get update && sudo apt-get -y install libssl-dev
#RUN sudo apt-get -y install libfftw3-3 libfftw3-bin libfftw3-dev libfftw3-double3

# Install additional R packages
RUN R -e "install.packages(c('shiny', 'shinyjs', 'plotly', 'DT', 'dygraphs', 'xts', 'Matrix'))"
RUN R -e "install.packages(c('data.table', 'dplyr', 'plotly', 'ggplot2', 'prophet', 'lme4'))"
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/shinydashboard/shinydashboard_0.5.3.tar.gz', repos=NULL, type='source')"
