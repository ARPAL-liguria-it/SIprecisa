FROM rocker/r-ver:4.3.3

RUN /rocker_scripts/setup_R.sh https://packagemanager.posit.co/cran/__linux__/jammy/latest
RUN echo "\noptions(shiny.port=3838, shiny.host='0.0.0.0')" >> /usr/local/lib/R/etc/Rprofile.site

# system libraries of general use
RUN apt-get update -y && apt-get install -y \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    libxt-dev \
    make \
    zlib1g-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    git \
    libicu-dev \
    && rm -rf /var/lib/apt/lists/*
    
# pin renv version
ENV RENV_VERSION="1.0.7"
RUN R -q -e "options(warn=2); install.packages('remotes')"
RUN R -q -e "options(warn=2); remotes::install_version('renv', '${RENV_VERSION}')"

# install R dependencies
# do this before copying the app-code, to ensure this layer is cached
WORKDIR /build
COPY SIprecisa/renv.lock /build/renv.lock
RUN R -q -e 'options(warn=2); renv::restore(library = .libPaths())'

# install TinyTex and latex dependencies
ENV PATH="$PATH:/usr/local/texlive/bin/linux"
ENV CTAN_REPO="https://mirror.ctan.org/systems/texlive/tlnet"
RUN /rocker_scripts/install_texlive.sh
RUN tlmgr install \
    koma-script \
    graphics \
    babel-italian \
    hyphen-italian \
    microtype \
    tex-gyre \
    fancyhdr \
    lastpage \
    booktabs

# install R code
COPY SIprecisa /build/SIprecisa
RUN R CMD INSTALL /build/SIprecisa

RUN groupadd -g 1000 shiny && useradd -c 'shiny' -u 1000 -g 1000 -m -d /home/shiny -s /sbin/nologin shiny
USER shiny

EXPOSE 3838

CMD R -e "SIprecisa::run_app()"
