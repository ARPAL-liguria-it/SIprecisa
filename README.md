# SIprecisa

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://github.com/andreabz/SIprecisa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/andreabz/SIprecisa/actions/workflows/R-CMD-check.yaml) [![test-coverage](https://github.com/andreabz/SIprecisa/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/andreabz/SIprecisa/actions/workflows/test-coverage.yaml)

<!-- badges: end -->

SIprecisa è stato sviluppato per ARPAL allo scopo di determinare parametri prestazionali di precisione e giustezza per serie di misure indipendenti e coppie di misure indipendenti.

Il software permette di sottoporre a test sequenziali più di un analita, riassumendo i risultati in un unico report in formato pdf.

Per maggiori informazioni a riguardo delle funzionalità e caratteristiche di SIprecisa, si rimanda al [file di informazioni](https://github.com/andreabz/SIprecisa/blob/shinyproxy/SIprecisa/README.md) contenuto nella sottocartella `SIprecisa`.

#### Installazione e utilizzo mediante docker e shinyproxy

1.  installare *docker* seguendo le istruzioni specifiche per il sistema operativo in uso (ad esempio, per [Ubuntu](https://docs.docker.com/engine/install/ubuntu/))

2.  creare nella propria `home` la cartella `shinyproxy` e scaricare al suo interno il [Dockerfile](https://github.com/openanalytics/shinyproxy-config-examples/blob/master/02-containerized-docker-engine/Dockerfile) e l'[application.yml](https://github.com/openanalytics/shinyproxy-config-examples/blob/master/02-containerized-docker-engine/application.yml) di esempio sviluppati dagli sviluppatori di *shinyproxy*.

3.  creare la rete attraverso cui i *container* di *docker* comunicheranno

    ``` bash
    sudo docker network create sp-example-net
    ```

4.  aggiungere nella sezione `specs` del file `application.yml` le informazioni relative a *SIprecisa*

    ``` bash
      - id: SIprecisa
        container-cmd: ["R", "-e", "SIprecisa::run_app()"]
        container-image: siprecisa:latest
        container-network: sp-example-net
    ```

5.  dalla cartella `shinyproxy` si crea l'immagine che *docker* utilizzerà per avviare *shinyproxy*

    ``` bash
    sudo docker build . -t shinyproxy
    ```

6.  scaricare il ramo `shinyproxy` del repository di [*SIprecisa*](https://github.com/andreabz/SIprecisa/tree/shinyproxy)

7.  dalla cartella `SIprecisa` si crea l'immagine che *shinyproxy* utilizzerà per avviare *SIprecisa*

    ``` bash
    docker build -f Dockerfile --progress=plain -t siprecisa:latest .
    ```

8.  avviare *shinyproxy*

    ``` bash
    docker run --detach -v /var/run/docker.sock:/var/run/docker.sock:ro --group-add $(getent group docker | cut -d: -f3) --net sp-example-net -p 8080:8080 shinyproxy
    ```

9.  aprire un browser web all'indirizzo <http://localhost:8080/> e inserire le seguenti credenziali

    ``` bash
    nome utente: jack
    password: password
    ```

10. selezionare `SIprecisa` e seguire le istruzioni e la documentazione.

[Istruzioni](https://www.shinyproxy.io/documentation/deployment/#containerized-shinyproxy) più complete e maggiori possibilità di [personalizzazione](https://www.shinyproxy.io/documentation/configuration/) sono disponibili sul sito web del progetto [*shinyproxy*](https://www.shinyproxy.io/).
