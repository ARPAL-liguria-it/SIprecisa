# SIprecisa

**Shiny application for the evaluation of precision and trueness in analytical measurements.**

SIprecisa è un'applicazione sviluppata in R e Shiny per supportare la valutazione di alcuni parametri di prestazione dei metodi analitici, con particolare riferimento a **precisione** e **trueness (bias)**.

L'applicazione consente di analizzare serie indipendenti di misure, confrontare risultati con valori di riferimento e tenere conto dell'incertezza di misura associata ai valori assegnati. I risultati possono essere raccolti in un report PDF.

Il progetto è sviluppato da **ARPAL – Agenzia Regionale per la Protezione dell'Ambiente Ligure** e utilizza R/Shiny come ambiente per rendere riproducibili e documentare i calcoli statistici alla base dell'analisi.

## A cosa serve

La valutazione delle prestazioni di un metodo analitico richiede di distinguere aspetti diversi della misura.

SIprecisa è stato progettato per affrontare in particolare:

* la **precisione** di serie indipendenti di misure;
* la **trueness** attraverso la valutazione del bias;
* il confronto con un **valore di riferimento**;
* il confronto tra risultati ottenuti in condizioni diverse;
* la valutazione dell'accordo tra risultati tenendo conto delle **incertezze di misura**;
* l'individuazione di eventuali osservazioni anomale prima dell'analisi statistica.

L'applicazione permette inoltre di analizzare più analiti consecutivamente e di raccogliere i risultati in un unico report.

## Cosa può fare

L'applicazione comprende funzioni per:

* statistiche descrittive di base;
* verifica della normalità mediante test di **Shapiro-Wilk**;
* individuazione di potenziali outlier mediante **GESD**;
* esclusione manuale di osservazioni, quando motivata;
* stima di parametri di precisione;
* valutazione della trueness e del bias;
* confronto del bias mediante **t-test**;
* valutazione dell'accordo mediante **En** quando sono disponibili valori di riferimento e relative incertezze;
* generazione automatica di un report PDF.

Le analisi sono organizzate in una sequenza guidata:

**Scopo → Dati → Stime → Report**

Una volta confermata una fase, l'interfaccia non consente di tornare alle fasi precedenti. Questa scelta serve a mantenere esplicita la sequenza con cui vengono definiti lo scopo dell'analisi, i dati e le elaborazioni successive.

## Dati di ingresso

I dati vengono forniti mediante file CSV.

Il formato prevede:

* separatore dei campi: `,`
* separatore decimale: `.`
* una o più serie di misure per analita;
* numero di osservazioni variabile in funzione dell'analisi richiesta.

Per le valutazioni di trueness possono essere forniti:

* un valore di riferimento;
* la relativa incertezza estesa;
* oppure due risultati di misura corredati dalle rispettive incertezze.

L'applicazione supporta quindi diversi scenari sperimentali, che vengono selezionati nella fase iniziale.

## Come sono stati scelti i test statistici

I metodi statistici implementati non sono scelti semplicemente in funzione della disponibilità di una funzione in R, ma sono associati allo specifico problema metrologico o statistico affrontato.

### Normalità

La verifica della normalità utilizza `stats::shapiro.test()`.

L'implementazione è coerente con il metodo descritto in **ISO 5479:1997** e consente inoltre di riprodurre l'esempio riportato nell'articolo originale di Shapiro e Wilk (1965).

### Individuazione di valori anomali

Per l'individuazione di potenziali valori anomali viene utilizzato il metodo **GESD (Generalized Extreme Studentized Deviate)**, con riferimento a **UNI ISO 16269-4:2019**, §4.3 e Allegato A.

L'applicazione distingue l'individuazione statistica di un potenziale outlier dalla decisione di escluderlo dall'analisi: l'esclusione non è quindi una conseguenza automatica del test.

### Precisione

I parametri di precisione vengono calcolati a partire dalla struttura delle serie di misure selezionata dall'utente.

L'obiettivo è fornire stime utilizzabili nella valutazione delle prestazioni del metodo, mantenendo separata la componente di variabilità sperimentale dalle conclusioni che possono essere tratte sul metodo analitico.

### Trueness e bias

La trueness viene valutata attraverso il confronto dei risultati con valori di riferimento.

Quando appropriato, il bias viene valutato mediante `stats::t.test()`, secondo lo schema statistico previsto dal caso analizzato.

L'approccio è coerente con i principi riportati, tra gli altri riferimenti metodologici, in **UNI ISO 2854:1988**.

### Incertezza e En

Quando sono disponibili valori assegnati accompagnati dalla relativa incertezza, l'accordo viene valutato mediante il parametro **En**.

Il calcolo segue quanto previsto dalla **ISO 13528:2022**, §9.7.

L'utilizzo di En permette di tenere conto non soltanto della differenza tra i risultati, ma anche delle incertezze associate ai valori confrontati.

### Riferimenti metodologici

I principali riferimenti utilizzati nello sviluppo sono:

* ISO 5479:1997 — *Statistical interpretation of data — Tests for departure from the normal distribution*;
* UNI ISO 16269-4:2019 — *Interpretazione statistica dei dati — Parte 4: Rilevazione e trattamento dei valori anomali*;
* UNI ISO 2854:1988 — *Interpretazione statistica dei dati — Tecniche di stima e prove di ipotesi relative a medie e varianze*;
* ISO 13528:2022 — *Statistical methods for use in proficiency testing by interlaboratory comparison*;
* Eurachem — *The Fitness for Purpose of Analytical Methods: A Laboratory Guide to Method Validation and Related Topics*.

## Controllo del software

SIprecisa è sviluppato come pacchetto R con struttura **golem** e comprende una suite di test automatici.

I test riguardano sia le funzioni di calcolo sia diversi aspetti dell'applicazione e vengono eseguiti automaticamente attraverso **GitHub Actions**.

Il progetto comprende più di 450 test automatici e una copertura del codice intorno al 93%.

Questi test hanno lo scopo di individuare regressioni e verificare il comportamento atteso del software durante lo sviluppo e la manutenzione.

**La presenza di test automatici e un'elevata code coverage non costituiscono, da soli, una validazione formale del software o del metodo analitico.** L'idoneità di SIprecisa a uno specifico utilizzo deve essere valutata nel contesto operativo e regolamentare in cui viene utilizzato.

Eventuali problemi possono essere segnalati attraverso la sezione [Issues](https://github.com/ARPAL-liguria-it/SIprecisa/issues) del repository.

## Installazione e utilizzo mediante Docker e ShinyProxy

Questa sezione descrive una modalità di installazione utilizzata per distribuire SIprecisa attraverso **Docker** e **ShinyProxy**.

La procedura viene mantenuta nel README anche come documentazione operativa per poter ricostruire l'ambiente di esecuzione dopo periodi prolungati senza manutenzione del sistema.

### 1. Installare Docker

Installare Docker seguendo le istruzioni relative al proprio sistema operativo.

Per Ubuntu:

https://docs.docker.com/engine/install/ubuntu/

### 2. Preparare ShinyProxy

Creare la directory di lavoro:

```bash
mkdir ~/shinyproxy
cd ~/shinyproxy
```

Scaricare il `Dockerfile` e il file `application.yml` di esempio dal repository:

https://github.com/openanalytics/shinyproxy-config-examples

### 3. Creare la rete Docker

```bash
sudo docker network create sp-example-net
```

### 4. Configurare SIprecisa in ShinyProxy

Nel file `application.yml`, aggiungere SIprecisa alla sezione `specs`:

```yaml
- id: SIprecisa
  container-cmd: ["R", "-e", "SIprecisa::run_app()"]
  container-image: siprecisa:latest
  container-network: sp-example-net
```

### 5. Creare l'immagine di ShinyProxy

Dalla directory `shinyproxy`:

```bash
sudo docker build . -t shinyproxy
```

### 6. Scaricare SIprecisa

Clonare il repository e, se necessario, selezionare il branch destinato alla distribuzione mediante ShinyProxy:

```bash
git clone https://github.com/ARPAL-liguria-it/SIprecisa.git
```

### 7. Creare l'immagine Docker di SIprecisa

Dalla directory del repository SIprecisa:

```bash
docker build -f Dockerfile --progress=plain -t siprecisa:latest .
```

### 8. Avviare ShinyProxy

Dalla directory di ShinyProxy:

```bash
docker run --restart=unless-stopped \
  --name shinyproxy \
  -dv /var/run/docker.sock:/var/run/docker.sock:ro \
  --group-add $(getent group docker | cut -d: -f3) \
  --net sp-example-net \
  -p 8080:8080 \
  shinyproxy
```

### 9. Accedere all'applicazione

Aprire:

http://localhost:8080/

Nella configurazione di esempio di ShinyProxy, le credenziali sono:

* **nome utente:** `jack`
* **password:** `password`

Se queste credenziali vengono utilizzate in un ambiente reale, devono essere sostituite con una configurazione di autenticazione appropriata.

### 10. Avviare SIprecisa

Dalla pagina iniziale di ShinyProxy selezionare **SIprecisa** e seguire la procedura guidata dell'applicazione.

### Documentazione ShinyProxy

Per ulteriori informazioni sulla configurazione e sulla distribuzione:

* https://www.shinyproxy.io/documentation/
* https://www.shinyproxy.io/documentation/configuration/

## Stato del progetto

SIprecisa è un'applicazione sviluppata per un contesto operativo di analisi chimiche e sottoposta a sviluppo e manutenzione nel tempo.

Il repository contiene sia il codice dell'applicazione sia gli strumenti necessari per eseguire test automatici e costruire l'ambiente di distribuzione.

L'uso del software deve comunque essere valutato in relazione:

* allo scopo specifico dell'analisi;
* ai dati disponibili;
* alle procedure del laboratorio;
* ai requisiti normativi e di qualità applicabili;
* alle eventuali procedure interne di verifica, validazione o qualificazione del software.

## Licenza

SIprecisa è distribuito secondo i termini della **GNU Affero General Public License v3.0 (AGPL-3.0)**.

## Autore

**Andrea Bazzano**

ORCID: https://orcid.org/0000-0002-9353-3919

Repository:

https://github.com/ARPAL-liguria-it/SIprecisa


10. selezionare `SIprecisa` e seguire le istruzioni e la documentazione.

[Istruzioni](https://www.shinyproxy.io/documentation/deployment/#containerized-shinyproxy) più complete e maggiori possibilità di [personalizzazione](https://www.shinyproxy.io/documentation/configuration/) sono disponibili sul sito web del progetto [*shinyproxy*](https://www.shinyproxy.io/).
