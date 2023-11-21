<!-- README.md is generated from README.Rmd. Please edit that file -->

# SIprecisa

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/andreabz/SIprecisa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/andreabz/SIprecisa/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/andreabz/SIprecisa/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/andreabz/SIprecisa/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

Il software SIprecisa è stato sviluppato per ARPAL allo scopo di
calcolare indicatori di precisione e giustezza in misurazioni chimiche e
fisiche di laboratorio.

Il software permette di sottoporre a test sequenziali più di un analita,
riassumendo i risultati in un unico report in formato pdf.

#### Cosa può fare SIprecisa?

- ottenere statistiche relative alla precisione e giustezza di serie di
  valori;
- effettuare un test di normalità (Shapiro-Wilk) sulle serie di valori;
- identificare mediante il test *generalized extreme studentized
  deviate* (GESD) la presenza di potenziali valori anomali;
- escludere manualmente i valori potenzialmente anomali;
- effettuare un test ( $t$-test, Welch test o calcolo dell’ $E_n$) per
  verificare la significatività del bias;
- esportare i risultati in un report .pdf.

#### Di cosa ha bisogno SIprecisa?

Un file .csv con il punto (.) come separatore decimale e la virgola (,)
come separatore di campo. Ogni colonna presente nel file deve avere
un’intestazione testuale ma il numero e la tipologia di colonne dipende
dallo scopo:

<details>
<summary>
se vuoi stimare la ripetibilità e il recupero sulla stessa serie di
misure, clicca qui
</summary>

Ti servirà organizzare il file con:

- una colonna testuale con i nomi degli analiti di interesse;
- una colonna numerica con i valori delle misure.

Ognuna delle due serie potrà avere da un minimo di 6 a un massimo di 30
valori per analita.

Inoltre, nel corso dei calcoli per la determinazione del recupero
servirà fornire il valore riferimento e la sua incertezza estesa.

</details>
<details>
<summary>
se vuoi stimare solo la ripetibilità su una serie di misure duplicate,
clicca qui
</summary>

Ti servirà organizzare il file con:

- una colonna testuale con i nomi degli analiti di interesse;
- una colonna numerica con i valori della prima serie di misure;
- una colonna numerica con i valori della seconda serie di misure;

Serviranno da un minimo di 8 a un massimo di 30 coppie di valori per
analita.

</details>
<details>
<summary>
se vuoi stimare il recupero di una singola misura, clicca qui
</summary>

Ti servirà organizzare il file con:

- una colonna testuale con i nomi degli analiti di interesse;
- una colonna numerica con i valori delle misure;
- una colonna numerica con l’incertezza estesa associata alla misure.

Ogni serie potrà avere un solo valore per analita.

Inoltre, nel corso dei calcoli per la determinazione del recupero
servirà fornire il valore riferimento e la sua incertezza estesa.

</details>

<br>

#### Come sono stati scelti i test di SIprecisa?

Il test per verificare la normalità di una serie di dati è stato
implementato utilizzando la funzione `shapiro.test` della libreria
`stats` del software R. L’implementazione risulta coerente con quanto
riportato nella norma ISO 5479:1997 e in grado di replicare i risultati
contenuti nell’esempio 1 della sezione 4 (pag. 606) dell’articolo
Shapiro, S. S. e Wilk, M. B. (1965). *An analysis of variance test for
normality (complete samples)*, Biometrika, *52*, 3 and 2. doi:
[10.2307/2333709](https://doi.org/10.2307/2333709)

Il test *generalized extreme studentized deviate* (GESD) per
l’identificazione dei valori anomali è stato implementato sulla base del
paragrafo 4.3 e dell’Annex A della norma UNI ISO 16269-4:2019. Il test è
stato in grado di replicare i risultati riportati come esempio nella
medesima norma (pag. 15).

I test per il confronto tra due medie e tra una media e un valore noto
sono stati implementati utilizzando la funzione `t.test` della libreria
`stats` del software R. L’implementazione risulta coerente e in grado di
replicare i risultati riportati nella norma UNI ISO 2854:1988 (prospetti
B’, C’ e D’).

Il test per il confronto di due valori dotati di incertezza estesa, il
calcolo dell’ $E_n$ è stato implementato sulla base di quanto riportato
nella norma ISO 13528:2022, al paragrafo 9.7. Il calcolo implementato è
stato in grado di replicare i risultati riportati nella sezione E.4.
della medesima norma.

#### Come funziona SIprecisa?

L’applicazione è suddivisa in quattro schede con cui l’utente deve
interagire in sequenza. Ogni scheda possiede un pulsante *Avanti*
posizionato in basso a sinistra, una volta cliccato su di esso e
confermata la propria scelta, l’utente non potrà tornare indietro.

<details>
<summary>
Dalla scheda <b> Scopo </b>, clicca qui
</summary>

1.  selezionare una delle opzioni disponibili;
2.  leggere le istruzioni nella parte a destra dello schermo;
3.  cliccare su *Avanti* e confermare la propria scelta.

</details>
<details>
<summary>
Dalla scheda <b> Dati </b>, clicca qui
</summary>

1.  leggere le istruzioni nella parte destra dello schermo;
2.  caricare il file .csv;
3.  controllare e selezionare le variabili di interesse nei menù a
    tendina;
4.  cliccare su *Avanti* e confermare la propria scelta.

</details>
<details>
<summary>
Dalla scheda <b> Stime </b>, clicca qui
</summary>

1.  leggere le istruzioni nella parte a destra dello schermo;
2.  selezionare il parametro di interesse;
3.  digitare le unità di misura;
4.  digitare le eventuali altre informazioni richieste e, se presente,
    cliccare *Calcola*;
5.  specificare il livello di confidenza desiderato;
6.  visualizzare il grafici e le statistiche di base;
7.  eventualmente rimuovere dei punti cliccando su di essi;
8.  visualizzare le prestazioni di precisioni e giustezza spostandosi
    tra le schede nella parte destra dello schermo;
9.  cliccare su *Salva* per salvare il risultato;
10. ripetere i punti dal 2. al 9. per tutti gli analiti di interesse;
11. cliccare su *Avanti* e confermare la propria scelta.

Nel caso si voglia modificare un risultato già salvato:

1.  accedere al menù a tendina in alto;
2.  selezionare il parametro di interesse;
3.  cliccare su *Cancella*;
4.  fare le modifiche volute;
5.  cliccare su *Salva*;
6.  seguire i punti 10. e 11. dell’elenco puntato precedente.

</details>
<details>
<summary>
Dalla scheda <b> Report </b>, clicca qui
</summary>

1.  completare i campi con le informazioni accessorie;
2.  selezionare le sezioni da includere nel report;
3.  cliccare su *Crea il report*;
4.  aspettare che il file .pdf compaia tra i file scaricati.

</details>

<br>

#### SIprecisa è validato?

SIprecisa è basato sull’ambiente software R e molte delle sue funzioni
sono utilizzate in ambito professionale da milioni di persone, da circa
30 anni.

La correttezza dei risultati forniti dalle funzioni impiegate da
SIprecisa, l’interazione tra le funzioni e la stabilità dell’interfaccia
utente, sono oggetto di oltre 450 test. Tali test sono eseguiti in
automatico a ogni nuovo rilascio di versione. La frazione di codice
coperta dai test è circa il 93% del totale. L’esito dei controlli e la
percentuale di codice coperta dai test per l’ultima versione rilasciata,
è riassunto nelle etichette aggiornate automaticamente e presenti a
inizio di questo file.

Si consiglia, inoltre, di rendere disponibile SIprecisa attraverso un
server Linux ad accesso controllato o distribuirlo mediante *docker*.

A ogni modo, è sempre meglio rimanere allerta: [segnala eventuali
bachi](https://github.com/andreabz/SIconfronta/issues).

#### Con quale licenza è rilasciato SIprecisa?

Con la Affero GPL versione 3.

[![gplv3](https://www.gnu.org/graphics/agplv3-with-text-100x42.png)](https://www.gnu.org/graphics/agplv3-with-text-100x42.png)

Il testo integrale della licenza è disponibile a questo indirizzo:
<https://www.gnu.org/licenses/agpl-3.0.en.html#license-text>.

Copyright (C) 2023, Andrea Bazzano, <andrea.bazzano@arpal.liguria.it>.
