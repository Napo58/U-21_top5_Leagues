# Analisi delle Prestazioni e dei Valori di Mercato dei Calciatori Under-21 Europei

Questo repository contiene uno script in R (`tesi_2023.R`) sviluppato per raccogliere, pulire e analizzare i dati relativi alle prestazioni e alle quotazioni di mercato dei giovani talenti del calcio mondiale (giocatori con meno di 22 anni) nei **Top 5 Campionati Europei** (Serie A, LaLiga, Bundesliga, Premier League, Ligue 1) per la **stagione 2021/2022**.

Il progetto si articola attraverso tecniche di web scraping, pulizia approfondita dei dati, Alberi di Regressione e Algoritmi di Clustering (K-Means e Gerarchico).

---

## 📌 Obiettivi del Progetto

* **Data Scraping & Integration:** Estrazione automatica e unione dei dati sui valori di mercato e sulle statistiche di campo tramite il pacchetto `worldfootballR` (fonte Transfermarkt).
* **Data Cleaning Avanzato:** Gestione personalizzata dei dati mancanti, dei contratti in scadenza e consolidamento delle statistiche per i calciatori trasferiti a metà stagione.
* **Modellazione Predittiva:** Utilizzo di Alberi di Decisione/Regressione per identificare quali variabili (Gol, Minuti, Presenze, Ruolo) impattino maggiormente sul valore economico di un giovane calciatore.
* **Profilazione dei Giocatori:** Segmentazione dei calciatori in 4 profili comportamentali/prestazionali ben distinti attraverso la Cluster Analysis.

---

## 🛠️ Requisiti e Installazione

Per eseguire lo script in locale sul tuo ambiente R/RStudio, assicurati di installare tutte le librerie necessarie eseguendo il blocco di codice seguente:

```R
# Installazione di devtools se non presente
if (!require("devtools")) install.packages("devtools")

# Installazione della versione di sviluppo di worldfootballR da GitHub
devtools::install_github("JaseZiv/worldfootballR")

# Installazione dei pacchetti CRAN necessari
install.packages(c("dplyr", "rpart", "rpart.plot", "cluster", "tidyverse", 
                   "factoextra", "ggplot2", "ggridges", "gridExtra", 
                   "ggpubr", "NbClust"))

```

---

## 📂 Struttura del Flauto di Lavoro (Pipeline)

### 1. Pre-elaborazione e Pulizia dei Dati

* **Filtri di Selezione:** Sono inclusi solo i giocatori con un'età inferiore ai 22 anni (`player_age.x < 22`) e con almeno 90 minuti di gioco complessivi accumulati in stagione.
* **Risoluzione Trasferimenti Doppi:** Lo script unisce manualmente le statistiche spezzate dei giocatori che hanno cambiato squadra a stagione in corso (es. Kulusevski, Bentancur, Vlahović, Camavinga, Ferran Torres, ecc.).
* **Ingegneria delle Variabili:** La data di scadenza del contratto viene trasformata in una variabile numerica che indica gli anni di contratto rimanenti rispetto al 2022.

### 2. Analisi Statistica

#### 🌲 Alberi di Regressione (`rpart`)

Viene costruito un albero per spiegare il `Valore_di_mercato` in base alle performance di campo.

* Viene valutata la tabella del parametro di complessità (`cptable`) per analizzare l'errore di cross-validation (`xerror`).
* L'albero viene sottoposto a **potatura (pruning)** sul valore ottimale di CP per evitare l'overfitting ed ottenere un modello interpretabile e generalizzabile.

#### 👥 Cluster Analysis

* **Standardizzazione:** I dati numerici vengono scalati (`scale`) prima del calcolo della distanza euclidea.
* **Metodo Gerarchico:** Viene applicato il criterio di Ward (`ward.D`). L'indice Silhouette tramite `NbClust` supporta la scelta di una partizione ottimale in **4 gruppi**.
* **K-Means:** Viene eseguito l'algoritmo K-Means a 4 centroidi per classificare i giocatori. I cluster vengono poi analizzati e confrontati nelle loro medie.

---

## 📊 Visualizzazioni Generate

Lo script produce diversi grafici pronti per l'esportazione:

1. **Matrice di Correlazione (`pairs`):** Relazioni grafiche cross-variabile tra Presenze, Gol, Minuti giocati e Valore.
2. **Dendrogramma dei Giocatori:** Rappresentazione ad albero del clustering gerarchico con riquadri di divisione dei gruppi.
3. **Density Ridges (`ggridges`):** Grafici di densità montagnosa per confrontare come si distribuiscono l'Età, i Gol o il Valore di mercato nei 4 diversi gruppi identificati.
4. **Bar Chart di Distribuzione:** Grafici a barre (impilati e standardizzati al 100%) per vedere come le diverse nazionalità e i campionati si distribuiscono all'interno dei cluster.

---

## 📜 Citazioni

Il progetto fa ampio uso del pacchetto `worldfootballR`. Per citare formalmente le risorse utilizzate all'interno di un eventuale paper o tesi, puoi digitare nel terminale di R il comando `citation("worldfootballR")`.

```

```
