# Biodata Discovery Board App

This app is built with [Rhino](https://github.com/Appsilon/rhino).
You can see a deployed version here: 
[Biodata Discovery Board App Demo](https://omicsverse.fr/app/appsilon-homework).
Using the biodiversity data coming from the [Global Biodiversity Information Facility](https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165&month=1), this Shiny app visualizes observed species on the map, draw a timeline with selected species observation, and allows the user to explore associated pictures. 

## Prerequisites
This is an application built in [Shiny](https://shiny.rstudio.com/).
To run it, make sure you have R (>= 4.0.0) installed.
For JavaScript and Sass development you'll also need
[Node.js](https://nodejs.org/en/download/) (>= 16.0.0).

## Dependencies
Run `renv::restore(clean = TRUE)` to synchronize the project library with the lockfile
when you initially clone the repo or switch branches.

## Data
Application-ready data is included in `inst/extdata/biodiversity_test_data.db`. This dataset is restricted to avoid overloading the Git repository. <br/>

However, if you want to generate the whole dataset from raw sources. 

1. You can download the whole dataset [here](https://drive.usercontent.google.com/download?id=1l1ymMg-K_xLriFv1b8MgddH851d6n2sU&export=download&authuser=0&confirm=t&uuid=625527a1-37d8-42f2-bc52-5e094e7d3075&at=APZUnTW4MHTH-1FtcVJNpIiFYF5O%3A1710337610936). 
2. Then you will have to import occurence and multimedia csv file into an sqlite database
3. Finally execute this few sqlite queries on your sqlite database : 

```
-- Remove unecessary string in id column 
UPDATE multimedia SET id = REPLACE(id, '@OBS', '')

-- Split occcurence table by kingdom
PRAGMA foreign_keys=off;
CREATE TABLE IF NOT EXISTS occurence_Fungi AS
SELECT * FROM occurence WHERE 1=0;
INSERT INTO occurence_Fungi
SELECT * FROM occurence WHERE kingdom = 'Fungi';
CREATE INDEX "idx_occurence_Fungi_id" ON "occurence_Fungi" (
	"id"	ASC
);
CREATE INDEX "idx_occurence_Fungi_taxonRank" ON "occurence_Fungi" (
	"taxonRank"
);

CREATE TABLE IF NOT EXISTS occurence_Animalia AS
SELECT * FROM occurence WHERE 1=0;
INSERT INTO occurence_Animalia
SELECT * FROM occurence WHERE kingdom = 'Animalia';
CREATE INDEX "idx_occurence_Animalia_id" ON "occurence_Animalia" (
	"id"	ASC
);
CREATE INDEX "idx_occurence_Animalia_taxonRank" ON "occurence_Animalia" (
	"taxonRank"
);

CREATE TABLE IF NOT EXISTS occurence_Plantae AS
SELECT * FROM occurence WHERE 1=0;
INSERT INTO occurence_Plantae
SELECT * FROM occurence WHERE kingdom = 'Plantae';
CREATE INDEX "idx_occurence_Plantae_id" ON "occurence_Plantae" (
	"id"	ASC
);
CREATE INDEX "idx_occurence_Plantae_taxonRank" ON "occurence_Plantae" (
	"taxonRank"
);

PRAGMA foreign_keys=on;

-- Commit changes to make them permanent
COMMIT;
```

## Running
To run the app, use `Rscript -e 'shiny::runApp(launch.browser = TRUE)'`.
