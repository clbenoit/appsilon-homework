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

## Running
To run the app, use `Rscript -e 'shiny::runApp(launch.browser = TRUE)'`.

## Data

### Application-ready data 

<a href="https://git-lfs.com/" target="_blank">git-lfs</a> is required to fetch the example dataset

run `git lfs pull`

Then you'll find it here : `inst/extdata/biodiversity_data_poland.db`. This dataset is restricted to poland observations to save development time. <br/>

### However, if you want to generate the whole dataset from raw sources.
1. You can download the whole dataset [here](https://drive.usercontent.google.com/download?id=1l1ymMg-K_xLriFv1b8MgddH851d6n2sU&export=download&authuser=0&confirm=t&uuid=625527a1-37d8-42f2-bc52-5e094e7d3075&at=APZUnTW4MHTH-1FtcVJNpIiFYF5O%3A1710337610936). 
2. Then you will have to import occurence and multimedia csv files into an sqlite database
3. Finally execute these few sqlite queries on your sqlite database : 

```
-- Step 1 : Remove unecessary string in id column 
UPDATE multimedia SET id = REPLACE(id, '@OBS', '')
UPDATE occurence SET id = REPLACE(id, '@OBS', '')

-- Step 2 : Remove unsused columns 
CREATE TABLE occurence_tmp AS
SELECT column1, column2, /* other columns except the one you want to remove */
FROM occurence;
DROP TABLE occurence;
ALTER TABLE occurence_tmp RENAME TO occurence;

-- Step 3 : Convert taxonRank to integer
UPDATE occurence SET taxonRank = REPLACE(taxonRank, 'subspecies', '3');
UPDATE occurence SET taxonRank = REPLACE(taxonRank, 'multispecies', '2');
UPDATE occurence SET taxonRank = REPLACE(taxonRank, 'species', '1');
UPDATE occurence SET taxonRank = REPLACE(taxonRank, 'synonym', '4');
UPDATE occurence SET taxonRank = REPLACE(taxonRank, 'forma', '5');
UPDATE occurence SET taxonRank = REPLACE(taxonRank, 'hybrid', '6');
UPDATE occurence SET taxonRank = REPLACE(taxonRank, 'variety', '7');

-- Step 4 : Split occcurence table by kingdom
PRAGMA foreign_keys=off;
CREATE TABLE IF NOT EXISTS occurence_Fungi AS
SELECT * FROM occurence WHERE 1=0;
INSERT INTO occurence_Fungi
SELECT * FROM occurence WHERE kingdom = 'Fungi';
CREATE TABLE IF NOT EXISTS occurence_Animalia AS
SELECT * FROM occurence WHERE 1=0;
INSERT INTO occurence_Animalia
SELECT * FROM occurence WHERE kingdom = 'Animalia';
CREATE TABLE IF NOT EXISTS occurence_Plantae AS
SELECT * FROM occurence WHERE 1=0;
INSERT INTO occurence_Plantae
SELECT * FROM occurence WHERE kingdom = 'Plantae';

DROP TABLE occurence;
VACUUM;

-- Step 5 : Create species_family_match tables 
CREATE TABLE IF NOT EXISTS species_family_match_Fungi AS
SELECT DISTINCT scientificName, family, taxonRank
FROM occurence_Fungi;
CREATE TABLE IF NOT EXISTS species_family_match_Plantae AS
SELECT DISTINCT scientificName, family, taxonRank
FROM occurence_Plantae;
CREATE TABLE IF NOT EXISTS species_family_match_Animalia AS
SELECT DISTINCT scientificName, family, taxonRank
FROM occurence_Animalia;

-- Step 6 : Create indexes
CREATE INDEX "idx_multimedia_id" ON "multimedia" (
	"id"
);
CREATE INDEX "idx_occurence_Fungi_scientificName" ON "occurence_Fungi" (
	"scientificName"
);
CREATE INDEX "idx_occurence_Plantae_scientificName" ON "occurence_Plantae" (
	"scientificName"
);
CREATE INDEX "idx_occurence_Animalia_scientificName" ON "occurence_Animalia" (
	"scientificName"
);
CREATE INDEX "idx_occurence_Plantae_taxonRank" ON "species_family_match_Plantae" (
	"taxonRank"
);
CREATE INDEX "idx_occurence_Animalia_taxonRank" ON "species_family_match_Animalia" (
	"taxonRank"
);
CREATE INDEX "idx_occurence_Fungi_taxonRank" ON "species_family_match_Fungi" (
	"taxonRank"
);

PRAGMA foreign_keys=on;
-- Step 7 : Commit changes to make them permanent
COMMIT;
VACUUM;
```

4. Edit [config](config.yml) and .Renviron files to track your database in the app.
