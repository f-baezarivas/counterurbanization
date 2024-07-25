# Counterurbanization in the rural global south: evidence from Chile

The repository contains the R script  and the  to replicate the results presented in the paper "Counterurbanization in the rural global south: evidence from Chile", authored by Félix Rojo, Fernando Baeza and Voltaire Alvarado. This is a sub-study of the FONDECYT 1231643 project founded by the Chilean National Agency for Research and Development (ANID), PI Dr. Félix Rojo (frojo@uct.cl). In this project we aim to analyze the principles of residential choice related to the identities of the middle classes that have migrated from urban to rural areas in southern Chile. 

The [R script](Rojo et al 2024 script.R) is in the repository main folder 

The databases and shapefiles used can be found in the [Data](Data) folder and are all from public sources:
- Census data ([1992](Data\Censo1992_Persona_Full.Rds), [2002](Data\Censo2002_Persona_Full.Rds), [2017](Data\Censo2017_Persona_Full.Rds)), was obtained from the National Institute of Statistics. The pre-processing of the data was carried out by [Dr. Ricardo Truffello](https://estudiosurbanos.uc.cl/academicos/ricardo-truffello-robledo/) and included the merging of the dwellings, households and individuals databases, and the updating of the municipalities to the most recent version from the census district information. 
- The municipal codes and labels [DPA codes](Data\DPA_code.xls) is elaborated and updated by the Subsecretariat of Regional and Administrative Development (SUBDERE).

The shapefiles are available at the Chilean Geospatial Infrastructure (IDE Chile-Geoportal).
- The shapefile of political-administrative division [2010_Comunas](Data\2010_Comunas) is produced and updated by the Subsecretariat of Regional and Administrative Development: https://www.geoportal.cl/geoportal/catalog/36391/Divisi%C3%B3n%20Pol%C3%ADtica%20Administrativa%202023
- The shapefile of urban settlements, updated to 2017 [2017_Cities](Data\2017_Cities), is produced by the National Institute of Statistics: https://www.ine.gob.cl/herramientas/portal-de-mapas/geodatos-abiertos
- The shapefile of Rural Entities defined by the 2017 Census [2017_Entities](Data\2017_Entities) is produced by the National Institute of Statistics: https://www.ine.gob.cl/herramientas/portal-de-mapas/geodatos-abiertos

For information about the data analysis, contact Fernando Baeza (fernando.baeza@uc.cl).

