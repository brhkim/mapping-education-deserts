

/*
=============Data cleaning process=============

- The "Guatemala schools" folder is structured as follows: the "Schools 2008" and "Schools 2017" folders have the raw and built data for each year, along with the do-files that create these built files. The "Clean reconciled data" contains two standardized, clean files with from 2008 (n=~14,000), and 2017 (n=~16,000) with only public, primary schools and their lat/long. This folder also contains a do-file that creates these clean files from the built files.

- OLD statement: The 2008 school data is fairly clean, except it comes in a shapefile format, so I just export it as a .csv as "Schools_2008" within the "Schools 2008" folder.
---- Amendment to statement above: for some reason, the file above creates some weird "gridlines" when plotted (we suspect that there must be some rounding/habdling of decimals at some point that creates these in the lat/long field)
---- INSTEAD, the 2008 data comes from the same source, but instead of downloading as a shapefile, it is downloaded as an .xls file ("Listado de Escuelas - MINEDUC - 2008", under "Bases de datos - Tablas Excel"), as opposed to ("Escuelas de Guatemala(MINEDUC)", under "Datos SIG - Formato Shape") 

- The 2017 data is slightly more complicated. The location data of the schools is from 2020  (the raw file being "escuelas-guatemala-2020-da"). However, this data does not include the level of the school (i.e. primary/secondary) nor the ownership (public/private). However, I can merge 2017 admin data with the same school codes to retrieve this information. This comes from the file "Enrollment.dta", which I created for another project (please let me know if you'd like to see those do files, but they are basically a do-file converting to Stata format the raw data from the source I indicate below').


==============Sources for raw data=============

- Schools 2008: http://ide.segeplan.gob.gt/descargas.php
- School location 2020: https://datosabiertos.mineduc.gob.gt/dataset/establecimiento-educativos
- Enrollment data with school characteristics (2017): https://datosabiertos.mineduc.gob.gt/dataset/matricula-por-establecimiento
- Population data ("Unconstrained individual countries 2000-2020 (100 m resolution)"): https://www.worldpop.org/project/categories?id=3 
- Population data at 1km resolution: same source as 100 m, notice it is just the aggregated version of the 100 m (aggregated by source)

*/
