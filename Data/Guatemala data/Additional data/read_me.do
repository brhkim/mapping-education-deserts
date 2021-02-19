/*

The source of the elevation and slope data is WorldPop ("gtm_srtm_topo_100m.tif" for elevation and "gtm_srtm_slope_100m.tif" for the slope): https://www.worldpop.org/project/categories?id=14

The source of the roads data is the World Bank (caminos_gtm): https://datacatalog.worldbank.org/dataset/roads-guatemala

The source of the department-level shapefiles is the SEGEPLAN (departamentos_gtm): http://ide.segeplan.gob.gt/descargas.php

The source of the enrollment data is the Institute Nacional de Estadistica INE (https://www.ine.gob.gt/estadisticasine/index.php/matricula/reporte). The filters applied are periodo=2016 (latest year available), nivel=Nivel primaria, Departmento=All selected.

The enrollment data is saved under "Enrollment_Depts_Guatemala_2016.csv". This file should be very similar to the one that downloads directly from the website above with minor cosmetic modifications. Dan removed the top line to make the file easier to read in R/Stata, and translated the variable names. Furthermore, since the goal of this data is to merge it to the department-level shapefiles, Dan named the variable containing the department names "nombre", so it matches the variable name from the shapefile. Also, Dan removed all accents from the department names, and made them all upper case so they would match the shapefile names perfectly.

Notice that one line from the "Enrollment_Depts_Guatemala_2016.csv" file will not merge with the shapefiles ("REPUBLICA"), which is just the national average.

*/
