# Using machine learning to understand the drivers of nutrients in constrasting coastal ecosystems

</br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Nutrients are essential drivers and indicators of aquatic ecosystem function and health. Both water quality and climate factors influence nutrient sources, sinks, and processing, but these relationships can be complex (e.g., non-linear, or additive). Further, nutrient data are currently collected via grab-sampling and laboratory analysis, while water quality and climate data are available at high temporal resolution via in-situ sensors. The resulting issue is an inconsistency of available data streams. We seek to utilize random forests that presumably offer flexible, robust alternatives to traditional methods for modeling driver-response relationships (e.g. multiple linear regression). 
</br>

### **Project overview** 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;This repository houses data downloaded from NOAA's National Estuarine Research Reserve database (http://cdmo.baruch.sc.edu/), and scripts for importing, cleaning, processing, and analyzing data for nutrients, water quality, and climate collected at Old Woman Creek (a tributary to Lake Erie) and the York River estuary (a tributary to Chesapeake Bay).

 </br>

![Figrue 1. Old Women Creek (OWC) and York River site (CBV) portfolios. Site locations for meteorlogical, water quality, and nutrient stations. OWC is a fresh water estuary located near the town of Huran at Lake Erie and CBV is a brackish river located approximately 5 miles above Yorktown in the Chesapeake Bay. Map tiles by _Stamen Design_, under CC by 3.0. Data by _OpenStreetMap_, under ODbL. Image credits: Bonner Center, _Old Woman Creek Reserve_. & Durkee S., _Yortown Waterfront Walk_. 2020. ](Figs/sites.JPG = 250x250)
*Figrue 1. Old Women Creek (OWC) and York River site (CBV) portfolios. Site locations for meteorlogical, water quality, and nutrient stations. OWC is a fresh water estuary located near the town of Huran at Lake Erie and CBV is a brackish river located approximately 5 miles above Yorktown in the Chesapeake Bay. Map tiles by _Stamen Design_, under CC by 3.0. Data by _OpenStreetMap_, under ODbL. Image credits: Bonner Center, _Old Woman Creek Reserve_. & Durkee S., _Yortown Waterfront Walk_. 2020.*

</br>

### **Methods** 

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;We selected two NERR sites proximal to COMPASS study locations with contrasting ecosystem characteristics (small stream vs large estuary, freshwater vs saltwater, seasonal ice cover vs ice-free, etc.) to develop our statistical approach. Three nutrients (ammonium or NH4, nitrate or NO3, phosphate or PO4) and chlorophyll were selected as dependent variables. Water quality parameters (water temperature, specific conductivity, dissolved oxygen, pH, turbidity, and depth), meteorological parameters (air temperature, relative humidity, barometric pressure, wind direction and speed, photosynthetically active radiation or PAR, and precipitation), discharge from relevant USGS gaging stations, and day of year were selected as independent variables. Initial screening indicated random forest regression models out-performed numerous other potentially useful machine learning algorithms. 

</br>

### **References**
