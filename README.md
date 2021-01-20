# Data-Mining-Clustering-in-R
This project is about understanding the clustering approach to mining (unsupervised learning). In this project, you will be given weather data set in separate files for the state of Texas for the years 2006 to 2010 (5 years) for analysis using k-means clustering. The data set includes a number of weather stations in Texas (about 200 of them). Each station
collects weather data on an hourly basis for each day throughout the year!

http://itlab.uta.edu/downloads/cse5334_project2_TX_weather_datasets.zip

Data attributes:

Data format has been given in the following table. First record is the header
record. All 9's in a field (e.g., 9999.99 for DewP) indicates no report or
insufficient data.
The fields are separated by one or more whitespaces.
Field Description

STN Station number (WMO/DATSAV3 number) for the location.

WBAN WBAN number where applicable--this is the historical
"Weather Bureau Air Force Navy" number - with WBAN
being the acronym.

yearModa_hr The year (first 4 digits), The month (next 2 digits) and day
(the next 2 digits) and hour (0 to 23).

Temp Mean Temperature of that hour in degrees Fahrenheit to
tenths. Missing = 9999.9

DewP Mean dew point for that hour in degrees Fahrenheit to
tenths. Missing = 9999.9

Count Number of observations used in calculating dew point

SLP Sea level pressure for that hour in millibars to tenths.
Missing = 9999.9

The problem is to cluster stations that have similar weather during that month. First you need to
iterate for k = 2 to 8. Then for each k do the following (specific details below).
Cluster stations based on the vector generated for each station for the k values using different
starting stations for the initial centroid. Choose k stations randomly. Generate clusters of stations
for each k starting stations. Use one of the two stopping conditions: centroids do not change or not
more than 1 or 2% change.

Count Number of observations used in calculating sea level
pressure

STP Mean Station pressure for that hour in millibars to tenths.
Missing = 9999.9

Count Number of observations used in calculating Station
pressure

Visib Mean Visibility for that hour in miles to tenths. Missing =
999.9

Count Number of observations used in calculating Visibility

WDSP Mean wind speed for the hour in knots to tenths. Missing
= 999.9

Count Number of observations used in calculating Mean wind
speed

MXSDP Maximum sustained wind speed reported for that hour in
knots to tenths. Missing = 999.9

Gust Maximum wind gust reported for that hour in knots to
tenths. Missing = 999.9

PRCP A = 1 report of 6-hour precipitation amount.
B = Summation of 2 reports of 6-hour precipitation
amount.
C = Summation of 3 reports of 6-hour precipitation
amount.
D = Summation of 4 reports of 6-hour precipitation
amount.
E = 1 report of 12-hour precipitation amount.
F = Summation of 2 reports of 12-hour precipitation
amount.
G = 1 report of 24-hour precipitation amount.
H = Station reported '0' as the amount for the day (eg, from
6-hour reports), but also reported at least one occurrence
of precipitation in hourly, observations--this could indicate
a trace occurred, but should be considered as incomplete
data for the day.
I = Station did not report any precip data for the day and
did not report any occurrences of precipitation in its hourly
observations--it's still possible that precip occurred but was
not reported.

SNDP Snow depth in inches to tenths--last report for the day if
reported more than once. Missing = 999.9

FRSHIFT Indicators (1 = yes, 0 = no/not reported) for the occurrence
during the day of: Fog ('F' - 1st digit).
Rain or Drizzle ('R' - 2nd digit).
Snow or Ice Pellets ('S' - 3rd digit).
Hail ('H' - 4th digit).
Thunder ('T' - 5th digit).
Tornado or Funnel Cloud ('T' - 6th digit).

The problem is to cluster stations that have similar weather during that month. First you need to iterate for k = 2 to 8. Then for each k do the following (specific details below). Cluster stations based on the vector generated for each station for the k values using different starting stations for the initial centroid. Choose k stations randomly. Generate clusters of stations for each k starting stations. Use one of the two stopping conditions: centroids do not change or not more than 1 or 2% change.
