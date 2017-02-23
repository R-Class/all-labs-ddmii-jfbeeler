Lab5
================
Jon Beeler
February 21, 2017

Creating a "research database" of home values in Syracuse
=========================================================

Start with a dataset of home prices and assets collected from Zillow

``` r
#read in housing data from Zillow
my.url <- "https://raw.githubusercontent.com/lecy/hedonic-prices/master/Data/Housing%20Price%20In-Class%20Exercise%20(Responses).csv"
housingdat <- read.csv(my.url, stringsAsFactors=FALSE )

#subset for Street Addresses and Zip Codes
houses <- housingdat[ , c("Street.Address.of.House","Zip.Code") ]

#Prepare addresses to be geocoded
houses$Street.Address.of.House <- gsub( ",", "", houses$Street.Address.of.House )
houses$Street.Address.of.House <- gsub( "\\.", "", houses$Street.Address.of.House )
addresses <- paste( houses$Street.Address.of.House, "Syracuse, NY", houses$zip, sep=", " )
head(addresses)
```

    ## [1] "504 Winkworth Pkwy, Syracuse, NY, "
    ## [2] "136 Austin Ave, Syracuse, NY, "    
    ## [3] "701 Velasko Rd, Syracuse, NY, "    
    ## [4] "518 Wolcott Ave, Syracuse, NY, "   
    ## [5] "112 Wolcott Ave, Syracuse, NY, "   
    ## [6] "212 Roberts Ave, Syracuse, NY, "

``` r
#Geocode addresses
#lat.long <- geocode( addresses )
#write.csv(lat.long, file = "lat.lon.csv")
setwd( "/Users/beelerenator/Documents/Graduate School/MPA Syracuse/DDMII/Jon_Beeler_DDMII_Labs/Lab 5")

lat.long <- read.csv("lat.lon.csv")
lat.long <- lat.long[ c("lon","lat")]

houses <- cbind(houses, lat.long)

head(houses)
```

    ##   Street.Address.of.House Zip.Code       lon      lat
    ## 1      504 Winkworth Pkwy    13219 -76.19918 43.02561
    ## 2          136 Austin Ave    13207 -76.18848 43.02894
    ## 3          701 Velasko Rd    13207 -76.18540 43.02957
    ## 4         518 Wolcott Ave    13207 -76.18301 43.02757
    ## 5         112 Wolcott Ave    13207 -76.18324 43.03210
    ## 6         212 Roberts Ave    13207 -76.17196 43.03111

Add census tracts FIPS ID to each home (spatial join to census tracts)

``` r
#read in onondoga shapefiles
setwd( "/Users/beelerenator/Documents/Graduate School/MPA Syracuse/DDMII/Lab3 - Shapefiles")

onondoga <- readShapePoly( fn="tl_2010_36067_tract10", proj4string=CRS("+proj=longlat +datum=WGS84") )
dat.onondoga <- as.data.frame( onondoga )

#turn lat.long into spatial points and match it with onongoda map
lat.long <- SpatialPoints(lat.long, proj4string=CRS("+proj=longlat +datum=WGS84") )
poly.data.matched.to.points <- over(lat.long, onondoga )

#bind dataframes
houses.data <- cbind( houses, poly.data.matched.to.points )

head(houses.data)
```

    ##   Street.Address.of.House Zip.Code       lon      lat STATEFP10 COUNTYFP10
    ## 1      504 Winkworth Pkwy    13219 -76.19918 43.02561        36        067
    ## 2          136 Austin Ave    13207 -76.18848 43.02894        36        067
    ## 3          701 Velasko Rd    13207 -76.18540 43.02957        36        067
    ## 4         518 Wolcott Ave    13207 -76.18301 43.02757        36        067
    ## 5         112 Wolcott Ave    13207 -76.18324 43.03210        36        067
    ## 6         212 Roberts Ave    13207 -76.17196 43.03111        36        067
    ##   TRACTCE10     GEOID10 NAME10      NAMELSAD10 MTFCC10 FUNCSTAT10 ALAND10
    ## 1    004800 36067004800     48 Census Tract 48   G5020          S 1206079
    ## 2    004800 36067004800     48 Census Tract 48   G5020          S 1206079
    ## 3    004800 36067004800     48 Census Tract 48   G5020          S 1206079
    ## 4    004800 36067004800     48 Census Tract 48   G5020          S 1206079
    ## 5    004800 36067004800     48 Census Tract 48   G5020          S 1206079
    ## 6    005000 36067005000     50 Census Tract 50   G5020          S 1149255
    ##   AWATER10  INTPTLAT10   INTPTLON10
    ## 1        0 +43.0279451 -076.1920648
    ## 2        0 +43.0279451 -076.1920648
    ## 3        0 +43.0279451 -076.1920648
    ## 4        0 +43.0279451 -076.1920648
    ## 5        0 +43.0279451 -076.1920648
    ## 6    45948 +43.0272963 -076.1689815

Add census data to each home

``` r
#download census tract data
censuskey <- "432376ffd1a15d644d4cd2aa0d664a3f3b885726"
# download acs5 2014 for median fam income, single mother, population, uninsured, insured
acs5_2014 <- getCensus(name="acs5", vintage=2014, key=censuskey, 
    vars=c("B01001B_001E", "B01001_001E"), region="tract:*", regionin = "state: 36 + county: 067")

#rename variables
acs5_2014 <- plyr::rename(acs5_2014, c("B01001B_001E"="Black", "B01001_001E"="total.population"))
acs5_2014 <- mutate(acs5_2014, Black.Percent = Black/total.population)

#merge housing dataframe from above with census data by CENSUS TRACT
housing.census.dat <- merge(x=houses.data, y=acs5_2014, by.x="TRACTCE10", by.y="tract",all.x=T)

head(housing.census.dat)
```

    ##   TRACTCE10 Street.Address.of.House Zip.Code       lon      lat STATEFP10
    ## 1    000100          139 Pulaski St    13204 -76.17168 43.05846        36
    ## 2    000200             709 Wolf St    13208 -76.16095 43.07638        36
    ## 3    000200        1006 Wolf Street    13208 -76.15762 43.07961        36
    ## 4    000200        619 2nd N Street    13208 -76.16063 43.07537        36
    ## 5    000200            609 2nd N St    13208 -76.16025 43.07522        36
    ## 6    000300         308 Cadillac St    13208 -76.14986 43.07923        36
    ##   COUNTYFP10     GEOID10 NAME10     NAMELSAD10 MTFCC10 FUNCSTAT10 ALAND10
    ## 1        067 36067000100      1 Census Tract 1   G5020          S 4842958
    ## 2        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 3        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 4        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 5        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 6        067 36067000300      3 Census Tract 3   G5020          S  574912
    ##   AWATER10  INTPTLAT10   INTPTLON10 state county Black total.population
    ## 1  1284980 +43.0691355 -076.1730170    36    067    76              598
    ## 2        0 +43.0747759 -076.1583997    36    067   198             2917
    ## 3        0 +43.0747759 -076.1583997    36    067   198             2917
    ## 4        0 +43.0747759 -076.1583997    36    067   198             2917
    ## 5        0 +43.0747759 -076.1583997    36    067   198             2917
    ## 6        0 +43.0812047 -076.1483599    36    067   305             1742
    ##   Black.Percent
    ## 1    0.12709030
    ## 2    0.06787796
    ## 3    0.06787796
    ## 4    0.06787796
    ## 5    0.06787796
    ## 6    0.17508611

Aggregate crimes by census tract (spatial join) and add to the dataset

``` r
#load in crime data
crime.dat <- read.csv("https://raw.githubusercontent.com/lecy/hedonic-prices/master/Data/crime.lat.lon.csv", stringsAsFactors=FALSE )


x.y.coords <- crime.dat[ ,c("lon","lat")]  # extract coordinate columns

#turn lat.lon crime points into scale for shape files and add to onondoga map
crime <- SpatialPoints( x.y.coords, proj4string=CRS("+proj=longlat +datum=WGS84") )
identicalCRS( onondoga, crime )  # should be TRUE
```

    ## [1] TRUE

``` r
crime.onondoga <- over( crime, onondoga ) 

#turn crime in onondoga into data frame
crime.onondoga  <- as.data.frame(crime.onondoga)

#count number of crimes in given census tract
crime.onondoga  <-  as.data.frame(table(crime.onondoga$TRACTCE10))
crime.onondoga <- plyr::rename(crime.onondoga, c("Var1"="tract", "Freq"="crime"))

#merge housing dataframe with number of crimes by census tract
housing.census.crime.dat <- merge(x=housing.census.dat, y=crime.onondoga, by.x="TRACTCE10", by.y="tract",all.x=T)

head(housing.census.crime.dat) 
```

    ##   TRACTCE10 Street.Address.of.House Zip.Code       lon      lat STATEFP10
    ## 1    000100          139 Pulaski St    13204 -76.17168 43.05846        36
    ## 2    000200        1006 Wolf Street    13208 -76.15762 43.07961        36
    ## 3    000200        619 2nd N Street    13208 -76.16063 43.07537        36
    ## 4    000200            609 2nd N St    13208 -76.16025 43.07522        36
    ## 5    000200             709 Wolf St    13208 -76.16095 43.07638        36
    ## 6    000300          133 Harford Rd    13208 -76.14412 43.08204        36
    ##   COUNTYFP10     GEOID10 NAME10     NAMELSAD10 MTFCC10 FUNCSTAT10 ALAND10
    ## 1        067 36067000100      1 Census Tract 1   G5020          S 4842958
    ## 2        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 3        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 4        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 5        067 36067000200      2 Census Tract 2   G5020          S 1095299
    ## 6        067 36067000300      3 Census Tract 3   G5020          S  574912
    ##   AWATER10  INTPTLAT10   INTPTLON10 state county Black total.population
    ## 1  1284980 +43.0691355 -076.1730170    36    067    76              598
    ## 2        0 +43.0747759 -076.1583997    36    067   198             2917
    ## 3        0 +43.0747759 -076.1583997    36    067   198             2917
    ## 4        0 +43.0747759 -076.1583997    36    067   198             2917
    ## 5        0 +43.0747759 -076.1583997    36    067   198             2917
    ## 6        0 +43.0812047 -076.1483599    36    067   305             1742
    ##   Black.Percent crime
    ## 1    0.12709030    45
    ## 2    0.06787796    11
    ## 3    0.06787796    11
    ## 4    0.06787796    11
    ## 5    0.06787796    11
    ## 6    0.17508611     2

``` r
#class(crime.onondoga)

#these <- match(onondoga@data$TRACTCE10,crime.onondoga$tract)
#onondaga@data$crime <- crime.onondoga$crime[these]
```
