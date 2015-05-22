# CDC Fluoridation googleVis Chart
Brian High  
05/21/2015  

## Setup

Load the required R packages.


```r
for (pkg in c("XML", "googleVis")) {
    if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
        install.packages(pkg, repos="http://cran.fhcrc.org", dependencies=TRUE)
        if (! suppressWarnings(require(pkg, character.only=TRUE)) ) {
            stop(paste0(c("Can't load package: ", pkg, "!"), collapse = ""))
        }
    }
}
```

## Get the data

Load the CSV data file if you have it, otherwise create the CSV by scraping the 
fluoridation table from the CDC website.


```r
cdcfile <- "data/uw_cdc_dw_fluoridation.csv"
if (file.exists(cdcfile)) {
    # Read data from CSV
    cdcfl <- read.csv(file = cdcfile, header = TRUE)
} else {
    # Scrape from CDC web page and save as CSV for later
    library(XML)
    cdcflweb<-readHTMLTable("http://www.cdc.gov/fluoridation/statistics/2012stats.htm")
    cdcfl <- cdcflweb[[2]][3:53, c(1,4)]
    names(cdcfl) <- c("State", "Percent")
    cdcfl$State <- sapply(cdcfl$State, function (x) gsub("f[,gh]*$", "", x))
    write.csv(cdcfl, file=cdcfile, row.names=FALSE)
}
```

## Clean the data

Remove the % sign from Percent and convert to numeric type.


```r
cdcfl$Percent <- as.numeric(sub("%", "", cdcfl$Percent))
```

## Create the gvisGeoChart


```r
# Set options
op <- options(gvis.plot.tag='chart')

# Create gvisGeoChart
suppressPackageStartupMessages(library(googleVis))
G3 <- gvisGeoChart(cdcfl, "State", "Percent",
                    options=list(region="US", displayMode="regions",
                                 resolution="provinces",
                                 width=600, height=400))
plot(G3)
```

<!-- GeoChart generated in R 3.1.3 by googleVis 0.5.8 package -->
<!-- Thu May 21 09:40:50 2015 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataGeoChartID3e466eb333bb () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "Alabama",
78.4 
],
[
 "Alaska",
52.9 
],
[
 "Arizona",
57.8 
],
[
 "Arkansas",
66.9 
],
[
 "California",
63.7 
],
[
 "Colorado",
72.4 
],
[
 "Connecticut",
90.3 
],
[
 "Delaware",
86.3 
],
[
 "District of Columbia",
100 
],
[
 "Florida",
78 
],
[
 "Georgia",
96.3 
],
[
 "Hawaii",
10.8 
],
[
 "Idaho",
36.1 
],
[
 "Illinois",
98.5 
],
[
 "Indiana",
94.8 
],
[
 "Iowa",
92 
],
[
 "Kansas",
63.6 
],
[
 "Kentucky",
99.9 
],
[
 "Louisiana",
43.4 
],
[
 "Maine",
79.4 
],
[
 "Maryland",
97.2 
],
[
 "Massachusetts",
70.4 
],
[
 "Michigan",
90.2 
],
[
 "Minnesota",
98.8 
],
[
 "Mississippi",
58.2 
],
[
 "Missouri",
76.4 
],
[
 "Montana",
32 
],
[
 "Nebraska",
71.2 
],
[
 "Nevada",
73.5 
],
[
 "New Hampshire",
46 
],
[
 "New Jersey",
14.6 
],
[
 "New Mexico",
77 
],
[
 "New York",
71.8 
],
[
 "North Carolina",
87.5 
],
[
 "North Dakota",
96.7 
],
[
 "Ohio",
92.2 
],
[
 "Oklahoma",
70.1 
],
[
 "Oregon",
22.6 
],
[
 "Pennsylvania",
54.6 
],
[
 "Rhode Island",
83.9 
],
[
 "South Carolina",
93.8 
],
[
 "South Dakota",
93.6 
],
[
 "Tennessee",
89.7 
],
[
 "Texas",
79.6 
],
[
 "Utah",
51.7 
],
[
 "Vermont",
56.1 
],
[
 "Virginia",
96 
],
[
 "Washington",
63.6 
],
[
 "West Virginia",
91.1 
],
[
 "Wisconsin",
89.4 
],
[
 "Wyoming",
43.6 
] 
];
data.addColumn('string','State');
data.addColumn('number','Percent');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartGeoChartID3e466eb333bb() {
var data = gvisDataGeoChartID3e466eb333bb();
var options = {};
options["width"] =    600;
options["height"] =    400;
options["region"] = "US";
options["displayMode"] = "regions";
options["resolution"] = "provinces";

    var chart = new google.visualization.GeoChart(
    document.getElementById('GeoChartID3e466eb333bb')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "geochart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartGeoChartID3e466eb333bb);
})();
function displayChartGeoChartID3e466eb333bb() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartGeoChartID3e466eb333bb"></script>
 
<!-- divChart -->
  
<div id="GeoChartID3e466eb333bb" 
  style="width: 600; height: 400;">
</div>

```r
# Set options back to original options
options(op)
```

## Example Web Page

The HTML output from running `knitr` on the Rmd file (the original version of 
this document) is a stand-alone web page and can be found on 
[Brian High's UW website](http://staff.washington.edu/high/fluoridation/cdc-fl-example.html)