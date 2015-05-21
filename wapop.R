# wa county map example

# See related example:
# https://blogs.baylor.edu/alex_beaujean/2013/06/28/creating-a-map-in-r/

# See: 
# Churchill SS, Williams BJ, Villareale NL. Characteristics of Publicly 
# Insured Children with High Dental Expenses. Journal of Public Health Dentistry. 
# 2007 Fall;67(4):199-207. http://www.ncbi.nlm.nih.gov/pubmed/18087990

# And:
# Bussma Ahmed Bugis, “Early Childhood Caries and the Impact of Current U.S. 
# Medicaid Program: An Overview,” International Journal of Dentistry, vol. 2012, 
# Article ID 348237, 7 pages, 2012. doi:10.1155/2012/348237
# http://www.hindawi.com/journals/ijd/2012/348237/

# Medicaid data from Washington Health Care Authority:
# http://www.hca.wa.gov/medicaid/dentalproviders/Pages/dental_data.aspx

# Spatial data from: GADM http://gadm.org/

# install.packages(c("maptools", "ggplot2", "scales"))

download.file(url="http://biogeo.ucdavis.edu/data/gadm2/shp/USA_adm.zip", 
              destfile="data/USA_adm.zip")
unzip(zipfile="data/USA_adm.zip", overwrite = TRUE, exdir = 'data')
      
library(maptools)
usa <- readShapeSpatial("data/USA_adm2")
wa <- usa[usa$NAME_1=="Washington", ]
cnames <- read.csv(file = 'data/cnames.csv', header = TRUE)

head(cnames)

dent.file <- "data/wa_hca_dental_summary.xls"
dent.url <- "http://www.hca.wa.gov/medicaid/dentalproviders/documents/999cntysumall.XLS"
download.file(url=dent.url, destfile=dent.file,  mode="wb")

library(XLConnect)
dent.cnty <- readWorksheetFromFile(dent.file, sheet=1, header=FALSE,
                                   startRow=5, endRow=43, startCol=1, endCol=1)
dent.exp <- readWorksheetFromFile(dent.file, sheet=1, header=FALSE,
                                  startRow=5, endRow=43, startCol=25, endCol=25)
expenses <- data.frame(county=dent.cnty$Col1, FY2014=dent.exp$Col1, 
                       stringsAsFactors = FALSE)
head(expenses)

library(ggplot2)

theme_bare <- function(...) {
    theme_classic() + 
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
}

library(scales)
wa <- fortify(wa, region="NAME_2")

# Map of Washington State and counties with county names
ggplot() + geom_map(data=expenses, aes(map_id=county),
                    color="darkgreen", fill=NA, map=wa) + 
    expand_limits(x=wa$long, y=wa$lat) + theme_bare() + 
    geom_text(data=cnames, aes(long, lat, label = subregion), size=3)

# Map of Washington State Medicaid dental expenses
gmap <- ggplot() + geom_map(data=expenses, aes(map_id=county, fill=FY2014),
                    color="darkgreen", map=wa) + 
    expand_limits(x=wa$long, y=wa$lat) + theme_bare() +
    geom_text(data=cnames, aes(long, lat, label = subregion), size=3) +  
    labs(title = "Average Annual Medicaid Dental Expenses Per User in 2014 by County") +
    scale_fill_gradient2(space="Lab", high="darkgreen", mid="yellow", 
                         low="white", midpoint=300, name="US$")

library(gridExtra)
data.src <- paste0(collapse = ' ', c('Data sources:', 
                                     'WA DOH (www.doh.wa.gov),', 
                                     'US EPA (water.epa.gov)',
                                     'and HHS (www.hhs.gov)'))
gmap <- arrangeGrob(gmap, sub = textGrob(data.src, x=0, hjust=-0.1, vjust=0.1,
                                 gp = gpar(fontface="italic", fontsize=12)))
gmap

