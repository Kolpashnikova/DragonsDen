#### create an interactive tempogram ####
## load interactive tempogram

install.packages("devtools")

devtools::install_github("Kolpashnikova/package_R_tempogram")
library(tempogram)

## package for working with JSON
library(jsonlite)

temp <- tu_tempogram(data, w="WT06", granularity = 58)
tempogram(toJSON(temp))

## hint: it is better to specify width as "auto" and height as "550px"
## this is better for website responsiveness

temp <- tu_tempogram(data, w="WT06", granularity = 15)
tempogram(toJSON(temp), width="auto", height = "550px")

#### save the file as .html file ####

#### create github.io ####

#### push the file there ####

devtools::install_github("Kolpashnikova/package_R_transitions")
library(transitions)

trans <- tu_transitions(data)

transitions(toJSON(trans$trate), trans$activities)
