sess <- NeticaSession()
startSession(sess)
netman1 <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    row.names=1,stringsAsFactors=FALSE)

nodeman1 <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)

omegamat <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                           "miniPP-omega.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)
## profnames <- omegamat$Node
## rownames(omegamat) <- profnames
stopSession(sess)
