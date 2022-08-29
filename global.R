# INSTALL DEPENDENCIES ----

source("dependencies.R")

# DATA TRANSFORMATION AND NEW VARIABLES ----

conf <- spark_config()
conf$spark.driver.extraJavaOptions <- "-XX:+UseCompressedOops"
conf$spark.driver.maxResultSize <- "2g"
conf$spark.serializer <- "org.apache.spark.serializer.KryoSerializer"
conf["sparklyr.cores.local"] <- 4
conf["sparklyr.arrow"] <- TRUE
conf["sparklyr.shell.driver-memory"] <- "4G"
conf["sparklyr.connect.enablehivesupport"] <- FALSE
sc <- spark_connect(master = "local", config = conf)