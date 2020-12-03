# if you first use this script run all codes
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("mzR")

# after first run, run the following codes
library(mzR)

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("openxlsx")) {
  install.packages("openxlsx")
  library(openxlsx)
}

if (!require("reshape2")) {
  install.packages("reshape2")
  library(reshape2)
}
# put the file path
raw <- "20200928_zl_1_289_s9.mzXML"
# read raw
ms <- openMSfile(raw)
# read peaks
p <- peaks(ms)
hd <- header(ms)
# select ms1, scan no. and retention time
ms1 <- hd %>% filter(msLevel == 1) %>% select(seqNum, retentionTime)
# select m/z
p1 <- melt(p) %>% filter(Var2 == 1)
# select ms1, rename colume, and round ms1
ms1_p <- inner_join(p1, ms1, by=c("L1" = "seqNum")) %>% rename(MS_level = Var2, m_z = value, scan = L1) %>% mutate(m_z = round(m_z, 4), RTmin = round((retentionTime/60), 2)) %>% select(scan, RTmin, m_z)  
# select scans without 5 ppm
result1 <- ms1_p %>% mutate(error = (m_z - 596.3245)/596.3245*10^6) %>% filter(abs(error) <= 5)
result2 <- ms1_p %>% mutate(error = (m_z - 594.3199)/594.3199*10^6) %>% filter(abs(error) <= 5)
all_results <- full_join(result1, result2, by = "scan") %>% arrange(scan)
# change the file name if needed
write.xlsx(all_results, "MS1_find.xlsx")
