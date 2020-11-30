## Tidy human data.
## Group 1

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}

filelist <- list.files(path = "C:/Users/Tom Lin/Documents/backup-office desktop/upenn/20171113-lab protocol/20200423-postCIMAGE/example/1. human HEK293T soluble enrichment", recursive=T, pattern = "\\.txt$", full.names = T)

human_group1 <- rbindlist(sapply(filelist, fread, simplify = FALSE, USE.NAMES = T)) %>% mutate(symbol = toupper(symbol)) %>% rename(uniprot = ipi) %>% filter(!(str_detect(uniprot, "contaminant"))) %>% filter(!(str_detect(symbol, "KRT"))) %>% filter(noqp.set_1 >= 3)

# protein ratio
human_group1_final <- human_group1 %>% mutate(uniprot = as.character(uniprot), description = as.character(description)) %>% group_by(uniprot) %>% filter(!(mr.set_1 == "0")) %>% filter(sequence == "") %>% mutate(average_protein_ratio_h = mean(mr.set_1, na.rm = T), sd_h = sd(mr.set_1, na.rm = T)) %>% select(uniprot, symbol, description, average_protein_ratio_h, sd_h) %>% ungroup(.) %>% unique(.) #%>% filter(average_protein_ratio_h>=5)

#write.csv(human_group1_final, "human_group1.csv", row.names = F)

## Group 2

filelist <- list.files(path = "C:/Users/Tom Lin/Documents/backup-office desktop/upenn/20171113-lab protocol/20200423-postCIMAGE/example/2. human HEK293T soluble competition", recursive=T, pattern = "\\.txt$", full.names = T)

human_group2 <- rbindlist(sapply(filelist, fread, simplify = FALSE, USE.NAMES = T)) %>% mutate(symbol = toupper(symbol)) %>% rename(uniprot = ipi) %>% filter(!(str_detect(uniprot, "contaminant"))) %>% filter(!(str_detect(symbol, "KRT"))) %>% filter(noqp.set_1 >= 3)

# protein ratio
human_group2_final <- human_group2 %>% mutate(uniprot = as.character(uniprot), description = as.character(description)) %>% group_by(uniprot) %>% filter(!(mr.set_1 == "0")) %>% filter(sequence == "") %>% mutate(average_protein_ratio_h = mean(mr.set_1, na.rm = T), sd_h = sd(mr.set_1, na.rm = T)) %>% select(uniprot, symbol, description, average_protein_ratio_h, sd_h) %>% ungroup(.) %>% unique(.) #%>% filter(average_protein_ratio_h>=3)

#write.csv(human_group1_final, "human_group1.csv", row.names = F)

# merge two groups together
human_all <- full_join(human_group1_final, human_group2_final, by=c("uniprot", "symbol", "description")) %>% unique(.) #%>% full_join(., human_group3_final, by=c("uniprot", "symbol", "description")) %>% unique(.)

# if you have the third group, delete "#" before %>% 
#write.csv(human_all, "human_together_v1.csv", row.names = F)

## Mouse Group 1
filelist <- list.files(path = "C:/Users/Tom Lin/Documents/backup-office desktop/upenn/20171113-lab protocol/20200423-postCIMAGE/example/11. mouse brain membrane enrichment 4h", recursive=T, pattern = "\\.txt$", full.names = T)

mouse_group11 <- rbindlist(sapply(filelist, fread, simplify = FALSE, USE.NAMES = T)) %>% mutate(symbol = toupper(symbol)) %>% rename(uniprot = ipi) %>% filter(!(str_detect(uniprot, "contaminant"))) %>% filter(!(str_detect(symbol, "KRT"))) %>% filter(noqp.set_1 >= 3)

# protein ratio
mouse_group11_final <- mouse_group11 %>% mutate(uniprot = as.character(uniprot), description = as.character(description)) %>% group_by(uniprot) %>% filter(!(mr.set_1 == "0")) %>% filter(sequence == "") %>% mutate(average_protein_ratio_ms = mean(mr.set_1, na.rm = T), sd_ms = sd(mr.set_1, na.rm = T)) %>% select(uniprot, symbol, description, average_protein_ratio_ms, sd_ms) %>% ungroup(.) %>% unique(.) #%>% filter(average_protein_ratio_ms>=5)

#write.csv(mouse_group11_final, "mouse_group11.csv", row.names = F)

## Mouse group 2

filelist <- list.files(path = "C:/Users/Tom Lin/Documents/backup-office desktop/upenn/20171113-lab protocol/20200423-postCIMAGE/example/12. mouse brain membrane competition 4h", recursive=T, pattern = "\\.txt$", full.names = T)

mouse_group12 <- rbindlist(sapply(filelist, fread, simplify = FALSE, USE.NAMES = T)) %>% mutate(symbol = toupper(symbol)) %>% rename(uniprot = ipi) %>% filter(!(str_detect(uniprot, "contaminant"))) %>% filter(!(str_detect(symbol, "KRT"))) %>% filter(noqp.set_1 >= 3)

# protein ratio
mouse_group12_final <- mouse_group12 %>% mutate(uniprot = as.character(uniprot), description = as.character(description)) %>% group_by(uniprot) %>% filter(!(mr.set_1 == "0")) %>% filter(sequence == "") %>% mutate(average_protein_ratio_ms = mean(mr.set_1, na.rm = T), sd_ms = sd(mr.set_1, na.rm = T)) %>% select(uniprot, symbol, description, average_protein_ratio_ms, sd_ms) %>% ungroup(.) %>% unique(.) #%>% filter(average_protein_ratio_ms>=3)

#write.csv(mouse_group11_final, "mouse_group11.csv", row.names = F)

# merge two group together
mouse_all <- full_join(mouse_group11_final, mouse_group12_final, by=c("uniprot", "symbol", "description")) %>% unique(.) #%>% full_join(., mouse_group3_final, by=c("uniprot", "symbol", "description")) %>% unique(.)

# if you have the third group, delete "#" before %>% 
#write.csv(mouse_all, "mouse_together.csv", row.names = F)

## Merge human and mouse data

# accession table
accession <- read.csv("MGI_uniprot_sp_final.csv") %>% mutate(human_accession = as.character(human_accession), mouse_accession = as.character(mouse_accession))
# match human data with accession table
hu_accession1 <- inner_join(human_all, accession, by=c("uniprot" = "human_accession")) %>% select(-c(human_genename, HGNC.ID.x, mouse_genename, EntrezGene.ID.y, Mouse.MGI.ID.y)) %>% select(EntrezGene.ID.x, uniprot, symbol, description, contains("average_protein_ratio"), contains("sd"))
# match mouse data with accession data
ms_accession1 <- inner_join(mouse_all, accession, by=c("uniprot" = "mouse_accession")) %>% select(-c(human_genename, HGNC.ID.x, mouse_genename, EntrezGene.ID.y, Mouse.MGI.ID.y)) %>% select(EntrezGene.ID.x, uniprot, symbol, description, contains("average_protein_ratio"), contains("sd"))
# match human and mouse data
hu_ms_accession_ave_sd <- full_join(hu_accession1, ms_accession1, by = "EntrezGene.ID.x") %>% select(-EntrezGene.ID.x) %>% select(uniprot.x, symbol.x, description.x, contains("average_protein_ratio_h"), uniprot.y, symbol.y, description.y, contains("average_protein_ratio_ms"), contains("sd")) %>% unique(.)
# save result as csv
write.csv(hu_ms_accession_ave_sd, "human_mouse_accession_match_ave_sd.csv", row.names = F) 
# if you want to save as another file, please change the file name, which is "x.csv".

