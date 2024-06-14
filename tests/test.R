library(rpheno)

occs_1 = phenoccurrences("Asclepias incarnata")
occs_2 = phenoccurrences("Asclepias curassavica")

pheno_1 = phenology(occs_1)
pheno_2 = phenology(occs_2)

pheno_clean_1 = pheno_clean(pheno_1)
pheno_clean_2 = pheno_clean(pheno_2)

overlap_binary = pheno_overlap(pheno_clean_1, pheno_clean_2)

overlap_value = pheno_overlap(pheno_clean_1, pheno_clean_2, mode="gam")
