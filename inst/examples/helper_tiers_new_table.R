library(act)

# --- Create a tier table from scratch
tierTable <- act::helper_tiers_new_table(c("a","b","c", "d"), 
c("IntervalTier", "TextTier","IntervalTier","TextTier"))
tierTable
