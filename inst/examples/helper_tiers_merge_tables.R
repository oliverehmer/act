library(act)

# --- Create two tier tables from scratch
tierTable1 <- act::helper_tiers_new_table(c("a","b","c","d"),
c("IntervalTier", "TextTier","IntervalTier","TextTier"))

tierTable2 <- act::helper_tiers_new_table(c("a","b","x","y"),
c("IntervalTier", "TextTier","IntervalTier","TextTier"))

tierTable3 <- act::helper_tiers_merge_tables(tierTable1,tierTable2)
tierTable3

