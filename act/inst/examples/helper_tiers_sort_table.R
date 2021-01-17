# This function applies to the tier tables that are necessary in \code{@tiers} of a transcript.
# object. For clarity, we will create such a table from scratch.

library(act)

# --- Create a tier table from scratch
tierTable <- helper_tiers_new_table(c("a","b","c", "d"), 
c("IntervalTier", "TextTier","IntervalTier","TextTier"))

# --- Create a vector, defining the new order of the tiers.
sortVector <- c("c","a","d","b")

# Sort the table
tierTable.1 <- act::helper_tiers_sort_table(tierTable=tierTable, sortVector=sortVector)
tierTable.1

# --- Create a vector, in which the tier "c" is missing.
sortVector <- c("a","b","d")

# Sort the table, the missing tier will be inserted at the end.
tierTable.1 <- act::helper_tiers_sort_table(tierTable=tierTable, sortVector=sortVector)
tierTable.1

# --- Create a vector, in which the tier "c" is missing, 
# but define the place, where missing tiers will be inserted by "*"
sortVector <- c("a","\\*", "b","d")

# Sort the table. The missing tier "c" will be inserted in second place.
tierTable.2 <- act::helper_tiers_sort_table(tierTable=tierTable, sortVector=sortVector)
tierTable.2

# Sort the table, but delete tiers that are missing in the sort vector
# Note: If 'deleteTiersThatAreNotInTheSortVector=TRUE' tiers that are missing in the
# will be deleted, even if the 'sortVector' contains a "\\*".
tierTable.3 <- act::helper_tiers_sort_table(tierTable=tierTable, 
sortVector=sortVector, 
deleteTiersThatAreNotInTheSortVector=TRUE)
tierTable.3

# --- Create a vector, which contains tier names that are not present in 'tierTable'.
sortVector <- c("c","a","x", "y", "d","b")
tierTable.4 <- act::helper_tiers_sort_table(tierTable=tierTable, sortVector=sortVector)
tierTable.4

