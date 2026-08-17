.arrow_find_row <- function(ann, hit_startsec, hit_endsec, hit_tier, epsilon = 0.01) {
	if (nrow(ann) == 0) return(NA_integer_)

	hit_mid <- (hit_startsec + hit_endsec) / 2

	# candidates: hit_mid falls within annotation bounds (with epsilon)
	in_range <- which(ann$startsec - epsilon <= hit_mid & ann$endsec + epsilon >= hit_mid)

	if (length(in_range) == 1) return(in_range)

	if (length(in_range) > 1) {
		candidates <- ann[in_range, , drop = FALSE]
		exact <- which(candidates$tierName == hit_tier)
		if (length(exact) >= 1) return(in_range[exact[1]])
		raw_dist  <- adist(hit_tier, candidates$tierName)[1, ]
		max_len   <- pmax(nchar(hit_tier), nchar(candidates$tierName))
		norm_dist <- raw_dist / max_len
		return(in_range[which.min(norm_dist)])
	}

	# fallback: no annotation contains hit_mid; search by startsec proximity
	near <- which(abs(ann$startsec - hit_startsec) <= 0.5)
	if (length(near) == 0) return(NA_integer_)
	if (length(near) == 1) return(near)

	candidates <- ann[near, , drop = FALSE]
	exact <- which(candidates$tierName == hit_tier)
	if (length(exact) >= 1) return(near[exact[1]])
	raw_dist  <- adist(hit_tier, candidates$tierName)[1, ]
	max_len   <- pmax(nchar(hit_tier), nchar(candidates$tierName))
	norm_dist <- raw_dist / max_len
	return(near[which.min(norm_dist)])
}
