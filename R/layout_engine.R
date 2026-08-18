# ===== WRAP-AWARE ALIGNMENT ENGINE =====
# Token-based renderer for aligned print transcripts (GAT / Mondada score).
# The wrap decision is made BEFORE each token is written, so every written
# column is final. Developed and reviewed in _dev/new_docx_export/ (see the
# git history there for all design decisions and user rules).
#
# Public API (exported): helper_layout_render(), helper_layout_anchors(),
# helper_layout_bracket_pairs(), helper_layout_symbol_matches().
# Everything else in this file is internal.

# ======================================================================
# ==== source module: render_aligned.R ====

HARD_BREAK_CHAR <- "\u23ce"

# ===== TOP LEVEL =====

align_and_render <- function(ann, text_body_width, arrow_mode = "stem",
                             verbal_align = TRUE, layout_mode = "gat",
                             symbol_merge = FALSE, time_tolerance = 0.5,
                             time_tolerance_point = 0.2,
                             min_description = 10L, lone_symbol_fill = 2L) {
	if (!identical(arrow_mode, "space")) arrow_mode <- "stem"

	mm_anchor_chars <- unique(unlist(
		lapply(ann$align_chars[!is.na(ann$align_chars)],
		       function(x) strsplit(x, "")[[1]])))
	# Picture marks fold under a stricter tolerance than gesture marks
	# (user decision 2026-08-17: stills 0.2 s, others 0.5 s).
	point_rows <- !is.na(ann$align_mode) & ann$align_mode == "point"
	point_chars <- unique(unlist(lapply(
		ann$align_chars[!is.na(ann$align_chars) & point_rows],
		function(x) strsplit(x, "")[[1]])))

	if (identical(layout_mode, "mondada")) {
		ann <- concatenate_mondada_rows(ann, mm_anchor_chars,
		                                layer_seam = time_tolerance)
		main_fragment_starts <- attr(ann, "main_fragment_starts")
	} else {
		# GAT exception (user decision 2026-08-17): consecutive verbal
		# annotations made of nothing but pauses and symbols share a line.
		ann <- concatenate_mondada_rows(ann, mm_anchor_chars,
		                                layer_seam = time_tolerance,
		                                pause_only = TRUE,
		                                pause_line_limit = text_body_width)
		main_fragment_starts <- attr(ann, "main_fragment_starts")
		# A joined pause run stays ONE line (user comment 205_005 GAT K1,
		# 2026-08-17): no wrapping, and - via the wrap flag - no break
		# hints and no unfold breaks either.
		pause_rows <- ann$is_main & vapply(ann$content,
			.is_pause_only_content, logical(1),
			anchor_char_set = mm_anchor_chars)
		ann$wrap[pause_rows] <- FALSE
	}

	pairs <- if (verbal_align) compute_bracket_pairs(ann) else
		data.frame(i_row = integer(0), i_occurrence = integer(0),
		           j_row = integer(0), j_occurrence = integer(0))
	flattened <- apply_double_bracket_flattening(ann, pairs)
	ann <- flattened$ann
	pairs <- flattened$pairs

	ref_main <- resolve_reference_main(ann)
	mm_matches <- compute_mm_symbol_matches(ann, ref_main)

	all_anchor_chars <- unique(c("[", mm_anchor_chars))
	ann_base <- ann
	no_fold <- NULL
	unfold_rounds <- 0L
	merge_map <- NULL
	merge_events <- NULL
	repeat {
	ann <- ann_base
	merge_map <- NULL
	merge_events <- NULL
	if (isTRUE(symbol_merge) && length(mm_anchor_chars) > 0) {
		ann <- collapse_equivalent_layer_gaps(ann, mm_matches, mm_anchor_chars,
		                                      time_tolerance, point_chars,
		                                      time_tolerance_point)
		merged <- apply_symbol_merge(ann, mm_anchor_chars, mm_matches,
		                             time_tolerance, point_chars,
		                             time_tolerance_point, no_fold = no_fold)
		ann <- merged$ann
		merge_map <- merged$map
		merge_events <- merged$events
	}

	ann <- apply_bracket_interna_padding(ann, pairs)
	# Two passes over the MAIN rows: the first one only measures where the
	# symbols end up, the second one renders for real. Only with the
	# measured line/column of every symbol can we decide (a) where the
	# verbal line has to break early so a layer description keeps its
	# alignment, and (b) which spans really need widening - both depend on
	# the rendered layout, not on the raw text (user rules 2026-08-14).
	# The word carrying a symbol moves WITH its description: when a layer
	# annotation has to start in the next block, the verbal line breaks
	# before that word, so symbol and description stay together and keep
	# their alignment (user rule 2026-08-14). Iterative, because each break
	# shifts everything behind it.
	# A mark that would sit at the very end of a verbal line travels to the
	# start of the next line, so its description starts there with it. Then
	# the span widening runs on the new layout. Iterative, because moving a
	# mark shifts what follows.
	# Deliberately a SINGLE pass: moving a mark shifts everything behind it,
	# and chasing the follow-up cases would keep breaking lines until the
	# verbal text is shredded. Cases that only appear after this pass are
	# reported instead.
	# Order matters: widen FIRST, because that changes where the lines
	# break, and only then decide which mark really ends up at the very
	# edge and has to travel to the next line. Deciding it the other way
	# round moves marks that are nowhere near the edge in the final layout.
	preliminary <- .render_main_positions(ann, pairs, text_body_width,
	                                      ref_main, mm_matches, merge_map,
	                                      main_fragment_starts, arrow_mode,
	                                      all_anchor_chars)
	ann <- apply_cluster_close_open_reorder(ann, mm_matches, preliminary,
	                                        all_anchor_chars, time_tolerance,
	                                        point_chars, time_tolerance_point)
	ann <- .apply_unfold_breaks(ann, no_fold, merge_map)
	ann <- apply_mm_span_stretch(ann, mm_matches, all_anchor_chars, merge_map,
	                             preliminary, text_body_width)
	preliminary <- .render_main_positions(ann, pairs, text_body_width,
	                                      ref_main, mm_matches, merge_map,
	                                      main_fragment_starts, arrow_mode,
	                                      all_anchor_chars)
	ann <- apply_main_break_hints(ann, mm_matches, preliminary,
	                              text_body_width, min_description,
	                              anchor_chars = all_anchor_chars,
	                              merge_map = merge_map)
	# Moving a mark to the next line changes the spans around it, so the
	# widening has to run again on the new layout - otherwise the
	# description that just moved down has no room to reach its closing
	# symbol (user mock-up 2026-08-14). Widening itself shifts the line
	# breaks again, so this repeats until nothing changes: a span that got
	# too little room in one round is completed in the next.
	for (pass in seq_len(4L)) {
		preliminary <- .render_main_positions(ann, pairs, text_body_width,
		                                      ref_main, mm_matches, merge_map,
		                                      main_fragment_starts, arrow_mode,
		                                      all_anchor_chars)
		text_before <- ann$text
		ann <- apply_mm_span_stretch(ann, mm_matches, all_anchor_chars,
		                             merge_map, preliminary, text_body_width)
		if (identical(ann$text, text_before)) break
	}
	# Unfold at break (user rule 2026-08-17): folded marks whose closing
	# description runs across a line break are split again - one repeat
	# with those folds vetoed, then the reorder pass distributes closing
	# and opening halves onto their lines.
	unfold_rounds <- unfold_rounds + 1L
	if (unfold_rounds >= 4L) break
	found <- .folds_across_break(ann, merge_events, mm_matches, merge_map,
	                             preliminary, all_anchor_chars)
	if (!is.null(no_fold) && nrow(found) > 0) {
		seen <- paste(no_fold$row, no_fold$char, no_fold$occurrence)
		found <- found[!(paste(found$row, found$char, found$occurrence) %in%
		                 seen), , drop = FALSE]
	}
	if (nrow(found) == 0) break
	no_fold <- rbind(no_fold, found)
	}

	rendered_cache <- vector("list", nrow(ann))
	rendered_lines <- vector("list", nrow(ann))
	anchor_specs   <- vector("list", nrow(ann))
	lead_lines     <- vector("list", nrow(ann))
	report_list    <- list()
	warning_list   <- list()

	row_index <- seq_len(nrow(ann))
	process_key <- pmax(row_index, ifelse(is.na(ref_main), row_index, ref_main))
	if (nrow(mm_matches) > 0) {
		for (layer_row in unique(mm_matches$layer_row)) {
			latest_source <- max(mm_matches$main_row[mm_matches$layer_row == layer_row])
			process_key[layer_row] <- max(process_key[layer_row], latest_source)
		}
	}
	process_order <- order(process_key, !ann$is_main, row_index)

	render_row <- function(i) {
		if (!is.null(ann$indent_mode) && !is.na(ann$indent_mode[i]) &&
		    identical(ann$indent_mode[i], "text")) {
			ann$text[i] <<- apply_text_indent(ann, i, rendered_cache, ref_main)
		}
		anchor_info <- compute_anchors(ann, i, rendered_cache, pairs,
		                               text_body_width, ref_main, mm_matches,
		                               merge_map, main_fragment_starts)
		anchor_specs[[i]] <<- anchor_info$anchors
		if (length(anchor_info$warnings) > 0) {
			warning_list <<- c(warning_list, anchor_info$warnings)
		}
		has_anchor_target <- nrow(anchor_info$anchors) > 0 &&
			any(!is.na(anchor_info$anchors$target_col))
		if (!has_anchor_target && isTRUE(ann$is_main[i])) {
			ann$text[i] <<- apply_speaker_continuation(ann, i, rendered_lines)
		}
		width_i <- if (isTRUE(ann$wrap[i])) text_body_width else Inf
		# A gesture counts as "started earlier" either by its time stamp or
		# by the Mondada marker ">>" (annotation cut off at the excerpt
		# boundary, so the times cannot show it).
		lead_allowed <- !isTRUE(ann$is_main[i]) && !is.na(ref_main[i]) &&
			(ann$startsec[i] < ann$startsec[ref_main[i]] - 0.02 ||
			 stringr::str_detect(stringr::str_trim(ann$content[i]), "^>>"))
		result <- render_annotation_tokens(
			text          = ann$text[i],
			anchors       = anchor_info$anchors,
			width         = width_i,
			prefix_first  = ann$prefix_first[i],
			prefix_cont   = ann$prefix_cont[i],
			arrow_mode    = arrow_mode,
			pair_mode     = identical(ann$align_mode[i], "bracket"),
			lead_allowed  = lead_allowed,
			min_description = min_description
		)
		if (isTRUE(ann$is_main[i])) {
			result$lines <- .collapse_latch_in_lines(result$lines,
			                                         all_anchor_chars)
			result$lines <- .drop_space_after_leading_mark(
				result$lines, all_anchor_chars, nchar(ann$prefix_cont[i]))
			result$lines <- .duplicate_latch_at_break(
				result$lines, nchar(ann$prefix_cont[i]), width_i)
		}
		rendered_lines[[i]] <<- result$lines
		lead_lines[[i]] <<- result$lead_lines
		if (!is.null(result$moved_anchors) && nrow(anchor_specs[[i]]) > 0) {
			for (m in seq_len(nrow(result$moved_anchors))) {
				hit <- anchor_specs[[i]]$char == result$moved_anchors$char[m] &
					anchor_specs[[i]]$occurrence == result$moved_anchors$occurrence[m]
				anchor_specs[[i]]$target_line[hit] <<-
					anchor_specs[[i]]$target_line[hit] + 1L
			}
		}
		if (nrow(result$placements) > 0) {
			result$placements$row <- i
			report_list[[length(report_list) + 1]] <<- result$placements
		}
		rendered_cache[[i]] <<- list(
			lines     = result$lines,
			positions = extract_anchor_positions(result$lines, all_anchor_chars)
		)
	}

	# MAIN rows first, then the close-bracket alignment that may still
	# shift whole main lines, and only THEN the layer rows: their anchors
	# have to measure the FINAL main layout, otherwise every mark under a
	# bracket-shifted line is off by the shift (user comment 207_024 K0,
	# 2026-08-17).
	for (i in process_order[ann$is_main[process_order]]) render_row(i)
	if (nrow(pairs) > 0) {
		rendered_lines <- align_close_bracket_lines(ann, rendered_lines, pairs,
		                                            text_body_width)
		for (i in which(ann$is_main)) {
			if (is.null(rendered_cache[[i]])) next
			rendered_cache[[i]] <- list(
				lines     = rendered_lines[[i]],
				positions = extract_anchor_positions(rendered_lines[[i]],
				                                     all_anchor_chars))
		}
	}
	for (i in process_order[!ann$is_main[process_order]]) render_row(i)

	ann$rendered_lines <- rendered_lines
	ann$anchor_specs <- anchor_specs
	ann$lead_lines <- lead_lines
	report_so_far <- if (length(report_list) > 0) {
		do.call(rbind, report_list)
	} else {
		NULL
	}
	indented <- apply_indent_alignment(ann, report_so_far, all_anchor_chars,
	                                   text_body_width)
	ann <- indented$ann
	if (!is.null(report_so_far)) report_list <- list(indented$report)
	ann <- .pad_lone_symbol_lines(ann, all_anchor_chars, lone_symbol_fill)
	attr(ann, "anchor_report") <- if (length(report_list) > 0) {
		do.call(rbind, report_list)
	} else {
		data.frame(char = character(0), occurrence = integer(0),
		           target_col = integer(0), placed_col = integer(0),
		           degraded = logical(0), note = character(0),
		           row = integer(0))
	}
	# Final truth: an anchor sitting on its target column is not degraded,
	# no matter what happened on the way there (indent, widening, moves).
	report_final <- attr(ann, "anchor_report")
	if (!is.null(report_final) && nrow(report_final) > 0) {
		on_target <- !is.na(report_final$target_col) &
			!is.na(report_final$placed_col) &
			report_final$placed_col == report_final$target_col &
			report_final$note != "moved_to_next_line"
		report_final$degraded[on_target] <- FALSE
		report_final$note[on_target & report_final$note == "not_at_target"] <- ""
		attr(ann, "anchor_report") <- report_final
	}
	attr(ann, "render_warnings") <- warning_list
	attr(ann, "bracket_pairs") <- pairs
	attr(ann, "merge_events") <- merge_events
	attr(ann, "ref_main") <- ref_main
	ann
}

# ===== PASS 1: MEASURE THE MAIN ROWS =====
# Renders the main rows exactly as pass 2 would, but only keeps the symbol
# positions (char, occurrence, col, line). Nothing is emitted.

.render_main_positions <- function(ann, pairs, text_body_width, ref_main,
                                   mm_matches, merge_map,
                                   main_fragment_starts, arrow_mode,
                                   all_anchor_chars) {
	cache <- vector("list", nrow(ann))
	for (i in which(ann$is_main)) {
		anchor_info <- compute_anchors(ann, i, cache, pairs, text_body_width,
		                               ref_main, mm_matches, merge_map,
		                               main_fragment_starts)
		width_i <- if (isTRUE(ann$wrap[i])) text_body_width else Inf
		result <- render_annotation_tokens(
			text         = ann$text[i],
			anchors      = anchor_info$anchors,
			width        = width_i,
			prefix_first = ann$prefix_first[i],
			prefix_cont  = ann$prefix_cont[i],
			arrow_mode   = arrow_mode,
			pair_mode    = identical(ann$align_mode[i], "bracket"))
		result$lines <- .collapse_latch_in_lines(result$lines, all_anchor_chars)
		result$lines <- .drop_space_after_leading_mark(result$lines,
			all_anchor_chars, nchar(ann$prefix_cont[i]))
		cache[[i]] <- list(
			lines     = result$lines,
			positions = extract_anchor_positions(result$lines, all_anchor_chars))
	}
	cache
}

# ===== EARLY BREAK OF THE VERBAL LINE =====
# A symbol that opens a layer description must not sit at the very end of a
# verbal line - the description would have no room and would lose its
# alignment. In that case the verbal line breaks BEFORE the word carrying
# the symbol, so symbol and description start together on the next line.
# Realised by inserting the hard-break character, which the renderer
# already honours.

apply_main_break_hints <- function(ann, mm_matches, preliminary,
                                   text_body_width, min_description,
                                   one_per_row = TRUE,
                                   anchor_chars = character(0),
                                   merge_map = NULL) {
	if (nrow(mm_matches) == 0 || !is.finite(text_body_width)) return(ann)
	for (main_row in unique(mm_matches$main_row)) {
		# A row that never wraps must not get a hard break either - the
		# hint would turn a deliberately single-line row into two lines
		# (user comment 205_005 GAT K1, 2026-08-17).
		if (!isTRUE(ann$wrap[main_row])) next
		cache <- preliminary[[main_row]]
		if (is.null(cache) || nrow(cache$positions) == 0) next
		positions <- cache$positions
		graphemes <- split_graphemes(ann$text[main_row])
		breaks <- integer(0)
		my_matches <- mm_matches[mm_matches$main_row == main_row, , drop = FALSE]
		for (k in seq_len(nrow(my_matches))) {
			# only symbols that OPEN a description with text behind it
			layer_row <- my_matches$layer_row[k]
			description <- .description_after_symbol(ann, layer_row,
			                                         my_matches$char[k],
			                                         my_matches$layer_occurrence[k])
			if (description < min_description) next
			hit <- positions[positions$char == my_matches$char[k] &
			                 positions$occurrence == my_matches$main_occurrence[k], ,
			                 drop = FALSE]
			if (nrow(hit) == 0) next
			if ((text_body_width - hit$col[1]) >= min_description) next
			index <- .symbol_index_in_text(graphemes, my_matches$char[k],
			                               my_matches$main_occurrence[k])
			if (is.na(index) || index <= 1L) next
			# Never break INSIDE a marker cluster: the break moves in
			# front of the whole cluster, so "+&" stays together (user
			# report 205_005 GAT line 66, 2026-08-17). Closing marks are
			# the exception: their description ends on the previous line,
			# so the break stops behind them and only the opening halves
			# travel down (unfold at break, user mock-up 207_024
			# 2026-08-17).
			while (index > 1L && graphemes[index - 1L] %in% anchor_chars) {
				if (.main_symbol_closes(ann, mm_matches, merge_map,
				                        main_row, graphemes, index - 1L)) break
				index <- index - 1L
			}
			if (index <= 1L) next
			# The break goes right BEFORE the symbol - only the mark itself
			# travels to the next line, the word it was attached to stays
			# ("ABro|" -> "ABro" / "|bien ..."). Breaking before the whole
			# word would tear the verbal line apart (user mock-up
			# 2026-08-14).
			breaks <- c(breaks, index)
		}
		if (length(breaks) == 0) next
		breaks <- sort(unique(breaks), decreasing = TRUE)
		# One break per pass: every break shifts everything behind it, so
		# the remaining candidates have to be measured again.
		if (isTRUE(one_per_row)) breaks <- breaks[length(breaks)]
		for (position in breaks) {
			# A mark that starts a line needs no space behind it - the space
			# only separated it from the following word while it still sat
			# at the end of the previous line (user rule 2026-08-14).
			if (position < length(graphemes) &&
			    stringr::str_detect(graphemes[position + 1L], "^\\s$")) {
				graphemes <- graphemes[-(position + 1L)]
			}
			graphemes <- append(graphemes, HARD_BREAK_CHAR, after = position - 1L)
		}
		ann$text[main_row] <- paste(graphemes, collapse = "")
	}
	ann
}

# Length of the description a symbol OPENS. A symbol counts as opening
# when a word follows it directly (no space) - "|moves ..." opens,
# "... position|" closes. Only opening symbols need room behind them.
.description_after_symbol <- function(ann, row, char, occurrence) {
	graphemes <- split_graphemes(ann$text[row])
	index <- .symbol_index_in_text(graphemes, char, occurrence)
	if (is.na(index) || index >= length(graphemes)) return(0L)
	following <- graphemes[index + 1L]
	if (stringr::str_detect(following, "^\\s$")) return(0L)
	rest <- graphemes[(index + 1L):length(graphemes)]
	stop_at <- which(rest == char)[1]
	if (is.na(stop_at)) length(rest) else stop_at - 1L
}

# Unfolded marks get a hard break between their halves: the closing half
# stays on the line where its description ends, the opening half starts
# the next line (user mock-up 207_024, 2026-08-17). Without this break the
# whole cluster is one unbreakable token and wraps down as a block.

.apply_unfold_breaks <- function(ann, no_fold, merge_map) {
	if (is.null(no_fold) || nrow(no_fold) == 0) return(ann)
	for (k in seq_len(nrow(no_fold))) {
		r <- no_fold$row[k]
		# Only MAIN rows carry the hard break; an unfolded layer seam is
		# distributed by its anchors, a break in the layer text would
		# corrupt the close/open role detection.
		if (!isTRUE(ann$is_main[r])) next
		if (!isTRUE(ann$wrap[r])) next
		g <- no_fold$char[k]
		occurrence <- remap_symbol_occurrence(merge_map, r, g,
		                                      no_fold$occurrence[k])
		graphemes <- split_graphemes(ann$text[r])
		index <- .symbol_index_in_text(graphemes, g, occurrence)
		if (is.na(index) || index <= 1L) next
		if (graphemes[index - 1L] == HARD_BREAK_CHAR) next
		graphemes <- append(graphemes, HARD_BREAK_CHAR, after = index - 1L)
		ann$text[r] <- paste(graphemes, collapse = "")
	}
	ann
}

# Role of a main-text mark, derived from its matched layer symbols: it
# CLOSES when at least one match maps onto this rendered occurrence and
# none of the mapped matches opens a description. A still-folded mark
# carries an opening half too and therefore never counts as closing.

.main_symbol_closes <- function(ann, mm_matches, merge_map, main_row,
                                graphemes, at) {
	g <- graphemes[at]
	rendered_occurrence <- sum(graphemes[seq_len(at)] == g)
	candidates <- mm_matches[mm_matches$main_row == main_row &
	                         mm_matches$char == g, , drop = FALSE]
	found <- FALSE
	for (k in seq_len(nrow(candidates))) {
		mapped <- remap_symbol_occurrence(merge_map, main_row, g,
		                                  candidates$main_occurrence[k])
		if (is.na(mapped) || mapped != rendered_occurrence) next
		if (.description_after_symbol(ann, candidates$layer_row[k], g,
		                              candidates$layer_occurrence[k]) > 0L) {
			return(FALSE)
		}
		found <- TRUE
	}
	found
}

.symbol_index_in_text <- function(graphemes, char, occurrence) {
	hits <- which(graphemes == char)
	if (length(hits) < occurrence) return(NA_integer_)
	hits[occurrence]
}

# ===== REFERENCE MAIN RESOLUTION =====
# Layer rows are matched to the main-tier row with the largest TEMPORAL
# overlap - not by walk-back in sort order. Layers that start before their
# main (continued gestures, leading annotations) still find the right main.
# Fallbacks: last preceding main, then first main of the transcript.

resolve_reference_main <- function(ann) {
	n <- nrow(ann)
	ref <- rep(NA_integer_, n)
	main_rows <- which(ann$is_main)
	if (length(main_rows) == 0) return(ref)
	base_lower <- tolower(sub("#.*$", "", ann$tierName))
	main_names_lower <- tolower(ann$tierName[main_rows])
	for (i in seq_len(n)) {
		if (ann$is_main[i]) {
			ref[i] <- i
			next
		}
		overlap <- pmin(ann$endsec[main_rows], ann$endsec[i]) -
		           pmax(ann$startsec[main_rows], ann$startsec[i])
		candidates <- which(overlap > 0)
		if (length(candidates) > 0) {
			base_hits <- candidates[main_names_lower[candidates] == base_lower[i]]
			pool <- if (length(base_hits) > 0) base_hits else candidates
			best_overlap <- max(overlap[pool])
			pool <- pool[overlap[pool] >= best_overlap - 1e-9]
			ref[i] <- main_rows[max(pool)]
		} else {
			preceding <- main_rows[main_rows < i]
			ref[i] <- if (length(preceding) > 0) max(preceding) else main_rows[1]
		}
	}
	ref
}

# ===== SCORE MODE: CONCATENATION AND LINE NUMBERS =====
# Score mode (Mondada partitur): annotations of the same tier are joined
# into one flowing row. Seamless boundaries (<= 0.02s) always join; across
# larger gaps MAIN tiers join unless another main tier's annotation STARTS
# inside the gap (= turn change breaks the flow; the pause itself is
# already written as GAT notation at the fragment end). Layer tiers join
# unconditionally (their lines are re-distributed per target main line by
# the interleaver). Separator: no space when a boundary grapheme is an
# anchor char - EXCEPT layer joins across a real gap, which always get a
# space so that two boundary symbols stay separate and match main symbols
# monotonically (F1/F2, user GO 2026-08-13).
#
# Because joined rows span time gaps, plain whole-row interpolation would
# skew symbol times. Each joined row therefore carries a symbol time table
# (char, occurrence, time, index, fragment_start, flush_candidate) built
# per ORIGINAL fragment; matching and pairing use it when present.

.is_pause_only_content <- function(x, anchor_char_set) {
	if (is.na(x)) return(FALSE)
	stripped <- gsub("\\((?:[0-9.]+|[.\\-]{1,3})\\)", "", x)
	stripped <- gsub("[[:space:]]", "", stripped)
	g <- strsplit(stripped, "")[[1]]
	length(g) == 0 || all(g %in% anchor_char_set)
}

concatenate_mondada_rows <- function(ann, anchor_char_set,
                                     layer_seam = 0.08,
                                     pause_only = FALSE,
                                     pause_line_limit = Inf) {
	# pause_only (GAT exception, user decision 2026-08-17): ONLY verbal
	# annotations consisting of nothing but pauses and annotation symbols
	# are joined into one line; everything else keeps one line per
	# annotation, and layer rows are never joined.
	is_pause_only <- function(x) {
		.is_pause_only_content(x, anchor_char_set)
	}
	main_fragment_starts <- ann$startsec[ann$is_main]
	if (nrow(ann) < 2) {
		ann <- .attach_symbol_time_tables(ann, NULL, anchor_char_set)
		attr(ann, "main_fragment_starts") <- main_fragment_starts
		return(ann)
	}
	seam_tolerance <- 0.02
	# Layer fragments use the merge-guard tolerance instead: a latch or a
	# boundary within 0.08 s still denotes the same point (user comments
	# 207_021 K0/K2, 2026-08-17); the verbal seam stays strict.
	fragment_rows <- lapply(seq_len(nrow(ann)), function(i) {
		data.frame(startsec = ann$startsec[i], endsec = ann$endsec[i],
		           content = ann$content[i], sep_before = "",
		           stringsAsFactors = FALSE)
	})
	keep <- rep(TRUE, nrow(ann))
	last_of_tier <- list()
	for (i in seq_len(nrow(ann))) {
		tier <- ann$tierName[i]
		previous <- last_of_tier[[tier]]
		join <- FALSE
		gap <- NA_real_
		if (!is.null(previous) && !is.na(ann$content[i]) &&
		    !is.na(ann$content[previous])) {
			gap <- ann$startsec[i] - ann$endsec[previous]
			seam_i <- if (ann$is_main[i]) seam_tolerance else layer_seam
			if (isTRUE(pause_only) &&
			    (!ann$is_main[i] ||
			     !is_pause_only(ann$content[i]) ||
			     !is_pause_only(ann$content[previous]))) {
				join <- FALSE
			} else if (isTRUE(pause_only)) {
				# pause runs join unless a NON-pause main starts in the gap
				turn_break <- any(ann$is_main &
				                  !vapply(ann$content, is_pause_only, logical(1)) &
				                  ann$startsec > ann$endsec[previous] &
				                  ann$startsec < ann$startsec[i])
				# ... and only while the joined line still fits the
				# transcript width - a pause run never wraps, so the join
				# starts a NEW line instead of growing a mega block
				# (user comment 205_005, 2026-08-17).
				fits <- nchar(ann$content[previous]) + 1L +
					nchar(ann$content[i]) <=
					pause_line_limit - nchar(ann$prefix_first[i])
				join <- !turn_break && fits
			} else if (gap <= seam_i) {
				join <- TRUE
			} else if (ann$is_main[i]) {
				turn_break <- any(ann$is_main & ann$tierName != tier &
				                  ann$startsec > ann$endsec[previous] &
				                  ann$startsec < ann$startsec[i])
				join <- !turn_break
			} else {
				# A layer row joins across a gap only while it stays inside
				# one turn: as soon as ANY main annotation starts in the gap,
				# the fragments anchor into different main rows, and one
				# joined row could not be distributed by the interleaver
				# (its anchors would point outside its group).
				turn_break <- any(ann$is_main &
				                  ann$startsec > ann$endsec[previous] &
				                  ann$startsec < ann$startsec[i])
				join <- !turn_break
			}
		}
		if (join) {
			# Glue without a space only when BOTH sides carry a symbol that
			# must not be separated from its neighbour: an anchor char or
			# the GAT latching "=". A word on either side always keeps the
			# space ("laterAL;+" + "y busca" -> "laterAL;+ y busca").
			glue_chars <- c(anchor_char_set, "=")
			previous_g <- split_graphemes(ann$content[previous])
			next_g <- split_graphemes(ann$content[i])
			left_glue <- length(previous_g) > 0 &&
				previous_g[length(previous_g)] %in% glue_chars
			right_glue <- length(next_g) > 0 && next_g[1] %in% glue_chars
			# A symbol next to a pause bracket already separates the two -
			# no extra space is inserted there: "(0.1)" + "&(0.2)" becomes
			# "(0.1)&(0.2)" (user comment K4, 2026-08-16).
			left_pause <- length(previous_g) > 0 &&
				previous_g[length(previous_g)] == ")"
			right_pause <- length(next_g) > 0 && next_g[1] == "("
			pause_glue <- (left_pause && right_glue) ||
				(left_glue && right_pause)
			layer_gap_join <- !ann$is_main[i] && gap > layer_seam
			separator <- if (((left_glue && right_glue) || pause_glue) &&
			                 !layer_gap_join) "" else " "
			# Two latch marks meeting head-on are ONE latch, so the text
			# carries a single "=". Should the line break exactly there,
			# .duplicate_latch_at_break() writes the second one back - GAT
			# needs it on both sides then. Reducing here (and not after
			# rendering) matters: otherwise the wrap decision is made on a
			# line that is one character too long (user rule 2026-08-14).
			next_content <- ann$content[i]
			if (identical(separator, "") &&
			    length(previous_g) > 0 && length(next_g) > 0 &&
			    previous_g[length(previous_g)] == "=" && next_g[1] == "=") {
				next_content <- paste(next_g[-1], collapse = "")
			}
			# Seamlessly adjacent layer annotations share ONE boundary
			# symbol (user comment 206_010 K0, 2026-08-16; latch "="
			# transparent, 207_021 K0/K2). The fold itself happens in
			# apply_symbol_merge - the adjacent marks stay in the text
			# here, so an unfold at a line break can veto the fold
			# (user mock-up 207_024, 2026-08-17).
			new_fragment <- fragment_rows[[i]][1, ]
			new_fragment$sep_before <- separator
			new_fragment$content <- next_content
			fragment_rows[[previous]] <- rbind(fragment_rows[[previous]],
			                                   new_fragment)
			ann$content[previous] <- paste0(ann$content[previous], separator,
			                                next_content)
			ann$endsec[previous] <- ann$endsec[i]
			if (!is.null(ann$space_after)) {
				ann$space_after[previous] <- ann$space_after[i]
			}
			keep[i] <- FALSE
		} else {
			last_of_tier[[tier]] <- i
		}
	}
	fragment_rows <- fragment_rows[keep]
	ann <- ann[keep, , drop = FALSE]
	fragment_rows <- .collapse_latch_around_marks(ann, fragment_rows,
	                                              anchor_char_set)
	ann$content <- vapply(fragment_rows, function(fragments) {
		paste0(fragments$sep_before, fragments$content, collapse = "")
	}, character(1))
	rownames(ann) <- NULL
	ann <- .attach_symbol_time_tables(ann, fragment_rows, anchor_char_set)
	attr(ann, "main_fragment_starts") <- main_fragment_starts
	ann
}

# GAT latching around a marker: "=#=" means one latch, not two - the "="
# in front of the mark is dropped ("=#=" -> "#=", user rule 2026-08-14).
.collapse_latch_around_marks <- function(ann, fragment_rows, anchor_char_set) {
	for (r in seq_len(nrow(ann))) {
		if (!isTRUE(ann$is_main[r])) next
		fragments <- fragment_rows[[r]]
		frag_g <- lapply(fragments$content, function(x) {
			if (is.na(x)) character(0) else split_graphemes(x)
		})
		sep_g <- lapply(fragments$sep_before, function(x) {
			if (is.na(x) || !nzchar(x)) character(0) else split_graphemes(x)
		})
		glyphs <- character(0)
		owner <- integer(0)
		is_sep <- logical(0)
		for (f in seq_len(nrow(fragments))) {
			glyphs <- c(glyphs, sep_g[[f]], frag_g[[f]])
			owner <- c(owner, rep(f, length(sep_g[[f]]) + length(frag_g[[f]])))
			is_sep <- c(is_sep, rep(TRUE, length(sep_g[[f]])),
			            rep(FALSE, length(frag_g[[f]])))
		}
		# Two latch marks with nothing but annotation symbols between them
		# are ONE latch: "=+&=" -> "+&=" (user comment K7, 2026-08-15;
		# generalises the old single-mark "=#=" -> "#=" rule, also across
		# the fragment seam).
		drop <- integer(0)
		i <- 1L
		n <- length(glyphs)
		while (i <= n) {
			if (glyphs[i] == "=" && !is_sep[i]) {
				j <- i + 1L
				while (j <= n && glyphs[j] %in% anchor_char_set) j <- j + 1L
				if (j <= n && j > i + 1L && glyphs[j] == "=") {
					drop <- c(drop, i)
					i <- j
					next
				}
			}
			i <- i + 1L
		}
		if (length(drop) > 0) {
			keep <- setdiff(seq_len(n), drop)
			for (f in seq_len(nrow(fragments))) {
				sel <- keep[owner[keep] == f & !is_sep[keep]]
				fragments$content[f] <- paste(glyphs[sel], collapse = "")
			}
			fragment_rows[[r]] <- fragments
		}
	}
	fragment_rows
}

.attach_symbol_time_tables <- function(ann, fragment_rows, anchor_char_set) {
	time_chars <- unique(c(anchor_char_set, "["))
	tables <- vector("list", nrow(ann))
	for (r in seq_len(nrow(ann))) {
		fragments <- if (is.null(fragment_rows)) {
			data.frame(startsec = ann$startsec[r], endsec = ann$endsec[r],
			           content = ann$content[r], sep_before = "",
			           stringsAsFactors = FALSE)
		} else {
			fragment_rows[[r]]
		}
		offset <- 0L
		rows_list <- list()
		occ_counter <- list()
		for (f in seq_len(nrow(fragments))) {
			offset <- offset + length(split_graphemes(fragments$sep_before[f]))
			if (is.na(fragments$content[f])) next
			graphemes <- split_graphemes(fragments$content[f])
			hits <- which(graphemes %in% time_chars)
			leading_end <- 0L
			g_idx <- 1L
			while (g_idx <= length(graphemes) &&
			       stringr::str_detect(graphemes[g_idx], "^\\s$")) {
				g_idx <- g_idx + 1L
			}
			while (g_idx <= length(graphemes) &&
			       graphemes[g_idx] %in% time_chars) {
				leading_end <- g_idx
				g_idx <- g_idx + 1L
			}
			word_positions <- which(!(graphemes %in% time_chars) &
			                        !stringr::str_detect(graphemes, "^\\s$"))
			trailing_from <- if (length(word_positions) == 0) {
				1L
			} else {
				max(word_positions) + 1L
			}
			duration <- max(fragments$endsec[f] - fragments$startsec[f], 0)
			for (hit in hits) {
				char <- graphemes[hit]
				n <- occ_counter[[char]]
				if (is.null(n)) n <- 0L
				occ_counter[[char]] <- n + 1L
				rows_list[[length(rows_list) + 1]] <- data.frame(
					char = char, occurrence = n + 1L,
					time = fragments$startsec[f] +
						duration * (hit - 1L) / max(length(graphemes) - 1L, 1L),
					index = offset + hit,
					fragment_start = fragments$startsec[f],
					fragment_end = fragments$endsec[f],
					fragment_index = hit,
					flush_candidate = hit <= leading_end,
					trailing_candidate = hit >= trailing_from
				)
			}
			offset <- offset + length(graphemes)
		}
		if (length(rows_list) > 0) {
			tables[[r]] <- do.call(rbind, rows_list)
		}
	}
	ann$symbol_times_table <- tables
	ann
}

# ===== INDENT ALIGNMENT (F9, user rule 2026-08-13) =====
# A layer symbol can be unreachable because text precedes it while its
# target sits at the very start of a main line - nothing can be padded to
# the left of column 1. Instead of leaving the symbol misaligned, the MAIN
# line is indented until it sits under the layer symbol. Everything else
# that targets the same main line is indented by the same amount, so all
# other alignments within that line are preserved.

apply_indent_alignment <- function(ann, report, anchor_chars,
                                   text_body_width = Inf) {
	if (is.null(report) || nrow(report) == 0) {
		return(list(ann = ann, report = report))
	}
	stuck <- report[report$degraded &
	                report$note == "target_within_prefix", , drop = FALSE]
	if (nrow(stuck) == 0) return(list(ann = ann, report = report))

	prefix_len <- nchar(ann$prefix_cont)
	line_of_anchor <- function(row, char, occurrence) {
		positions <- extract_anchor_positions(ann$rendered_lines[[row]],
		                                      anchor_chars)
		hit <- positions[positions$char == char &
		                 positions$occurrence == occurrence, , drop = FALSE]
		if (nrow(hit) == 0) NA_integer_ else hit$line[1]
	}

	# deficit per (main_row, main_line), and per layer line causing it
	demands <- list()
	for (k in seq_len(nrow(stuck))) {
		row <- stuck$row[k]
		specs <- ann$anchor_specs[[row]]
		if (is.null(specs) || nrow(specs) == 0) next
		spec <- specs[specs$char == stuck$char[k] &
		              specs$occurrence == stuck$occurrence[k], , drop = FALSE]
		if (nrow(spec) == 0 || is.na(spec$source_row[1]) ||
		    is.na(spec$target_line[1])) next
		layer_line <- line_of_anchor(row, stuck$char[k], stuck$occurrence[k])
		if (is.na(layer_line)) next
		key <- paste(spec$source_row[1], spec$target_line[1])
		deficit <- stuck$placed_col[k] - stuck$target_col[k]
		entry <- demands[[key]]
		if (is.null(entry)) {
			entry <- list(main_row = spec$source_row[1],
			              main_line = spec$target_line[1],
			              max_deficit = 0L, lines = list())
		}
		entry$max_deficit <- max(entry$max_deficit, deficit)
		line_key <- paste(row, layer_line)
		entry$lines[[line_key]] <- max(
			if (is.null(entry$lines[[line_key]])) 0L else entry$lines[[line_key]],
			deficit)
		demands[[key]] <- entry
	}
	if (length(demands) == 0) return(list(ann = ann, report = report))

	shift_line <- function(line, pad, n) {
		if (n <= 0) return(line)
		paste0(substr(line, 1L, pad), strrep(" ", n),
		       substr(line, pad + 1L, nchar(line)))
	}

	for (entry in demands) {
		n <- entry$max_deficit
		if (n <= 0) next
		main_row <- entry$main_row
		main_line <- entry$main_line
		lines_main <- ann$rendered_lines[[main_row]]
		if (main_line > length(lines_main)) next
		# The indent must not push the verbal line past the transcript
		# width - the format has priority over the alignment of a single
		# symbol (user rule 2026-08-14, comment K1 on 702).
		if (is.finite(text_body_width)) {
			current <- length(split_graphemes(lines_main[main_line]))
			if (current + n > text_body_width) next
		}
		lines_main[main_line] <- shift_line(lines_main[main_line],
		                                    prefix_len[main_row], n)
		ann$rendered_lines[[main_row]] <- lines_main

		# every layer line targeting this main line moves with it, reduced
		# by the deficit it caused itself
		for (row in which(!ann$is_main)) {
			specs <- ann$anchor_specs[[row]]
			if (is.null(specs) || nrow(specs) == 0) next
			specs <- specs[!is.na(specs$target_col) &
			               !is.na(specs$target_line) &
			               specs$source_row == main_row &
			               specs$target_line == main_line, , drop = FALSE]
			if (nrow(specs) == 0) next
			lines_row <- ann$rendered_lines[[row]]
			touched <- unique(stats::na.omit(vapply(seq_len(nrow(specs)),
				function(s) line_of_anchor(row, specs$char[s], specs$occurrence[s]),
				integer(1))))
			for (line_index in touched) {
				if (line_index > length(lines_row)) next
				own <- entry$lines[[paste(row, line_index)]]
				if (is.null(own)) own <- 0L
				lines_row[line_index] <- shift_line(lines_row[line_index],
				                                    prefix_len[row], n - own)
			}
			ann$rendered_lines[[row]] <- lines_row
		}
	}

	# The indent moved the targets themselves - carry that into the specs
	# and the report so both keep describing the rendered result.
	for (row in seq_len(nrow(ann))) {
		specs <- ann$anchor_specs[[row]]
		if (is.null(specs) || nrow(specs) == 0) next
		for (s in seq_len(nrow(specs))) {
			if (is.na(specs$target_col[s]) || is.na(specs$source_row[s]) ||
			    is.na(specs$target_line[s])) next
			entry <- demands[[paste(specs$source_row[s], specs$target_line[s])]]
			if (is.null(entry) || entry$max_deficit <= 0) next
			specs$target_col[s] <- specs$target_col[s] + entry$max_deficit
		}
		ann$anchor_specs[[row]] <- specs
	}
	for (k in seq_len(nrow(report))) {
		row <- report$row[k]
		specs <- ann$anchor_specs[[row]]
		if (is.null(specs) || nrow(specs) == 0) next
		spec <- specs[specs$char == report$char[k] &
		              specs$occurrence == report$occurrence[k], , drop = FALSE]
		if (nrow(spec) == 0 || is.na(spec$source_row[1]) ||
		    is.na(spec$target_line[1])) next
		entry <- demands[[paste(spec$source_row[1], spec$target_line[1])]]
		if (is.null(entry) || entry$max_deficit <= 0) next
		own <- entry$lines[[paste(row, line_of_anchor(row, report$char[k],
		                                              report$occurrence[k]))]]
		if (is.null(own)) own <- 0L
		report$target_col[k] <- report$target_col[k] + entry$max_deficit
		report$placed_col[k] <- report$placed_col[k] + entry$max_deficit - own
		if (identical(report$note[k], "target_within_prefix")) {
			report$degraded[k] <- FALSE
			report$note[k] <- "aligned_by_indent"
		}
	}
	list(ann = ann, report = report)
}

# Interleaves layer lines with the printed lines of their main (Mondada:
# multimodal annotations follow EVERY verbal line, not the whole block).
# Each rendered layer line is assigned to the main line its anchors target
# (target_line); anchorless continuation lines stay with their segment.
# Rows without any anchor at all (translations, comments) keep their block
# position after the main's LAST line. Used in both layout modes (F5,
# user rule 2026-08-13); only the line numbering differs between them.
# Returns the output plan: one row per printed line, with the source row
# and the group main row (NA for rows before the first main).

# A continuation whose closing mark is the ONLY thing left on the line
# reads badly ("◊" alone). The mark gets a short fill in front of it and
# the verbal line - plus everything else pointing at it - is indented by
# the same amount, so the alignment holds (user rule 2026-08-14).
.pad_lone_symbol_lines <- function(ann, anchor_chars, fill_width) {
	if (fill_width <= 0L) return(ann)
	prefix_len <- nchar(ann$prefix_cont)
	# A line that CONTINUES a description starts with its fill character -
	# the shift has to extend that fill, not punch spaces into it
	# (user comment K3 on 207, 2026-08-14).
	shift <- function(line, pad, n) {
		if (n <= 0 || nchar(line) < pad) return(line)
		body <- substr(line, pad + 1L, nchar(line))
		lead <- substr(body, 1L, 1L)
		filler <- if (lead %in% c("-", "=", ".", ",")) lead else " "
		paste0(substr(line, 1L, pad), strrep(filler, n), body)
	}
	for (row in which(!ann$is_main)) {
		lines_row <- ann$rendered_lines[[row]]
		specs <- ann$anchor_specs[[row]]
		if (is.null(lines_row) || is.null(specs) || nrow(specs) == 0) next
		pad <- prefix_len[row]
		positions <- extract_anchor_positions(lines_row, anchor_chars)
		for (line_index in seq_along(lines_row)) {
			body <- substr(lines_row[line_index], pad + 1L,
			               nchar(lines_row[line_index]))
			body_g <- split_graphemes(body)
			# a line holding nothing but marks (one or several, e.g. "†♦")
			if (length(body_g) == 0L || length(body_g) > 3L ||
			    !all(body_g %in% anchor_chars)) next
			hit <- positions[positions$line == line_index &
			                 positions$char == body_g[1], , drop = FALSE]
			if (nrow(hit) == 0) next
			body <- body_g[1]
			spec <- specs[specs$char == body &
			              specs$occurrence == hit$occurrence[1], , drop = FALSE]
			if (nrow(spec) == 0 || is.na(spec$source_row[1]) ||
			    is.na(spec$target_line[1])) next
			main_row <- spec$source_row[1]
			main_line <- spec$target_line[1]
			fill <- spec$pair_fill[1]
			if (is.na(fill) || fill == "") fill <- "-"

			main_lines <- ann$rendered_lines[[main_row]]
			if (main_line > length(main_lines)) next
			main_lines[main_line] <- shift(main_lines[main_line],
			                               prefix_len[main_row], fill_width)
			ann$rendered_lines[[main_row]] <- main_lines

			# everything else targeting this main line moves along
			for (other in which(!ann$is_main)) {
				other_specs <- ann$anchor_specs[[other]]
				if (is.null(other_specs) || nrow(other_specs) == 0) next
				hits <- other_specs[!is.na(other_specs$target_col) &
				                    !is.na(other_specs$target_line) &
				                    other_specs$source_row == main_row &
				                    other_specs$target_line == main_line, ,
				                    drop = FALSE]
				if (nrow(hits) == 0) next
				other_lines <- ann$rendered_lines[[other]]
				other_positions <- extract_anchor_positions(other_lines,
				                                            anchor_chars)
				touched <- unique(stats::na.omit(vapply(seq_len(nrow(hits)),
					function(s) {
						h <- other_positions[other_positions$char == hits$char[s] &
						                     other_positions$occurrence ==
						                     hits$occurrence[s], , drop = FALSE]
						if (nrow(h) == 0) NA_integer_ else h$line[1]
					}, integer(1))))
				for (li in touched) {
					if (li > length(other_lines)) next
					if (other == row && li == line_index) {
						other_lines[li] <- paste0(
							substr(other_lines[li], 1L, prefix_len[other]),
							strrep(fill, fill_width),
							substr(other_lines[li], prefix_len[other] + 1L,
							       nchar(other_lines[li])))
					} else {
						other_lines[li] <- shift(other_lines[li],
						                         prefix_len[other], fill_width)
					}
				}
				ann$rendered_lines[[other]] <- other_lines
			}
		}
	}
	ann
}

# Printed line number of a main line, read back from the rendered text.

# Closes a description with "->l.XX" (Mondada, arrows always with a single
# dash). The reference replaces the trailing fill/arrow; when the line has
# no room left for the line number, the plain arrow is written instead -
# the resumed line below carries the continuation anyway.
.with_span_reference <- function(line, target, prefix_width, max_width = Inf) {
	reference <- if (is.na(target)) "->" else paste0("->l.", target)
	body <- substr(line, prefix_width + 1L, nchar(line))
	body <- sub("[-=]*>+$", "", body)
	body <- sub("\\s+$", "", body)
	if (is.finite(max_width) &&
	    prefix_width + nchar(body) + nchar(reference) > max_width) {
		reference <- "->"
	}
	paste0(substr(line, 1L, prefix_width), body, reference)
}

# Resumes a description on the target line: "->" directly in front of the
# symbol. The fill that used to run there becomes spaces, so the symbol
# keeps its column and the arrow sits glued to it (user comments K1/K8,
# 2026-08-15).
.with_span_resume <- function(line, prefix_width) {
	body <- substr(line, prefix_width + 1L, nchar(line))
	fill <- stringr::str_extract(body, "^[-=.,]+")
	if (is.na(fill) || nchar(fill) < 2L) return(line)
	rest <- substr(body, nchar(fill) + 1L, nchar(body))
	paste0(substr(line, 1L, prefix_width),
	       strrep(" ", nchar(fill) - 2L), "->", rest)
}

.with_row_label <- function(line, prefix_first, prefix_cont) {
	width <- nchar(prefix_cont)
	if (width == 0L || is.na(prefix_first) || nchar(line) < width) return(line)
	paste0(prefix_first, substr(line, width + 1L, nchar(line)))
}

# Overlays picture lines of one block into as few printed lines as
# possible. Colliding numbers first try the space-saving flipped form
# "08#" (digits left of the mark, anchor column unchanged); only when that
# collides as well does the entry keep its own line (user decisions
# 2026-08-17, 702_001 K0).
.merge_point_lines <- function(lines, pad) {
	merged <- character(0)
	for (line in lines) {
		placed <- FALSE
		for (mi in seq_along(merged)) {
			combo <- .overlay_point_line(merged[mi], line, pad)
			if (!is.null(combo)) {
				merged[mi] <- combo
				placed <- TRUE
				break
			}
		}
		if (!placed) merged <- c(merged, line)
	}
	merged
}

.overlay_point_line <- function(base, incoming, pad) {
	width <- max(nchar(base), nchar(incoming))
	base_g <- c(split_graphemes(base),
	            rep(" ", width - nchar(base)))
	in_g <- c(split_graphemes(incoming),
	          rep(" ", width - nchar(incoming)))
	conflict <- which(base_g != " " & in_g != " " &
	                  seq_len(width) > pad)
	if (length(conflict) > 0) {
		# try to flip the base label that reaches into the incoming one:
		# "#08" becomes "08#" with the mark keeping its column
		first_in <- which(in_g != " " & seq_len(width) > pad)[1]
		label_start <- NA_integer_
		for (b in which(base_g == "#")) {
			d <- b + 1L
			while (d <= width && stringr::str_detect(base_g[d], "^[0-9]$")) d <- d + 1L
			if (b < first_in && d - 1L >= first_in) {
				label_start <- b
				label_end <- d - 1L
				break
			}
		}
		if (is.na(label_start)) return(NULL)
		n_digits <- label_end - label_start
		flip_from <- label_start - n_digits
		if (flip_from <= pad) return(NULL)
		if (any(base_g[flip_from:(label_start - 1L)] != " ")) return(NULL)
		digits <- base_g[(label_start + 1L):label_end]
		base_g[flip_from:(label_start - 1L)] <- digits
		base_g[label_start] <- "#"
		base_g[(label_start + 1L):label_end] <- " "
		conflict <- which(base_g != " " & in_g != " " &
		                  seq_len(width) > pad)
		if (length(conflict) > 0) return(NULL)
	}
	out <- ifelse(base_g == " ", in_g, base_g)
	sub("\\s+$", "", paste(out, collapse = ""))
}

interleave_layer_lines <- function(result, max_span_blocks = Inf,
                                   text_width = Inf, embed_overlaps = FALSE,
                                   label_mode = "mondada",
                                   layer_order = NULL,
                                   wrap_marker = "mondada") {
	mm_chars <- unique(unlist(
		lapply(result$align_chars[!is.na(result$align_chars)],
		       function(x) strsplit(x, "")[[1]])))
	anchor_chars <- unique(c("[", mm_chars))
	n <- nrow(result)

	out <- list()
	emit <- function(row, lines, group) {
		if (length(lines) == 0) return(invisible(NULL))
		out[[length(out) + 1]] <<- data.frame(
			row = row, line = lines, group = group)
		invisible(NULL)
	}

	# ---- per-line homes: which main row and main line every rendered layer
	# line belongs to. A description that opens in one turn and closes in
	# the next distributes its lines over BOTH turns - the closing line
	# moves to the block whose verbal line carries its symbol (user
	# comments K7/K8/K9, 2026-08-16). ----
	homes <- vector("list", n)
	for (r in seq_len(n)) {
		if (isTRUE(result$is_main[r])) next
		lines_r <- result$rendered_lines[[r]]
		if (length(lines_r) == 0) next
		specs <- result$anchor_specs[[r]]
		if (is.null(specs) || nrow(specs) == 0) next
		specs <- specs[!is.na(specs$target_col) & !is.na(specs$target_line) &
		               !is.na(specs$source_row), , drop = FALSE]
		if (nrow(specs) == 0) next
		positions <- extract_anchor_positions(lines_r, anchor_chars)
		source <- rep(NA_integer_, length(lines_r))
		main_line <- rep(NA_integer_, length(lines_r))
		anchor_col <- rep(NA_integer_, length(lines_r))
		for (sp in seq_len(nrow(specs))) {
			hit <- positions[positions$char == specs$char[sp] &
			                 positions$occurrence == specs$occurrence[sp], , drop = FALSE]
			if (nrow(hit) == 0) next
			li <- hit$line[1]
			if (is.na(anchor_col[li]) || hit$col[1] < anchor_col[li]) {
				source[li] <- specs$source_row[sp]
				main_line[li] <- specs$target_line[sp]
				anchor_col[li] <- hit$col[1]
			}
		}
		if (all(is.na(source))) next
		lead <- if (!is.null(result$lead_lines)) result$lead_lines[[r]] else integer(0)
		first_known <- which(!is.na(source))[1]
		if (first_known > 1) {
			preceding <- seq_len(first_known - 1L)
			source[preceding] <- source[first_known]
			main_line[preceding] <- ifelse(preceding %in% lead, 0L,
			                               main_line[first_known])
		}
		# A continuation line without an anchor of its own belongs to the
		# NEXT verbal line: the description keeps running while the talk
		# moves on (user rule 2026-08-14). When the description CLOSES in
		# a different block, the continuation joins that closing block
		# instead - the reader finds the rest where it ends (user comment
		# K4, 2026-08-17).
		for (li in seq_along(source)) {
			if (!is.na(source[li])) next
			next_anchor <- which(!is.na(source) & seq_along(source) > li)
			next_anchor <- if (length(next_anchor)) next_anchor[1] else NA_integer_
			# The middle lines of a description RECTANGLE (open and close
			# on the same main line) stay in that block as well
			# (user rule 2026-08-17).
			if (!is.na(next_anchor) &&
			    (source[next_anchor] != source[li - 1L] ||
			     main_line[next_anchor] == main_line[li - 1L])) {
				source[li] <- source[next_anchor]
				main_line[li] <- main_line[next_anchor]
			} else {
				source[li] <- source[li - 1L]
				main_line[li] <- main_line[li - 1L] + 1L
			}
		}
		homes[[r]] <- data.frame(line = seq_along(lines_r),
		                         source = source, main_line = main_line)
	}

	# ---- segments: loose rows and (main + following layer rows) groups ----
	segments <- list()
	i <- 1L
	while (i <= n) {
		if (!isTRUE(result$is_main[i])) {
			segments[[length(segments) + 1]] <- list(kind = "loose", row = i)
			i <- i + 1L
			next
		}
		group_rows <- integer(0)
		j <- i + 1L
		while (j <= n && !isTRUE(result$is_main[j])) {
			group_rows <- c(group_rows, j)
			j <- j + 1L
		}
		segments[[length(segments) + 1]] <- list(kind = "group", main = i,
		                                         rows = group_rows)
		i <- j
	}
	adjacency_main <- rep(NA_integer_, n)
	for (s in seq_along(segments)) {
		seg <- segments[[s]]
		if (identical(seg$kind, "group") && length(seg$rows) > 0) {
			adjacency_main[seg$rows] <- seg$main
		}
	}

	# rows a main block has to consider: homed lines plus adjacency
	# fallback rows without any usable anchor
	rows_for_main <- function(m) {
		homed <- which(vapply(seq_len(n), function(r) {
			!is.null(homes[[r]]) && any(homes[[r]]$source == m)
		}, logical(1)))
		fallback <- which(adjacency_main == m &
		                  vapply(homes, is.null, logical(1)))
		unique(c(homed, fallback))
	}

	# ---- overlap followers (score mode): a main row whose fragment starts
	# with "[" paired into an EARLIER main row is printed directly below the
	# parent line that carries its partner bracket (user comment 206_003
	# K1, 2026-08-15). ----
	child_parent <- integer(0)
	child_occurrence <- integer(0)
	pairs <- attr(result, "bracket_pairs")
	if (isTRUE(embed_overlaps) && !is.null(pairs) && nrow(pairs) > 0) {
		for (p in seq_len(nrow(pairs))) {
			ci <- pairs$j_row[p]
			pa <- pairs$i_row[p]
			if (ci == pa || pa > ci) next
			if (!isTRUE(result$is_main[ci]) || !isTRUE(result$is_main[pa])) next
			key <- as.character(ci)
			if (key %in% names(child_parent)) next
			child_parent[key] <- pa
			child_occurrence[key] <- pairs$i_occurrence[p]
		}
	}
	seg_children <- list()
	is_child <- logical(length(segments))
	for (s in seq_along(segments)) {
		seg <- segments[[s]]
		if (!identical(seg$kind, "group")) next
		key <- as.character(seg$main)
		if (!(key %in% names(child_parent))) next
		pa_key <- as.character(child_parent[[key]])
		seg_children[[pa_key]] <- c(seg_children[[pa_key]], s)
		is_child[s] <- TRUE
	}

	# ---- ordering of the layer rows inside one block: the block speaker's
	# own rows first, then other actors; within one actor the layer order
	# of the multimodal matrix; picture rows always last (user decision
	# 2026-08-16). ----
	order_block_rows <- function(rows, main_row) {
		if (length(rows) < 2) return(rows)
		speaker <- tolower(sub("#.*$", "", result$tierName[main_row]))
		actor <- tolower(sub("#.*$", "", result$tierName[rows]))
		layer <- sub("^[^#]*#?", "", result$tierName[rows])
		if (is.null(layer_order)) {
			layer_rank <- rep(0L, length(layer))
		} else {
			layer_rank <- match(layer, layer_order)
			layer_rank[is.na(layer_rank)] <- length(layer_order) + 1L
		}
		is_picture <- vapply(rows, function(r) {
			identical(result$align_mode[r], "point")
		}, logical(1))
		rows[order(is_picture, actor != speaker, layer_rank, rows)]
	}

	emit_group <- function(main_row, group_rows, out_group) {
		main_lines <- result$rendered_lines[[main_row]]
		n_main <- length(main_lines)
		block_rows <- order_block_rows(union(group_rows, rows_for_main(main_row)),
		                               main_row)
		block_rows <- block_rows[!result$is_main[block_rows]]
		assignments <- lapply(block_rows, function(r) {
			if (!is.null(homes[[r]])) {
				h <- homes[[r]]
				assignment <- rep(NA_integer_, length(result$rendered_lines[[r]]))
				sel <- h$source == main_row
				assignment[h$line[sel]] <- pmin(pmax(h$main_line[sel], 0L), n_main)
				# Lines without an own anchor (middle lines of a wrapped
				# description rectangle) stay with the block of the
				# previous anchored line - but only BETWEEN this source's
				# first and last anchored line, and never a line that is
				# anchored to another source (user rule 2026-08-17).
				own_lines <- h$line[sel]
				foreign_lines <- h$line[!sel]
				if (length(own_lines) > 0 && anyNA(assignment)) {
					filled <- NA_integer_
					for (line_index in min(own_lines):max(own_lines)) {
						if (!is.na(assignment[line_index])) {
							filled <- assignment[line_index]
						} else if (!(line_index %in% foreign_lines)) {
							assignment[line_index] <- filled
						}
					}
				}
				return(assignment)
			}
			# no usable anchors anywhere: whole row under this block.
			# Unanchored picture rows go BELOW the complete annotation,
			# never between its wrapped lines (user decision Variante B,
			# 2026-08-17).
			lines_r <- result$rendered_lines[[r]]
			has_align <- !is.na(result$align_chars[r]) &&
				nchar(result$align_chars[r]) > 0
			if (identical(result$align_mode[r], "point")) {
				rep(n_main, length(lines_r))
			} else {
				rep(if (has_align) 1L else n_main, length(lines_r))
			}
		})
		# Mondada (example 7): a description running over many lines is cut
		# short with an arrow naming the line where it continues; lines
		# carrying nothing but fill are dropped. The line NUMBER is not
		# known yet (numbering runs on the interleaved plan), so a
		# placeholder names the target main row and line; it is resolved by
		# apply_mondada_line_numbers().
		if (is.finite(max_span_blocks) && max_span_blocks > 0) {
			for (g in seq_along(block_rows)) {
				row <- block_rows[g]
				if (identical(result$align_mode[row], "point")) next
				span <- assignments[[g]]
				if (length(span) < 3) next
				lines_row <- result$rendered_lines[[row]]
				pad <- nchar(result$prefix_cont[row])
				fill_only <- vapply(lines_row, function(line) {
					if (is.na(line)) return(FALSE)
					body <- substr(line, pad + 1L, nchar(line))
					body <- stringr::str_trim(body)
					nzchar(body) && !stringr::str_detect(body, "[\\p{L}\\p{N}]")
				}, logical(1), USE.NAMES = FALSE)
				in_block <- !is.na(span)
				runs <- rle(fill_only & in_block)
				stop_index <- cumsum(runs$lengths)
				start_index <- stop_index - runs$lengths + 1L
				for (rr in which(runs$values)) {
					from <- start_index[rr]
					to <- stop_index[rr]
					if (from < 2L || to >= length(span)) next
					if (is.na(span[from - 1L]) || is.na(span[to + 1L])) next
					# Dropping the run is only allowed when the line after
					# it RESUMES the same description (it starts with fill
					# the resume arrow can replace). Otherwise the run may
					# carry a closing symbol of its own and must stay
					# (user comment 320_001 K0, 2026-08-17).
					resume_body <- substr(lines_row[to + 1L], pad + 1L,
					                      nchar(lines_row[to + 1L]))
					if (!stringr::str_detect(resume_body, "^[-=.,]{2}")) next
					distance <- span[to + 1L] - span[from - 1L]
					target <- if (distance >= max_span_blocks) {
						sprintf("@%d.%d@", main_row, span[to + 1L])
					} else {
						NA_character_
					}
					lines_row[from - 1L] <- .with_span_reference(
						lines_row[from - 1L], target, pad, pad + text_width)
					lines_row[to + 1L] <- .with_span_resume(
						lines_row[to + 1L], pad)
					span[from:to] <- NA_integer_
				}
				result$rendered_lines[[row]] <- lines_row
				assignments[[g]] <- span
			}
		}
		# A continuation line followed by the pure-fill closing line of
		# its description (same block) merges into ONE line: the fill
		# runs from the end of the text to the closing symbol ("holds
		# it------|", user comment K4, 2026-08-17). A remaining fill line
		# that starts with spaces gets its leading gap filled as well
		# (user comment 206_010 K0, 2026-08-17).
		for (g in seq_along(block_rows)) {
			row <- block_rows[g]
			asg <- assignments[[g]]
			lines_row <- result$rendered_lines[[row]]
			pad <- nchar(result$prefix_cont[row])
			changed_row <- FALSE
			for (li in seq_along(asg)) {
				if (li >= length(asg)) next
				if (is.na(asg[li]) || is.na(asg[li + 1L])) next
				if (asg[li] != asg[li + 1L]) next
				body_next <- substr(lines_row[li + 1L], pad + 1L,
				                    nchar(lines_row[li + 1L]))
				fill_match <- stringr::str_match(body_next,
					"^([ ]*)([-=]{2,})([^\\p{L}\\p{N}]?)$")
				if (is.na(fill_match[1, 1])) next
				text_line <- sub("->$", "", lines_row[li])
				text_line <- sub("[ ]+$", "", text_line)
				close_col <- nchar(lines_row[li + 1L])
				# str_match: column 1 is the FULL match, groups start at 2
				if (nchar(text_line) >= pad + nchar(fill_match[1, 2]) +
				    1L && nchar(text_line) < close_col) {
					fill_char <- substr(fill_match[1, 3], 1, 1)
					symbol <- fill_match[1, 4]
					merged <- paste0(text_line,
						strrep(fill_char, close_col - nchar(text_line) -
						       nchar(symbol)), symbol)
					lines_row[li] <- merged
					lines_row[li + 1L] <- NA_character_
					asg[li + 1L] <- NA_integer_
					changed_row <- TRUE
				}
			}
			# Rectangle fill (user mock-up 2026-08-17): the middle lines
			# of a description broken inside ONE block run out in fill
			# characters to the closing column, so the block reads as one
			# body of text ending at its symbol.
			run_start_i <- NA_integer_
			for (li in seq_len(length(asg) + 1L)) {
				same_run <- li <= length(asg) && !is.na(asg[li]) &&
					!is.na(lines_row[li]) &&
					(is.na(run_start_i) || asg[li] == asg[run_start_i])
				if (same_run) {
					if (is.na(run_start_i)) run_start_i <- li
					next
				}
				if (!is.na(run_start_i) && li - run_start_i >= 3L) {
					last_i <- li - 1L
					tail_match <- stringr::str_match(lines_row[last_i],
						"([-=.,])\\1*([^\\p{L}\\p{N} ])$")
					if (!is.na(tail_match[1, 1])) {
						fill_char <- tail_match[1, 2]
						close_col <- nchar(lines_row[last_i])
						for (mid in (run_start_i + 1L):(last_i - 1L)) {
							body_end <- nchar(lines_row[mid])
							if (body_end >= close_col) next
							if (!stringr::str_detect(lines_row[mid],
							                         "[\\p{L}\\p{N}]$")) next
							lines_row[mid] <- paste0(lines_row[mid],
								strrep(fill_char, close_col - body_end))
							changed_row <- TRUE
						}
					}
				}
				run_start_i <- if (li <= length(asg) && !is.na(asg[li]) &&
				                   !is.na(lines_row[li])) li else NA_integer_
			}
			# leading gap of pure fill continuation lines
			for (li in seq_along(asg)) {
				if (li == 1L || is.na(asg[li]) || is.na(lines_row[li])) next
				body <- substr(lines_row[li], pad + 1L, nchar(lines_row[li]))
				gap_match <- stringr::str_match(body,
					"^([ ]+)([-=]{2,})")
				if (is.na(gap_match[1, 1])) next
				fill_char <- substr(gap_match[1, 3], 1, 1)
				lines_row[li] <- paste0(
					substr(lines_row[li], 1, pad),
					strrep(fill_char, nchar(gap_match[1, 2])),
					substr(lines_row[li], pad + nchar(gap_match[1, 2]) + 1L,
					       nchar(lines_row[li])))
				changed_row <- TRUE
			}
			if (changed_row) {
				keep_lines <- !is.na(lines_row)
				result$rendered_lines[[row]] <- lines_row
				assignments[[g]] <- asg
			}
		}
		# Mondada strict: a description wrapping WITHIN one block just
		# breaks - no arrow. "->" stays only when the description resumes
		# in a LATER block, where the reader has to search for it
		# (user comment K2, 2026-08-17; wrap_marker = "arrow" keeps the
		# old behaviour).
		if (identical(wrap_marker, "mondada")) {
			for (g in seq_along(block_rows)) {
				row <- block_rows[g]
				asg <- assignments[[g]]
				if (length(asg) < 2) next
				lines_row <- result$rendered_lines[[row]]
				for (li in seq_len(length(asg) - 1L)) {
					if (is.na(lines_row[li])) next
					if (is.na(asg[li]) || is.na(asg[li + 1L])) next
					if (asg[li] != asg[li + 1L]) next
					if (stringr::str_detect(lines_row[li], ">>$")) next
					lines_row[li] <- sub("->$", "", lines_row[li])
				}
				result$rendered_lines[[row]] <- lines_row
			}
		}
		child_segs <- seg_children[[as.character(main_row)]]
		child_line <- integer(0)
		if (length(child_segs) > 0) {
			positions <- extract_anchor_positions(main_lines, "[")
			child_line <- vapply(child_segs, function(s) {
				occ <- child_occurrence[[as.character(segments[[s]]$main)]]
				hit <- positions[positions$char == "[" &
				                 positions$occurrence == occ, , drop = FALSE]
				if (nrow(hit) > 0) hit$line[1] else n_main
			}, integer(1))
		}
		point_buffer <- list()
		flush_points <- function() {
			if (length(point_buffer) == 0) return(invisible(NULL))
			anchored_entries <- Filter(function(b) isTRUE(b$anchored),
			                           point_buffer)
			loose_entries <- Filter(function(b) !isTRUE(b$anchored),
			                        point_buffer)
			if (length(anchored_entries) > 0) {
				first_row <- anchored_entries[[1]]$row
				pad <- nchar(result$prefix_cont[first_row])
				merged <- .merge_point_lines(
					vapply(anchored_entries, function(b) b$line,
					       character(1)), pad)
				for (ml in merged) {
					emit(first_row,
					     .with_row_label(ml, result$prefix_first[first_row],
					                     result$prefix_cont[first_row]),
					     out_group)
				}
			}
			if (length(loose_entries) > 0) {
				# Pictures without a "#" anchor in the verbal line share
				# ONE row in temporal order - their columns carry no
				# alignment anyway (user comment 207_024 K2, 2026-08-17).
				first_row <- loose_entries[[1]]$row
				pad <- nchar(result$prefix_cont[first_row])
				joined <- paste(vapply(loose_entries, function(b) {
					stringr::str_trim(substr(b$line, pad + 1L,
					                         nchar(b$line)))
				}, character(1)), collapse = " ")
				emit(first_row,
				     .with_row_label(paste0(strrep(" ", pad), joined),
				                     result$prefix_first[first_row],
				                     result$prefix_cont[first_row]),
				     out_group)
			}
			point_buffer <<- list()
		}
		emit_layer_lines <- function(row, selected) {
			lines_row <- result$rendered_lines[[row]][selected]
			# Mondada (Conventions for multimodal transcription, examples
			# 10, 12, 19): the label is DROPPED when the actor is the
			# speaker of the verbal line above. label_mode = "always"
			# keeps it on every layer line instead.
			actor <- tolower(sub("#.*$", "", result$tierName[row]))
			speaker <- tolower(sub("#.*$", "", result$tierName[main_row]))
			needs_label <- identical(label_mode, "always") ||
				identical(result$align_mode[row], "point") ||
				!identical(actor, speaker)
			lines_row <- vapply(lines_row, function(line) {
				if (needs_label) {
					.with_row_label(line, result$prefix_first[row],
					                result$prefix_cont[row])
				} else {
					.with_row_label(line, result$prefix_cont[row],
					                result$prefix_cont[row])
				}
			}, character(1), USE.NAMES = FALSE)
			emit(row, lines_row, out_group)
		}
		for (g in seq_along(block_rows)) {
			selected <- which(assignments[[g]] == 0L)
			if (length(selected) > 0) emit_layer_lines(block_rows[g], selected)
		}
		for (k in seq_len(n_main)) {
			emit(main_row, main_lines[k], out_group)
			for (g in seq_along(block_rows)) {
				selected <- which(assignments[[g]] == k)
				if (length(selected) == 0) next
				row <- block_rows[g]
				if (identical(result$align_mode[row], "point") &&
				    length(selected) == 1L) {
					point_buffer[[length(point_buffer) + 1]] <- list(
						row = row,
						line = result$rendered_lines[[row]][selected],
						anchored = !is.null(homes[[row]]))
					next
				}
				emit_layer_lines(row, selected)
			}
			flush_points()
			for (c_idx in which(child_line == k)) {
				child <- segments[[child_segs[c_idx]]]
				emit_group(child$main, child$rows, out_group)
			}
		}
	}

	for (s in seq_along(segments)) {
		if (is_child[s]) next
		seg <- segments[[s]]
		if (identical(seg$kind, "loose")) {
			r <- seg$row
			if (is.null(homes[[r]]) && is.na(adjacency_main[r])) {
				emit(r, result$rendered_lines[[r]], NA_integer_)
			}
		} else {
			emit_group(seg$main, seg$rows, seg$main)
		}
	}
	if (length(out) == 0) {
		return(data.frame(row = integer(0), line = character(0),
		                  group = integer(0)))
	}
	plan <- .pack_disjoint_layer_lines(do.call(rbind, out), result,
	                                   anchor_chars)
	.dedupe_tier_labels(plan, result)
}

# The tier label appears ONCE per block: further printed lines of the
# same tier inside one block drop it and carry plain spaces instead
# (user rule 2026-08-17).
.dedupe_tier_labels <- function(plan, result) {
	if (nrow(plan) < 2) return(plan)
	seen <- character(0)
	for (i in seq_len(nrow(plan))) {
		r <- plan$row[i]
		# every printed MAIN line starts a new visual block - the labels
		# reappear there so no layer line sits far from its attribution
		if (isTRUE(result$is_main[r])) {
			seen <- character(0)
			next
		}
		label <- result$prefix_first[r]
		if (!nzchar(stringr::str_trim(label))) next
		if (!startsWith(plan$line[i], label)) next
		# Keyed on the PRINTED label alone, not the tier name or group:
		# different layers of the same actor (teaf#mm-part, teaf#mm-gaze)
		# share one sigle, and adjacent groups without a main line in
		# between form ONE visual block for the reader. The seen set
		# resets at every printed main line.
		key <- label
		if (key %in% seen) {
			plan$line[i] <- paste0(result$prefix_cont[r],
				substr(plan$line[i], nchar(label) + 1L,
				       nchar(plan$line[i])))
		} else {
			seen <- c(seen, key)
		}
	}
	plan
}

# Two printed layer lines of the same tier share one line when their
# bodies occupy disjoint columns: the closing line of one annotation and
# the opening line of the next then read as one flow (user comments
# 205_005 GAT K2/K3, 2026-08-17). Only the FIRST printed line of a row
# may dock onto the line above - a wrapped description must stay below
# its own first line. A shared boundary mark (same anchor char in the
# same column) overlays into ONE mark, mirroring the seam rule of the
# score mode (user comment 206_010 K0).
.pack_disjoint_layer_lines <- function(plan, result, anchor_chars) {
	if (nrow(plan) < 2) return(plan)
	keep <- rep(TRUE, nrow(plan))
	seen <- character(0)
	previous <- 1L
	seen <- paste(plan$group[1], plan$row[1])
	for (i in 2:nrow(plan)) {
		a_row <- plan$row[previous]
		b_row <- plan$row[i]
		b_key <- paste(plan$group[i], b_row)
		first_of_row <- !(b_key %in% seen)
		seen <- c(seen, b_key)
		mergeable <- first_of_row &&
			!is.na(plan$group[previous]) && !is.na(plan$group[i]) &&
			plan$group[previous] == plan$group[i] &&
			a_row != b_row &&
			!isTRUE(result$is_main[a_row]) && !isTRUE(result$is_main[b_row]) &&
			!identical(result$align_mode[a_row], "point") &&
			!identical(result$align_mode[b_row], "point") &&
			identical(result$tierName[a_row], result$tierName[b_row]) &&
			!stringr::str_detect(plan$line[previous], "->>?$")
		combo <- NULL
		if (mergeable) {
			combo <- .overlay_disjoint_lines(plan$line[previous], plan$line[i],
				nchar(result$prefix_cont[b_row]), anchor_chars)
		}
		if (!is.null(combo)) {
			plan$line[previous] <- combo
			keep[i] <- FALSE
		} else {
			previous <- i
		}
	}
	plan[keep, , drop = FALSE]
}

.overlay_disjoint_lines <- function(base, incoming, pad, anchor_chars) {
	width <- max(nchar(base), nchar(incoming))
	base_g <- c(split_graphemes(base), rep(" ", width - nchar(base)))
	in_g <- c(split_graphemes(incoming), rep(" ", width - nchar(incoming)))
	beyond <- seq_len(width) > pad
	conflict <- base_g != " " & in_g != " " & beyond
	shared_mark <- conflict & base_g == in_g & base_g %in% anchor_chars
	if (any(conflict & !shared_mark)) return(NULL)
	out <- ifelse(beyond & base_g == " ", in_g, base_g)
	sub("\\s+$", "", paste(out, collapse = ""))
}

# Numbers the printed verbal lines of the interleaved plan. Runs on the
# PLAN so the numbers follow the visual order (overlap followers are
# embedded mid-turn) and so the digit count comes from the FINAL number of
# printed lines - three digits only from 100 lines up (user comment K16,
# 2026-08-15). Also resolves the "->l.@row.k@" placeholders left by the
# span-reference rule, now that the numbers exist.
apply_mondada_line_numbers <- function(plan, result, offset = 0L,
                                       slot_width = 2L) {
	if (is.null(result$number_lines) || nrow(plan) == 0) return(plan)
	numbered <- vapply(plan$row, function(r) {
		isTRUE(result$number_lines[r])
	}, logical(1))
	digits <- max(2L, nchar(as.character(sum(numbered))))
	digits <- min(digits, max(slot_width, 2L))
	counter <- 0L
	numbers <- rep(NA_character_, nrow(plan))
	for (p in which(numbered)) {
		counter <- counter + 1L
		number <- formatC(counter, width = digits, flag = "0")
		numbers[p] <- number
		field <- stringr::str_pad(number, slot_width, side = "right")
		line <- plan$line[p]
		if (nchar(line) >= offset + slot_width) {
			plan$line[p] <- paste0(
				substr(line, 1, offset), field,
				substr(line, offset + slot_width + 1, nchar(line)))
		}
	}
	# ---- resolve span-reference placeholders ----
	ph <- stringr::str_match(plan$line, "->l\\.@(\\d+)\\.(\\d+)@")
	for (p in which(!is.na(ph[, 1]))) {
		main_row <- as.integer(ph[p, 2])
		k <- as.integer(ph[p, 3])
		main_positions <- which(plan$row == main_row)
		replacement <- if (k >= 1L && k <= length(main_positions) &&
		                   !is.na(numbers[main_positions[k]])) {
			paste0("->l.", numbers[main_positions[k]])
		} else {
			"->"
		}
		plan$line[p] <- sub("->l\\.@\\d+\\.\\d+@", replacement, plan$line[p])
	}
	plan
}

# ===== SPEAKER CONTINUATION =====
# A main annotation that seamlessly continues the same speaker's previous
# annotation (start == previous end) and has no anchor of its own starts
# its content right after the previous annotation's rendered text - the
# turn reads as one continuous stretch. (User rule 2026-08-10.)

apply_speaker_continuation <- function(ann, i, rendered_lines) {
	text <- ann$text[i]
	if (!stringr::str_detect(stringr::str_trim(text), "^\\[")) return(text)
	candidates <- which(ann$is_main & seq_len(nrow(ann)) < i &
	                    ann$tierName == ann$tierName[i] &
	                    abs(ann$endsec - ann$startsec[i]) < 0.02)
	if (length(candidates) == 0) return(text)
	previous_row <- max(candidates)
	previous_content <- stringr::str_trim(ann$content[previous_row])
	if (stringr::str_detect(previous_content,
	                        "\\((\\.|-{1,3}|[0-9]+(\\.[0-9]+)?)\\)$")) {
		return(text)
	}
	lines <- rendered_lines[[previous_row]]
	if (is.null(lines) || length(lines) == 0) return(text)

	last_line <- lines[length(lines)]
	graphemes <- split_graphemes(last_line)
	non_space <- which(!stringr::str_detect(graphemes, "^\\s$"))
	if (length(non_space) == 0) return(text)
	last_col <- max(non_space)

	prefix_len <- nchar(ann$prefix_cont[i])
	lead <- last_col + 1L - (prefix_len + 1L)
	if (lead <= 0) return(text)
	paste0(strrep(" ", lead), text)
}

# ===== TEXT INDENT FOR LAYER ROWS (content.indent = "text") =====
# Indents translation/glossing rows so their text starts under the first
# actual text character of the RENDERED main line (leading padding and
# skip-regex matches like =, [, <<...> are skipped).

apply_text_indent <- function(ann, i, rendered_cache, ref_main) {
	text <- ann$text[i]
	main_row <- ref_main[i]
	if (is.na(main_row) || main_row == i) return(text)
	cache <- rendered_cache[[main_row]]
	if (is.null(cache) || length(cache$lines) == 0) return(text)

	first_line <- cache$lines[1]
	prefix_len <- nchar(ann$prefix_cont[main_row])
	body <- substr(first_line, prefix_len + 1L, nchar(first_line))
	leading <- stringr::str_extract(body, "^\\s*")
	leading_n <- if (is.na(leading)) 0L else nchar(leading)

	skip_len <- 0L
	skip_regex <- if (!is.null(ann$indent_skip)) ann$indent_skip[i] else NA_character_
	if (!is.na(skip_regex)) {
		rest <- substr(body, leading_n + 1L, nchar(body))
		skip_match <- stringr::str_extract(rest, skip_regex)
		if (!is.na(skip_match)) skip_len <- nchar(skip_match)
	}
	total <- leading_n + skip_len
	if (total > 0) paste0(strrep(" ", total), text) else text
}

# ===== GRAPHEMES AND TOKENIZATION =====

split_graphemes <- function(text) {
	if (is.na(text) || nchar(text) == 0) return(character(0))
	stringi::stri_split_boundaries(text, type = "character")[[1]]
}

tokenize_content <- function(text) {
	graphemes <- split_graphemes(text)
	if (length(graphemes) == 0) return(list())
	kind <- ifelse(graphemes == HARD_BREAK_CHAR, "break",
	        ifelse(stringr::str_detect(graphemes, "^\\s$"), "space", "word"))
	run_id <- cumsum(c(TRUE, kind[-1] != kind[-length(kind)] |
	                          kind[-1] == "break"))
	tokens <- list()
	for (r in unique(run_id)) {
		idx <- which(run_id == r)
		tokens[[length(tokens) + 1]] <- list(
			type      = kind[idx[1]],
			graphemes = graphemes[idx]
		)
	}
	# GAT latching "=" is a legal break point: the line may end on "=" and
	# the next one starts with the matching "=" (user rule 2026-08-14).
	# The "=" stays with the left part, so no space appears at the break.
	split_tokens <- list()
	for (token in tokens) {
		if (!identical(token$type, "word")) {
			split_tokens[[length(split_tokens) + 1]] <- token
			next
		}
		# Break points inside a word: the GAT latching "=" and the
		# underscore of compound forms ("seguí_seGUÍ"). Both stay with the
		# left part, so no space appears at the break (user rule 2026-08-14).
		cuts <- which(token$graphemes %in% c("=", "_"))
		cuts <- cuts[cuts < length(token$graphemes)]
		if (length(cuts) == 0) {
			split_tokens[[length(split_tokens) + 1]] <- token
			next
		}
		starts <- c(1L, cuts + 1L)
		ends <- c(cuts, length(token$graphemes))
		for (k in seq_along(starts)) {
			split_tokens[[length(split_tokens) + 1]] <- list(
				type = "word",
				graphemes = token$graphemes[starts[k]:ends[k]]
			)
		}
	}
	split_tokens
}

# After wrapping: two latch marks that ended up NEXT TO EACH OTHER on the
# same line denote one latch and collapse to a single "=" ("==" -> "=",
# "=#=" -> "#="). Across a line break both are kept.
# A space between a mark and the word behind it is only needed when the
# mark hangs on the PREVIOUS word ("hI◊ce| fue"). Where the mark itself
# starts a word - because a space or the line start precedes it - the
# space behind it is dropped: "| |fue", "◊TAC,#" (user rule 2026-08-14).
.drop_space_after_leading_mark <- function(lines, anchor_chars, prefix_width) {
	for (k in seq_along(lines)) {
		graphemes <- split_graphemes(lines[k])
		drop <- integer(0)
		for (idx in seq_along(graphemes)) {
			if (!(graphemes[idx] %in% anchor_chars)) next
			if (idx >= length(graphemes) - 1L) next
			if (!stringr::str_detect(graphemes[idx + 1L], "^ $")) next
			# the word must really follow, not another space or mark
			following <- graphemes[idx + 2L]
			if (stringr::str_detect(following, "^\\s$") ||
			    following %in% anchor_chars) next
			starts_word <- idx <= prefix_width + 1L ||
				stringr::str_detect(graphemes[idx - 1L], "^\\s$")
			if (starts_word) drop <- c(drop, idx + 1L)
		}
		if (length(drop) > 0) {
			lines[k] <- paste(graphemes[-drop], collapse = "")
		}
	}
	lines
}

# GAT latching across a line break is written on BOTH sides: the line ends
# on "=" and the next one starts with "=" again. The annotation itself
# carries only one, so the second is added here (user rule 2026-08-14).
.duplicate_latch_at_break <- function(lines, prefix_width, width) {
	if (length(lines) < 2L) return(lines)
	for (k in seq_len(length(lines) - 1L)) {
		if (!stringr::str_detect(lines[k], "=$")) next
		body <- substr(lines[k + 1L], prefix_width + 1L, nchar(lines[k + 1L]))
		# Already latched at the start - either directly ("=con") or behind
		# a mark ("#=con"): nothing to add, otherwise "=#=" would grow back
		# after the in-line reduction.
		if (stringr::str_detect(body, "^.?=")) next
		if (is.finite(width) &&
		    length(split_graphemes(lines[k + 1L])) >= width) next
		lines[k + 1L] <- paste0(substr(lines[k + 1L], 1L, prefix_width),
		                        "=", body)
	}
	lines
}

.collapse_latch_in_lines <- function(lines, anchor_chars) {
	for (k in seq_along(lines)) {
		graphemes <- split_graphemes(lines[k])
		drop <- integer(0)
		for (idx in seq_along(graphemes)) {
			if (graphemes[idx] != "=") next
			j <- idx + 1L
			while (j <= length(graphemes) && graphemes[j] %in% anchor_chars) {
				j <- j + 1L
			}
			if (j <= length(graphemes) && graphemes[j] == "=") {
				drop <- c(drop, idx)
			}
		}
		if (length(drop) > 0) {
			lines[k] <- paste(graphemes[-drop], collapse = "")
		}
	}
	lines
}

# ===== RENDERER =====

render_annotation_tokens <- function(text, anchors, width, prefix_first,
                                     prefix_cont, arrow_mode, pair_mode,
                                     start_col = NULL, lead_allowed = FALSE,
                                     min_description = 10L,
                                     continuation_arrow = 2L) {
	tokens <- tokenize_content(text)
	prefix_first_g <- split_graphemes(prefix_first)
	if (!is.null(start_col) && start_col > length(prefix_first_g) + 1L) {
		prefix_first_g <- c(prefix_first_g,
		                    rep(" ", start_col - 1L - length(prefix_first_g)))
	}
	prefix_cont_g  <- split_graphemes(prefix_cont)
	prefix_len     <- length(prefix_cont_g)

	state <- new.env(parent = emptyenv())
	state$lines        <- character(0)
	state$line_g       <- prefix_first_g
	state$pending_g    <- character(0)
	state$open_fill    <- NA_character_
	state$open_line    <- NA_integer_
	state$last_anchor_source <- NA_integer_
	state$close_target <- NA_integer_
	state$mm_edge      <- NA_integer_
	state$mm_fill      <- NA_character_
	state$last_anchor_line <- NA_integer_
	state$has_placed_anchor <- FALSE
	state$lead_lines <- integer(0)
	state$moved_anchors <- list()
	state$has_more <- FALSE
	state$anchor_on_line <- FALSE
	state$at_start     <- TRUE
	state$occ_counter  <- list()
	state$placements   <- list()

	cur_col <- function() length(state$line_g) + 1L

	# While a pair is open and more text follows, the last columns are kept
	# free for the continuation arrow - otherwise a full line has no room
	# left for it (user rule 2026-08-14).
	effective_width <- function() {
		if (!is.finite(width)) return(width)
		# Reserve the FULL arrow ("->"), not just its head: a line ending
		# in a bare ">" reads as if the gesture stopped there (user rule
		# 2026-08-14).
		if (!is.na(state$open_fill) && isTRUE(state$has_more)) {
			return(width - continuation_arrow)
		}
		width
	}

	line_has_content <- function() {
		base_len <- if (length(state$lines) == 0) length(prefix_first_g) else prefix_len
		length(state$line_g) > base_len
	}

	finalize_line <- function(continued, restart_prefix = FALSE) {
		# Inside a span whose closing mark sits on the SAME line, every
		# wrapped line is filled up to the closing column - the
		# description forms a rectangle whose right edge is the closing
		# mark, and no continuation arrow is added (user rule 2026-08-17).
		mm_filled <- FALSE
		if (continued && !is.na(state$mm_edge) && !is.na(state$mm_fill) &&
		    is.finite(width)) {
			room <- state$mm_edge - length(state$line_g)
			if (room > 0) {
				state$line_g <- c(state$line_g, rep(state$mm_fill, room))
			}
			mm_filled <- TRUE
		}
		if (!mm_filled &&
		    continued && !is.na(state$open_fill) && is.finite(width)) {
			room <- width - length(state$line_g)
			if (room >= 2) {
				last_g <- if (length(state$line_g) > 0) state$line_g[length(state$line_g)] else ""
				if (last_g != ">") {
					# Mondada writes the continuation arrow with a single
					# dash ("->"), never built from the pair fill - a fill of
					# spaces would leave a bare ">" (user comment K0,
					# 2026-08-15).
					state$line_g <- c(state$line_g, "-", ">")
				}
			}
		}
		state$lines <- c(state$lines, paste(state$line_g, collapse = ""))
		state$anchor_on_line <- FALSE
		# A layer row that is distributed over several main lines repeats
		# its tier label on every new main line - otherwise the continuation
		# lines lose their speaker/tier attribution once interleaved
		# (F7, 2026-08-13).
		state$line_g <- if (restart_prefix) {
			c(prefix_first_g[seq_len(min(length(prefix_first_g),
			                             length(prefix_cont_g)))],
			  rep(" ", max(0L, length(prefix_cont_g) - length(prefix_first_g))))
		} else {
			prefix_cont_g
		}
		state$pending_g <- character(0)
	}

	write_graphemes <- function(g) {
		state$line_g <- c(state$line_g, g)
	}

	next_occurrence <- function(char) {
		n <- state$occ_counter[[char]]
		if (is.null(n)) n <- 0L
		state$occ_counter[[char]] <- n + 1L
		n + 1L
	}

	anchor_lookup <- function(char, occurrence) {
		if (is.null(anchors) || nrow(anchors) == 0) return(NULL)
		hit <- which(anchors$char == char & anchors$occurrence == occurrence)
		if (length(hit) == 0) return(NULL)
		anchors[hit[1], , drop = FALSE]
	}

	record_placement <- function(anchor, placed_col, degraded, note) {
		state$placements[[length(state$placements) + 1]] <- data.frame(
			char = anchor$char, occurrence = anchor$occurrence,
			target_col = anchor$target_col, placed_col = placed_col,
			degraded = degraded, note = note
		)
	}

	update_pair_state <- function(anchor_type, occurrence_after = NA,
	                              char_after = NA) {
		if (!pair_mode) return(invisible(NULL))
		if (anchor_type %in% c("open", "both")) {
			state$open_fill <- state$pair_fill
			state$open_line <- length(state$lines) + 1L
			# Column of the NEXT closing anchor ON THE SAME target line:
			# while the pair is open the plain text must not run past it -
			# the description breaks there and every wrapped line is
			# filled up to that column, so the description forms a
			# rectangle whose right edge is the closing mark
			# (user rules 2026-08-17).
			state$close_target <- NA_integer_
			state$mm_edge <- NA_integer_
			state$mm_fill <- NA_character_
			if (!is.null(anchors) && nrow(anchors) > 0 && !is.na(occurrence_after)) {
				own <- which(anchors$char == char_after &
				             anchors$occurrence == occurrence_after)
				if (length(own) == 0) {
					own <- which(anchors$occurrence == occurrence_after)
				}
				own_line <- if (length(own) > 0) anchors$target_line[own[1]] else NA
				own_col <- if (length(own) > 0) anchors$target_col[own[1]] else NA
				later <- if (length(own) > 0) {
					seq_len(nrow(anchors)) > own[1]
				} else {
					anchors$occurrence > occurrence_after
				}
				nxt <- which(later & anchors$type == "close" &
				             !is.na(anchors$target_col) &
				             (is.na(own_line) | is.na(anchors$target_line) |
				              anchors$target_line == own_line))
				# The rectangle is an EMERGENCY layout and stays flat: it
				# forms only when the description fits into at most TWO
				# text lines of the rectangle (opening line plus one full
				# line; the closing line carries the last word). Longer
				# descriptions keep the full line width, and spans
				# narrower than min_description never form one
				# (user decision 2026-08-17).
				if (length(nxt) > 0 &&
				    anchors$target_col[nxt[1]] - prefix_len >=
				    	min_description) {
					close_col <- anchors$target_col[nxt[1]]
					graphemes_text <- split_graphemes(text)
					open_index <- .symbol_index_in_text(graphemes_text,
						anchors$char[own[1]], anchors$occurrence[own[1]])
					close_index <- .symbol_index_in_text(graphemes_text,
						anchors$char[nxt[1]], anchors$occurrence[nxt[1]])
					description_length <- if (!is.na(open_index) &&
					                          !is.na(close_index)) {
						close_index - open_index - 1L
					} else {
						NA_integer_
					}
					room_first <- close_col -
						(if (!is.na(own_col)) own_col else prefix_len) - 1L
					room_full <- close_col - prefix_len - 1L
					if (!is.na(description_length) &&
					    description_length <= room_first + room_full) {
						state$close_target <- close_col
						state$mm_edge <- close_col
						state$mm_fill <- state$pair_fill
					}
				}
			}
		} else if (anchor_type == "close") {
			state$open_fill <- NA_character_
			state$open_line <- NA_integer_
			state$close_target <- NA_integer_
			state$mm_edge <- NA_integer_
			state$mm_fill <- NA_character_
		}
		invisible(NULL)
	}

	state$pair_fill <- if (!is.null(anchors) && nrow(anchors) > 0 &&
	                       !is.null(anchors$pair_fill)) {
		anchors$pair_fill[1]
	} else {
		"-"
	}

	anchor_chars <- if (!is.null(anchors) && nrow(anchors) > 0) {
		unique(anchors$char)
	} else {
		character(0)
	}

	emit_plain <- function(token_g) {
		if (!state$at_start && !line_has_content()) {
			state$pending_g <- character(0)
		}
		p <- length(state$pending_g)
		w <- length(token_g)
		if (state$at_start) {
			write_graphemes(state$pending_g)
			state$pending_g <- character(0)
			write_graphemes(token_g)
		} else if ((cur_col() + p + w - 1L <= effective_width() ||
		            !line_has_content()) &&
		           !(line_has_content() &&
		             !is.na(state$open_fill) && !is.na(state$close_target) &&
		             cur_col() + p + w - 1L >= state$close_target &&
		             prefix_len + w < state$close_target)) {
			write_graphemes(state$pending_g)
			state$pending_g <- character(0)
			write_graphemes(token_g)
		} else {
			finalize_line(continued = TRUE)
			write_graphemes(token_g)
		}
		state$at_start <- FALSE
	}

	emit_anchor_token <- function(token_g, token_anchors) {
		if (!state$at_start && !line_has_content()) {
			state$pending_g <- character(0)
		}
		primary_idx <- which(!is.na(token_anchors$target_col))[1]
		if (is.na(primary_idx)) {
			for (k in seq_len(nrow(token_anchors))) {
				update_pair_state(token_anchors$type[k],
				                  token_anchors$occurrence[k],
				                  token_anchors$char[k])
			}
			emit_plain(token_g)
			return(invisible(NULL))
		}
		primary <- token_anchors[primary_idx, , drop = FALSE]

		token_str <- paste(token_g, collapse = "")
		arrow_match <- stringr::str_match(token_str, "^([-=]*)(>+)")
		arrow_lead <- !is.na(arrow_match[1, 1]) &&
			primary$offset == (nchar(arrow_match[1, 2]) +
			                   nchar(arrow_match[1, 3]) + 1L)
		fill_char <- if (arrow_lead) {
			if (identical(arrow_mode, "space")) " " else {
				if (nchar(arrow_match[1, 2]) > 0) substr(arrow_match[1, 2], 1, 1) else "-"
			}
		} else {
			primary$fill_before
		}
		if (is.na(fill_char) || fill_char == "") fill_char <- " "

		expansion <- expand_token_internally(token_g, token_anchors, primary_idx)
		token_g   <- expansion$graphemes
		offsets   <- expansion$offsets

		phase_inside <- !arrow_lead && !identical(fill_char, " ")

		assemble <- function(start_col) {
			tg <- token_g
			off <- offsets
			if (phase_inside) {
				n_insert <- primary$target_col - (start_col + off[primary_idx] - 1L)
				if (n_insert < 0) return(NULL)
				if (n_insert > 0) {
					# Pure fill, never an arrowhead: in front of a closing
					# symbol the line ends in fill characters - ">" belongs
					# only to a line-end continuation arrow (user comment K3,
					# 2026-08-15).
					run <- rep(fill_char, n_insert)
					insert_at <- off[primary_idx] - 1L
					tg <- append(tg, run, after = insert_at)
					shift <- off >= off[primary_idx]
					off[shift] <- off[shift] + n_insert
				}
				list(tg = tg, off = off, pad = 0L)
			} else {
				pad <- primary$target_col - (off[primary_idx] - 1L) - start_col
				if (pad < 0) return(NULL)
				list(tg = tg, off = off, pad = pad)
			}
		}

		p <- length(state$pending_g)
		placed_plan <- assemble(cur_col() + p)
		fits <- !is.null(placed_plan) && (!is.finite(width) ||
			(cur_col() + p + placed_plan$pad + length(placed_plan$tg) - 1L) <=
				effective_width())
		# F6 (2026-08-13): a rendered layer line must belong to exactly ONE
		# line of its main - that is what the interleaver distributes. When
		# the targeted main line changes, force the line-break path (which
		# also runs the F4 split when the anchor sits left of the text).
		# Only break when this line already carries a symbol of the previous
		# main line. A line holding nothing but continuation text belongs to
		# the new block anyway, so breaking would strand a word on its own
		# ("the" / "left..." instead of "the left...", user rule 2026-08-14).
		line_changed <- identical(primary$domain, "multimodal") &&
			!is.na(primary$target_line) && !is.na(state$last_anchor_line) &&
			primary$target_line != state$last_anchor_line &&
			isTRUE(state$anchor_on_line)
		# F8: description of a gesture that started before this verbal
		# annotation goes on a line ABOVE it - so the first anchor of such
		# a row must take the split path, not the degrade path.
		lead_split <- lead_allowed && length(state$lines) == 0L &&
			!state$has_placed_anchor && offsets[primary_idx] > 1L

		if (!is.null(placed_plan) && fits && !line_changed) {
			# When the anchor sits so close to the right edge that hardly any
			# description fits behind it, only the symbol stays on this line
			# and the description starts in the next block (user rule
			# 2026-08-14).
			# A gap in front of the symbol continues a preceding fill run
			# of dots or commas instead of falling back to spaces (user
			# comments K10/K15, 2026-08-15).
			pad_char <- fill_char
			if (identical(pad_char, " ")) {
				tail_g <- c(state$line_g, state$pending_g)
				tail_g <- tail_g[tail_g != " "]
				last <- if (length(tail_g)) tail_g[length(tail_g)] else ""
				if (last %in% c(".", ",") && length(tail_g) >= 2 &&
				    tail_g[length(tail_g) - 1L] == last) {
					pad_char <- last
					state$pending_g[state$pending_g == " "] <- pad_char
				}
			}
			write_graphemes(state$pending_g)
			state$pending_g <- character(0)
			if (placed_plan$pad > 0) {
				write_graphemes(rep(pad_char, placed_plan$pad))
			}
		} else if (is.null(placed_plan) && primary$domain == "multimodal" &&
		           !lead_split &&
		           (is.na(primary$source_row) ||
		            is.na(state$last_anchor_source) ||
		            primary$source_row == state$last_anchor_source) &&
		           (is.na(primary$target_line) ||
		            is.na(state$last_anchor_line) ||
		            primary$target_line == state$last_anchor_line) &&
		           is.null(assemble(prefix_len + 1L))) {
			# Degrading is the LAST resort: only when the target column is
			# unreachable even on a fresh line (description denser than
			# its span, e.g. marks two columns apart). Otherwise the token
			# splits below so the symbol reaches its column exactly
			# (user mock-up 2026-08-17).
			record_placement(primary, cur_col() + p + offsets[primary_idx] - 1L,
			                 TRUE, "target_before_current_col")
			for (k in seq_len(nrow(token_anchors))) {
				update_pair_state(token_anchors$type[k],
				                  token_anchors$occurrence[k],
				                  token_anchors$char[k])
			}
			emit_plain(token_g)
			return(invisible(NULL))
		} else {
			# The description is longer than its span: it wraps early so
			# the closing symbol still reaches its column. Always noted in
			# the report (user decision 2026-08-17).
			if (is.null(placed_plan) && !line_changed &&
			    identical(primary$domain, "multimodal")) {
				record_placement(primary, primary$target_col, FALSE,
				                 "wrapped_to_span")
			}
			# F4 (user GO 2026-08-13): the anchor may be unreachable on a
			# fresh line because token text precedes it (e.g. "RL|" whose
			# "|" must sit left of column 3). Split the token at the anchor:
			# the leading text stays on the current line, the new line then
			# starts with fill up to the target column.
			split_at <- offsets[primary_idx]
			leading_anchor <- any(!is.na(token_anchors$target_col) &
			                      offsets < split_at)
			# Split also when the break is forced by a BLOCK change: the text
			# in front of the symbol stays up here if it still fits together
			# with the continuation arrow (user rule 2026-08-14).
			# Split whenever the whole token would have to move down but the
			# text IN FRONT of the anchor still fits up here (with room for
			# the continuation arrow). Keeps the description together
			# instead of spreading it over an extra line - user comment K1
			# on 702, 2026-08-14.
			split_for_block <- line_has_content() && split_at > 1L &&
				(!is.finite(width) ||
				 (cur_col() + length(state$pending_g) + (split_at - 1L) - 1L +
				  continuation_arrow) <= width)
			if (split_at > 1L && !leading_anchor &&
			    (is.null(assemble(prefix_len + 1L)) || split_for_block) &&
			    (line_has_content() || lead_split)) {
				head_g <- token_g[seq_len(split_at - 1L)]
				tail_g <- token_g[split_at:length(token_g)]
				# The text staying up here must leave room for the
				# continuation arrow as well - and inside a description
				# rectangle it must not cross the closing column either
				# (user rule 2026-08-17).
				room <- !is.finite(width) ||
					(cur_col() + length(state$pending_g) +
					 length(head_g) - 1L) <= effective_width()
				if (room && !is.na(state$mm_edge) &&
				    (cur_col() + length(state$pending_g) +
				     length(head_g) - 1L) > state$mm_edge) {
					room <- FALSE
				}
				if (room) {
					write_graphemes(state$pending_g)
					state$pending_g <- character(0)
					write_graphemes(head_g)
					state$at_start <- FALSE
					if (lead_split) {
						state$lead_lines <- c(state$lead_lines,
						                      length(state$lines) + 1L)
						# The gesture continues into the verbal line below,
						# so the lead line ends with a continuation arrow.
						if (is.finite(width)) {
							room <- width - length(state$line_g)
							if (room >= 2L) {
								write_graphemes(c(rep(fill_char, room - 2L),
								                  "-", ">"))
							}
						}
					}
					tail_keep <- offsets >= split_at
					tail_anchors <- token_anchors[tail_keep, , drop = FALSE]
					tail_anchors$offset <- offsets[tail_keep] - split_at + 1L
					finalize_line(continued = TRUE,
					              restart_prefix = line_changed || lead_split)
					emit_anchor_token(tail_g, tail_anchors)
					return(invisible(NULL))
				}
			}
			if (line_has_content()) {
				finalize_line(continued = TRUE, restart_prefix = line_changed)
			}
			state$pending_g <- character(0)
			placed_plan <- assemble(prefix_len + 1L)
			if (is.null(placed_plan)) {
				placed_plan <- list(tg = token_g, off = offsets, pad = 0L)
				record_placement(primary,
				                 prefix_len + offsets[primary_idx],
				                 TRUE, "target_within_prefix")
			}
			if (placed_plan$pad > 0) {
				write_graphemes(rep(fill_char, placed_plan$pad))
			}
		}

		token_g <- placed_plan$tg
		offsets <- placed_plan$off
		token_start <- cur_col()

		# When the anchor sits so close to the right edge that hardly any
		# description fits behind it, only the symbol stays on this line and
		# the description starts in the next block (user rule 2026-08-14).
		anchor_col <- token_start + offsets[primary_idx] - 1L
		tail_length <- length(token_g) - offsets[primary_idx]
		# The layer keeps its normal word wrap: symbol stays at its column,
		# the description runs on into the next block. Room is made in the
		# VERBAL line instead (see apply_main_break_hints), so nothing has
		# to be pushed around here. Only a symbol with literally no space
		# behind it takes the whole annotation along (user decision C,
		# 2026-08-14).
		if (identical(primary$domain, "multimodal") && tail_length > 0 &&
		    !identical(primary$type, "point") &&
		    is.finite(width) && (width - anchor_col) < 2L &&
		    all(is.na(token_anchors$target_col) |
		        offsets <= offsets[primary_idx])) {
			if (line_has_content()) {
				finalize_line(continued = FALSE, restart_prefix = TRUE)
			}
			for (k in seq_len(nrow(token_anchors))) {
				anchor_k <- token_anchors[k, , drop = FALSE]
				if (!is.na(anchor_k$target_col)) {
					state$moved_anchors[[length(state$moved_anchors) + 1]] <-
						data.frame(char = anchor_k$char,
						           occurrence = anchor_k$occurrence)
					record_placement(anchor_k,
					                 cur_col() + offsets[k] - 1L,
					                 FALSE, "moved_to_next_line")
					state$has_placed_anchor <- TRUE
				state$anchor_on_line <- TRUE
					if (!is.na(anchor_k$source_row)) {
						state$last_anchor_source <- anchor_k$source_row
					}
					if (!is.na(anchor_k$target_line)) {
						state$last_anchor_line <- anchor_k$target_line + 1L
					}
				}
				update_pair_state(anchor_k$type, anchor_k$occurrence,
				                  anchor_k$char)
			}
			emit_plain(token_g)
			return(invisible(NULL))
		}

		write_graphemes(token_g)
		state$at_start <- FALSE

		for (k in seq_len(nrow(token_anchors))) {
			anchor_k <- token_anchors[k, , drop = FALSE]
			placed <- token_start + offsets[k] - 1L
			if (!is.na(anchor_k$target_col)) {
				already <- vapply(state$placements, function(pl) {
					pl$char == anchor_k$char && pl$occurrence == anchor_k$occurrence
				}, logical(1))
				if (!any(already)) {
					degraded <- placed != anchor_k$target_col
					record_placement(anchor_k, placed, degraded,
					                 if (degraded) "not_at_target" else "")
				}
				state$has_placed_anchor <- TRUE
				state$anchor_on_line <- TRUE
				if (!is.na(anchor_k$source_row)) {
					state$last_anchor_source <- anchor_k$source_row
				}
				if (!is.na(anchor_k$target_line)) {
					state$last_anchor_line <- anchor_k$target_line
				}
			}
			update_pair_state(anchor_k$type, anchor_k$occurrence,
			                  anchor_k$char)
		}
		invisible(NULL)
	}

	for (token_index in seq_along(tokens)) {
		token <- tokens[[token_index]]
		state$has_more <- token_index < length(tokens)
		if (token$type == "break") {
			if (line_has_content() || length(state$lines) > 0) {
				finalize_line(continued = TRUE)
			}
			state$at_start <- FALSE
		} else if (token$type == "space") {
			state$pending_g <- c(state$pending_g, token$graphemes)
		} else {
			token_anchor_rows <- list()
			offsets <- integer(0)
			for (g_idx in seq_along(token$graphemes)) {
				g <- token$graphemes[g_idx]
				if (g %in% anchor_chars) {
					occ <- next_occurrence(g)
					hit <- anchor_lookup(g, occ)
					if (!is.null(hit)) {
						hit$offset <- g_idx
						token_anchor_rows[[length(token_anchor_rows) + 1]] <- hit
					}
				}
			}
			if (length(token_anchor_rows) > 0) {
				token_anchors <- do.call(rbind, token_anchors_list <- token_anchor_rows)
				targeted <- !is.na(token_anchors$target_col)
				sources <- unique(paste(token_anchors$source_row[targeted],
				                        token_anchors$target_line[targeted]))
				sources <- sources[!is.na(sources)]
				if (nrow(token_anchors) > 1 && length(sources) > 1) {
					segment_start <- 1L
					ordered <- order(token_anchors$offset)
					for (a_idx in seq_along(ordered)) {
						anchor_row <- token_anchors[ordered[a_idx], , drop = FALSE]
						next_anchor <- if (a_idx < length(ordered)) {
							token_anchors[ordered[a_idx + 1L], , drop = FALSE]
						} else {
							NULL
						}
						split_here <- !is.null(next_anchor) &&
							!is.na(anchor_row$source_row) &&
							!is.na(next_anchor$source_row) &&
							(anchor_row$source_row != next_anchor$source_row ||
							 (!is.na(anchor_row$target_line) &&
							  !is.na(next_anchor$target_line) &&
							  anchor_row$target_line != next_anchor$target_line))
						if (split_here || is.null(next_anchor)) {
							segment_end <- if (is.null(next_anchor)) {
								length(token$graphemes)
							} else {
								anchor_row$offset
							}
							seg_graphemes <- token$graphemes[segment_start:segment_end]
							seg_anchors <- token_anchors[
								token_anchors$offset >= segment_start &
								token_anchors$offset <= segment_end, , drop = FALSE]
							seg_anchors$offset <- seg_anchors$offset - segment_start + 1L
							emit_anchor_token(seg_graphemes, seg_anchors)
							segment_start <- segment_end + 1L
							if (is.null(next_anchor)) break
						}
					}
				} else {
					emit_anchor_token(token$graphemes, token_anchors)
				}
			} else {
				emit_plain(token$graphemes)
			}
		}
	}
	finalize_line(continued = FALSE)

	lines <- sub("\\s+$", "", state$lines)
	# A line counts as empty when nothing but the prefix is on it - the
	# prefix itself (tier label) is not content.
	bodies <- substr(lines, prefix_len + 1L, nchar(lines))
	keep <- !stringr::str_detect(bodies, "^\\s*$")
	if (!any(keep)) keep[1] <- TRUE
	if (!keep[1] && length(lines) > 1) {
		first_keep <- which(keep)[1]
		body <- substr(lines[first_keep], prefix_len + 1L, nchar(lines[first_keep]))
		lines[first_keep] <- paste0(prefix_first, body)
	}
	lead_lines <- if (length(state$lead_lines) > 0) {
		match(state$lead_lines[keep[state$lead_lines]], which(keep))
	} else {
		integer(0)
	}
	lines <- lines[keep]

	placements <- if (length(state$placements) > 0) {
		do.call(rbind, state$placements)
	} else {
		data.frame(char = character(0), occurrence = integer(0),
		           target_col = integer(0), placed_col = integer(0),
		           degraded = logical(0), note = character(0))
	}
	list(lines = lines, placements = placements, lead_lines = lead_lines,
	     moved_anchors = if (length(state$moved_anchors) > 0) do.call(rbind, state$moved_anchors) else NULL)
}

# ===== INTERNAL TOKEN EXPANSION (multi-anchor tokens) =====

expand_token_internally <- function(token_g, token_anchors, primary_idx) {
	offsets <- token_anchors$offset
	if (nrow(token_anchors) <= 1) {
		return(list(graphemes = token_g, offsets = offsets))
	}
	primary <- token_anchors[primary_idx, , drop = FALSE]
	base_col_of_offset1 <- NA_integer_
	if (!is.na(primary$target_col)) {
		base_col_of_offset1 <- primary$target_col - (offsets[primary_idx] - 1L)
	}
	if (is.na(base_col_of_offset1)) {
		return(list(graphemes = token_g, offsets = offsets))
	}
	k_order <- order(offsets)
	for (k in k_order) {
		if (k == primary_idx) next
		target_k <- token_anchors$target_col[k]
		if (is.na(target_k)) next
		projected <- base_col_of_offset1 + offsets[k] - 1L
		if (target_k > projected) {
			n_insert <- target_k - projected
			fill_k <- token_anchors$fill_before[k]
			if (is.na(fill_k) || fill_k == "") fill_k <- " "
			insert_at <- offsets[k] - 1L
			token_g <- append(token_g, rep(fill_k, n_insert), after = insert_at)
			shift <- offsets >= offsets[k]
			offsets[shift] <- offsets[shift] + n_insert
		}
	}
	list(graphemes = token_g, offsets = offsets)
}

# ===== CLOSE-BRACKET POST-PASS (verbal pairs) =====
# After wrapping, equal segment lengths no longer guarantee equal closing
# columns. Three ways to move a "]" into its partner column: a "]" alone on
# its continuation line shifts freely; interna fill directly in front of a
# trailing "]" can shrink; a continuation line whose only bracket is this
# "]" can be indented as a whole. The smallest column every member can
# reach wins (user comment 205_005 K1, 2026-08-16).

align_close_bracket_lines <- function(ann, rendered_lines, pairs,
                                      text_body_width = Inf) {
	if (nrow(pairs) == 0) return(rendered_lines)

	node_i <- paste(pairs$i_row, pairs$i_occurrence)
	node_j <- paste(pairs$j_row, pairs$j_occurrence)
	nodes <- unique(c(node_i, node_j))
	label <- stats::setNames(seq_along(nodes), nodes)
	changed <- TRUE
	while (changed) {
		changed <- FALSE
		for (p in seq_len(nrow(pairs))) {
			a <- label[[node_i[p]]]
			b <- label[[node_j[p]]]
			if (a != b) {
				label[label == max(a, b)] <- min(a, b)
				changed <- TRUE
			}
		}
	}

	for (component in unique(label)) {
		component_nodes <- names(label)[label == component]
		members <- list()
		for (node in component_nodes) {
			parts <- strsplit(node, " ")[[1]]
			row <- as.integer(parts[1])
			occurrence <- as.integer(parts[2])
			if (is.null(rendered_lines[[row]])) next
			pos <- close_bracket_position(ann$text[row], occurrence,
			                              rendered_lines[[row]])
			if (!is.null(pos)) {
				members[[length(members) + 1]] <- c(list(row = row), pos)
			}
		}
		if (length(members) < 2) next

		cols  <- vapply(members, function(m) m$col, integer(1))
		if (length(unique(cols)) == 1) next

		# reachable column range per member
		ranges <- lapply(members, function(m) {
			line <- rendered_lines[[m$row]][m$line]
			prefix_len <- nchar(ann$prefix_cont[m$row])
			if (isTRUE(m$alone)) {
				return(c(prefix_len + 1L, Inf))
			}
			if (isTRUE(m$at_end) && m$trail_fill > 0L) {
				return(c(m$col - m$trail_fill, m$col))
			}
			if (m$line > 1L && isTRUE(m$only_bracket)) {
				room <- if (is.finite(text_body_width)) {
					max(0L, text_body_width - nchar(line))
				} else {
					0L
				}
				return(c(m$col, m$col + room))
			}
			c(m$col, m$col)
		})
		lo <- max(vapply(ranges, function(r) r[1], numeric(1)))
		hi <- min(vapply(ranges, function(r) r[2], numeric(1)))
		if (lo > hi) next
		target <- as.integer(lo)

		for (m_idx in seq_along(members)) {
			member <- members[[m_idx]]
			if (member$col == target) next
			row <- member$row
			line <- rendered_lines[[row]][member$line]
			prefix_len <- nchar(ann$prefix_cont[row])
			if (isTRUE(member$alone)) {
				rendered_lines[[row]] <- shift_lone_bracket_line(
					rendered_lines[[row]], member$line,
					ann$prefix_cont[row], target)
				next
			}
			if (isTRUE(member$at_end) && member$col > target) {
				drop_n <- member$col - target
				head <- substr(line, 1L, member$col - 1L - drop_n)
				rendered_lines[[row]][member$line] <- paste0(head, "]")
				next
			}
			if (member$line > 1L && isTRUE(member$only_bracket) &&
			    member$col < target) {
				indent <- target - member$col
				body <- substr(line, prefix_len + 1L, nchar(line))
				rendered_lines[[row]][member$line] <- paste0(
					substr(line, 1L, prefix_len),
					strrep(" ", indent), body)
			}
		}
	}
	rendered_lines
}

close_bracket_position <- function(text, open_occurrence, lines) {
	seg <- extract_bracket_segment(text, open_occurrence)
	if (is.null(seg)) return(NULL)
	close_occurrence <- stringr::str_count(substr(text, 1, seg$end),
	                                       stringr::fixed("]"))
	count <- 0L
	for (line_index in seq_along(lines)) {
		graphemes <- split_graphemes(lines[line_index])
		hits <- which(graphemes == "]")
		for (h in hits) {
			count <- count + 1L
			if (count == close_occurrence) {
				alone <- identical(stringr::str_trim(lines[line_index]), "]")
				trail <- 0L
				k <- h - 1L
				while (k >= 1L && graphemes[k] == " ") {
					trail <- trail + 1L
					k <- k - 1L
				}
				only_bracket <- sum(graphemes %in% c("[", "]")) == 1L
				return(list(line = line_index, col = h, alone = alone,
				            at_end = h == length(graphemes),
				            trail_fill = trail,
				            only_bracket = only_bracket))
			}
		}
	}
	NULL
}

shift_lone_bracket_line <- function(lines, line_index, prefix_cont, target_col) {
	prefix_len <- nchar(prefix_cont)
	if (target_col <= prefix_len) return(lines)
	lines[line_index] <- paste0(prefix_cont,
	                            strrep(" ", target_col - prefix_len - 1L), "]")
	lines
}

# ===== POSITION EXTRACTION FROM RENDERED LINES =====

extract_anchor_positions <- function(lines, chars) {
	rows <- list()
	occ_counter <- list()
	for (line_index in seq_along(lines)) {
		graphemes <- split_graphemes(lines[line_index])
		hits <- which(graphemes %in% chars)
		for (h in hits) {
			g <- graphemes[h]
			n <- occ_counter[[g]]
			if (is.null(n)) n <- 0L
			occ_counter[[g]] <- n + 1L
			rows[[length(rows) + 1]] <- data.frame(
				char = g, occurrence = n + 1L,
				line = line_index, col = h
			)
		}
	}
	if (length(rows) == 0) {
		return(data.frame(char = character(0), occurrence = integer(0),
		                  line = integer(0), col = integer(0)))
	}
	do.call(rbind, rows)
}

# ======================================================================
# ==== source module: compute_anchors.R ====

# ===== ANCHOR COMPUTATION =====
# Builds the anchor specification for one annotation, reading target columns
# from ALREADY RENDERED predecessor annotations (rendered_cache).
# Anchor spec columns: char, occurrence, target_col, target_line, fill_before,
# type, source_row, domain ("verbal"|"multimodal"), pair_fill.
#
# Renderer coupling contract (lane renderer, plan Folgearbeiten 2):
# compute_bracket_pairs(), compute_mm_symbol_matches(), scan_layer_symbols()
# and compute_anchors() must stay callable WITHOUT the paper renderer. The
# only renderer-provided input is rendered_cache[[source_row]]$positions,
# a data.frame (char, occurrence, col, line). Do not widen this interface -
# an alternative lane renderer must be able to fill the same cache.

compute_anchors <- function(ann, i, rendered_cache, pairs, text_body_width,
                            ref_main, mm_matches, merge_map = NULL,
                            main_fragment_starts = NULL) {
	anchor_rows <- list()
	warnings <- list()

	# ---- verbal anchors ----
	if (isTRUE(ann$is_main[i]) && nrow(pairs) > 0) {
		my_pairs <- pairs[pairs$j_row == i, , drop = FALSE]
		for (p in seq_len(nrow(my_pairs))) {
			source_row <- my_pairs$i_row[p]
			cache <- rendered_cache[[source_row]]
			if (is.null(cache)) next
			pos <- cache$positions
			hit <- pos[pos$char == "[" & pos$occurrence == my_pairs$i_occurrence[p], , drop = FALSE]
			if (nrow(hit) == 0) next
			target_col <- hit$col[1]
			if (isTRUE(ann$wrap[i]) && is.finite(text_body_width) &&
			    target_col > text_body_width) {
				warnings[[length(warnings) + 1]] <- list(
					kind = "mixed_wrap", row = i, tier = ann$tierName[i],
					char = "[", occurrence = my_pairs$j_occurrence[p],
					target_col = target_col,
					startsec = ann$startsec[i], endsec = ann$endsec[i],
					content = ann$content[i],
					main_tier = ann$tierName[source_row],
					main_content = ann$content[source_row],
					main_startsec = ann$startsec[source_row],
					main_endsec = ann$endsec[source_row]
				)
				next
			}
			anchor_rows[[length(anchor_rows) + 1]] <- data.frame(
				char = "[", occurrence = my_pairs$j_occurrence[p],
				target_col = target_col, target_line = hit$line[1],
				fill_before = " ",
				type = "open", source_row = source_row,
				domain = "verbal", pair_fill = NA_character_
			)
		}
	}

	# ---- unpaired outer of a double bracket hugs the anchored inner ----
	if (length(anchor_rows) > 0) {
		graphemes <- split_graphemes(ann$content[i])
		positions <- which(graphemes == "[")
		if (length(positions) >= 2 && positions[1] <= 2L &&
		    positions[2] == positions[1] + 1L) {
			anchors_so_far <- do.call(rbind, anchor_rows)
			has_outer <- any(anchors_so_far$char == "[" & anchors_so_far$occurrence == 1L)
			inner_hit <- anchors_so_far[anchors_so_far$char == "[" &
			                            anchors_so_far$occurrence == 2L, , drop = FALSE]
			if (!has_outer && nrow(inner_hit) > 0 &&
			    !is.na(inner_hit$target_col[1]) && inner_hit$target_col[1] > 2) {
				anchor_rows[[length(anchor_rows) + 1]] <- data.frame(
					char = "[", occurrence = 1L,
					target_col = inner_hit$target_col[1] - 1L,
					target_line = inner_hit$target_line[1],
					fill_before = " ", type = "open",
					source_row = inner_hit$source_row[1],
					domain = "verbal", pair_fill = NA_character_
				)
			}
		}
	}

	# ---- multimodal anchors (time-matched, possibly across mains) ----
	align_chars <- ann$align_chars[i]
	align_mode  <- ann$align_mode[i]
	if (!is.na(align_chars) && nchar(align_chars) > 0 && !is.na(align_mode)) {
		char_vector <- strsplit(align_chars, "")[[1]]
		layer_symbols <- scan_layer_symbols(ann$text[i], char_vector)
		pair_fill <- ann$filler_inside[i]
		if (is.na(pair_fill) || pair_fill == "") pair_fill <- "-"
		context_row <- ref_main[i]
		my_matches <- mm_matches[mm_matches$layer_row == i, , drop = FALSE]

		# A leading symbol of a fragment that starts flush with a main
		# annotation needs no counterpart: it is temporally correct at the
		# start of its segment. Uses the per-fragment time table when the
		# row was concatenated, otherwise the row's own leading block
		# (F3, user GO 2026-08-13).
		flush_tolerance <- 0.05
		time_table <- if (!is.null(ann$symbol_times_table)) {
			ann$symbol_times_table[[i]]
		} else {
			NULL
		}
		if (is.null(main_fragment_starts)) {
			main_fragment_starts <- ann$startsec[ann$is_main]
		}
		leading_block_end <- 0L
		if (is.null(time_table)) {
			graphemes_i <- split_graphemes(ann$text[i])
			g_idx <- 1L
			while (g_idx <= length(graphemes_i) &&
			       stringr::str_detect(graphemes_i[g_idx], "^\\s$")) {
				g_idx <- g_idx + 1L
			}
			while (g_idx <= length(graphemes_i) &&
			       graphemes_i[g_idx] %in% char_vector) {
				leading_block_end <- g_idx
				g_idx <- g_idx + 1L
			}
		}
		is_flush_leading <- function(sym) {
			if (is.null(time_table)) {
				if (sym$index > leading_block_end) return(FALSE)
				reference <- if (!is.na(context_row)) {
					ann$startsec[context_row]
				} else {
					NA_real_
				}
				return(!is.na(reference) &&
				       abs(ann$startsec[i] - reference) <= flush_tolerance)
			}
			hits <- time_table[time_table$char == sym$char, , drop = FALSE]
			if (nrow(hits) == 0) return(FALSE)
			remapped <- vapply(hits$occurrence, function(occurrence) {
				remap_symbol_occurrence(merge_map, i, sym$char, occurrence)
			}, integer(1))
			hits <- hits[remapped == sym$occurrence, , drop = FALSE]
			if (nrow(hits) == 0 || !any(hits$flush_candidate)) return(FALSE)
			hits <- hits[hits$flush_candidate, , drop = FALSE]
			any(vapply(hits$time, function(symbol_time) {
				any(abs(main_fragment_starts - symbol_time) <= flush_tolerance)
			}, logical(1)))
		}

		for (s in seq_len(nrow(layer_symbols))) {
			sym <- layer_symbols[s, , drop = FALSE]
			char_matches <- my_matches[my_matches$char == sym$char, , drop = FALSE]
			if (nrow(char_matches) > 0 && !is.null(merge_map)) {
				remapped <- vapply(char_matches$layer_occurrence, function(occurrence) {
					remap_symbol_occurrence(merge_map, i, sym$char, occurrence)
				}, integer(1))
				match_hit <- char_matches[remapped == sym$occurrence, , drop = FALSE]
			} else {
				match_hit <- char_matches[char_matches$layer_occurrence == sym$occurrence, , drop = FALSE]
			}
			target_col <- NA_integer_
			target_line <- NA_integer_
			source_row <- NA_integer_
			if (nrow(match_hit) > 0) {
				source_row <- match_hit$main_row[1]
				cache <- rendered_cache[[source_row]]
				if (!is.null(cache)) {
					main_occurrence <- remap_symbol_occurrence(
						merge_map, source_row, sym$char, match_hit$main_occurrence[1])
					pos <- cache$positions
					hit <- pos[pos$char == sym$char &
					           pos$occurrence == main_occurrence, , drop = FALSE]
					if (nrow(hit) > 0) {
						target_col <- hit$col[1]
						target_line <- hit$line[1]
					}
				}
			}
			if (is.na(target_col) && nrow(match_hit) == 0 &&
			    !is_flush_leading(sym)) {
				warnings[[length(warnings) + 1]] <- list(
					kind = "unmatched_symbol", row = i,
					tier = ann$tierName[i], char = sym$char,
					occurrence = sym$occurrence,
					startsec = ann$startsec[i], endsec = ann$endsec[i],
					content = ann$content[i],
					main_tier = if (!is.na(context_row)) ann$tierName[context_row] else NA_character_,
					main_content = if (!is.na(context_row)) ann$content[context_row] else NA_character_,
					main_startsec = if (!is.na(context_row)) ann$startsec[context_row] else NA_real_,
					main_endsec = if (!is.na(context_row)) ann$endsec[context_row] else NA_real_
				)
			} else if (!is.na(target_col) && isTRUE(ann$wrap[i]) &&
			           is.finite(text_body_width) &&
			           target_col > text_body_width) {
				warnings[[length(warnings) + 1]] <- list(
					kind = "mixed_wrap", row = i, tier = ann$tierName[i],
					char = sym$char, occurrence = sym$occurrence,
					target_col = target_col,
					startsec = ann$startsec[i], endsec = ann$endsec[i],
					content = ann$content[i],
					main_tier = ann$tierName[source_row],
					main_content = ann$content[source_row],
					main_startsec = ann$startsec[source_row],
					main_endsec = ann$endsec[source_row]
				)
				target_col <- NA_integer_
				target_line <- NA_integer_
			}
			sym_type <- if (identical(align_mode, "point")) "point" else sym$type
			fill_before <- if (sym_type %in% c("close", "both")) {
				detect_phase_fill(sym$stretch_before, pair_fill)
			} else {
				" "
			}
			anchor_rows[[length(anchor_rows) + 1]] <- data.frame(
				char = sym$char, occurrence = sym$occurrence,
				target_col = target_col, target_line = target_line,
				fill_before = fill_before,
				type = sym_type,
				source_row = if (is.na(source_row)) context_row else source_row,
				domain = "multimodal", pair_fill = pair_fill
			)
		}
	}

	anchors <- if (length(anchor_rows) > 0) {
		do.call(rbind, anchor_rows)
	} else {
		data.frame(char = character(0), occurrence = integer(0),
		           target_col = integer(0), target_line = integer(0),
		           fill_before = character(0),
		           type = character(0), source_row = integer(0),
		           domain = character(0), pair_fill = character(0))
	}
	list(anchors = anchors, warnings = warnings)
}

# ===== TIME-BASED MULTIMODAL SYMBOL MATCHING =====
# Layer symbols may find their counterparts in ANY main annotation of the
# same speaker (symbols of one gesture are distributed across annotation
# boundaries - e.g. mounted behind a pause). Every symbol gets an
# interpolated time; per character, layer and pooled main symbol sequences
# are aligned monotonically (DP with skip penalty). Consumption is global:
# each main symbol matches at most one layer symbol. (User decision A,
# 2026-08-10, calibrated against 702_001.)
# Handover exception (user GO 2026-08-10): a main symbol consumed by the
# TRAILING symbol block of a layer annotation stays available for a layer
# annotation of the SAME tier that starts seamlessly where the consuming
# annotation ends - the boundary symbol is written once in the main but
# serves both layer sides (close of one action, open of the next).

compute_mm_symbol_matches <- function(ann, ref_main) {
	empty <- data.frame(layer_row = integer(0), char = character(0),
	                    layer_occurrence = integer(0),
	                    main_row = integer(0), main_occurrence = integer(0))
	skip_penalty <- 1.5
	handover_tolerance <- 0.02
	main_rows_all <- which(ann$is_main)
	if (length(main_rows_all) == 0) return(empty)

	# Per-fragment times when the row was concatenated (score mode), plain
	# whole-row interpolation otherwise. The table also flags the trailing
	# symbol block per fragment, which drives the handover rule.
	symbol_times <- function(row, char_vector) {
		table_row <- if (!is.null(ann$symbol_times_table)) {
			ann$symbol_times_table[[row]]
		} else {
			NULL
		}
		if (!is.null(table_row)) {
			hits <- table_row[table_row$char %in% char_vector, , drop = FALSE]
			if (nrow(hits) == 0) return(NULL)
			return(data.frame(char = hits$char, occurrence = hits$occurrence,
			                  time = hits$time, index = hits$index,
			                  trailing = hits$trailing_candidate))
		}
		graphemes <- split_graphemes(ann$content[row])
		hits <- which(graphemes %in% char_vector)
		if (length(hits) == 0) return(NULL)
		occurrence <- stats::ave(seq_along(hits), graphemes[hits],
		                         FUN = seq_along)
		duration <- max(ann$endsec[row] - ann$startsec[row], 0)
		times <- ann$startsec[row] +
			duration * (hits - 1L) / max(length(graphemes) - 1L, 1L)
		word_positions <- which(!(graphemes %in% char_vector) &
		                        !stringr::str_detect(graphemes, "^\\s$"))
		trailing_from <- if (length(word_positions) == 0) {
			1L
		} else {
			max(word_positions) + 1L
		}
		data.frame(char = graphemes[hits], occurrence = occurrence,
		           time = times, index = hits,
		           trailing = hits >= trailing_from)
	}

	used_main <- character(0)
	handover <- list()
	rows_list <- list()

	for (i in seq_len(nrow(ann))) {
		chars <- ann$align_chars[i]
		if (is.na(chars) || nchar(chars) == 0 || is.na(ann$align_mode[i])) next
		char_vector <- strsplit(chars, "")[[1]]
		layer_symbols <- symbol_times(i, char_vector)
		if (is.null(layer_symbols)) next

		arrow_lead_row <- stringr::str_detect(
			stringr::str_trim(ann$content[i]), "^[-=]*>+")
		row_penalty <- if (arrow_lead_row) 10 else skip_penalty

		main_symbols <- NULL
		for (m in main_rows_all) {
			ms <- symbol_times(m, char_vector)
			if (!is.null(ms)) {
				ms$row <- m
				main_symbols <- rbind(main_symbols, ms)
			}
		}
		if (is.null(main_symbols)) next

		for (symbol_char in unique(layer_symbols$char)) {
			layer_seq <- layer_symbols[layer_symbols$char == symbol_char, , drop = FALSE]
			# Within one row, text order IS chronological order - the
			# interpolated times only carry noise at fragment seams, so
			# sorting by time could cross two near-simultaneous marks.
			layer_seq <- layer_seq[order(layer_seq$index), , drop = FALSE]
			main_seq <- main_symbols[main_symbols$char == symbol_char, , drop = FALSE]
			main_keys <- paste(main_seq$row, symbol_char, main_seq$occurrence)
			consumed <- main_keys %in% used_main
			shareable <- vapply(main_keys, function(key) {
				info <- handover[[key]]
				!is.null(info) && info$trailing &&
					ann$tierName[info$consumer_row] == ann$tierName[i] &&
					abs(ann$endsec[info$consumer_row] - ann$startsec[i]) <= handover_tolerance
			}, logical(1), USE.NAMES = FALSE)
			main_seq <- main_seq[!consumed | shareable, , drop = FALSE]
			if (nrow(main_seq) == 0) next
			# Same guard for the main side: per row, lift time inversions
			# (fragment-seam interpolation noise) up to text order before
			# sorting, and break time ties by text position.
			main_seq <- main_seq[order(main_seq$row, main_seq$index), , drop = FALSE]
			main_seq$time <- stats::ave(main_seq$time, main_seq$row,
			                            FUN = cummax)
			main_seq <- main_seq[order(main_seq$time, main_seq$row,
			                           main_seq$index), , drop = FALSE]

			assignment <- monotone_match(layer_seq$time, main_seq$time, row_penalty)
			for (k in seq_along(assignment)) {
				if (is.na(assignment[k])) next
				m_idx <- assignment[k]
				rows_list[[length(rows_list) + 1]] <- data.frame(
					layer_row = i, char = symbol_char,
					layer_occurrence = layer_seq$occurrence[k],
					main_row = main_seq$row[m_idx],
					main_occurrence = main_seq$occurrence[m_idx])
				key <- paste(main_seq$row[m_idx], symbol_char,
				             main_seq$occurrence[m_idx])
				if (key %in% used_main) {
					handover[[key]] <- NULL
				} else {
					used_main <- c(used_main, key)
					handover[[key]] <- list(
						consumer_row = i,
						trailing = isTRUE(layer_seq$trailing[k]))
				}
			}
		}
	}
	if (length(rows_list) == 0) return(empty)
	do.call(rbind, rows_list)
}

monotone_match <- function(layer_times, main_times, penalty) {
	n <- length(layer_times)
	k <- length(main_times)
	cost <- matrix(0, n + 1L, k + 1L)
	for (a in seq_len(n)) cost[a + 1L, 1L] <- a * penalty
	for (a in seq_len(n)) {
		for (b in seq_len(k)) {
			cost[a + 1L, b + 1L] <- min(
				cost[a, b] + abs(layer_times[a] - main_times[b]),
				cost[a, b + 1L] + penalty,
				cost[a + 1L, b])
		}
	}
	assignment <- rep(NA_integer_, n)
	a <- n
	b <- k
	while (a > 0 && b > 0) {
		current <- cost[a + 1L, b + 1L]
		if (abs(current - (cost[a, b] + abs(layer_times[a] - main_times[b]))) < 1e-9) {
			assignment[a] <- b
			a <- a - 1L
			b <- b - 1L
		} else if (abs(current - (cost[a, b + 1L] + penalty)) < 1e-9) {
			a <- a - 1L
		} else {
			b <- b - 1L
		}
	}
	assignment
}

# ===== LAYER SYMBOL SCAN + CLASSIFICATION =====
# Whitespace-context classification as in legacy, extended by type "both"
# for sandwiched symbols (close of current action AND open of the next).

scan_layer_symbols <- function(text, char_vector) {
	graphemes <- split_graphemes(text)
	is_anchor <- graphemes %in% char_vector
	occ_counter <- list()
	rows <- list()
	last_symbol_index <- 0L
	for (g_idx in seq_along(graphemes)) {
		g <- graphemes[g_idx]
		if (!is_anchor[g_idx]) next
		n <- occ_counter[[g]]
		if (is.null(n)) n <- 0L
		occ_counter[[g]] <- n + 1L

		left_identical <- g_idx > 1 && graphemes[g_idx - 1] == g
		right_identical <- g_idx < length(graphemes) && graphemes[g_idx + 1] == g

		if (left_identical && right_identical) {
			type <- "both"
		} else if (right_identical) {
			type <- "close"
		} else if (left_identical) {
			type <- "open"
		} else {
			left_index <- g_idx - 1L
			while (left_index >= 1 && is_anchor[left_index] &&
			       graphemes[left_index] != g) {
				left_index <- left_index - 1L
			}
			right_index <- g_idx + 1L
			while (right_index <= length(graphemes) && is_anchor[right_index] &&
			       graphemes[right_index] != g) {
				right_index <- right_index + 1L
			}
			before <- if (left_index >= 1) graphemes[left_index] else " "
			after  <- if (right_index <= length(graphemes)) graphemes[right_index] else " "
			before_is_space <- stringr::str_detect(before, "^\\s$")
			after_is_space  <- stringr::str_detect(after, "^\\s$")

			type <- if (before_is_space && !after_is_space) {
				"open"
			} else if (!before_is_space && after_is_space) {
				"close"
			} else if (!before_is_space && !after_is_space) {
				"both"
			} else {
				"open"
			}
		}

		stretch_start <- last_symbol_index + 1L
		stretch_before <- if (g_idx > stretch_start) {
			paste(graphemes[stretch_start:(g_idx - 1L)], collapse = "")
		} else {
			""
		}
		rows[[length(rows) + 1]] <- data.frame(
			char = g, occurrence = n + 1L, type = type,
			stretch_before = stretch_before, index = g_idx
		)
		last_symbol_index <- g_idx
	}
	if (length(rows) == 0) {
		return(data.frame(char = character(0), occurrence = integer(0),
		                  type = character(0), stretch_before = character(0),
		                  index = integer(0)))
	}
	do.call(rbind, rows)
}

detect_phase_fill <- function(stretch_before, default_filler) {
	if (is.na(stretch_before) || nchar(stretch_before) == 0) {
		return(default_filler)
	}
	arrow_match <- stringr::str_match(stretch_before, "([-=])\\1*>+$")
	if (!is.na(arrow_match[1, 1])) return(arrow_match[1, 2])
	phase_match <- stringr::str_match(stretch_before, "([.,-])\\1*$")
	if (!is.na(phase_match[1, 1])) return(phase_match[1, 2])
	default_filler
}

# ======================================================================
# ==== source module: compute_bracket_pairs.R ====

# ===== VERBAL BRACKET PAIRING (time-interpolated) =====
# Every opening bracket gets an interpolated time: annotation start +
# duration * (grapheme position / annotation length). A follower annotation
# (first "[" within its first two graphemes) pairs its bracket(s) with the
# TEMPORALLY CLOSEST free bracket of an earlier annotation whose interval
# contains the follower's bracket time (end tolerance 0.15 s).
# Rules (calibrated against 1101_005_00_puente EAF + user reference):
# - a speaker never overlaps themselves (same-tier candidates skipped)
# - brackets consumed as follower anchors (j-side) are never targets
# - an origin bracket already claimed by a follower may only be shared by
#   followers whose bracket time lies within 0.2 s of the first claim
# - double-bracket starts ("[[") pair occurrence 1 AND 2 separately, with
#   distinct targets
# - follow-up brackets of single-bracket followers are chained sequentially

compute_bracket_pairs <- function(ann) {
	pairs <- data.frame(
		i_row        = integer(0),
		i_occurrence = integer(0),
		j_row        = integer(0),
		j_occurrence = integer(0)
	)
	n <- nrow(ann)
	if (n == 0) return(pairs)

	end_tolerance <- 0.15
	share_tolerance <- 0.2

	bracket_info <- vector("list", n)
	open_counts <- integer(n)
	for (r in seq_len(n)) {
		if (!isTRUE(ann$is_main[r]) || is.na(ann$content[r])) next
		tbl <- if (!is.null(ann$symbol_times_table)) ann$symbol_times_table[[r]] else NULL
		if (!is.null(tbl)) {
			tbl <- tbl[tbl$char == "[", , drop = FALSE]
			if (nrow(tbl) == 0) next
			tbl <- tbl[order(tbl$occurrence), , drop = FALSE]
			bracket_info[[r]] <- data.frame(
				occurrence = tbl$occurrence, time = tbl$time,
				fragment_start = tbl$fragment_start,
				fragment_end = tbl$fragment_end,
				fragment_index = tbl$fragment_index)
			open_counts[r] <- nrow(tbl)
		} else {
			graphemes <- split_graphemes(ann$content[r])
			positions <- which(graphemes == "[")
			open_counts[r] <- length(positions)
			if (length(positions) == 0) next
			duration <- max(ann$endsec[r] - ann$startsec[r], 0)
			times <- ann$startsec[r] +
				duration * (positions - 1L) / max(length(graphemes) - 1L, 1L)
			bracket_info[[r]] <- data.frame(
				occurrence = seq_along(positions), time = times,
				fragment_start = ann$startsec[r],
				fragment_end = ann$endsec[r],
				fragment_index = positions)
		}
	}

	main_rows <- which(ann$is_main & open_counts > 0)
	if (length(main_rows) < 2) return(pairs)

	j_consumed <- lapply(open_counts, function(k) rep(FALSE, k))
	claim_time <- lapply(open_counts, function(k) rep(NA_real_, k))

	add_pair <- function(i, x, j, occ, t_value) {
		pairs <<- rbind(pairs, data.frame(
			i_row = i, i_occurrence = x, j_row = j, j_occurrence = occ))
		j_consumed[[j]][occ] <<- TRUE
		if (is.na(claim_time[[i]][x])) claim_time[[i]][x] <<- t_value
	}

	sim_tolerance <- 0.1

	find_best <- function(j, j_unit_start, bracket_time_j, used_targets,
	                      allow_consumed, simultaneous_only) {
		best <- NULL
		best_score <- Inf
		for (i in main_rows) {
			if (i == j) next
			if (identical(ann$tierName[i], ann$tierName[j])) next
			info_i <- bracket_info[[i]]
			for (x in seq_len(nrow(info_i))) {
				fx_start <- info_i$fragment_start[x]
				fx_end   <- info_i$fragment_end[x]
				earlier <- fx_start < j_unit_start ||
					(fx_start == j_unit_start && i < j)
				if (!earlier) next
				start_diff <- abs(fx_start - j_unit_start)
				if (simultaneous_only && start_diff > sim_tolerance) next
				if (!simultaneous_only && allow_consumed &&
				    start_diff <= sim_tolerance) next
				if (bracket_time_j < fx_start - 1e-9) next
				if (bracket_time_j >= fx_end + end_tolerance) next
				if (!allow_consumed && j_consumed[[i]][x]) next
				if (paste(i, x) %in% used_targets) next
				if (!is.na(claim_time[[i]][x]) &&
				    abs(bracket_time_j - claim_time[[i]][x]) > share_tolerance) next
				score <- abs(info_i$time[x] - bracket_time_j)
				if (score < best_score - 1e-9) {
					best_score <- score
					best <- c(i, x)
				}
			}
		}
		best
	}

	# Follower units: one per FRAGMENT whose leading "[" sits within the
	# fragment's first two graphemes. On unconcatenated rows this is exactly
	# the old per-row follower detection (fragment == annotation).
	units <- list()
	for (j in main_rows) {
		info <- bracket_info[[j]]
		for (fragment_start in unique(info$fragment_start)) {
			sel <- which(info$fragment_start == fragment_start)
			sel <- sel[order(info$fragment_index[sel])]
			if (info$fragment_index[sel[1]] > 2L) next
			units[[length(units) + 1]] <- list(
				j = j, start = fragment_start,
				occ_sequence = info$occurrence[sel],
				bracket_times = info$time[sel],
				double = length(sel) >= 2L &&
					info$fragment_index[sel[2]] == info$fragment_index[sel[1]] + 1L)
		}
	}
	if (length(units) == 0) return(pairs)
	unit_order <- order(vapply(units, function(u) u$start, numeric(1)),
	                    vapply(units, function(u) u$j, numeric(1)))

	for (u in units[unit_order]) {
		j <- u$j
		if (u$double) {
			used_targets <- character(0)
			inner_time <- u$bracket_times[2]
			best_inner <- find_best(j, u$start, inner_time, used_targets,
			                        allow_consumed = TRUE,
			                        simultaneous_only = FALSE)
			if (!is.null(best_inner)) {
				add_pair(best_inner[1], best_inner[2], j, u$occ_sequence[2],
				         inner_time)
				used_targets <- c(used_targets, paste(best_inner[1], best_inner[2]))
			}
			outer_time <- u$bracket_times[1]
			best_outer <- find_best(j, u$start, outer_time, used_targets,
			                        allow_consumed = FALSE,
			                        simultaneous_only = TRUE)
			if (!is.null(best_outer)) {
				add_pair(best_outer[1], best_outer[2], j, u$occ_sequence[1],
				         outer_time)
			}
		} else {
			bracket_time_j <- u$bracket_times[1]
			best <- find_best(j, u$start, bracket_time_j, character(0),
			                  allow_consumed = FALSE,
			                  simultaneous_only = FALSE)
			if (is.null(best)) next
			add_pair(best[1], best[2], j, u$occ_sequence[1], bracket_time_j)
			m <- 1L
			while ((best[2] + m) <= open_counts[best[1]] &&
			       (1L + m) <= length(u$occ_sequence) &&
			       !j_consumed[[j]][u$occ_sequence[1L + m]]) {
				add_pair(best[1], best[2] + m, j, u$occ_sequence[1L + m],
				         bracket_time_j)
				m <- m + 1L
			}
		}
	}
	pairs
}

# ======================================================================
# ==== source module: bracket_interna_padding.R ====

# ===== BRACKET INTERNA PADDING (verbal only) =====
# Pass 1 (global, before rendering): pads paired [...] segments to equal
# length so their closing brackets can end up in the same column.
# Insert-position rules ported from legacy act:::.align_brackets().

apply_bracket_interna_padding <- function(ann, pairs) {
	ann$text <- ann$content
	if (nrow(pairs) == 0) return(ann)

	nested_outer <- function(row, occurrence) {
		if (occurrence != 1L) return(FALSE)
		graphemes <- split_graphemes(ann$content[row])
		positions <- which(graphemes == "[")
		length(positions) >= 2L && positions[1] <= 2L &&
			positions[2] == positions[1] + 1L
	}

	for (p in seq_len(nrow(pairs))) {
		i <- pairs$i_row[p]
		j <- pairs$j_row[p]
		if (nested_outer(i, pairs$i_occurrence[p]) ||
		    nested_outer(j, pairs$j_occurrence[p])) next

		seg_i <- extract_bracket_segment(ann$text[i], pairs$i_occurrence[p])
		seg_j <- extract_bracket_segment(ann$text[j], pairs$j_occurrence[p])
		if (is.null(seg_i) || is.null(seg_j)) next

		difference <- seg_j$length - seg_i$length
		if (difference > 0) {
			ann$text[i] <- pad_bracket_segment(ann$text[i], seg_i, difference,
			                                   ann$filler_inside[i])
		} else if (difference < 0) {
			ann$text[j] <- pad_bracket_segment(ann$text[j], seg_j, abs(difference),
			                                   ann$filler_inside[j])
		}
	}
	ann
}

extract_bracket_segment <- function(text, occurrence) {
	if (is.na(text)) return(NULL)
	open_positions <- stringr::str_locate_all(text, stringr::fixed("["))[[1]][, "start"]
	if (length(open_positions) < occurrence) return(NULL)
	start_position <- open_positions[occurrence]
	rest <- substr(text, start_position + 1L, nchar(text))
	close_offset <- stringr::str_locate(rest, stringr::fixed("]"))[1, "start"]
	if (is.na(close_offset)) return(NULL)
	end_position <- start_position + close_offset
	list(
		start   = start_position,
		end     = end_position,
		content = substr(text, start_position, end_position),
		length  = end_position - start_position + 1L,
		before  = stringr::str_trim(substr(text, start_position - 1L, start_position - 1L)),
		after   = stringr::str_trim(substr(text, end_position + 1L, end_position + 1L))
	)
}

pad_bracket_segment <- function(text, segment, difference, filler_inside) {
	insert_char <- detect_bracket_filler_new(segment$content, filler_inside,
	                                         phase_chars = FALSE)
	inner <- substr(segment$content, 2L, nchar(segment$content) - 1L)
	space_inside <- stringi::stri_locate_last(inner, regex = " ")[1, "start"]

	if (segment$after == "" || stringr::str_detect(segment$after, "\\W")) {
		insert_position <- segment$end - 1L
	} else if (!is.na(space_inside)) {
		insert_position <- segment$start + space_inside
	} else if (segment$before == "" || stringr::str_detect(segment$before, "\\W")) {
		insert_position <- segment$start
	} else {
		insert_position <- segment$end - 1L
		insert_char <- "_"
	}

	paste0(
		substr(text, 1L, insert_position),
		strrep(insert_char, difference),
		substr(text, insert_position + 1L, nchar(text))
	)
}

# ===== DOUBLE BRACKET FLATTENING =====
# "[[" marks two overlaps starting simultaneously. When the INNER bracket's
# partner lies temporally (and thus visually) LEFT of the OUTER bracket's
# partner, the nesting is flattened into two sequential segments:
# "[[A] rest]" -> "[A][rest]" (the outer "[" moves behind the inner "]").
# Pair occurrences of the row are swapped accordingly. (User rule 2026-08-10.)

apply_double_bracket_flattening <- function(ann, pairs) {
	if (nrow(pairs) == 0) return(list(ann = ann, pairs = pairs))

	bracket_time <- function(row, occurrence) {
		graphemes <- split_graphemes(ann$content[row])
		positions <- which(graphemes == "[")
		if (occurrence > length(positions)) return(NA_real_)
		duration <- max(ann$endsec[row] - ann$startsec[row], 0)
		ann$startsec[row] +
			duration * (positions[occurrence] - 1L) / max(length(graphemes) - 1L, 1L)
	}

	for (j in unique(pairs$j_row)) {
		row_pairs <- pairs[pairs$j_row == j, , drop = FALSE]
		if (!(1L %in% row_pairs$j_occurrence && 2L %in% row_pairs$j_occurrence)) next

		graphemes <- split_graphemes(ann$content[j])
		positions <- which(graphemes == "[")
		if (length(positions) < 2 || positions[1] > 2L ||
		    positions[2] != positions[1] + 1L) next

		outer_pair <- row_pairs[row_pairs$j_occurrence == 1L, ][1, ]
		inner_pair <- row_pairs[row_pairs$j_occurrence == 2L, ][1, ]
		outer_target_time <- bracket_time(outer_pair$i_row, outer_pair$i_occurrence)
		inner_target_time <- bracket_time(inner_pair$i_row, inner_pair$i_occurrence)
		if (is.na(outer_target_time) || is.na(inner_target_time)) next
		if (inner_target_time >= outer_target_time) next

		close_positions <- which(graphemes == "]")
		inner_close <- close_positions[close_positions > positions[2]][1]
		if (is.na(inner_close)) next

		before <- if (positions[1] > 1) {
			paste(graphemes[1:(positions[1] - 1L)], collapse = "")
		} else {
			""
		}
		inner_segment <- paste(graphemes[positions[2]:inner_close], collapse = "")
		rest <- if (inner_close < length(graphemes)) {
			paste(graphemes[(inner_close + 1L):length(graphemes)], collapse = "")
		} else {
			""
		}
		rest <- sub("^\\s+", "", rest)
		ann$content[j] <- paste0(before, inner_segment, "[", rest)

		swap <- pairs$j_row == j & pairs$j_occurrence %in% c(1L, 2L)
		pairs$j_occurrence[swap] <- ifelse(pairs$j_occurrence[swap] == 1L, 2L, 1L)
	}
	list(ann = ann, pairs = pairs)
}

# ===== SYMBOL MERGE (identical anchors in one cluster -> one) =====
# Markers have no temporal extension, so identical anchor symbols within
# ONE uninterrupted run of anchor characters denote the same point in time
# and collapse into a single symbol - even when other marker types stand
# between them ("|#|" -> "|#", "|#|#" -> "|#"). The first occurrence wins,
# so the order of the different marker types is preserved. A space or any
# text ends the cluster, because that means real temporal distance.
# Purely textual rule (user decisions 2026-08-10 / 2026-08-13). The
# occurrence remap lets several matched anchors target the merged symbol.

apply_symbol_merge <- function(ann, anchor_char_set, mm_matches = NULL,
                               time_tolerance = 0.5,
                               point_chars = character(0),
                               time_tolerance_point = 0.2,
                               no_fold = NULL) {
	merge_map <- vector("list", nrow(ann))
	events <- list()
	for (r in seq_len(nrow(ann))) {
		if (is.na(ann$content[r])) next
		graphemes <- split_graphemes(ann$content[r])
		if (length(graphemes) == 0) next
		keep <- rep(TRUE, length(graphemes))
		occ_original <- list()
		occ_merged <- list()
		map_row <- list()
		cluster <- list()
		cluster_first <- list()
		for (g_idx in seq_along(graphemes)) {
			g <- graphemes[g_idx]
			if (g %in% anchor_char_set) {
				original <- if (is.null(occ_original[[g]])) 1L else occ_original[[g]] + 1L
				occ_original[[g]] <- original
				in_cluster <- !is.null(cluster[[g]])
				tol_g <- if (g %in% point_chars) time_tolerance_point else time_tolerance
				vetoed <- !is.null(no_fold) && nrow(no_fold) > 0 &&
					any(no_fold$row == r & no_fold$char == g &
					    no_fold$occurrence == original)
				mergeable <- in_cluster && !vetoed &&
					.symbols_time_equivalent(ann, mm_matches, r, g,
					                         cluster_first[[g]], original,
					                         tol_g)
				if (in_cluster) {
					events[[length(events) + 1]] <- data.frame(
						row = r, char = g,
						occurrence_first = cluster_first[[g]],
						occurrence_second = original,
						merged = mergeable,
						vetoed = vetoed,
						time_first = .layer_time_for_main(ann, mm_matches, r, g,
						                                  cluster_first[[g]]),
						time_second = .layer_time_for_main(ann, mm_matches, r, g,
						                                   original),
						stringsAsFactors = FALSE)
				}
				if (mergeable) {
					keep[g_idx] <- FALSE
					merged <- cluster[[g]]
				} else {
					merged <- if (is.null(occ_merged[[g]])) 1L else occ_merged[[g]] + 1L
					occ_merged[[g]] <- merged
					cluster[[g]] <- merged
					cluster_first[[g]] <- original
				}
				if (is.null(map_row[[g]])) map_row[[g]] <- integer(0)
				map_row[[g]][original] <- merged
			} else if (g == "=") {
				# Latching means "no gap": a "=" between two marks does not
				# end the cluster, so "|=|" folds to one "|" under the
				# same time guard (user comments 207_021 K0/K2,
				# 2026-08-17).
			} else {
				cluster <- list()
				cluster_first <- list()
			}
		}
		if (any(!keep)) {
			ann$content[r] <- paste(graphemes[keep], collapse = "")
		}
		merge_map[[r]] <- map_row
	}
	list(ann = ann, map = merge_map,
	     events = if (length(events) > 0) do.call(rbind, events) else NULL)
}

# Unfold at break: a fold is undone when the closing description of the
# merged mark runs past a line break - the closing half then belongs on
# the line where its description ends, the opening half on the next line
# (user mock-up 207_024, 2026-08-17). This detector compares, per merged
# event, the rendered line of the merged mark with the line of the SAME
# annotation's opening mark; a later line means the fold crossed a break.

.folds_across_break <- function(ann, merge_events, mm_matches, merge_map,
                                preliminary, anchor_char_set = character(0)) {
	empty <- data.frame(row = integer(0), char = character(0),
	                    occurrence = integer(0), stringsAsFactors = FALSE)
	if (is.null(merge_events) || is.null(mm_matches) ||
	    nrow(mm_matches) == 0) return(empty)
	merged_events <- merge_events[merge_events$merged, , drop = FALSE]
	out <- list()
	for (k in seq_len(nrow(merged_events))) {
		ev <- merged_events[k, , drop = FALSE]
		r <- ev$row
		g <- ev$char
		if (!isTRUE(ann$is_main[r])) {
			# Layer-row fold (a shared seam mark): crossed a break when
			# the two matched main marks render on different lines.
			seam <- mm_matches[mm_matches$layer_row == r &
			                   mm_matches$char == g &
			                   mm_matches$layer_occurrence %in%
			                   	c(ev$occurrence_first,
			                   	  ev$occurrence_second), , drop = FALSE]
			if (nrow(seam) < 2) next
			lines_hit <- rep(NA_integer_, 2)
			rows_hit <- rep(NA_integer_, 2)
			for (m in 1:2) {
				main_row <- seam$main_row[m]
				cache_m <- preliminary[[main_row]]
				if (is.null(cache_m)) break
				occ_m <- remap_symbol_occurrence(merge_map, main_row, g,
				                                 seam$main_occurrence[m])
				pos <- cache_m$positions[cache_m$positions$char == g &
				                         cache_m$positions$occurrence == occ_m, ,
				                         drop = FALSE]
				if (nrow(pos) == 0) break
				lines_hit[m] <- pos$line[1]
				rows_hit[m] <- main_row
			}
			if (anyNA(lines_hit)) next
			if (rows_hit[1] != rows_hit[2] || lines_hit[1] != lines_hit[2]) {
				out[[length(out) + 1]] <- data.frame(
					row = r, char = g,
					occurrence = ev$occurrence_second,
					stringsAsFactors = FALSE)
			}
			next
		}
		cache <- preliminary[[r]]
		if (is.null(cache)) next
		merged_occ <- remap_symbol_occurrence(merge_map, r, g,
		                                      ev$occurrence_first)
		hit <- cache$positions[cache$positions$char == g &
		                       cache$positions$occurrence == merged_occ, ,
		                       drop = FALSE]
		if (nrow(hit) == 0) next
		# A word-internal fold stays folded: unfolding would put the hard
		# break inside the word and tear it apart ("l%eVA:Nta" ->
		# "l%" / "%eVA:Nta"). Only clusters with a word boundary on at
		# least one side may unfold (user mock-up 207_024, 2026-08-17).
		graphemes_r <- split_graphemes(ann$text[r])
		merged_index <- .symbol_index_in_text(graphemes_r, g, merged_occ)
		if (!is.na(merged_index)) {
			cluster_chars <- c(anchor_char_set, "=")
			left_index <- merged_index
			while (left_index > 1L &&
			       graphemes_r[left_index - 1L] %in% cluster_chars) {
				left_index <- left_index - 1L
			}
			right_index <- merged_index
			while (right_index < length(graphemes_r) &&
			       graphemes_r[right_index + 1L] %in% cluster_chars) {
				right_index <- right_index + 1L
			}
			left_grapheme <- if (left_index > 1L) {
				graphemes_r[left_index - 1L]
			} else {
				" "
			}
			right_grapheme <- if (right_index < length(graphemes_r)) {
				graphemes_r[right_index + 1L]
			} else {
				" "
			}
			word_left <- stringr::str_detect(left_grapheme,
			                                 "^[\\p{L}\\p{N}]$")
			word_right <- stringr::str_detect(right_grapheme,
			                                  "^[\\p{L}\\p{N}]$")
			if (word_left && word_right) next
		}
		# The closing half may sit on EITHER of the two folded
		# occurrences - the matcher assigns them by time, not by order.
		candidates <- mm_matches[mm_matches$main_row == r &
		                         mm_matches$char == g &
		                         mm_matches$main_occurrence %in%
		                         	c(ev$occurrence_first,
		                         	  ev$occurrence_second), , drop = FALSE]
		for (cm in seq_len(nrow(candidates))) {
			layer_row <- candidates$layer_row[cm]
			layer_occ <- candidates$layer_occurrence[cm]
			if (layer_occ <= 1L) next
			open_match <- mm_matches[mm_matches$layer_row == layer_row &
			                         mm_matches$char == g &
			                         mm_matches$layer_occurrence ==
			                         	layer_occ - 1L, , drop = FALSE]
			if (nrow(open_match) == 0 || open_match$main_row[1] != r) next
			open_occ <- remap_symbol_occurrence(merge_map, r, g,
			                                    open_match$main_occurrence[1])
			open_hit <- cache$positions[cache$positions$char == g &
			                            cache$positions$occurrence == open_occ, ,
			                            drop = FALSE]
			if (nrow(open_hit) == 0) next
			if (hit$line[1] > open_hit$line[1]) {
				out[[length(out) + 1]] <- data.frame(
					row = r, char = g,
					occurrence = ev$occurrence_second,
					stringsAsFactors = FALSE)
				break
			}
		}
	}
	if (length(out) == 0) return(empty)
	do.call(rbind, out)
}

# Two layer symbols that were matched to the SAME main symbol denote the
# same point in time. When only whitespace separates them, that whitespace
# is dropped so they become textually adjacent - the cluster merge then
# folds them into one symbol (user rule 2026-08-13).

collapse_equivalent_layer_gaps <- function(ann, mm_matches, anchor_char_set,
                                           time_tolerance = 0.5,
                                           point_chars = character(0),
                                           time_tolerance_point = 0.2) {
	if (nrow(mm_matches) == 0) return(ann)
	for (i in seq_len(nrow(ann))) {
		chars <- ann$align_chars[i]
		if (is.na(chars) || nchar(chars) == 0 || is.na(ann$align_mode[i])) next
		char_vector <- strsplit(chars, "")[[1]]
		my_matches <- mm_matches[mm_matches$layer_row == i, , drop = FALSE]
		if (nrow(my_matches) < 2) next
		graphemes <- split_graphemes(ann$content[i])
		positions <- symbol_positions_in_text(ann$content[i], char_vector)
		if (nrow(positions) < 2) next
		positions <- positions[order(positions$index), , drop = FALSE]

		match_of <- function(char, occurrence) {
			hit <- my_matches[my_matches$char == char &
			                  my_matches$layer_occurrence == occurrence, , drop = FALSE]
			if (nrow(hit) == 0) return(NULL)
			hit[1, , drop = FALSE]
		}
		# Same point in time = same marker cluster in the main AND the two
		# main symbols actually pass the merge guard.
		same_point <- function(char, occurrence_a, occurrence_b) {
			hit_a <- match_of(char, occurrence_a)
			hit_b <- match_of(char, occurrence_b)
			if (is.null(hit_a) || is.null(hit_b)) return(FALSE)
			if (hit_a$main_row[1] != hit_b$main_row[1]) return(FALSE)
			main_row <- hit_a$main_row[1]
			cluster_a <- .symbol_cluster_start(ann$content[main_row],
			                                   anchor_char_set, char,
			                                   hit_a$main_occurrence[1])
			cluster_b <- .symbol_cluster_start(ann$content[main_row],
			                                   anchor_char_set, char,
			                                   hit_b$main_occurrence[1])
			if (is.na(cluster_a) || is.na(cluster_b) || cluster_a != cluster_b) {
				return(FALSE)
			}
			.symbols_time_equivalent(ann, mm_matches, main_row, char,
			                         hit_a$main_occurrence[1],
			                         hit_b$main_occurrence[1],
			                         if (char %in% point_chars)
			                         	time_tolerance_point else time_tolerance)
		}

		drop <- integer(0)
		for (k in seq_len(nrow(positions) - 1L)) {
			if (positions$char[k] != positions$char[k + 1L]) next
			between <- seq_len(0)
			if (positions$index[k + 1L] > positions$index[k] + 1L) {
				between <- (positions$index[k] + 1L):(positions$index[k + 1L] - 1L)
			}
			if (length(between) == 0) next
			if (!all(stringr::str_detect(graphemes[between], "^\\s$"))) next
			if (!same_point(positions$char[k], positions$occurrence[k],
			                positions$occurrence[k + 1L])) next
			drop <- c(drop, between)
		}
		if (length(drop) > 0) {
			ann$content[i] <- paste(graphemes[-drop], collapse = "")
		}
	}
	ann
}

remap_symbol_occurrence <- function(merge_map, row, char, occurrence) {
	if (is.null(merge_map)) return(occurrence)
	map_row <- merge_map[[row]]
	if (is.null(map_row) || is.null(map_row[[char]])) return(occurrence)
	if (is.na(occurrence) || occurrence > length(map_row[[char]])) return(occurrence)
	map_row[[char]][occurrence]
}

# Guard for the cluster merge (user rule 2026-08-13): two identical main
# symbols may only be folded into one when the non-verbal annotations that
# reference them denote the SAME point in time. Two stills marks that carry
# two different pictures, or two gesture boundaries with a real gap between
# them, keep their own mark. Without matches on either side the purely
# textual rule applies (adjacent identical symbols = same point).

.symbols_time_equivalent <- function(ann, mm_matches, main_row, char,
                                     occurrence_a, occurrence_b, tolerance) {
	if (is.null(mm_matches) || nrow(mm_matches) == 0) return(TRUE)
	if (is.null(occurrence_a) || is.na(occurrence_a)) return(TRUE)
	time_of <- function(occurrence) {
		hit <- mm_matches[mm_matches$main_row == main_row &
		                  mm_matches$char == char &
		                  mm_matches$main_occurrence == occurrence, , drop = FALSE]
		if (nrow(hit) == 0) return(NA_real_)
		.layer_symbol_time(ann, hit$layer_row[1], char, hit$layer_occurrence[1])
	}
	time_a <- time_of(occurrence_a)
	time_b <- time_of(occurrence_b)
	if (is.na(time_a) || is.na(time_b)) return(TRUE)
	abs(time_a - time_b) <= tolerance
}

.layer_symbol_time <- function(ann, row, char, occurrence) {
	table_row <- if (!is.null(ann$symbol_times_table)) {
		ann$symbol_times_table[[row]]
	} else {
		NULL
	}
	if (!is.null(table_row)) {
		hit <- table_row[table_row$char == char &
		                 table_row$occurrence == occurrence, , drop = FALSE]
		if (nrow(hit) > 0) return(hit$time[1])
	}
	graphemes <- split_graphemes(ann$content[row])
	hits <- which(graphemes == char)
	if (length(hits) < occurrence) return(NA_real_)
	duration <- max(ann$endsec[row] - ann$startsec[row], 0)
	ann$startsec[row] +
		duration * (hits[occurrence] - 1L) / max(length(graphemes) - 1L, 1L)
}

.layer_time_for_main <- function(ann, mm_matches, main_row, char, occurrence) {
	if (is.null(mm_matches) || nrow(mm_matches) == 0) return(NA_real_)
	hit <- mm_matches[mm_matches$main_row == main_row &
	                  mm_matches$char == char &
	                  mm_matches$main_occurrence == occurrence, , drop = FALSE]
	if (nrow(hit) == 0) return(NA_real_)
	.layer_symbol_time(ann, hit$layer_row[1], char, hit$layer_occurrence[1])
}

.symbol_cluster_start <- function(text, char_vector, char, occurrence) {
	graphemes <- split_graphemes(text)
	seen <- 0L
	cluster_start <- NA_integer_
	for (g_idx in seq_along(graphemes)) {
		if (!(graphemes[g_idx] %in% char_vector)) {
			if (graphemes[g_idx] == "=") next
			cluster_start <- NA_integer_
			next
		}
		if (is.na(cluster_start)) cluster_start <- g_idx
		if (graphemes[g_idx] == char) {
			seen <- seen + 1L
			if (seen == occurrence) return(cluster_start)
		}
	}
	NA_integer_
}


# ===== CLUSTER CLOSE/OPEN REORDER (user rule 2026-08-17, 207_024 K0) =====
# When closing and opening marks of different gestures meet in ONE marker
# cluster, the CLOSING marks come first (each stretched later to the end
# of its description by the span stretch), the OPENING marks follow - so
# a line break inside the cluster falls between them and every
# description closes on the line it runs on. Marks are only reordered
# when their times lie within the fold tolerance (near-simultaneous).

apply_cluster_close_open_reorder <- function(ann, mm_matches, preliminary,
                                             all_anchor_chars,
                                             time_tolerance = 0.5,
                                             point_chars = character(0),
                                             time_tolerance_point = 0.2) {
	if (nrow(mm_matches) == 0) return(ann)
	for (main_row in unique(mm_matches$main_row)) {
		if (!isTRUE(ann$is_main[main_row])) next
		graphemes <- split_graphemes(ann$text[main_row])
		if (length(graphemes) == 0) next
		occ_counter <- list()
		marks <- list()
		cluster_id <- 0L
		in_cluster <- FALSE
		for (g_idx in seq_along(graphemes)) {
			g <- graphemes[g_idx]
			if (g %in% all_anchor_chars) {
				if (!in_cluster) {
					cluster_id <- cluster_id + 1L
					in_cluster <- TRUE
				}
				n <- occ_counter[[g]]
				if (is.null(n)) n <- 0L
				occ_counter[[g]] <- n + 1L
				marks[[length(marks) + 1]] <- list(
					char = g, occurrence = n + 1L, index = g_idx,
					cluster = cluster_id)
			} else if (g == "=") {
				# latch is transparent inside a cluster
			} else {
				in_cluster <- FALSE
			}
		}
		if (length(marks) < 2) next
		clusters <- split(marks, vapply(marks, function(m) m$cluster, integer(1)))
		changed <- FALSE
		for (cl in clusters) {
			if (length(cl) < 2) next
			info <- lapply(cl, function(m) {
				hit <- mm_matches[mm_matches$main_row == main_row &
				                  mm_matches$char == m$char &
				                  mm_matches$main_occurrence == m$occurrence, ,
				                  drop = FALSE]
				if (nrow(hit) == 0) return(NULL)
				layer_row <- hit$layer_row[1]
				layer_occ <- hit$layer_occurrence[1]
				lay_g <- split_graphemes(ann$content[layer_row])
				pos <- which(lay_g == m$char)
				if (layer_occ > length(pos)) return(NULL)
				at <- pos[layer_occ]
				# Role from the direct neighbours, so marks in the middle
				# of a concatenated layer row are classified as well: a
				# description follows directly = open, text directly in
				# front = close (user mock-up 207_024, 2026-08-17).
				follows <- if (at < length(lay_g)) lay_g[at + 1L] else " "
				precedes <- if (at > 1L) lay_g[at - 1L] else " "
				opens <- !stringr::str_detect(follows, "^[[:space:]]$") &&
					!(follows %in% all_anchor_chars)
				role <- if (opens) "open"
					else if (!stringr::str_detect(precedes, "^[[:space:]]$")) "close"
					else NA_character_
				if (is.na(role)) return(NULL)
				lay_time <- .layer_symbol_time(ann, layer_row, m$char, layer_occ)
				# estimated end column of the description: the open mark of
				# the SAME layer annotation in the preliminary layout plus
				# the description length
				desc_end <- NA_real_
				if (identical(role, "close") && layer_occ > 1L) {
					open_occ_layer <- layer_occ - 1L
					open_hit <- mm_matches[mm_matches$layer_row == layer_row &
					                       mm_matches$char == m$char &
					                       mm_matches$layer_occurrence == open_occ_layer, ,
					                       drop = FALSE]
					if (nrow(open_hit) > 0 && !is.null(preliminary)) {
						cache <- preliminary[[main_row]]
						if (!is.null(cache)) {
							p_hit <- cache$positions[
								cache$positions$char == m$char &
								cache$positions$occurrence ==
									open_hit$main_occurrence[1], , drop = FALSE]
							if (nrow(p_hit) > 0) {
								desc_end <- p_hit$col[1] +
									(pos[layer_occ] - pos[open_occ_layer])
							}
						}
					}
				}
				c(m, list(role = role, time = lay_time, desc_end = desc_end))
			})
			if (any(vapply(info, is.null, logical(1)))) next
			roles <- vapply(info, function(x) x$role, character(1))
			if (!any(roles == "close") || !any(roles == "open")) next
			times <- vapply(info, function(x) x$time, numeric(1))
			if (any(is.na(times))) next
			tol <- min(vapply(info, function(x) {
				if (x$char %in% point_chars) time_tolerance_point else time_tolerance
			}, numeric(1)))
			if (diff(range(times)) > tol) next
			ends <- vapply(info, function(x) {
				if (is.na(x$desc_end)) Inf else x$desc_end
			}, numeric(1))
			ord <- order(roles != "close", ifelse(roles == "close", ends, times))
			if (identical(ord, seq_along(info))) next
			idx <- vapply(info, function(x) x$index, integer(1))
			graphemes[sort(idx)] <- vapply(info[ord], function(x) x$char,
			                               character(1))
			changed <- TRUE
		}
		if (changed) {
			ann$text[main_row] <- paste(graphemes, collapse = "")
		}
	}
	ann
}

# ===== MULTIMODAL SPAN STRETCH (main widening) =====
# When the layer stretch between two consecutive symbols is LONGER than the
# corresponding span in the main text, the MAIN is widened: spaces are
# inserted before the symbol cluster containing the closing symbol.
# (User decision 2026-08-10, from the 208_001 review.)

apply_mm_span_stretch <- function(ann, mm_matches, all_anchor_chars,
                                  merge_map = NULL, preliminary = NULL,
                                  text_body_width = Inf) {
	if (nrow(mm_matches) == 0) return(ann)
	# Widening only helps when both symbols end up on the SAME rendered
	# line - across a line break the layer text has the full width anyway
	# and the inserted spaces would only tear the verbal line apart
	# (user rule 2026-08-14, measured in pass 1).
	# How many columns the layer really has between two symbols. On one
	# line that is simply the distance; across a line break it is the rest
	# of the first line PLUS the start of the next one up to the symbol -
	# so a description running over the break gets room as well
	# (user rule 2026-08-14).
	available_columns <- function(main_row, char_a, occurrence_a,
	                              char_b, occurrence_b, fallback,
	                              prefix_width) {
		if (is.null(preliminary) || !is.finite(text_body_width)) return(fallback)
		cache <- preliminary[[main_row]]
		if (is.null(cache) || nrow(cache$positions) == 0) return(fallback)
		positions <- cache$positions
		hit_a <- positions[positions$char == char_a &
		                   positions$occurrence == occurrence_a, , drop = FALSE]
		hit_b <- positions[positions$char == char_b &
		                   positions$occurrence == occurrence_b, , drop = FALSE]
		if (nrow(hit_a) == 0 || nrow(hit_b) == 0) return(fallback)
		if (hit_a$line[1] == hit_b$line[1]) {
			available <- hit_b$col[1] - hit_a$col[1]
			attr(available, "same_line") <- TRUE
			attr(available, "close_col") <- hit_b$col[1]
			return(available)
		}
		if (hit_b$line[1] < hit_a$line[1]) return(Inf)
		rest_of_first <- text_body_width - hit_a$col[1]
		start_of_last <- hit_b$col[1] - prefix_width - 1L
		# full lines in between carry the description as well
		full_lines <- max(0L, hit_b$line[1] - hit_a$line[1] - 1L) *
			max(0L, text_body_width - prefix_width)
		rest_of_first + full_lines + start_of_last
	}
	# ---- phase 1: collect every layer span requirement ----
	# Several layer rows may anchor into the SAME pair of main symbols; the
	# span has to hold the WIDEST description, and the shrink pass must not
	# go below that either (user comment K11, 2026-08-15).
	span_list <- list()
	for (i in seq_len(nrow(ann))) {
		chars <- ann$align_chars[i]
		if (is.na(chars) || nchar(chars) == 0 || is.na(ann$align_mode[i])) next
		char_vector <- strsplit(chars, "")[[1]]
		layer_positions <- symbol_positions_in_text(ann$text[i], char_vector)
		if (nrow(layer_positions) < 2) next
		my_matches <- mm_matches[mm_matches$layer_row == i, , drop = FALSE]
		if (nrow(my_matches) == 0) next

		match_of <- function(k) {
			hits <- my_matches[my_matches$char == layer_positions$char[k], , drop = FALSE]
			if (nrow(hits) == 0) return(hits)
			remapped <- vapply(hits$layer_occurrence, function(occurrence) {
				remap_symbol_occurrence(merge_map, i, layer_positions$char[k],
				                        occurrence)
			}, integer(1))
			hits[remapped == layer_positions$occurrence[k], , drop = FALSE]
		}

		for (k in seq_len(nrow(layer_positions) - 1L)) {
			match_a <- match_of(k)
			match_b <- match_of(k + 1L)
			if (nrow(match_a) == 0 || nrow(match_b) == 0) next
			if (match_a$main_row[1] != match_b$main_row[1]) {
				# Marks in DIFFERENT main rows (the GAT standard case):
				# the closing row is widened so the description does not
				# shrivel to a crumb before its symbol - the symbol moves
				# right through spaces in the verbal line (user comment
				# K4, decision 2026-08-17).
				occurrence_b <- remap_symbol_occurrence(merge_map,
					match_b$main_row[1], match_b$char[1],
					match_b$main_occurrence[1])
				span_list[[length(span_list) + 1]] <- data.frame(
					main_row = match_b$main_row[1],
					char_a = match_b$char[1], occurrence_a = occurrence_b,
					char_b = match_b$char[1], occurrence_b = occurrence_b,
					needed = layer_positions$index[k + 1L] -
						layer_positions$index[k],
					char_set = chars, cross = TRUE,
					stringsAsFactors = FALSE)
				next
			}
			main_row <- match_a$main_row[1]
			occurrence_a <- remap_symbol_occurrence(merge_map, main_row,
			                                        match_a$char[1],
			                                        match_a$main_occurrence[1])
			occurrence_b <- remap_symbol_occurrence(merge_map, main_row,
			                                        match_b$char[1],
			                                        match_b$main_occurrence[1])
			span_list[[length(span_list) + 1]] <- data.frame(
				main_row = main_row,
				char_a = match_a$char[1], occurrence_a = occurrence_a,
				char_b = match_b$char[1], occurrence_b = occurrence_b,
				needed = layer_positions$index[k + 1L] - layer_positions$index[k],
				char_set = chars, cross = FALSE,
				stringsAsFactors = FALSE)
		}
	}
	if (length(span_list) == 0) return(ann)
	spans <- do.call(rbind, span_list)
	# Cross-row spans aggregate per ROW: one widening moves the whole
	# closing cluster, several would tear it apart and overflow the line.
	span_key <- ifelse(spans$cross,
	                   paste("cross", spans$main_row),
	                   paste(spans$main_row, spans$char_a, spans$occurrence_a,
	                         spans$char_b, spans$occurrence_b))
	keep_max <- vapply(split(seq_len(nrow(spans)), span_key), function(rows) {
		rows[which.max(spans$needed[rows])]
	}, integer(1))
	spans <- spans[keep_max, , drop = FALSE]

	# ---- phase 2: widen or shrink each span on the main row ----
	for (sp in seq_len(nrow(spans))) {
		main_row <- spans$main_row[sp]
		# A row that never wraps stays compact: widening would inflate the
		# single line far beyond the transcript width. Its descriptions
		# wrap below their anchors instead (user comment 205_005 GAT K1,
		# 2026-08-17).
		if (!isTRUE(ann$wrap[main_row])) next
		char_vector <- strsplit(spans$char_set[sp], "")[[1]]
		if (isTRUE(spans$cross[sp])) {
			# widen the CLOSING row: push the symbol right so the final
			# description line gets room. Only for rows that render to a
			# single line (the index-to-column mapping is exact there).
			if (is.null(preliminary) || !is.finite(text_body_width)) next
			cache <- preliminary[[main_row]]
			if (is.null(cache) || length(cache$lines) != 1L) next
			positions_b <- symbol_positions_in_text(ann$text[main_row],
			                                        char_vector)
			b <- positions_b[positions_b$char == spans$char_b[sp] &
			                 positions_b$occurrence == spans$occurrence_b[sp], ,
			                 drop = FALSE]
			if (nrow(b) == 0) next
			prefix_b <- nchar(ann$prefix_cont[main_row])
			target_index <- min(spans$needed[sp],
			                    text_body_width - prefix_b - 1L)
			insert_n <- target_index - b$index[1]
			if (insert_n <= 0) next
			graphemes <- split_graphemes(ann$text[main_row])
			insert_at <- b$index[1] - 1L
			while (insert_at > 0L &&
			       graphemes[insert_at] %in% all_anchor_chars) {
				insert_at <- insert_at - 1L
			}
			# Never tear a word apart with blanks ("CLA   &|ro."): the
			# insertion moves in front of the word when a space is
			# available there; otherwise the gap is written with
			# underscores so the word stays readable (user rule
			# 2026-08-17, same behaviour as the same-line branch).
			insert_char <- " "
			word_char <- function(g) {
				stringr::str_detect(g, "^[\\p{L}\\p{N}]$")
			}
			cluster_end <- b$index[1]
			while (cluster_end < length(graphemes) &&
			       graphemes[cluster_end + 1L] %in% all_anchor_chars) {
				cluster_end <- cluster_end + 1L
			}
			left_grapheme <- if (insert_at >= 1L) graphemes[insert_at] else " "
			right_grapheme <- if (cluster_end < length(graphemes)) {
				graphemes[cluster_end + 1L]
			} else {
				" "
			}
			if (word_char(left_grapheme) && word_char(right_grapheme)) {
				earlier_anchor <- which(
					graphemes[seq_len(insert_at)] %in% all_anchor_chars)
				lower_bound <- if (length(earlier_anchor) > 0) {
					max(earlier_anchor)
				} else {
					0L
				}
				space_positions <- which(stringr::str_detect(graphemes, "^\\s$"))
				candidates <- space_positions[space_positions > lower_bound &
				                              space_positions <= insert_at]
				if (length(candidates) > 0) {
					insert_at <- max(candidates)
				} else {
					insert_char <- "_"
				}
			}
			graphemes <- append(graphemes, rep(insert_char, insert_n),
			                    after = insert_at)
			ann$text[main_row] <- paste(graphemes, collapse = "")
			next
		}
		main_positions <- symbol_positions_in_text(ann$text[main_row], char_vector)
		a <- main_positions[main_positions$char == spans$char_a[sp] &
		                    main_positions$occurrence == spans$occurrence_a[sp], , drop = FALSE]
		b <- main_positions[main_positions$char == spans$char_b[sp] &
		                    main_positions$occurrence == spans$occurrence_b[sp], , drop = FALSE]
		if (nrow(a) == 0 || nrow(b) == 0) next
		if (b$index[1] <= a$index[1]) next
		needed    <- spans$needed[sp]
		available <- available_columns(main_row, spans$char_a[sp],
		                               spans$occurrence_a[sp],
		                               spans$char_b[sp], spans$occurrence_b[sp],
		                               fallback = b$index[1] - a$index[1],
		                               prefix_width = nchar(ann$prefix_cont[main_row]))
		if (needed < available && isTRUE(attr(available, "same_line"))) {
			# Overshoot: an earlier cross-line estimate widened the main
			# further than the widest description needs. Give the surplus
			# back by shrinking multi-space runs between the two symbols -
			# never below a single space, so natural word gaps survive
			# (user comment K11, 2026-08-15).
			surplus <- available - needed
			graphemes <- split_graphemes(ann$text[main_row])
			span_idx <- seq(a$index[1] + 1L, b$index[1] - 1L)
			is_fill <- graphemes %in% c(" ", "_")
			# Fill left of ANOTHER span's closing symbol is load-bearing:
			# removing it pulls that symbol left of its description end.
			# Each run may only shrink by the smallest surplus of every
			# span whose closing symbol sits at or right of the run while
			# its opening symbol sits left of it (user mock-up 207_024,
			# 2026-08-17).
			other_span_cap <- function(run_start_idx) {
				cap <- Inf
				for (o in seq_len(nrow(spans))) {
					if (o == sp || spans$main_row[o] != main_row ||
					    isTRUE(spans$cross[o])) next
					other_chars <- strsplit(spans$char_set[o], "")[[1]]
					other_positions <- symbol_positions_in_text(
						ann$text[main_row], other_chars)
					oa <- other_positions[
						other_positions$char == spans$char_a[o] &
						other_positions$occurrence == spans$occurrence_a[o], ,
						drop = FALSE]
					ob <- other_positions[
						other_positions$char == spans$char_b[o] &
						other_positions$occurrence == spans$occurrence_b[o], ,
						drop = FALSE]
					if (nrow(oa) == 0 || nrow(ob) == 0) next
					if (oa$index[1] >= run_start_idx ||
					    ob$index[1] < run_start_idx) next
					cap <- min(cap,
					           max(0L, (ob$index[1] - oa$index[1]) -
					                   spans$needed[o]))
				}
				cap
			}
			drop <- integer(0)
			run_start <- NA_integer_
			for (g_idx in c(span_idx, b$index[1])) {
				if (g_idx %in% span_idx && is_fill[g_idx]) {
					if (is.na(run_start)) run_start <- g_idx
				} else if (!is.na(run_start)) {
					run_len <- g_idx - run_start
					# Next to a symbol or a pause bracket the symbol itself
					# is the separator, so the run may vanish completely;
					# between two words one space survives (user comment
					# K5, 2026-08-17).
					before <- if (run_start > 1L) graphemes[run_start - 1L] else " "
					after <- graphemes[g_idx]
					side_free <- function(g_before, g_after) {
						(g_before %in% all_anchor_chars || g_before == ")") &&
						(g_after %in% all_anchor_chars || g_after == "(")
					}
					keep_min <- if (side_free(before, after)) 0L else 1L
					take <- min(surplus, run_len - keep_min,
					            other_span_cap(run_start))
					if (take > 0L) {
						drop <- c(drop, seq(run_start, length.out = take))
						surplus <- surplus - take
					}
					run_start <- NA_integer_
					if (surplus == 0L) break
				}
			}
			if (length(drop) > 0) {
				ann$text[main_row] <- paste(graphemes[-drop], collapse = "")
			}
			next
		}
		if (needed <= available) next
		# Widening is EXACT only when both symbols sit on one rendered
		# line. Across a line break, inserting pushes the closing symbol
		# into the wrap and the shortfall never closes - every pass would
		# insert again (runaway seen 2026-08-16). The layer wraps there
		# anyway and the interleaver distributes its lines.
		if (!isTRUE(attr(available, "same_line")) &&
		    !is.null(preliminary) && is.finite(text_body_width)) next
		# Same rule when the widening would push the CLOSING symbol out of
		# the line: it wraps away and the premise breaks. Text behind the
		# symbol may wrap freely, so only the symbol column is capped
		# (user comment K3, 2026-08-16).
		close_col <- attr(available, "close_col")
		if (isTRUE(attr(available, "same_line")) && !is.null(close_col) &&
		    is.finite(text_body_width) &&
		    close_col + (needed - available) > text_body_width) next

		graphemes <- split_graphemes(ann$text[main_row])
		insert_at <- b$index[1] - 1L
		# Never step back BEYOND the opening symbol: inside a marker
		# cluster like "|#|#" the space has to go between the two marks,
		# otherwise the whole cluster shifts and the distance stays the
		# same (2026-08-14). A CLOSING mark stops the slide: it sits at
		# the end of its own description and must not be pushed right
		# along with this span (user mock-up 207_024, 2026-08-17).
		while (insert_at > a$index[1] &&
		       graphemes[insert_at] %in% all_anchor_chars &&
		       !.main_symbol_closes(ann, mm_matches, merge_map, main_row,
		                            graphemes, insert_at)) {
			insert_at <- insert_at - 1L
		}
		insert_char <- " "
		is_word_char <- function(g) {
			stringr::str_detect(g, "^[\\p{L}\\p{N}]$")
		}
		cluster_end <- b$index[1]
		while (cluster_end < length(graphemes) &&
		       graphemes[cluster_end + 1L] %in% all_anchor_chars) {
			cluster_end <- cluster_end + 1L
		}
		left_grapheme  <- if (insert_at >= 1L) graphemes[insert_at] else " "
		right_grapheme <- if (cluster_end < length(graphemes)) {
			graphemes[cluster_end + 1L]
		} else {
			" "
		}
		if (is_word_char(left_grapheme) && is_word_char(right_grapheme)) {
			space_positions <- which(stringr::str_detect(graphemes, "^\\s$"))
			candidates <- space_positions[space_positions > a$index[1] &
			                              space_positions <= insert_at]
			if (length(candidates) > 0) {
				insert_at <- max(candidates)
			} else {
				insert_char <- "_"
			}
		}
		graphemes <- append(graphemes, rep(insert_char, needed - available),
		                    after = insert_at)
		ann$text[main_row] <- paste(graphemes, collapse = "")
	}
	ann
}

symbol_positions_in_text <- function(text, char_vector) {
	graphemes <- split_graphemes(text)
	occ_counter <- list()
	rows <- list()
	for (g_idx in seq_along(graphemes)) {
		g <- graphemes[g_idx]
		if (!g %in% char_vector) next
		n <- occ_counter[[g]]
		if (is.null(n)) n <- 0L
		occ_counter[[g]] <- n + 1L
		rows[[length(rows) + 1]] <- data.frame(
			char = g, occurrence = n + 1L, index = g_idx
		)
	}
	if (length(rows) == 0) {
		return(data.frame(char = character(0), occurrence = integer(0),
		                  index = integer(0)))
	}
	do.call(rbind, rows)
}

# Two-branch filler autodetect (same logic as fixed act code):
# - always: arrow pattern at segment end -> stem char
# - phase_chars = TRUE (multimodal): trailing Mondada phase chars . , -
detect_bracket_filler_new <- function(bracket_content, default_filler,
                                      phase_chars = FALSE) {
	if (is.na(default_filler) || default_filler == "") default_filler <- " "
	if (is.na(bracket_content) || nchar(bracket_content) < 3) return(default_filler)
	inner <- substr(bracket_content, 2L, nchar(bracket_content) - 1L)
	if (nchar(inner) == 0) return(default_filler)
	arrow_match <- stringr::str_match(inner, "([-=])\\1*>+$")
	if (!is.na(arrow_match[1, 1])) return(arrow_match[1, 2])
	if (phase_chars) {
		phase_match <- stringr::str_match(inner, "([.,-])\\1*$")
		if (!is.na(phase_match[1, 1])) return(phase_match[1, 2])
	}
	default_filler
}

# ======================================================================
# ==== source module: prepare_annotations.R ====

# ===== ANNOTATION PREPARATION (ported from act:::.docx_prerender_prep) =====
# Builds the engine's ann frame from an act::transcript + act::layout.
# Deliberately OMITS from the legacy prep: legacy .align_brackets() /
# .align_layers() calls, the leading-space cleanup (both replaced by the
# engine) and the translation indent (moved into the engine, computed
# against the RENDERED main line). Everything else is a faithful copy.

prepare_annotations_new <- function(t, l, layout_mode = "gat",
                                    fig_replace = TRUE,
                                    fig_tier_regex = "^stills(#|$)") {
	ann <- t@annotations
	ann$tierName <- as.character(ann$tierName)

	# ===== STYLE MATCHING (is.main) =====
	ann$format.is.main <- FALSE
	if (!is.null(l@docx.styles.user) && nrow(l@docx.styles.user) > 0) {
		ann$format.style.matched <- FALSE
		for (row_idx in seq_len(nrow(l@docx.styles.user))) {
			style_row <- l@docx.styles.user[row_idx, ]
			matched <- rep(FALSE, nrow(ann))
			if (!is.na(style_row$match.regex)) {
				matched <- matched | grepl(style_row$match.regex, ann$tierName, perl = TRUE)
			}
			unset <- !ann$format.style.matched & matched
			ann$format.style.matched[unset] <- TRUE
			if (!is.null(style_row$is.main.tier) && !is.na(style_row$is.main.tier) &&
			    style_row$is.main.tier) {
				ann$format.is.main[unset] <- TRUE
			}
		}
		ann$format.style.matched <- NULL
	}

	# ===== SORT: time + tier order =====
	tier_order <- match(ann$tierName, t@tiers$name)
	tier_order[is.na(tier_order)] <- 999L
	ann <- ann[order(ann$startsec, tier_order), ]

	# ===== PREFIX PARTS =====
	ann$spacebefore <- stringr::str_pad("", width = l@spacesbefore, side = "left", pad = " ")

	style_default_name <- get_style_base(l, "transcript.default")$docx.template.name
	ann$format.show            <- TRUE
	ann$format.style           <- style_default_name
	ann$format.line.nr.show    <- isTRUE(l@line.nr.show)
	ann$format.acronym.show    <- TRUE
	ann$format.acronym.case    <- NA_character_
	ann$format.acronym.search  <- NA_character_
	ann$format.acronym.replace <- NA_character_
	ann$format.acronym.width   <- 0L
	ann$format.acronym.ending  <- NA_character_
	ann$format.space.after     <- NA_character_
	ann$format.content.wrap    <- TRUE
	ann$format.filler.inside   <- " "
	ann$format.align.char      <- NA_character_
	ann$format.align.mode      <- NA_character_
	ann$format.content.indent  <- NA_character_
	ann$format.indent.skip     <- NA_character_
	ann$format.align.arrow     <- NA_character_

	for (tier_name in unique(ann$tierName)) {
		format_tier <- get_style_user(l, name = tier_name)
		rows <- which(ann$tierName == tier_name)
		if (!is.na(format_tier$show))               ann$format.show[rows]            <- format_tier$show
		if (!is.na(format_tier$line.nr.show))       ann$format.line.nr.show[rows]    <- format_tier$line.nr.show
		if (!is.na(format_tier$acronym.show))       ann$format.acronym.show[rows]    <- format_tier$acronym.show
		if (!is.na(format_tier$docx.template.name)) ann$format.style[rows]           <- format_tier$docx.template.name
		if (!is.na(format_tier$acronym.case))       ann$format.acronym.case[rows]    <- format_tier$acronym.case
		if (!is.na(format_tier$acronym.search))     ann$format.acronym.search[rows]  <- format_tier$acronym.search
		if (!is.na(format_tier$acronym.replace))    ann$format.acronym.replace[rows] <- format_tier$acronym.replace
		if (!is.na(format_tier$acronym.width))      ann$format.acronym.width[rows]   <- format_tier$acronym.width
		if (!is.na(format_tier$acronym.ending))     ann$format.acronym.ending[rows]  <- format_tier$acronym.ending
		if (!is.na(format_tier$space.after))        ann$format.space.after[rows]     <- format_tier$space.after
		if (!is.na(format_tier$content.wrap))       ann$format.content.wrap[rows]    <- format_tier$content.wrap
		if (!is.na(format_tier$content.indent.align.filler.inside)) ann$format.filler.inside[rows] <- format_tier$content.indent.align.filler.inside
		if (!is.na(format_tier$content.indent.align.char)) ann$format.align.char[rows] <- format_tier$content.indent.align.char
		if (!is.na(format_tier$content.indent.align.mode)) ann$format.align.mode[rows] <- format_tier$content.indent.align.mode
		if (!is.na(format_tier$content.indent))     ann$format.content.indent[rows]  <- format_tier$content.indent
		if (!is.na(format_tier$content.indent.text.skip)) ann$format.indent.skip[rows] <- format_tier$content.indent.text.skip
		if (!is.null(format_tier$content.indent.align.arrow) &&
		    !is.na(format_tier$content.indent.align.arrow)) {
			ann$format.align.arrow[rows] <- format_tier$content.indent.align.arrow
		}
	}

	# ===== LINE NUMBERS =====
	ann$line <- ""
	numbered_rows <- which(ann$format.line.nr.show == TRUE)
	if (length(numbered_rows) > 0) {
		nums <- as.character(seq_along(numbered_rows))
		nums[seq_len(min(length(nums), 9))] <- stringr::str_pad(
			nums[seq_len(min(length(nums), 9))], width = 2, side = "left", pad = "0")
		ann$line[numbered_rows] <- nums
	}

	# ===== SPEAKER ACRONYMS =====
	tier_names <- as.character(unique(ann$tierName))
	text_body_width_speaker_default <- max(nchar(tier_names))
	if (!is.na(l@speaker.width)) {
		if (l@speaker.width != -1) {
			text_body_width_speaker_default <- l@speaker.width
		}
	}

	ann$speaker <- ann$tierName
	trppauses_pos <- stringr::str_detect(ann$content, options()$act.pauseIdentifierGATRegEx)
	trppauses_pos[is.na(trppauses_pos)] <- FALSE
	if (length(trppauses_pos) > 0) {
		ann$speaker[trppauses_pos] <- "%TRPPAUSE%$(\u00a7($&\u00a7%/%"
	}

	is_main_tier <- ann$format.is.main
	last_main_speaker <- ""
	current_main_speaker <- ""
	ann$speaker_base <- ""
	ann_prev_main_speaker <- character(nrow(ann))
	for (i in seq_len(nrow(ann))) {
		ann_prev_main_speaker[i] <- last_main_speaker
		if (is_main_tier[i]) {
			current_main_speaker <- ann$tierName[i]
			if (trppauses_pos[i]) {
				last_main_speaker <- ""
			} else {
				last_main_speaker <- current_main_speaker
			}
		}
		ann$speaker_base[i] <- current_main_speaker
	}
	sameSpeaker_pos <- tolower(ann_prev_main_speaker) == tolower(ann$speaker_base) & is_main_tier
	if (isTRUE(l@speaker.repeat)) {
		sameSpeaker_pos <- rep(FALSE, length(sameSpeaker_pos))
	}
	ann$speaker[sameSpeaker_pos] <- ""
	included_speakers_pos <- !sameSpeaker_pos
	if (length(trppauses_pos) > 0) {
		ann$speaker[trppauses_pos] <- ""
		included_speakers_pos[which(trppauses_pos)] <- FALSE
	}

	for (i in which(included_speakers_pos)) {
		speaker_text <- ann$speaker[i]
		acronym_case <- ann$format.acronym.case[i]
		if (!is.na(acronym_case)) {
			speaker_text <- switch(acronym_case,
				"toupper"    = toupper(speaker_text),
				"tolower"    = tolower(speaker_text),
				"capitalize" = paste0(toupper(substr(speaker_text, 1, 1)),
				                      tolower(substr(speaker_text, 2, nchar(speaker_text)))),
				speaker_text
			)
		}
		acronym_search  <- ann$format.acronym.search[i]
		acronym_replace <- ann$format.acronym.replace[i]
		if (!is.na(acronym_search)) {
			speaker_text <- sub(acronym_search,
			                    ifelse(is.na(acronym_replace), "", acronym_replace),
			                    speaker_text)
		} else if (!is.na(l@speaker.regex)) {
			extracted <- stringr::str_extract(speaker_text, l@speaker.regex)
			if (!is.na(extracted)) speaker_text <- extracted
		}
		acronym_width <- ann$format.acronym.width[i]
		if (is.na(acronym_width) || acronym_width == 0) {
			acronym_width <- text_body_width_speaker_default
		}
		if (acronym_width > 0) {
			speaker_text <- substr(speaker_text, 1, acronym_width)
		}
		acronym_ending <- ann$format.acronym.ending[i]
		if (is.na(acronym_ending)) {
			acronym_ending <- l@speaker.ending
		}
		speaker_text <- paste0(speaker_text, acronym_ending)
		ann$speaker[i] <- speaker_text
	}

	text_body_width_speaker <- nchar(l@speaker.ending)
	if (any(included_speakers_pos)) {
		text_body_width_speaker <- max(nchar(ann$speaker[included_speakers_pos]))
	}
	ann$speaker <- stringr::str_pad(ann$speaker, width = text_body_width_speaker,
	                                side = "right", pad = " ")

	# ===== LINE NUMBER + ACRONYM PADDING, PREFIX =====
	any_line_nr_shown <- any(ann$format.line.nr.show)
	line_nr_width <- if (any_line_nr_shown && any(ann$line != "")) {
		max(nchar(ann$line[ann$line != ""]))
	} else {
		0
	}
	ann$format.line.nr <- ifelse(ann$format.line.nr.show, ann$line,
	                             strrep(" ", line_nr_width))
	if (identical(layout_mode, "mondada")) {
		# Score mode numbers per PRINTED verbal line (apply_score_line_numbers
		# writes into this slot after rendering); the per-annotation numbers
		# would be misleading, so the slot stays blank at prep time.
		ann$format.line.nr <- strrep(" ", line_nr_width)
		ann$line <- ""
	}
	ann$format.acronym <- ifelse(ann$format.acronym.show & !sameSpeaker_pos,
	                             ann$speaker,
	                             strrep(" ", text_body_width_speaker))
	line_sep <- if (line_nr_width > 0) " " else ""
	ann$format.prefix <- paste0(ann$spacebefore, ann$format.line.nr, line_sep,
	                            ann$format.acronym)

	# ===== BLOCK SPACE (space.after) =====
	ann$format.insert.space.after <- FALSE
	visible_rows <- which(ann$format.show)
	if (length(visible_rows) > 0) {
		for (vi in seq_along(visible_rows)) {
			i <- visible_rows[vi]
			space_after_val <- ann$format.space.after[i]
			if (is.na(space_after_val) || space_after_val == "no") next
			if (space_after_val == "always") {
				ann$format.insert.space.after[i] <- TRUE
			} else if (space_after_val == "block") {
				if (vi == length(visible_rows)) {
					ann$format.insert.space.after[i] <- TRUE
				} else {
					i_next <- visible_rows[vi + 1]
					if (ann$format.is.main[i_next]) {
						ann$format.insert.space.after[i] <- TRUE
					}
				}
			}
		}
	}

	# ===== CONTENT =====
	ann$content_render <- ifelse(ann$format.is.main,
	                             stringr::str_trim(ann$content), ann$content)
	ann$content_render <- stringr::str_replace_all(ann$content_render, "\\r?\n", "")
	linebreak_char <- getOption("act.layout.linebreak.char", HARD_BREAK_CHAR)
	if (!identical(linebreak_char, HARD_BREAK_CHAR) && nzchar(linebreak_char)) {
		ann$content_render <- stringr::str_replace_all(ann$content_render,
			stringr::fixed(linebreak_char), HARD_BREAK_CHAR)
	}

	# ===== FIGURES: still id -> "#<number>" =====
	# The number is the trailing digit group of the still id, kept EXACTLY
	# as annotated including a leading zero (207_021_01 -> #01, user rule
	# 2026-08-14); rows without digits get a sequential fallback.
	# The "#" then anchors under the main's "#" via the point alignment.
	if (isTRUE(fig_replace)) {
		fig_rows <- which(stringr::str_detect(ann$tierName, fig_tier_regex))
		fig_counter <- 0L
		for (i in fig_rows) {
			fig_counter <- fig_counter + 1L
			id_digits <- stringr::str_extract(
				stringr::str_trim(ann$content_render[i]), "[0-9]+$")
			number <- if (!is.na(id_digits)) id_digits else as.character(fig_counter)
			ann$content_render[i] <- paste0("#", number)
		}
	}

	# ===== PER-ROW LINE-NR WIDTH FOR NON-MAIN ROWS =====
	if (any(!ann$format.is.main)) {
		for (i in which(!ann$format.is.main)) {
			main_candidates <- which(ann$format.is.main[seq_len(i)])
			if (length(main_candidates) == 0) next
			main_idx <- max(main_candidates)
			main_line_width <- nchar(ann$line[main_idx])
			if (main_line_width <= 0 || main_line_width == nchar(ann$format.line.nr[i])) next
			ann$format.line.nr[i] <- strrep(" ", main_line_width)
			ann$format.prefix[i]  <- paste0(ann$spacebefore[i], ann$format.line.nr[i],
			                                line_sep, ann$format.acronym[i])
		}
	}

	# ===== CONTENT INDENT "none": minimal prefix =====
	if (any(!ann$format.is.main)) {
		for (i in which(!ann$format.is.main)) {
			if (identical(ann$format.content.indent[i], "none")) {
				none_prefix_parts <- character(0)
				if (ann$format.line.nr.show[i] && ann$line[i] != "") {
					none_prefix_parts <- c(none_prefix_parts, ann$line[i])
				}
				if (ann$format.acronym.show[i] && !sameSpeaker_pos[i]) {
					none_prefix_parts <- c(none_prefix_parts,
					                       stringr::str_trim(ann$speaker[i]))
				}
				ann$format.prefix[i] <- if (length(none_prefix_parts) > 0) {
					paste0(paste(none_prefix_parts, collapse = ""), " ")
				} else {
					""
				}
			}
		}
	}

	# ===== ENGINE FRAME =====
	# transcript.width is the TOTAL width of the printed transcript: the
	# prefix column (line number + sigle) lies INSIDE it, the wrap edge and
	# the page edge are the same value (user decision 2026-08-17).
	engine_width <- if (is.na(l@transcript.width) || l@transcript.width == -1) {
		Inf
	} else {
		max(20L, l@transcript.width)
	}

	# Catch-all multimodal tiers (#mm, #mm1, #mm2, ...) carry symbols of
	# SEVERAL sets (e.g. "+&" when two participants act together), so their
	# style row defines no align chars of its own. They anchor on the union
	# of every align char defined for the other multimodal styles.
	align_char_union <- paste(unique(unlist(strsplit(
		ann$format.align.char[!is.na(ann$format.align.char)], ""))),
		collapse = "")
	derive_rows <- identical_chr(ann$format.content.indent, "align") &
		is.na(ann$format.align.char) & nzchar(align_char_union) &
		stringr::str_detect(ann$tierName, "#mm[0-9]*$")
	ann$format.align.char[derive_rows] <- align_char_union
	ann$format.align.mode[derive_rows] <-
		ifelse(is.na(ann$format.align.mode[derive_rows]), "bracket",
		       ann$format.align.mode[derive_rows])

	# A space directly after the leading annotation symbol is dropped in
	# layer content - the symbol sits glued to its description (user
	# comment 206_003 K1, 2026-08-17).
	for (i in which(!ann$format.is.main & !is.na(ann$format.align.char))) {
		chars <- strsplit(ann$format.align.char[i], "")[[1]]
		g <- split_graphemes(ann$content_render[i])
		if (length(g) >= 3 && g[1] %in% chars && g[2] == " ") {
			ann$content_render[i] <- paste(g[-2], collapse = "")
		}
	}

	engine_ann <- data.frame(
		annotationID  = if (!is.null(ann$annotationID)) ann$annotationID else NA_integer_,
		tierName      = ann$tierName,
		content       = ann$content_render,
		startsec      = ann$startsec,
		endsec        = ann$endsec,
		is_main       = ann$format.is.main,
		prefix_first  = ann$format.prefix,
		prefix_cont   = strrep(" ", nchar(ann$format.prefix)),
		wrap          = ann$format.content.wrap,
		align_chars   = ifelse(identical_chr(ann$format.content.indent, "align"),
		                       ann$format.align.char, NA_character_),
		align_mode    = ifelse(identical_chr(ann$format.content.indent, "align"),
		                       ann$format.align.mode, NA_character_),
		filler_inside = ann$format.filler.inside,
		indent_mode   = ann$format.content.indent,
		indent_skip   = ann$format.indent.skip,
		show          = ann$format.show,
		space_after   = ann$format.insert.space.after,
		style         = ann$format.style,
		number_lines  = ann$format.line.nr.show & ann$format.is.main
	)

	arrow_modes <- ann$format.align.arrow[!is.na(ann$format.align.arrow)]
	arrow_mode <- if (length(arrow_modes) > 0 && arrow_modes[1] == "space") "space" else "stem"

	list(
		engine_ann    = engine_ann,
		engine_width  = engine_width,
		arrow_mode    = arrow_mode,
		number_offset = l@spacesbefore,
		number_width  = max(2L, line_nr_width)
	)
}

identical_chr <- function(x, value) {
	!is.na(x) & x == value
}

# ======================================================================
# ==== source module: verify_alignment.R ====

# ===== VERIFICATION OF RENDERED ALIGNMENT =====
# Checks the engine result. Returns a data.frame of failures (zero rows =
# all good). Used by the fixture tests via stopifnot on nrow == 0.

verify_alignment <- function(result, text_body_width,
                             allow_overflow = TRUE) {
	failures <- list()
	fail <- function(kind, detail) {
		failures[[length(failures) + 1]] <<- data.frame(kind = kind,
		                                                detail = detail)
	}

	report <- attr(result, "anchor_report")

	# ---- 1. every non-degraded anchor sits at its target column ----
	if (!is.null(report) && nrow(report) > 0) {
		# "moved_to_next_line": the annotation was deliberately moved to the
		# next block, so the symbol no longer sits at the original column.
		bad <- report[!report$degraded &
		              report$note != "moved_to_next_line" &
		              !is.na(report$target_col) &
		              report$placed_col != report$target_col, , drop = FALSE]
		for (b in seq_len(nrow(bad))) {
			fail("anchor_off_target", sprintf(
				"row %d char %s occ %d: placed %d, target %d",
				bad$row[b], bad$char[b], bad$occurrence[b],
				bad$placed_col[b], bad$target_col[b]))
		}
	}

	# ---- 2. line width (overflow of single long words is allowed) ----
	for (i in seq_len(nrow(result))) {
		if (!isTRUE(result$wrap[i])) next
		lines <- result$rendered_lines[[i]]
		for (line_index in seq_along(lines)) {
			line_len <- length(split_graphemes(lines[line_index]))
			if (line_len > text_body_width) {
				body <- substr(lines[line_index],
				               nchar(result$prefix_cont[i]) + 1L,
				               nchar(lines[line_index]))
				single_long_token <- !stringr::str_detect(
					stringr::str_trim(body), "\\s")
				if (!(allow_overflow && single_long_token)) {
					fail("line_too_wide", sprintf(
						"row %d line %d: %d > %d", i, line_index,
						line_len, text_body_width))
				}
			}
		}
	}

	# ---- 3. no hard-break char in any output line ----
	for (i in seq_len(nrow(result))) {
		lines <- result$rendered_lines[[i]]
		if (any(stringr::str_detect(lines, stringr::fixed(HARD_BREAK_CHAR)))) {
			fail("hard_break_in_output", sprintf("row %d", i))
		}
	}

	# ---- 4. no whitespace-only lines ----
	for (i in seq_len(nrow(result))) {
		lines <- result$rendered_lines[[i]]
		if (length(lines) > 0 && any(stringr::str_detect(lines, "^\\s*$"))) {
			fail("blank_line", sprintf("row %d", i))
		}
	}

	if (length(failures) == 0) {
		return(data.frame(kind = character(0), detail = character(0)))
	}
	do.call(rbind, failures)
}

# Column of the n-th occurrence of a character in rendered lines (test helper).
find_rendered_col <- function(lines, char, occurrence = 1L) {
	count <- 0L
	for (line in lines) {
		graphemes <- split_graphemes(line)
		hits <- which(graphemes == char)
		for (h in hits) {
			count <- count + 1L
			if (count == occurrence) return(h)
		}
	}
	NA_integer_
}

# ======================================================================
# ==== source module: alignment_report.R ====

# ===== ALIGNMENT REPORT =====
# Writes a plain-text report next to the exported transcript listing every
# inconsistency the alignment run detected. Findings are grouped into three
# types (user decision 2026-08-14):
#   A  the annotation needs to be changed in ELAN
#   B  technically caused by the layout (width, description length)
#   C  informational - what the engine did on its own
# Each finding shows the rendered block it occurs in, marks the spot with
# "^" and states what to do.

# Report path derived from the exported file: same folder, same base name,
# suffix "__alignment.txt" (user decision 2026-08-14). When no output file
# is written the caller has to name the report explicitly.
alignment_report_path <- function(path_output) {
	if (is.null(path_output) || is.na(path_output)) return(NULL)
	base <- tools::file_path_sans_ext(path_output)
	paste0(base, "__alignment.txt")
}

build_alignment_report <- function(result, plan, transcript_name,
                                   layout_mode, text_body_width,
                                   time_tolerance) {
	findings <- c(
		.report_findings_unmatched(result),
		.report_findings_foreign_symbol(result),
		.report_findings_spare_marks(result),
		.report_findings_merge_blocked(result),
		.report_findings_unpaired_bracket(result),
		.report_findings_mixed_wrap(result),
		.report_findings_degraded(result),
		.report_findings_overflow(result, text_body_width),
		.report_findings_info(result)
	)
	order_key <- vapply(findings, function(f) f$id, character(1))
	findings <- findings[order(order_key)]

	types <- substr(vapply(findings, function(f) f$id, character(1)), 1, 1)
	lines <- c(
		strrep("=", 78),
		sprintf("ALIGNMENT REPORT   %s", transcript_name),
		sprintf("Layout: %s | width: %s | simultaneity tolerance: %.2f s",
		        layout_mode,
		        if (is.finite(text_body_width)) text_body_width else "unlimited",
		        time_tolerance),
		strrep("=", 78),
		sprintf("SUMMARY   check annotation: %d   technical: %d   notes: %d",
		        sum(types == "A"), sum(types == "B"), sum(types == "C")),
		""
	)
	if (length(findings) == 0) {
		lines <- c(lines, "No inconsistencies found.", "")
		return(lines)
	}
	lines <- c(lines, .report_format_blocks(findings, result, plan))
	lines <- c(lines,
		strrep("-", 78),
		"LEGEND",
		"  A  the annotation should be corrected in ELAN",
		"  B  caused by the layout - shorten the description or widen the transcript",
		"  C  note only - what the engine did to make the alignment work",
		"")
	lines
}

# ---- finding constructors -------------------------------------------------

.report_findings_unmatched <- function(result) {
	warnings <- attr(result, "render_warnings")
	if (is.null(warnings) || length(warnings) == 0) return(list())
	out <- list()
	for (w in warnings) {
		if (!identical(w$kind, "unmatched_symbol")) next
		is_stills <- stringr::str_detect(w$tier, "^stills")
		out[[length(out) + 1]] <- list(
			id = if (is_stills) "A2" else "A1",
			title = if (is_stills) {
				"Picture mark missing in the verbal line"
			} else {
				"Symbol without counterpart in the verbal line"
			},
			row = w$row, char = w$char, occurrence = w$occurrence,
			tier = w$tier, startsec = w$startsec, endsec = w$endsec,
			description = if (is_stills) {
				sprintf(paste0("The still \"%s\" carries a \"%s\" mark, but the verbal ",
				               "annotation %s\n  has no free \"%s\" at that point."),
				        .report_shorten(w$content, 40), w$char,
				        if (!is.na(w$main_tier)) w$main_tier else "(none)", w$char)
			} else {
				sprintf(paste0("The \"%s\" (occurrence %d) of \"%s\" finds no \"%s\" in the ",
				               "verbal annotation\n  %s \"%s\" (%s - %s)."),
				        w$char, w$occurrence, .report_shorten(w$content, 40), w$char,
				        if (!is.na(w$main_tier)) w$main_tier else "(none)",
				        .report_shorten(w$main_content, 40),
				        helper_format_time(w$main_startsec),
				        helper_format_time(w$main_endsec))
			},
			advice = if (is_stills) {
				paste0("Add a \"", w$char, "\" to the verbal annotation at the moment the ",
				       "still\n    was taken, or remove the surplus still.")
			} else {
				paste0("Check in ELAN: is the symbol missing in the verbal annotation, ",
				       "does\n    the layer annotation have one symbol too many, or does it ",
				       "belong to\n    a different turn?")
			})
	}
	out
}

# A symbol belonging to a DIFFERENT set turned up in this tier - typically a
# copy-paste from another participant, or a look-alike character (the
# lozenges and diamonds are hard to tell apart on screen). Such a symbol is
# never aligned, because the tier only knows its own set (2026-08-14).
.report_findings_foreign_symbol <- function(result) {
	own <- unique(unlist(lapply(result$align_chars[!is.na(result$align_chars)],
	                            function(x) strsplit(x, "")[[1]])))
	if (length(own) == 0) return(list())
	out <- list()
	for (i in seq_len(nrow(result))) {
		chars <- result$align_chars[i]
		if (is.na(chars) || nchar(chars) == 0) next
		mine <- strsplit(chars, "")[[1]]
		graphemes <- split_graphemes(result$content[i])
		foreign <- unique(graphemes[graphemes %in% own & !(graphemes %in% mine)])
		if (length(foreign) == 0) next
		out[[length(out) + 1]] <- list(
			id = "A6", title = "Symbol of a different set in this tier",
			row = i, char = foreign[1], occurrence = 1L,
			tier = result$tierName[i],
			startsec = result$startsec[i], endsec = result$endsec[i],
			description = sprintf(paste0("This tier is annotated with \"%s\", but the ",
			                             "annotation also contains\n  \"%s\". Such a symbol ",
			                             "cannot be aligned here."),
			                      chars, paste(foreign, collapse = " ")),
			advice = paste0("Either the symbol belongs to another participant (then the ",
			                "annotation\n    belongs in that tier, or in a combined tier), ",
			                "or a look-alike\n    character was typed - check it against the ",
			                "symbol table."))
	}
	out
}

# More position marks in the verbal line than pictures annotated: every "#"
# should have its still. The opposite case (picture without mark) is A2.
.report_findings_spare_marks <- function(result) {
	picture_rows <- which(!is.na(result$align_mode) &
	                      result$align_mode == "point")
	if (length(picture_rows) == 0) return(list())
	mark <- unique(unlist(lapply(result$align_chars[picture_rows],
	                             function(x) strsplit(x, "")[[1]])))[1]
	if (is.na(mark)) return(list())
	pictures <- sum(vapply(picture_rows, function(r) {
		length(which(split_graphemes(result$content[r]) == mark))
	}, integer(1)))
	main_rows <- which(result$is_main)
	marks <- sum(vapply(main_rows, function(r) {
		length(which(split_graphemes(result$content[r]) == mark))
	}, integer(1)))
	if (marks <= pictures) return(list())
	list(list(
		id = "A7", title = "More position marks than pictures",
		row = main_rows[1], char = NA_character_, occurrence = NA_integer_,
		tier = result$tierName[main_rows[1]],
		startsec = result$startsec[main_rows[1]],
		endsec = result$endsec[main_rows[length(main_rows)]],
		description = sprintf(paste0("The verbal lines carry %d \"%s\" marks, but only %d ",
		                             "pictures are\n  annotated - %d mark%s without a picture."),
		                      marks, mark, pictures, marks - pictures,
		                      if (marks - pictures == 1) "" else "s"),
		advice = paste0("Either pictures are missing in the stills tier, or the surplus ",
		                "marks\n    should be removed from the verbal annotations.")))
}

.report_findings_merge_blocked <- function(result) {
	events <- attr(result, "merge_events")
	if (is.null(events) || nrow(events) == 0) return(list())
	vetoed <- if (is.null(events$vetoed)) rep(FALSE, nrow(events)) else events$vetoed
	blocked <- events[!events$merged & !vetoed &
	                  !is.na(events$time_first) &
	                  !is.na(events$time_second), , drop = FALSE]
	out <- list()
	for (k in seq_len(nrow(blocked))) {
		difference <- abs(blocked$time_second[k] - blocked$time_first[k])
		out[[length(out) + 1]] <- list(
			id = "A3",
			title = "Adjacent marks refer to different points in time",
			row = blocked$row[k], char = blocked$char[k],
			occurrence = blocked$occurrence_first[k],
			tier = result$tierName[blocked$row[k]],
			startsec = result$startsec[blocked$row[k]],
			endsec = result$endsec[blocked$row[k]],
			description = sprintf(paste0("Two \"%s\" marks stand directly next to each other, ",
			                             "but the non-verbal\n  annotations they belong to are ",
			                             "%.2f s apart. The marks were therefore\n  kept ",
			                             "separate."),
			                      blocked$char[k], difference),
			advice = paste0("Either the two annotations should share one moment (align their ",
			                "times\n    in ELAN), or the marks belong at different places in ",
			                "the verbal line."))
	}
	out
}

.report_findings_unpaired_bracket <- function(result) {
	pairs <- attr(result, "bracket_pairs")
	out <- list()
	for (i in which(result$is_main)) {
		if (is.na(result$content[i])) next
		graphemes <- split_graphemes(result$content[i])
		positions <- which(graphemes == "[")
		if (length(positions) == 0 || positions[1] > 2L) next
		matched <- if (!is.null(pairs) && nrow(pairs) > 0) {
			any(pairs$j_row == i)
		} else {
			FALSE
		}
		if (matched) next
		out[[length(out) + 1]] <- list(
			id = "A4", title = "Overlap bracket without a partner",
			row = i, char = "[", occurrence = 1L,
			tier = result$tierName[i],
			startsec = result$startsec[i], endsec = result$endsec[i],
			description = paste0("This annotation starts with \"[\", but no other speaker ",
			                     "has an\n  overlapping \"[\" at that time."),
			advice = paste0("Check the times of both annotations in ELAN, or remove the ",
			                "bracket\n    if there is no overlap."))
	}
	out
}

.report_findings_mixed_wrap <- function(result) {
	warnings <- attr(result, "render_warnings")
	if (is.null(warnings) || length(warnings) == 0) return(list())
	out <- list()
	for (w in warnings) {
		if (!identical(w$kind, "mixed_wrap")) next
		out[[length(out) + 1]] <- list(
			id = "B2", title = "Target column beyond the line width",
			row = w$row, char = w$char, occurrence = w$occurrence,
			tier = w$tier, startsec = w$startsec, endsec = w$endsec,
			description = sprintf(paste0("The \"%s\" should sit at column %d, which is beyond ",
			                             "the line width."),
			                      w$char, w$target_col),
			advice = "Enable wrapping for this tier (content.wrap) or widen the transcript.")
	}
	out
}

.report_findings_degraded <- function(result) {
	report <- attr(result, "anchor_report")
	if (is.null(report) || nrow(report) == 0) return(list())
	bad <- report[isTRUE(report$degraded) | report$degraded, , drop = FALSE]
	bad <- bad[!is.na(bad$target_col), , drop = FALSE]
	out <- list()
	for (k in seq_len(nrow(bad))) {
		out[[length(out) + 1]] <- list(
			id = "B1", title = "Symbol could not reach its target column",
			row = bad$row[k], char = bad$char[k], occurrence = bad$occurrence[k],
			tier = result$tierName[bad$row[k]],
			startsec = result$startsec[bad$row[k]],
			endsec = result$endsec[bad$row[k]],
			description = sprintf(paste0("The \"%s\" sits at column %d instead of %d - the ",
			                             "text in front of it is\n  longer than the space up ",
			                             "to the target."),
			                      bad$char[k], bad$placed_col[k], bad$target_col[k]),
			advice = "Shorten the description in ELAN, or widen the transcript.")
	}
	out
}

.report_findings_overflow <- function(result, text_body_width) {
	if (!is.finite(text_body_width)) return(list())
	out <- list()
	for (i in seq_len(nrow(result))) {
		if (!isTRUE(result$wrap[i])) next
		lines <- result$rendered_lines[[i]]
		for (line_index in seq_along(lines)) {
			line_length <- length(split_graphemes(lines[line_index]))
			if (line_length <= text_body_width) next
			out[[length(out) + 1]] <- list(
				id = "B3", title = "Line exceeds the transcript width",
				row = i, char = NA_character_, occurrence = NA_integer_,
				line_index = line_index,
				tier = result$tierName[i],
				startsec = result$startsec[i], endsec = result$endsec[i],
				description = sprintf(paste0("This line is %d characters wide, the limit is ",
				                             "%d - a single word cannot\n  be broken."),
				                      line_length, text_body_width),
				advice = "Widen the transcript, or shorten the word in ELAN.")
		}
	}
	out
}

.report_findings_info <- function(result) {
	out <- list()
	report <- attr(result, "anchor_report")
	if (!is.null(report) && nrow(report) > 0) {
		indented <- report[report$note == "aligned_by_indent", , drop = FALSE]
		for (row in unique(indented$row)) {
			out[[length(out) + 1]] <- list(
				id = "C1", title = "Verbal line indented to reach a symbol",
				row = row, char = NA_character_, occurrence = NA_integer_,
				tier = result$tierName[row],
				startsec = result$startsec[row], endsec = result$endsec[row],
				description = paste0("A symbol of this line could not be reached from the ",
				                     "left, so the verbal\n  line was indented until it lined ",
				                     "up."),
				advice = NULL)
		}
	}
	if (!is.null(report) && nrow(report) > 0) {
		wrapped <- report[report$note == "wrapped_to_span", , drop = FALSE]
		for (row in unique(wrapped$row)) {
			out[[length(out) + 1]] <- list(
				id = "C4", title = "Description longer than its span - wrapped early",
				row = row, char = NA_character_, occurrence = NA_integer_,
				tier = result$tierName[row],
				startsec = result$startsec[row], endsec = result$endsec[row],
				description = paste0("The description does not fit between its two marks in the ",
				                     "verbal line.
  It was broken early so its last line ends ",
				                     "exactly at the closing symbol."),
				advice = paste0("Nothing to fix in the layout. Consider shortening the ",
				                "description in ELAN
  if the extra lines disturb the score."))
		}
	}
	if (!is.null(result$lead_lines)) {
		for (row in seq_len(nrow(result))) {
			lead <- result$lead_lines[[row]]
			if (is.null(lead) || length(lead) == 0) next
			out[[length(out) + 1]] <- list(
				id = "C2", title = "Description placed above the verbal line",
				row = row, char = NA_character_, occurrence = NA_integer_,
				tier = result$tierName[row],
				startsec = result$startsec[row], endsec = result$endsec[row],
				description = paste0("This gesture started before the verbal annotation, so ",
				                     "its description\n  was placed above the verbal line."),
				advice = NULL)
		}
	}
	events <- attr(result, "merge_events")
	if (!is.null(events) && nrow(events) > 0 && any(events$merged)) {
		merged <- events[events$merged, , drop = FALSE]
		for (row in unique(merged$row)) {
			chars <- paste(unique(merged$char[merged$row == row]), collapse = " ")
			out[[length(out) + 1]] <- list(
				id = "C3", title = "Marks merged",
				row = row, char = NA_character_, occurrence = NA_integer_,
				tier = result$tierName[row],
				startsec = result$startsec[row], endsec = result$endsec[row],
				description = sprintf(paste0("Identical marks (%s) standing next to each other ",
				                             "were merged into one,\n  because they denote the ",
				                             "same moment."), chars),
				advice = NULL)
		}
	}
	if (!is.null(events) && nrow(events) > 0 && !is.null(events$vetoed) &&
	    any(events$vetoed)) {
		unfolded <- events[events$vetoed, , drop = FALSE]
		for (row in unique(unfolded$row)) {
			chars <- paste(unique(unfolded$char[unfolded$row == row]),
			               collapse = " ")
			out[[length(out) + 1]] <- list(
				id = "C5", title = "Marks unfolded at the line break",
				row = row, char = NA_character_, occurrence = NA_integer_,
				tier = result$tierName[row],
				startsec = result$startsec[row], endsec = result$endsec[row],
				description = sprintf(paste0("Marks (%s) that denote the same moment were kept ",
				                             "separate: their\n  descriptions end and start on ",
				                             "different lines, so the closing mark\n  stays with ",
				                             "its description and the opening mark starts the ",
				                             "next line."), chars),
				advice = NULL)
		}
	}
	out
}

# ---- formatting -----------------------------------------------------------

# Findings are grouped by the PRINTED verbal line they belong to, so one
# block is shown once with all its markers - matching how the transcript
# reads (user layout request 2026-08-14).

.report_format_blocks <- function(findings, result, plan) {
	is_main_line <- !is.na(plan$group) & plan$row == plan$group
	main_positions <- which(is_main_line)

	located <- lapply(findings, function(f) {
		spot <- .report_spot(f, result, plan)
		f$plan_index <- spot$plan_index
		f$column <- spot$column
		if (is.na(f$plan_index) || length(main_positions) == 0) {
			f$block_start <- NA_integer_
			f$block_end <- NA_integer_
			return(f)
		}
		earlier <- main_positions[main_positions <= f$plan_index]
		anchor <- if (length(earlier) > 0) max(earlier) else min(main_positions)
		f$block_start <- if (length(main_positions[main_positions < anchor]) == 0) {
			1L
		} else {
			anchor
		}
		later <- main_positions[main_positions > anchor]
		f$block_end <- if (length(later) == 0) nrow(plan) else later[1] - 1L
		f
	})

	keys <- vapply(located, function(f) {
		if (is.na(f$block_start)) "zz" else sprintf("%06d", f$block_start)
	}, character(1))

	lines <- character(0)
	for (key in unique(keys)) {
		group <- located[keys == key]
		first <- group[[1]]
		label <- .report_line_label(plan, first$block_start, first$block_end)
		lines <- c(lines,
			strrep("-", 78),
			sprintf("%s | %s - %s | %s",
			        label,
			        helper_format_time(first$startsec),
			        helper_format_time(first$endsec),
			        first$tier),
			strrep("-", 78))

		if (is.na(first$block_start)) {
			lines <- c(lines, paste0("  ", result$rendered_lines[[first$row]]))
		} else {
			for (index in first$block_start:first$block_end) {
				lines <- c(lines, paste0("  ", plan$line[index]))
				marks <- vapply(group, function(f) {
					if (!is.na(f$plan_index) && f$plan_index == index &&
					    !is.na(f$column)) f$column else NA_integer_
				}, integer(1))
				marker <- .report_marker_line(marks, seq_along(group))
				if (!is.null(marker)) lines <- c(lines, paste0("  ", marker))
			}
		}
		lines <- c(lines, "")
		for (k in seq_along(group)) {
			f <- group[[k]]
			lines <- c(lines,
				sprintf("  [%d] %s  %s", k, f$id, f$title),
				paste0("      ", gsub("\n  ", "\n      ", f$description)))
			if (!is.null(f$advice)) {
				lines <- c(lines, paste0("      -> ",
				                         gsub("\n    ", "\n         ", f$advice)))
			}
			lines <- c(lines, "")
		}
	}
	lines
}

.report_marker_line <- function(columns, numbers) {
	valid <- !is.na(columns)
	if (!any(valid)) return(NULL)
	columns <- columns[valid]
	numbers <- numbers[valid]
	order_index <- order(columns)
	columns <- columns[order_index]
	numbers <- numbers[order_index]
	marker <- character(0)
	position <- 1L
	for (k in seq_along(columns)) {
		if (columns[k] < position) next
		marker <- c(marker, strrep(" ", columns[k] - position),
		            "^", as.character(numbers[k]))
		position <- columns[k] + 1L + nchar(as.character(numbers[k]))
	}
	paste(marker, collapse = "")
}

.report_line_label <- function(plan, block_start, block_end) {
	if (is.na(block_start)) return("line -")
	for (index in block_start:block_end) {
		number <- stringr::str_extract(plan$line[index], "^\\s*[0-9]+")
		if (!is.na(number)) {
			return(paste("line", stringr::str_trim(number)))
		}
	}
	"line -"
}

# Position of a finding inside the printed plan: which plan line it sits on
# and at which column.
.report_spot <- function(finding, result, plan) {
	row <- finding$row
	line_index <- finding$line_index
	column <- NA_integer_
	if (!is.na(finding$char)) {
		positions <- extract_anchor_positions(result$rendered_lines[[row]],
		                                      finding$char)
		hit <- positions[positions$char == finding$char &
		                 positions$occurrence == finding$occurrence, , drop = FALSE]
		if (nrow(hit) > 0) {
			line_index <- hit$line[1]
			column <- hit$col[1]
		}
	}
	if (is.null(line_index) || is.na(line_index)) line_index <- 1L
	rows_in_plan <- which(plan$row == row)
	plan_index <- if (length(rows_in_plan) == 0) {
		NA_integer_
	} else {
		rows_in_plan[min(line_index, length(rows_in_plan))]
	}
	list(plan_index = plan_index, column = column)
}

.report_shorten <- function(x, n) {
	if (is.null(x) || length(x) == 0 || is.na(x)) return("")
	if (nchar(x) > n) paste0(substr(x, 1, n - 3), "...") else x
}


# ======================================================================
# ==== public API ====

.layout_filter_transcript <- function(t, l, filterTierNames,
                                      filterSectionStartsec,
                                      filterSectionEndsec) {
	if (is.null(filterTierNames)) {
		filterTierNames <- t@tiers$name
	}
	filterTierNames <- helper_tiers_filter_create(
		tierNames              = filterTierNames,
		filterTierIncludeRegEx = l@filter.tier.includeRegEx,
		filterTierExcludeRegEx = l@filter.tier.excludeRegEx)
	t <- transcripts_filter_single(t,
		filterTierNames       = filterTierNames,
		filterSectionStartsec = filterSectionStartsec,
		filterSectionEndsec   = filterSectionEndsec)
	transcripts_cure_single(t,
		annotationsTimesReversed  = TRUE,
		annotationsOverlap        = TRUE,
		annotationsTimesBelowZero = FALSE,
		transcriptLengthZero      = TRUE,
		annotationsZeroDuration   = TRUE,
		tiersMissing              = FALSE,
		warning                   = FALSE)
}

.layout_mode_of <- function(l) {
	if (identical(l@layout.mode, "mondada")) "mondada" else "gat"
}

.layout_frame <- function(t, l, layout_mode, timeToleranceGesture,
                          figReplace, figTierRegex) {
	prep <- prepare_annotations_new(t, l, layout_mode = layout_mode,
	                                fig_replace = figReplace,
	                                fig_tier_regex = figTierRegex)
	ann <- prep$engine_ann
	mm_anchor_chars <- unique(unlist(
		lapply(ann$align_chars[!is.na(ann$align_chars)],
		       function(x) strsplit(x, "")[[1]])))
	if (identical(layout_mode, "mondada")) {
		ann <- concatenate_mondada_rows(ann, mm_anchor_chars,
		                                layer_seam = timeToleranceGesture)
	} else {
		ann <- concatenate_mondada_rows(ann, mm_anchor_chars,
		                                layer_seam = timeToleranceGesture,
		                                pause_only = TRUE,
		                                pause_line_limit = prep$engine_width)
	}
	list(ann = ann, mm_anchor_chars = mm_anchor_chars, prep = prep)
}

.layout_assemble_lines <- function(plan, result, layout_mode) {
	mondada <- identical(layout_mode, "mondada")
	lines <- character(0)
	previous_main_row <- NA_integer_
	emitted_any <- FALSE
	for (p in seq_len(nrow(plan))) {
		row_p <- plan$row[p]
		if (mondada) {
			if (emitted_any && isTRUE(result$is_main[row_p]) &&
			    isTRUE(result$number_lines[row_p])) {
				lines <- c(lines, "")
			}
		} else if (emitted_any && isTRUE(result$is_main[row_p]) &&
		           !identical(row_p, previous_main_row)) {
			lines <- c(lines, "")
		}
		if (isTRUE(result$show[row_p])) {
			lines <- c(lines, plan$line[p])
			if (isTRUE(result$is_main[row_p])) previous_main_row <- row_p
			emitted_any <- TRUE
		}
	}
	if (emitted_any) lines <- c(lines, "")
	lines
}

#' Render a transcript with the alignment engine
#'
#' Renders a single transcript into aligned print-transcript lines using the
#' wrap-aware alignment engine. The rendering mode comes from the slot
#' \code{layout.mode} of the layout object (\code{"gat"}: one line per
#' annotation; \code{"mondada"}: score layout), the folding of adjacent
#' identical marks from the slot \code{symbol.merge}.
#'
#' @param t Transcript object.
#' @param l Layout object; if \code{NULL} a default layout is used.
#' @param filterTierNames Vector of character strings; names of the tiers to include.
#' @param filterSectionStartsec Numeric; start of a section in seconds.
#' @param filterSectionEndsec Numeric; end of a section in seconds.
#' @param timeTolerancePoint Numeric; up to this distance in seconds two point marks (stills) count as the same moment.
#' @param timeToleranceGesture Numeric; up to this distance in seconds two span marks (gestures) count as the same moment.
#' @param layerOrder Vector of character strings; order of the multimodal layers within a block. \code{NULL} keeps the tier order of the annotation file.
#' @param minDescription Integer; minimum room in characters a description needs before the verbal line breaks early.
#' @param maxSpanBlocks Integer; maximum number of blocks a description may span before it is cut with a resume arrow.
#' @param figReplace Logical; if \code{TRUE} the content of picture tiers is replaced by a number mark.
#' @param figTierRegex Character string; regular expression identifying picture tiers.
#'
#' @return List with the rendered \code{lines}, the line \code{plan}, the
#' engine \code{result} frame, the filtered \code{transcript}, the
#' \code{engineWidth} and the \code{layoutMode}.
#'
#' @export
helper_layout_render <- function(t,
                                 l                     = NULL,
                                 filterTierNames       = NULL,
                                 filterSectionStartsec = NULL,
                                 filterSectionEndsec   = NULL,
                                 timeTolerancePoint    = 0.2,
                                 timeToleranceGesture  = 0.5,
                                 layerOrder            = NULL,
                                 minDescription        = 10L,
                                 maxSpanBlocks         = 3L,
                                 figReplace            = TRUE,
                                 figTierRegex          = "^stills(#|$)") {
	if (is.null(l)) l <- methods::new("layout")
	layout_mode <- .layout_mode_of(l)
	label_mode <- getOption("act.layout.label.mode", "mondada")
	if (!identical(label_mode, "always")) label_mode <- "mondada"
	wrap_marker <- getOption("act.layout.wrap.marker", "mondada")
	if (!identical(wrap_marker, "arrow")) wrap_marker <- "mondada"

	t <- .layout_filter_transcript(t, l, filterTierNames,
	                               filterSectionStartsec, filterSectionEndsec)
	if (nrow(t@annotations) == 0) {
		return(list(lines = character(0), plan = NULL, result = NULL,
		            transcript = t, engineWidth = NA_integer_,
		            layoutMode = layout_mode))
	}
	prep <- prepare_annotations_new(t, l, layout_mode = layout_mode,
	                                fig_replace = figReplace,
	                                fig_tier_regex = figTierRegex)
	result <- align_and_render(prep$engine_ann, prep$engine_width,
	                           arrow_mode = prep$arrow_mode,
	                           verbal_align = isTRUE(l@brackets.align),
	                           layout_mode = layout_mode,
	                           symbol_merge = isTRUE(l@symbol.merge),
	                           time_tolerance = timeToleranceGesture,
	                           time_tolerance_point = timeTolerancePoint,
	                           min_description = minDescription)
	plan <- interleave_layer_lines(result, maxSpanBlocks,
	                               text_width = prep$engine_width,
	                               embed_overlaps = identical(layout_mode, "mondada"),
	                               label_mode = label_mode,
	                               layer_order = layerOrder,
	                               wrap_marker = wrap_marker)
	if (identical(layout_mode, "mondada")) {
		plan <- apply_mondada_line_numbers(plan, result,
		                                   offset = prep$number_offset,
		                                   slot_width = prep$number_width)
	}
	lines <- .layout_assemble_lines(plan, result, layout_mode)
	list(lines = lines, plan = plan, result = result, transcript = t,
	     engineWidth = prep$engine_width, layoutMode = layout_mode)
}

#' Anchor positions of a rendered transcript
#'
#' Returns the anchor specification of every rendered row: which symbol of
#' which layer row targets which column and line of its verbal source row.
#' Intended for viewers that draw or edit aligned transcripts.
#'
#' @inheritParams helper_layout_render
#'
#' @return Data.frame with one row per anchored symbol (columns include
#' \code{row}, \code{tierName}, \code{char}, \code{occurrence},
#' \code{target_col}, \code{target_line}, \code{source_row}, \code{type}).
#'
#' @export
helper_layout_anchors <- function(t,
                                  l                     = NULL,
                                  filterTierNames       = NULL,
                                  filterSectionStartsec = NULL,
                                  filterSectionEndsec   = NULL,
                                  timeTolerancePoint    = 0.2,
                                  timeToleranceGesture  = 0.5,
                                  layerOrder            = NULL,
                                  minDescription        = 10L,
                                  maxSpanBlocks         = 3L,
                                  figReplace            = TRUE,
                                  figTierRegex          = "^stills(#|$)") {
	rendered <- helper_layout_render(t, l,
		filterTierNames       = filterTierNames,
		filterSectionStartsec = filterSectionStartsec,
		filterSectionEndsec   = filterSectionEndsec,
		timeTolerancePoint    = timeTolerancePoint,
		timeToleranceGesture  = timeToleranceGesture,
		layerOrder            = layerOrder,
		minDescription        = minDescription,
		maxSpanBlocks         = maxSpanBlocks,
		figReplace            = figReplace,
		figTierRegex          = figTierRegex)
	result <- rendered$result
	empty <- data.frame(row = integer(0), tierName = character(0),
	                    char = character(0), occurrence = integer(0),
	                    target_col = integer(0), target_line = integer(0),
	                    source_row = integer(0), type = character(0),
	                    stringsAsFactors = FALSE)
	if (is.null(result)) return(empty)
	collected <- list()
	for (i in seq_len(nrow(result))) {
		spec <- result$anchor_specs[[i]]
		if (is.null(spec) || nrow(spec) == 0) next
		spec$row <- i
		spec$tierName <- result$tierName[i]
		collected[[length(collected) + 1]] <- spec
	}
	if (length(collected) == 0) return(empty)
	do.call(rbind, collected)
}

#' Overlap bracket pairs of a transcript
#'
#' Computes which opening and closing overlap brackets of the verbal tiers
#' belong together, without rendering. Row indices refer to the engine
#' frame returned in the \code{ann} element.
#'
#' @inheritParams helper_layout_render
#'
#' @return List with the engine frame \code{ann} and the data.frame
#' \code{pairs} (columns \code{i_row}, \code{i_occurrence},
#' \code{j_row}, \code{j_occurrence}).
#'
#' @export
helper_layout_bracket_pairs <- function(t,
                                        l                     = NULL,
                                        filterTierNames       = NULL,
                                        filterSectionStartsec = NULL,
                                        filterSectionEndsec   = NULL,
                                        timeToleranceGesture  = 0.5,
                                        figReplace            = TRUE,
                                        figTierRegex          = "^stills(#|$)") {
	if (is.null(l)) l <- methods::new("layout")
	layout_mode <- .layout_mode_of(l)
	t <- .layout_filter_transcript(t, l, filterTierNames,
	                               filterSectionStartsec, filterSectionEndsec)
	if (nrow(t@annotations) == 0) {
		return(list(ann = NULL,
		            pairs = data.frame(i_row = integer(0),
		                               i_occurrence = integer(0),
		                               j_row = integer(0),
		                               j_occurrence = integer(0))))
	}
	frame <- .layout_frame(t, l, layout_mode, timeToleranceGesture,
	                       figReplace, figTierRegex)
	list(ann = frame$ann, pairs = compute_bracket_pairs(frame$ann))
}

#' Multimodal symbol matches of a transcript
#'
#' Matches every multimodal layer symbol to its verbal main symbol by time,
#' without rendering. Row indices refer to the engine frame returned in the
#' \code{ann} element.
#'
#' @inheritParams helper_layout_render
#'
#' @return List with the engine frame \code{ann} and the data.frame
#' \code{matches} (columns \code{layer_row}, \code{char},
#' \code{layer_occurrence}, \code{main_row}, \code{main_occurrence}).
#'
#' @export
helper_layout_symbol_matches <- function(t,
                                         l                     = NULL,
                                         filterTierNames       = NULL,
                                         filterSectionStartsec = NULL,
                                         filterSectionEndsec   = NULL,
                                         timeToleranceGesture  = 0.5,
                                         figReplace            = TRUE,
                                         figTierRegex          = "^stills(#|$)") {
	if (is.null(l)) l <- methods::new("layout")
	layout_mode <- .layout_mode_of(l)
	t <- .layout_filter_transcript(t, l, filterTierNames,
	                               filterSectionStartsec, filterSectionEndsec)
	if (nrow(t@annotations) == 0) {
		return(list(ann = NULL,
		            matches = data.frame(layer_row = integer(0),
		                                 char = character(0),
		                                 layer_occurrence = integer(0),
		                                 main_row = integer(0),
		                                 main_occurrence = integer(0))))
	}
	frame <- .layout_frame(t, l, layout_mode, timeToleranceGesture,
	                       figReplace, figTierRegex)
	ref_main <- resolve_reference_main(frame$ann)
	list(ann = frame$ann,
	     matches = compute_mm_symbol_matches(frame$ann, ref_main))
}

# Collected end-of-export warnings (mixed wrap, unmatched symbols) - one
# block per affected annotation, with times, problem description and an
# instruction what to do (plan decision 12, block format 2026-08-10).
report_render_warnings <- function(result, transcript_name = "") {
	warnings <- attr(result, "render_warnings")
	if (is.null(warnings) || length(warnings) == 0) return(invisible(NULL))

	cli::cli_alert_warning(
		"{length(warnings)} alignment anchor{?s} could not be aligned in transcript {transcript_name}:")

	rows <- vapply(warnings, function(w) w$row, numeric(1))
	for (row in unique(rows)) {
		entries <- warnings[rows == row]
		first <- entries[[1]]
		cli::cli_code(" ")
		cli::cli_verbatim(sprintf("  %s: \"%s\"",
			first$tier, .shorten_warning_text(first$content, 58)))
		cli::cli_verbatim(sprintf("  layer time:  %s - %s",
			helper_format_time(first$startsec),
			helper_format_time(first$endsec)))
		if (!is.null(first$main_tier) && !is.na(first$main_tier)) {
			cli::cli_verbatim(sprintf("  main tier:   %s: \"%s\"  (%s - %s)",
				first$main_tier,
				.shorten_warning_text(first$main_content, 45),
				helper_format_time(first$main_startsec),
				helper_format_time(first$main_endsec)))
		}

		kinds <- vapply(entries, function(w) w$kind, character(1))
		if (any(kinds == "unmatched_symbol")) {
			unmatched <- entries[kinds == "unmatched_symbol"]
			symbol_chars <- vapply(unmatched, function(w) w$char, character(1))
			occurrences <- vapply(unmatched, function(w) w$occurrence, numeric(1))
			for (symbol_char in unique(symbol_chars)) {
				occ_label <- paste(sort(occurrences[symbol_chars == symbol_char]),
				                   collapse = ", ")
				cli::cli_verbatim(sprintf(
					"  problem:     symbol \"%s\" (occurrence %s) has no matching \"%s\" in the main annotation",
					symbol_char, occ_label, symbol_char))
			}
			cli::cli_verbatim(paste0(
				"  what to do:  Check this spot in ELAN: either the matching symbol is\n",
				"               missing in the main annotation, or the layer annotation\n",
				"               has too many symbols / belongs to a different turn."))
		}
		if (any(kinds == "mixed_wrap")) {
			mixed <- entries[kinds == "mixed_wrap"]
			for (w in mixed) {
				cli::cli_verbatim(sprintf(
					"  problem:     target column %d for symbol \"%s\" lies beyond the line width",
					w$target_col, w$char))
			}
			cli::cli_verbatim(paste0(
				"  what to do:  Enable wrapping for this tier (content.wrap) or\n",
				"               increase the transcript width."))
		}
	}
	cli::cli_code(" ")
	invisible(NULL)
}

.shorten_warning_text <- function(x, n) {
	if (is.null(x) || is.na(x)) return("")
	if (nchar(x) > n) paste0(substr(x, 1, n - 3), "...") else x
}
