# Open-ended Text Insights (Any file -> Claude / Anthropic)
# MULTI-LABEL + DYNAMIC THEME DISCOVERY + THREE ANALYSIS MODES + THEME BANK LOCK + MERGE UI + TAG MATRIX
#
# ANALYSIS MODES (the "JSON scripts") - pick per run, all run on the SAME responses:
#   1) attribute    Evaluative questions ("why did you choose / what's beneficial / why did you stop")
#                   -> theme + real sentiment, up to 5 themes/response  (original behaviour)
#   2) exploratory  Definitional questions ("what does 'wealth platform' mean to you?"), SINGLE-PASS
#                   -> broad conceptual dimensions, aggressive synonym merging, sentiment de-emphasised,
#                      max 2 dimensions/response, emergent bank
#   3) codebook     Definitional questions, TWO-PASS (recommended for meaning questions)
#                   -> Stage 1: induce a compact fixed taxonomy (~6-12 dimensions + definitions) from a sample
#                      Stage 2: classify every response against that LOCKED codebook (no new categories)
#                      This removes the sequential-fragmentation problem of exploratory mode.
#   4) opinion      Open OPINION questions ("what do you think about X / what do you mean by that"),
#                   SENTIMENT-FIRST, MULTI-STAGE:
#                   -> Stage 1: split each answer into segments; label EACH segment positive / negative /
#                      neutral with a rationale + evidence quote (one answer can span several sentiments).
#                   -> Stage 2: WITHIN each sentiment bucket, induce a separate class -> subclass framework
#                      (what the positives are happy about; what the negatives want improved; etc.).
#                   -> Stage 3: code every segment against its bucket's framework (class + subclass).
#                      ADAPTIVE COVERAGE: segments that don't fit go to an "unclassified" pool; if that
#                      pool is a fair share (>=5% and >=8 segments), NEW classes/subclasses are induced
#                      from it and the pool is re-coded (up to 2 rounds). Only a tiny true residual is
#                      labelled "Other / Unclassified". No valid segment is ever dropped to NA.
#                   -> Stage 4: write "the message" (headline + summary + implication) for each class,
#                      AND attach the 3 most informative RAW verbatim respondent responses per class
#                      (column 'raw_examples' in opinion_classes.csv; each quoted, newline-separated).
#                   ALSO: each RESPONSE is scored on the PAD affect model (Pleasure / Arousal /
#                   Dominance), each in [-1, 1], in the SAME Stage-1 call (no extra cost). The
#                   'PAD space (3D)' tab shows a 3D kernel-density view of the sample distribution.
#                   See the 'Opinion Report' and 'PAD space (3D)' tabs. Needs an API key.
#
# Auto-saved CSVs are suffixed by mode so runs don't overwrite each other. Codebook mode also writes codebook_<mode>.csv.
#
# API: Anthropic Messages API (https://api.anthropic.com/v1/messages)
#  - Auth header: x-api-key ; required headers: anthropic-version, content-type
#  - system prompt is a TOP-LEVEL field ; max_tokens is REQUIRED ; no response_format
#  - Set your key in .Renviron as ANTHROPIC_API_KEY (usethis::edit_r_environ())
#  - API credits are prepaid and SEPARATE from Claude Pro (Plans & Billing in the console)
#
# Author: Investment Trends / @s.aranya

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(haven)
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(readr); library(httr2); library(jsonlite); library(ggplot2)
  library(plotly); library(lubridate); library(tibble); library(rlang)
  library(tidytext); library(digest)
  library(shinybusy); library(shinycssloaders)
  library(scales)
})

if (!requireNamespace("readxl", quietly = TRUE)) {
  message("Package 'readxl' not installed. Install it for .xlsx support: install.packages('readxl')")
}
HAS_CLD3 <- requireNamespace("cld3", quietly = TRUE)

options(shiny.maxRequestSize = 500*1024^2)

# ---------------------------- Config ----------------------------
DEFAULT_THEME_BANK <- character(0)
SENTIMENT_LEVELS <- c("satisfied","neutral","need_to_improve")
DEFAULT_MODEL <- "claude-haiku-4-5-20251001"

MAX_THEMES_ATTRIBUTE   <- 5
MAX_THEMES_EXPLORATORY <- 3
MAX_THEMES_CODEBOOK    <- 3
MAX_THEMES_PER_RESPONSE <- MAX_THEMES_ATTRIBUTE  # legacy default

# Codebook induction settings
CODEBOOK_SAMPLE_N <- 250      # max responses sampled to induce the codebook
CODEBOOK_TEXT_TRUNC <- 300    # truncate each sampled response to this many chars
CODEBOOK_TARGET_DIM_DEFAULT <- 10

# ---- Opinion mode (sentiment-first) settings ----
# Literal sentiment vocabulary used ONLY by opinion mode. Kept separate from
# SENTIMENT_LEVELS so the rest of the app is untouched.
OPINION_SENTIMENT_LEVELS <- c("positive","negative","neutral")
# Human-readable labels for the three buckets in reports.
OPINION_SENTIMENT_LABELS <- c(positive = "Positive", negative = "Negative", neutral = "Neutral")
MAX_SEGMENTS_PER_RESPONSE   <- 6   # max sentiment segments extracted from one answer
MAX_CLASSES_PER_SEGMENT     <- 1   # each segment gets its single best class
OPINION_CLASS_TARGET_DEFAULT <- 6  # target parent classes induced per sentiment bucket

# ---- Adaptive coverage (opinion mode) ----
# Nothing valid is dropped to NA. Segments the model can't place go into an
# "unclassified" pool; if that pool is a fair share, NEW classes/subclasses are
# induced from it and the pool is re-coded. Only a tiny true residual becomes "Other".
OPINION_OTHER_LABEL   <- "Other / Unclassified"
OPINION_GAP_MIN_SHARE <- 0.05   # induce new classes if unclassified >= 5% of a bucket
OPINION_GAP_MIN_N     <- 8      # ...and at least this many segments
OPINION_MAX_GAP_ROUNDS <- 2     # how many times to grow the framework then re-code
OPINION_GAP_TARGET_N  <- 3      # target new classes to induce per gap round
OPINION_SNAP_MAX_RATIO <- 0.34  # fuzzy snap tolerance (normalised edit distance)
# Map opinion sentiment -> the app's legacy 3-level scheme so the Overview/Data
# tabs (bubble, bars, metrics) keep working when opinion mode is run.
opinion_to_legacy_sentiment <- function(s) {
  dplyr::case_when(
    s == "positive" ~ "satisfied",
    s == "negative" ~ "need_to_improve",
    TRUE            ~ "neutral"
  )
}
opinion_sentiment_score <- function(s) {
  dplyr::case_when(s == "positive" ~ 3L, s == "negative" ~ -3L, TRUE ~ 0L)
}

# ---- PAD affect model (per-response, each dimension scaled to [-1, 1]) ----
PAD_DIMS <- c("pleasure","arousal","dominance")
PAD_GRID_N_DEFAULT   <- 16     # resolution of the 3D density grid per axis
PAD_BANDWIDTH_DEFAULT <- 0.25  # Gaussian KDE bandwidth (in the [-1,1] space)
PAD_MAX_POINTS_KDE   <- 1500   # cap points used for the KDE (speed)

clamp11 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  pmax(-1, pmin(1, x))
}

# Hand-rolled 3D Gaussian kernel-density estimate over [-1,1]^3.
# Returns a long data.frame (px, ay, dz, dens) suitable for a plotly isosurface.
pad_density_grid <- function(X, grid_n = PAD_GRID_N_DEFAULT, h = PAD_BANDWIDTH_DEFAULT) {
  X <- as.matrix(X)
  X <- X[stats::complete.cases(X), , drop = FALSE]
  if (nrow(X) < 5) return(NULL)
  if (nrow(X) > PAD_MAX_POINTS_KDE) { set.seed(11); X <- X[sample(nrow(X), PAD_MAX_POINTS_KDE), , drop = FALSE] }
  h <- max(as.numeric(h), 0.05)
  ax <- seq(-1, 1, length.out = grid_n)
  dens <- array(0, dim = c(grid_n, grid_n, grid_n))
  kx_all <- outer(ax, X[, 1], function(g, x) dnorm((g - x) / h))  # grid_n x N
  ky_all <- outer(ax, X[, 2], function(g, x) dnorm((g - x) / h))
  kz_all <- outer(ax, X[, 3], function(g, x) dnorm((g - x) / h))
  N <- nrow(X)
  for (i in seq_len(N)) {
    dens <- dens + outer(outer(kx_all[, i], ky_all[, i]), kz_all[, i])
  }
  dens <- dens / (N * h^3)
  g <- expand.grid(px = ax, ay = ax, dz = ax)
  g$dens <- as.vector(dens)
  g
}

ANTHROPIC_URL <- "https://api.anthropic.com/v1/messages"
ANTHROPIC_VERSION <- "2023-06-01"

MODEL_CHOICES <- c(
  "Haiku 4.5 (fast, cheap)"  = "claude-haiku-4-5-20251001",
  "Sonnet 5 (balanced)"      = "claude-sonnet-5",
  "Opus 4.8 (max reasoning)" = "claude-opus-4-8"
)

MODE_CHOICES <- c(
  "Attribute / evaluative (with sentiment)"            = "attribute",
  "Exploratory / conceptual (single-pass)"             = "exploratory",
  "Codebook (two-stage, fixed taxonomy)"               = "codebook",
  "Opinion (sentiment-first, class -> subclass)"       = "opinion"
)

`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a

to_chr <- function(x) {
  out <- if (inherits(x, "POSIXt")) format(x, "%Y-%m-%d %H:%M:%S") else as.character(x)
  out[is.na(x)] <- NA_character_
  out
}

log_msg <- function(...) {
  message(sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste(..., collapse = " ")))
}

# Resolve (and create) the folder where auto-saved outputs are written.
# Blank -> "analysis_output" under the current working directory. "~" is expanded.
DEFAULT_OUTPUT_DIR <- file.path(getwd(), "analysis_output")
resolve_out_dir <- function(path) {
  path <- trimws(as.character(path %||% ""))
  if (!nzchar(path)) path <- DEFAULT_OUTPUT_DIR
  path <- path.expand(path)
  ok <- tryCatch({ dir.create(path, showWarnings = FALSE, recursive = TRUE); dir.exists(path) },
                 error = function(e) FALSE)
  if (!ok) {
    log_msg("Could not create output dir:", path, "- falling back to", DEFAULT_OUTPUT_DIR)
    path <- DEFAULT_OUTPUT_DIR
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
  path
}

# ---------- Anthropic helpers ----------
anthropic_text <- function(content) {
  blocks <- content$content
  if (is.null(blocks) || !length(blocks)) return("")
  txt <- vapply(blocks, function(b) {
    if (!is.null(b$type) && identical(b$type, "text")) as.character(b$text %||% "") else ""
  }, character(1))
  paste(txt, collapse = "")
}

parse_model_json <- function(txt, simplify = TRUE) {
  if (is.null(txt) || !nzchar(txt)) return(NULL)
  clean <- txt
  clean <- gsub("```json", "", clean, fixed = TRUE)
  clean <- gsub("```",     "", clean, fixed = TRUE)
  clean <- trimws(clean)
  fj <- function(s) tryCatch(jsonlite::fromJSON(s, simplifyVector = simplify,
                                                simplifyDataFrame = simplify,
                                                simplifyMatrix = simplify), error = function(e) NULL)
  out <- fj(clean)
  if (!is.null(out)) return(out)
  starts <- gregexpr("\\{", clean)[[1]]
  ends   <- gregexpr("\\}", clean)[[1]]
  if (length(starts) && starts[1] > 0 && length(ends) && ends[1] > 0) {
    sub <- substr(clean, starts[1], ends[length(ends)])
    out <- fj(sub)
  }
  out
}

anthropic_perform <- function(req, tag = "request") {
  req <- req |> req_error(is_error = function(resp) FALSE)
  resp <- tryCatch(req_perform(req), error = function(e) {
    log_msg("Anthropic", tag, "network error:", conditionMessage(e)); NULL
  })
  if (is.null(resp)) return(NULL)
  st <- resp_status(resp)
  if (st >= 400) {
    body <- tryCatch(resp_body_string(resp), error = function(e) "")
    log_msg("Anthropic", tag, "HTTP", st, "-", substr(body, 1, 300))
    return(NULL)
  }
  tryCatch(resp_body_json(resp), error = function(e) {
    log_msg("Anthropic", tag, "body parse error:", conditionMessage(e)); NULL
  })
}

anthropic_chat_json <- function(api_key, sys_prompt, u_prompt, model = DEFAULT_MODEL,
                                max_tokens = 1500L, temperature = NULL, tag = "request",
                                simplify = TRUE) {
  if (is.null(api_key) || !nzchar(api_key)) return(NULL)
  # NOTE: `temperature` is intentionally NOT sent. Newer Claude models reject it
  # ("temperature is deprecated for this model" -> HTTP 400). The argument is kept
  # only for backwards compatibility with existing call sites and is ignored.
  body <- list(
    model = model, max_tokens = max_tokens,
    system = sys_prompt,
    messages = list(list(role = "user", content = u_prompt))
  )
  req <- request(ANTHROPIC_URL) |>
    req_headers("x-api-key" = api_key, "anthropic-version" = ANTHROPIC_VERSION,
                "content-type" = "application/json") |>
    req_options(timeout = 120) |>
    req_body_json(body)
  content <- anthropic_perform(req, tag = tag)
  if (is.null(content)) return(NULL)
  parse_model_json(anthropic_text(content), simplify = simplify)
}

esc_html <- function(x) {
  x <- as.character(x %||% ""); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}

normalise_text <- function(x) {
  x <- as.character(x %||% ""); x[is.na(x)] <- ""
  x <- tolower(x); x <- gsub("’|‘|`", "'", x)
  x <- gsub("[^a-z0-9\\s]", " ", x); x <- stringr::str_squish(x); x
}

canonical_theme <- function(x) {
  x <- as.character(x %||% ""); x[is.na(x)] <- ""
  x <- gsub("[\r\n\t]+", " ", x); x <- stringr::str_squish(x)
  x <- stringr::str_to_lower(x); x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_trim(x); x <- stringr::str_to_title(x)
  x[!nzchar(x)] <- ""; x
}

map_to_existing_theme <- function(theme_vec, theme_bank) {
  theme_vec <- as.character(theme_vec %||% character(0)); theme_vec[is.na(theme_vec)] <- ""
  if (!length(theme_vec)) return(character(0))
  if (!length(theme_bank)) return(canonical_theme(theme_vec))
  theme_bank <- canonical_theme(theme_bank)
  bank_norm <- normalise_text(theme_bank)
  bank_map <- stats::setNames(theme_bank, bank_norm)
  vapply(theme_vec, function(th) {
    key <- normalise_text(th)
    if (nzchar(key) && key %in% names(bank_map)) bank_map[[key]] else canonical_theme(th)
  }, character(1))
}

dedupe_theme_rows <- function(theme_df) {
  if (is.null(theme_df) || !nrow(theme_df)) return(tibble())
  theme_df %>%
    mutate(
      theme = canonical_theme(theme),
      sentiment_score = suppressWarnings(as.integer(sentiment_score)),
      sentiment_score = if_else(is.na(sentiment_score), 0L, sentiment_score),
      sentiment_score = pmax(-5L, pmin(5L, sentiment_score)),
      is_new_theme = suppressWarnings(as.integer(is_new_theme)),
      is_new_theme = if_else(is.na(is_new_theme), 0L, is_new_theme),
      sentiment = if_else(sentiment %in% SENTIMENT_LEVELS, sentiment, "neutral"),
      evidence = as.character(evidence %||% ""),
      rationale = as.character(rationale %||% "")
    ) %>%
    filter(!is.na(theme), theme != "") %>%
    arrange(response_id, respondent_id, theme, desc(abs(sentiment_score))) %>%
    group_by(respondent_id, response_id, theme) %>%
    slice_head(n = 1) %>%
    ungroup()
}

# ---------- File loader ----------
read_any_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "sav") return(as_tibble(haven::read_sav(path)))
  if (ext == "csv") return(as_tibble(readr::read_csv(path, show_col_types = FALSE, progress = FALSE)))
  if (ext %in% c("xlsx","xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl not installed. install.packages('readxl')")
    return(as_tibble(readxl::read_excel(path)))
  }
  stop("Unsupported file type. Please upload .sav, .csv, or .xlsx")
}

# ---------- Prep ----------
prepare_single_responses <- function(dat, id_var, text_var, date_var = NULL,
                                     extra1_var = NULL, extra2_var = NULL) {
  stopifnot(all(c(id_var, text_var) %in% names(dat)))
  out <- dat %>%
    transmute(
      respondent_id = .data[[id_var]],
      response_text = to_chr(.data[[text_var]]),
      extra_1 = if (!is.null(extra1_var) && nzchar(extra1_var) && extra1_var %in% names(dat)) to_chr(.data[[extra1_var]]) else NA_character_,
      extra_2 = if (!is.null(extra2_var) && nzchar(extra2_var) && extra2_var %in% names(dat)) to_chr(.data[[extra2_var]]) else NA_character_
    )
  if (!is.null(date_var) && nzchar(date_var) && date_var %in% names(dat)) {
    out <- out %>% mutate(raw_date = to_chr(.data[[date_var]]))
  } else out <- out %>% mutate(raw_date = NA_character_)
  out %>%
    mutate(
      response_text = na_if(trimws(response_text), ""),
      response_text = na_if(response_text, "NULL"),
      response_text = na_if(response_text, "NILL"),
      response_text = na_if(response_text, "NA")
    ) %>%
    filter(!is.na(response_text), nzchar(response_text))
}

# ---------- Offline dynamic fallback ----------
fallback_multi_dynamic <- function(text, theme_bank = character(0), allow_new_themes = TRUE,
                                   question_context = NULL, max_themes = MAX_THEMES_PER_RESPONSE) {
  t <- tolower(text %||% "")
  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  useless_terms <- c("na","n/a","nil","none","nothing","no comment","can't say","cant say",
                     "dont know","do not know","idk","-",".","no idea","blank")
  flag_useless <- as.integer(nchar(str_squish(t)) < 5 ||
                               any(str_detect(t, paste0("\\b(", paste(useless_terms, collapse="|"), ")\\b"))))
  if (flag_useless == 1L) return(list(themes = tibble(), flag_useless = 1L, discovery = 0L, new_themes = character(0)))
  
  clauses <- unlist(str_split(text, "\\.|;|\\band\\b|\\bbut\\b|\\balso\\b"))
  clauses <- str_squish(clauses); clauses <- clauses[nzchar(clauses)]
  stop_words_vec <- unique(stop_words$word)
  survey_generic_terms <- c("question","survey","response","comment","comments","feedback",
                            "respondent","respondents","overall","experience","what","which","how","why","when","where","who",
                            "does","do","did","is","are","was","were","can","could","would","should","please","tell","describe","about","regarding")
  question_tokens <- unlist(str_split(normalise_text(question_context), "\\s+"))
  question_tokens <- question_tokens[nzchar(question_tokens)]
  question_tokens <- question_tokens[!(question_tokens %in% c(stop_words_vec, survey_generic_terms))]
  question_tokens <- question_tokens[nchar(question_tokens) >= 4]
  
  build_theme_from_clause <- function(cl) {
    cln <- normalise_text(cl); toks <- unlist(str_split(cln, "\\s+"))
    toks <- toks[nzchar(toks)]; toks <- toks[!(toks %in% stop_words_vec)]; toks <- toks[nchar(toks) >= 4]
    if (length(toks) < 2 && length(question_tokens)) toks <- unique(c(toks, head(question_tokens, 2)))
    if (!length(toks)) return("")
    canonical_theme(paste(head(unique(toks), 2), collapse = " "))
  }
  theme_candidates <- vapply(clauses, build_theme_from_clause, character(1))
  theme_candidates <- theme_candidates[nzchar(theme_candidates)]
  if (!length(theme_candidates)) theme_candidates <- "General Feedback"
  if (length(theme_bank)) theme_candidates <- map_to_existing_theme(theme_candidates, theme_bank)
  if (!allow_new_themes && length(theme_bank)) {
    keep <- normalise_text(theme_candidates) %in% normalise_text(theme_bank)
    theme_candidates <- theme_candidates[keep]
  }
  theme_candidates <- unique(theme_candidates); theme_candidates <- head(theme_candidates, max_themes)
  if (!length(theme_candidates)) return(list(themes = tibble(), flag_useless = 0L, discovery = 0L, new_themes = character(0)))
  
  pos_words <- c("great","easy","low","good","excellent","reliable","fast","helpful","responsive",
                 "friendly","reasonable","clean","smooth","simple","clear")
  neg_words <- c("high","poor","slow","limited","crash","bug","issue","confusing","down",
                 "expensive","bad","worse","difficult","hard","problem","delay")
  tokens <- unlist(str_split(normalise_text(text), "\\s+"))
  pos <- sum(tokens %in% pos_words); neg <- sum(tokens %in% neg_words)
  ratio <- (pos - neg) / max(pos + neg, 1)
  sentiment <- ifelse(ratio >= 0.2, "satisfied", ifelse(ratio <= -0.2, "need_to_improve", "neutral"))
  score <- max(-5, min(5, round(ratio * 5)))
  is_new <- if (length(theme_bank)) as.integer(!(normalise_text(theme_candidates) %in% normalise_text(theme_bank))) else rep(1L, length(theme_candidates))
  
  themes <- tibble(
    theme = theme_candidates, sentiment = sentiment, sentiment_score = as.integer(score),
    is_new_theme = is_new, evidence = "",
    rationale = if_else(nzchar(question_context), "Offline dynamic heuristic + question context", "Offline dynamic heuristic")
  )
  list(themes = themes, flag_useless = 0L,
       discovery = as.integer(any(themes$is_new_theme == 1L)),
       new_themes = unique(themes$theme[themes$is_new_theme == 1L]))
}

# ---------- System-prompt builders ----------
build_sys_prompt_attribute <- function(question_block, bank_text, allow_new_themes, max_themes) {
  paste0(
    "You are an analyst coding survey comments into THEMES with per-theme sentiment.\n",
    "A single comment may contain MULTIPLE distinct attributes.\n",
    "Your output must be HIGH RECALL and EVIDENCE-ANCHORED.\n\n",
    "SURVEY QUESTION / ANALYSIS CONTEXT:\n", question_block, "\n\n",
    "Use the survey question context to interpret ambiguous or shorthand comments.\n",
    "However, NEVER create a theme from the question alone. Every theme must be grounded in the comment.\n\n",
    "Return STRICT JSON with keys: themes, flag_useless, discovery, new_themes.\n",
    "themes is an ARRAY (0..", max_themes, ") of objects with keys:\n",
    "  theme (string)\n  sentiment (one of: satisfied, neutral, need_to_improve)\n",
    "  sentiment_score (INTEGER in [-5,5])\n  is_new_theme (0/1)\n",
    "  evidence (EXACT quote from the comment, <=12 words)\n  rationale (<=12 words)\n\n",
    "EXISTING THEME BANK:\n", bank_text, "\n\n",
    "==================== WORKING METHOD ====================\n",
    "Step 1 - Split the comment into ATOMIC CLAUSES (one distinct idea, feature, issue, or benefit each).\n",
    "Step 2 - Assign EACH clause to ONE concise canonical theme (short noun phrase, 1-4 words). No sentence-like or micro-variant labels.\n",
    "Step 3 - Reuse the existing theme bank whenever a clause matches. Use the bank theme EXACTLY. No synonyms/spelling/tense/plural variants of a bank theme.\n",
    if (allow_new_themes) {
      "Step 4 - New themes allowed ONLY when no bank theme fits. Make them portable/canonical. Good: Withdrawal Speed, Research Quality. Bad: The App Keeps Crashing.\n\n"
    } else {
      "Step 4 - NEW THEMES ARE NOT ALLOWED. Use ONLY bank themes; map to the closest.\n\n"
    },
    "==================== SENTIMENT ====================\n",
    " - satisfied: clearly positive (+2..+5)\n - neutral: factual (0)\n - need_to_improve: complaint/downside/friction (-2..-5)\n",
    "Contrast: 'higher'/'expensive'/'aware of cheaper' => need_to_improve; 'however'/'but' introducing a downside => need_to_improve; 'not too high' can still be positive.\n\n",
    "==================== OUTPUT RULES ====================\n",
    "Return at most ", max_themes, " theme objects. If no meaningful info, flag_useless=1 and themes=[].\n",
    "is_new_theme=1 only for genuinely new themes; discovery=1 if any is_new_theme=1 else 0.\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
}

build_sys_prompt_exploratory <- function(question_block, bank_text, allow_new_themes, max_themes) {
  paste0(
    "You are a research analyst performing CONCEPTUAL CONTENT ANALYSIS on open-ended answers to a DEFINITIONAL question.\n",
    "Respondents describe what a CONCEPT MEANS to them. They are NOT evaluating a product or giving reasons.\n\n",
    "SURVEY QUESTION / CONTEXT:\n", question_block, "\n\n",
    "GOAL: Map each answer to the BROAD CONCEPTUAL DIMENSION(S) it expresses - the underlying idea, NOT surface keywords.\n",
    "Build a SMALL, HIGH-LEVEL taxonomy. AGGRESSIVELY MERGE near-synonyms and related ideas into ONE canonical dimension.\n\n",
    "==================== CONSOLIDATION RULES ====================\n",
    "Prefer FEWER, BROADER dimensions. Never split one idea into finer variants.\n",
    "Group outcome/emotional ideas: 'financial freedom','financial security','peace of mind','not worrying about money','stability','independence' => ONE dimension.\n",
    "Group planning/management ideas: 'planning','managing money','organising finances','budgeting','control' => ONE dimension.\n",
    "Group growth/investing ideas: 'growing wealth','building wealth','returns','investing to grow' => ONE dimension.\n",
    "Group consolidation/tooling ideas: 'one place for everything','single view','all accounts together','dashboard' => ONE dimension.\n",
    "Reuse an existing bank dimension whenever the idea is the same OR closely related. Create NEW only when genuinely distinct.\n\n",
    "LABELS: short canonical noun phrases, 1-3 words (e.g. 'Financial Security','Wealth Growth','Consolidated View','Financial Planning','Advice & Guidance').\n",
    "Assign AT MOST ", max_themes, " dimension(s) per answer; most need only 1.\n\n",
    "SENTIMENT: usually NEUTRAL for definitional answers. Use neutral/0 unless clearly evaluative.\n\n",
    "Return STRICT JSON with keys: themes, flag_useless, discovery, new_themes.\n",
    "themes = ARRAY (0..", max_themes, ") of: theme, sentiment (satisfied|neutral|need_to_improve), sentiment_score [-5,5], is_new_theme (0/1), evidence (<=12 words), rationale (<=12 words).\n\n",
    "EXISTING DIMENSION BANK:\n", bank_text, "\n\n",
    if (!allow_new_themes) "NEW DIMENSIONS ARE NOT ALLOWED. Map to the closest existing bank dimension.\n\n"
    else "Create a new dimension ONLY when nothing in the bank reasonably fits.\n\n",
    "==================== OUTPUT RULES ====================\n",
    "Return at most ", max_themes, " dimension objects. If empty/uninformative, flag_useless=1 and themes=[].\n",
    "is_new_theme=1 only for genuinely new dimensions; discovery=1 if any is_new_theme=1 else 0.\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
}

build_sys_prompt_codebook <- function(question_block, codebook_text, max_themes) {
  paste0(
    "You are coding open-ended survey answers against a FIXED CODEBOOK. NEW CATEGORIES ARE NOT ALLOWED.\n\n",
    "SURVEY QUESTION / CONTEXT:\n", question_block, "\n\n",
    "CODEBOOK (assign answers to these dimensions; use the names EXACTLY as written):\n", codebook_text, "\n\n",
    "Assign each answer to the closest codebook dimension(s). AT MOST ", max_themes, " per answer; usually 1.\n",
    "Never invent a new label. If an answer is a poor fit, still choose the closest codebook dimension.\n",
    "SENTIMENT: usually NEUTRAL for definitional answers; use non-neutral only if the respondent is clearly evaluative.\n\n",
    "Return STRICT JSON with keys: themes, flag_useless, discovery, new_themes.\n",
    "themes = ARRAY (0..", max_themes, ") of objects: theme (MUST be an EXACT codebook name), sentiment (satisfied|neutral|need_to_improve), sentiment_score [-5,5], is_new_theme (always 0), evidence (EXACT quote <=12 words), rationale (<=12 words).\n",
    "If empty/uninformative, flag_useless=1 and themes=[]. Always discovery=0 and new_themes=[].\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
}

# ---------- Stage 1: induce codebook ----------
induce_codebook_with_anthropic <- function(samples, api_key, model = DEFAULT_MODEL,
                                           question_context = NULL,
                                           target_n = CODEBOOK_TARGET_DIM_DEFAULT) {
  empty <- tibble(name = character(), definition = character())
  if (is.null(api_key) || !nzchar(api_key)) return(empty)
  samples <- as.character(samples %||% character(0))
  samples <- samples[nzchar(str_squish(samples))]
  if (!length(samples)) return(empty)
  
  set.seed(42)  # reproducible sample so the codebook is stable across reruns on the same data
  if (length(samples) > CODEBOOK_SAMPLE_N) samples <- sample(samples, CODEBOOK_SAMPLE_N)
  samples <- substr(samples, 1, CODEBOOK_TEXT_TRUNC)
  joined <- paste0(seq_along(samples), ". ", samples, collapse = "\n")
  
  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  question_block <- if (nzchar(question_context)) question_context else "NONE PROVIDED"
  
  sys_prompt <- paste0(
    "You are a research analyst building a CODEBOOK for content analysis of open-ended survey answers.\n",
    "You will receive a SAMPLE of answers to ONE definitional question. Induce a compact, MUTUALLY EXCLUSIVE taxonomy.\n\n",
    "SURVEY QUESTION / CONTEXT:\n", question_block, "\n\n",
    "Produce between 6 and ", target_n, " high-level conceptual DIMENSIONS that together cover the sample well.\n",
    "Each dimension has: a short canonical label (1-3 words) and a one-sentence definition.\n",
    "Dimensions must be BROAD and NON-OVERLAPPING. Merge near-synonyms. Avoid micro-categories and avoid overlap.\n",
    "Order dimensions from most to least common in the sample.\n\n",
    "Return STRICT JSON exactly as: {\"dimensions\":[{\"name\":\"...\",\"definition\":\"...\"}, ...]}\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
  u_prompt <- paste0("Question context:\n", question_block,
                     "\n\nSample answers:\n", joined, "\n\nReturn JSON only.")
  
  out <- anthropic_chat_json(api_key, sys_prompt, u_prompt, model = model,
                             max_tokens = 2000L, temperature = 0, tag = "codebook")
  if (is.null(out)) return(empty)
  
  dims <- out$dimensions
  if (is.null(dims)) return(empty)
  if (!is.data.frame(dims)) {
    dims <- tryCatch(dplyr::bind_rows(lapply(dims, function(d) {
      as.data.frame(as.list(d), stringsAsFactors = FALSE)
    })), error = function(e) NULL)
  }
  if (is.null(dims) || !nrow(dims)) return(empty)
  
  nm <- if ("name" %in% names(dims)) as.character(dims$name) else ""
  df <- if ("definition" %in% names(dims)) as.character(dims$definition) else ""
  tibble(name = canonical_theme(nm), definition = df) %>%
    filter(nzchar(name)) %>%
    distinct(name, .keep_all = TRUE)
}

# =====================================================================
# EXPLORATORY NARRATIVE PIPELINE (theme -> sub-theme -> synthesis report)
# =====================================================================

# ---- Stage 1: induce a hierarchical theme/sub-theme framework ----
induce_framework_with_anthropic <- function(samples, api_key, model = DEFAULT_MODEL,
                                            question_context = NULL, target_n = 12) {
  empty <- tibble(theme = character(), theme_def = character(),
                  sub_theme = character(), sub_theme_def = character())
  if (is.null(api_key) || !nzchar(api_key)) return(empty)
  samples <- as.character(samples %||% character(0))
  samples <- samples[nzchar(str_squish(samples))]
  if (!length(samples)) return(empty)
  
  set.seed(42)
  if (length(samples) > CODEBOOK_SAMPLE_N) samples <- sample(samples, CODEBOOK_SAMPLE_N)
  samples <- substr(samples, 1, CODEBOOK_TEXT_TRUNC)
  joined <- paste0(seq_along(samples), ". ", samples, collapse = "\n")
  
  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  qb <- if (nzchar(question_context)) question_context else "NONE PROVIDED"
  
  sys_prompt <- paste0(
    "You are a research analyst building a HIERARCHICAL CODING FRAMEWORK (themes and sub-themes) for open-ended ",
    "answers to an exploratory/definitional survey question.\n\n",
    "SURVEY QUESTION / CONTEXT:\n", qb, "\n\n",
    "From the SAMPLE, induce AT LEAST ", target_n, " parent THEMES that together cover the responses.\n",
    "Each parent theme MUST contain 2 to 5 SUB-THEMES capturing the distinct angles/facets within it.\n",
    "Parent themes are broad and NON-OVERLAPPING; sub-themes carry the specific detail.\n",
    "Each theme and sub-theme has a short label (1-4 words) and a one-sentence definition.\n",
    "Order themes from most to least common in the sample.\n\n",
    "Return STRICT JSON exactly as:\n",
    "{\"themes\":[{\"name\":\"\",\"definition\":\"\",\"subthemes\":[{\"name\":\"\",\"definition\":\"\"}]}]}\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
  u_prompt <- paste0("Question context:\n", qb, "\n\nSample answers:\n", joined, "\n\nReturn JSON only.")
  
  out <- anthropic_chat_json(api_key, sys_prompt, u_prompt, model = model,
                             max_tokens = 3000L, temperature = 0, tag = "framework", simplify = FALSE)
  if (is.null(out)) return(empty)
  themes <- out$themes
  if (is.null(themes) || !length(themes)) return(empty)
  
  rows <- list()
  for (t in themes) {
    tn <- canonical_theme(t$name %||% ""); if (!nzchar(tn)) next
    tdef <- as.character(t$definition %||% "")
    subs <- t$subthemes
    if (is.null(subs) || !length(subs)) {
      rows[[length(rows) + 1]] <- tibble(theme = tn, theme_def = tdef,
                                         sub_theme = NA_character_, sub_theme_def = NA_character_)
    } else {
      for (s in subs) {
        sn <- canonical_theme(s$name %||% ""); if (!nzchar(sn)) next
        rows[[length(rows) + 1]] <- tibble(theme = tn, theme_def = tdef,
                                           sub_theme = sn, sub_theme_def = as.character(s$definition %||% ""))
      }
    }
  }
  if (!length(rows)) return(empty)
  bind_rows(rows) %>% distinct(theme, sub_theme, .keep_all = TRUE)
}

framework_to_text <- function(fw) {
  if (is.null(fw) || !nrow(fw)) return("NONE")
  fw %>%
    mutate(sub_line = if_else(is.na(sub_theme) | !nzchar(sub_theme), NA_character_,
                              paste0("  - ", sub_theme, ": ", coalesce(sub_theme_def, "")))) %>%
    group_by(theme, theme_def) %>%
    summarise(subs = paste(na.omit(sub_line), collapse = "\n"), .groups = "drop") %>%
    mutate(block = paste0("THEME: ", theme, " - ", coalesce(theme_def, ""),
                          if_else(nzchar(subs), paste0("\n", subs), ""))) %>%
    pull(block) %>% paste(collapse = "\n\n")
}

# ---- Stage 2: code one response against the fixed framework ----
code_response_framework <- function(text, api_key, fw_text, theme_names, sub_map,
                                    model = DEFAULT_MODEL, question_context = NULL, max_themes = 2) {
  empty <- tibble(theme = character(), sub_theme = character(), evidence = character())
  if (is.null(api_key) || !nzchar(api_key)) return(empty)
  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  qb <- if (nzchar(question_context)) question_context else "NONE PROVIDED"
  
  sys_prompt <- paste0(
    "You are coding ONE survey answer against a FIXED hierarchical framework. ",
    "NEW themes or sub-themes are NOT allowed.\n\n",
    "SURVEY QUESTION / CONTEXT:\n", qb, "\n\n",
    "FRAMEWORK:\n", fw_text, "\n\n",
    "Assign the answer to AT MOST ", max_themes, " parent theme(s). For each chosen parent theme, ",
    "pick the SINGLE best sub-theme belonging to that theme.\n",
    "theme and sub_theme MUST be copied EXACTLY from the framework above.\n",
    "Return STRICT JSON: {\"assignments\":[{\"theme\":\"\",\"sub_theme\":\"\",\"evidence\":\"\"}],\"flag_useless\":0}\n",
    "evidence = an exact quote from the answer, <=15 words.\n",
    "If the answer is empty/uninformative, set flag_useless=1 and assignments=[].\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
  u_prompt <- paste0("Answer:\n", text, "\n\nReturn JSON only.")
  
  out <- anthropic_chat_json(api_key, sys_prompt, u_prompt, model = model,
                             max_tokens = 800L, temperature = 0, tag = "fw_code")
  if (is.null(out)) return(empty)
  a <- out$assignments
  if (is.null(a) || length(a) == 0) return(empty)
  if (!is.data.frame(a)) a <- tryCatch(as_tibble(a), error = function(e) NULL)
  if (is.null(a) || !nrow(a)) return(empty)
  
  a <- as_tibble(a) %>%
    mutate(theme = map_to_existing_theme(as.character(theme), theme_names),
           sub_theme = as.character(sub_theme %||% ""),
           evidence = as.character(evidence %||% "")) %>%
    filter(theme %in% theme_names) %>%
    distinct(theme, .keep_all = TRUE) %>%
    slice_head(n = max_themes)
  if (!nrow(a)) return(empty)
  
  # enforce sub_theme validity within its parent theme
  a$sub_theme <- vapply(seq_len(nrow(a)), function(i) {
    valid <- sub_map[[a$theme[i]]] %||% character(0)
    if (!length(valid)) return(NA_character_)
    m <- map_to_existing_theme(a$sub_theme[i], valid)
    if (length(m) && m %in% valid) m else NA_character_
  }, character(1))
  
  a %>% select(theme, sub_theme, evidence)
}

cached_code_framework <- function(text, api_key, fw_text, theme_names, sub_map,
                                  model, question_context, max_themes, fw_sig) {
  key <- digest::digest(paste0("fwcode|", text, "|model=", model, "|maxt=", max_themes,
                               "|fw=", fw_sig, "|q=", stringr::str_squish(question_context %||% "")))
  if (!is.null(.cache_env[[key]])) return(.cache_env[[key]])
  res <- code_response_framework(text, api_key, fw_text, theme_names, sub_map,
                                 model = model, question_context = question_context, max_themes = max_themes)
  .cache_env[[key]] <- res
  res
}

# ---- Stage 3: synthesize an executive narrative for one parent theme ----
synthesize_theme_anthropic <- function(theme_name, theme_def, sub_counts_df, sample_texts,
                                       api_key, model = DEFAULT_MODEL, question_context = NULL) {
  default <- list(headline = theme_name, summary = "", quotes = character(0), business_implication = "")
  if (is.null(api_key) || !nzchar(api_key)) return(default)
  sample_texts <- as.character(sample_texts %||% character(0))
  sample_texts <- sample_texts[nzchar(str_squish(sample_texts))]
  if (!length(sample_texts)) return(default)
  sample_texts <- substr(sample_texts, 1, 300)
  joined <- paste0(seq_along(sample_texts), ". ", sample_texts, collapse = "\n")
  
  subtxt <- if (!is.null(sub_counts_df) && nrow(sub_counts_df)) {
    paste0("- ", sub_counts_df$sub_theme, " (", sub_counts_df$mentions, " mentions)", collapse = "\n")
  } else "none"
  
  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  qb <- if (nzchar(question_context)) question_context else "NONE PROVIDED"
  
  sys_prompt <- paste0(
    "You are writing an EXECUTIVE INSIGHT summary for ONE theme from an open-ended survey question.\n",
    "Base EVERYTHING strictly on the provided responses. Do NOT invent facts or quotes.\n\n",
    "Write:\n",
    " - headline: <=12 words, specific and insight-bearing (NOT just the theme name)\n",
    " - summary: 2 to 4 sentences describing what respondents express, the nuances, sub-patterns, and any tension or disagreement\n",
    " - quotes: 3 SHORT verbatim excerpts copied EXACTLY from the provided responses, <=20 words each, representative of the theme\n",
    " - business_implication: 1 to 2 sentences on what this means or what the business should consider\n\n",
    "Return STRICT JSON: {\"headline\":\"\",\"summary\":\"\",\"quotes\":[\"\",\"\",\"\"],\"business_implication\":\"\"}\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
  u_prompt <- paste0("Question context:\n", qb,
                     "\n\nTheme: ", theme_name, "\nTheme definition: ", theme_def,
                     "\n\nSub-themes (with counts):\n", subtxt,
                     "\n\nSample responses coded to this theme:\n", joined, "\n\nReturn JSON only.")
  
  out <- anthropic_chat_json(api_key, sys_prompt, u_prompt, model = model,
                             max_tokens = 1200L, temperature = 0.3, tag = "synthesis")
  if (is.null(out)) return(default)
  q <- out$quotes
  q <- if (is.null(q)) character(0) else as.character(unlist(q))
  q <- q[nzchar(str_squish(q))]
  list(
    headline = as.character(out$headline %||% theme_name),
    summary = as.character(out$summary %||% ""),
    quotes = q,
    business_implication = as.character(out$business_implication %||% "")
  )
}

# ---- Select the most informative RAW verbatim responses for one class ----
# Returns up to `n` EXACT respondent responses for a class (never paraphrased).
# The model is used ONLY to CHOOSE indices from a numbered list, so the returned
# text is guaranteed verbatim (pulled straight from `raw_texts`). If the API fails
# or is unavailable, falls back to the longest (most informative) distinct answers.
select_raw_examples_anthropic <- function(raw_texts, api_key, class_name, class_def, sentiment,
                                           model = DEFAULT_MODEL, question_context = NULL, n = 3) {
  raw_texts <- as.character(raw_texts %||% character(0))
  raw_texts <- stringr::str_squish(raw_texts)
  raw_texts <- raw_texts[nzchar(raw_texts)]
  raw_texts <- unique(raw_texts)
  if (!length(raw_texts)) return(character(0))
  if (length(raw_texts) <= n) return(raw_texts)

  # deterministic fallback: the longest (most informative) distinct responses
  fallback <- head(raw_texts[order(nchar(raw_texts), decreasing = TRUE)], n)
  if (is.null(api_key) || !nzchar(api_key)) return(fallback)

  # cap candidates sent to the model (keep the richer/longer ones) for cost/latency
  cand <- raw_texts
  if (length(cand) > 60) cand <- head(cand[order(nchar(cand), decreasing = TRUE)], 60)
  cand_trunc <- substr(cand, 1, 400)
  joined <- paste0(seq_along(cand), ". ", cand_trunc, collapse = "\n")

  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  qb <- if (nzchar(question_context)) question_context else "NONE PROVIDED"

  sys_prompt <- paste0(
    "You are selecting the MOST INFORMATIVE and REPRESENTATIVE verbatim survey responses for ONE ",
    "opinion class. You will receive a NUMBERED list of raw respondent responses coded to this class.\n\n",
    "SURVEY QUESTION / CONTEXT:\n", qb, "\n\n",
    "Sentiment bucket: ", sentiment, "\n",
    "Class: ", class_name, "\nClass definition: ", class_def, "\n\n",
    "Choose the ", n, " responses that BEST and MOST CLEARLY reflect this class and are the most ",
    "informative (rich, specific, articulate). Prefer distinct responses that together cover the class ",
    "well; avoid near-duplicates and avoid empty/low-content answers.\n",
    "Return STRICT JSON exactly as: {\"indices\":[n1,n2,n3]} using the NUMBERS from the list above.\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
  u_prompt <- paste0("Responses:\n", joined, "\n\nReturn JSON only.")

  out <- anthropic_chat_json(api_key, sys_prompt, u_prompt, model = model,
                             max_tokens = 200L, temperature = 0, tag = "raw_examples")
  idx <- if (!is.null(out)) suppressWarnings(as.integer(unlist(out$indices))) else integer(0)
  idx <- idx[!is.na(idx) & idx >= 1 & idx <= length(cand)]
  idx <- unique(idx)
  if (!length(idx)) return(fallback)
  chosen <- cand[head(idx, n)]
  # top up with the fallback if the model returned fewer than n valid indices
  if (length(chosen) < n) chosen <- unique(c(chosen, fallback))
  head(chosen, n)
}

# Format raw examples as: each response wrapped in double quotes, one per line.
format_raw_examples <- function(x) {
  x <- as.character(x %||% character(0))
  x <- x[nzchar(stringr::str_squish(x))]
  if (!length(x)) return("")
  paste0('"', x, '"', collapse = "\n")
}

# ---- Build the readable HTML report ----
build_exploratory_report_html <- function(question, total_responses, themes_summ, sub_counts, quotes_list) {
  ind <- "#3A3E96"; grn <- "#5CCB09"
  header <- paste0(
    "<div style='font-family:Segoe UI,Helvetica,Arial,sans-serif;max-width:900px;'>",
    "<h1 style='color:", ind, ";margin-bottom:2px;'>Open-ended Insight Report</h1>",
    "<div style='color:#555;font-size:14px;margin-bottom:4px;'>Question: ",
    esc_html(if (nzchar(question)) question else "(not provided)"), "</div>",
    "<div style='color:#555;font-size:13px;margin-bottom:18px;'>Based on ", total_responses,
    " responses. Themes ordered by prevalence.</div>"
  )
  if (is.null(themes_summ) || !nrow(themes_summ)) {
    return(paste0(header, "<p>No themes produced.</p></div>"))
  }
  themes_summ <- themes_summ %>% arrange(desc(mentions))
  blocks <- vapply(seq_len(nrow(themes_summ)), function(i) {
    th   <- themes_summ$theme[i]
    men  <- themes_summ$mentions[i]
    shr  <- scales::percent(themes_summ$share[i], accuracy = 0.1)
    head <- esc_html(themes_summ$headline[i])
    summ <- esc_html(themes_summ$summary[i])
    impl <- esc_html(themes_summ$business_implication[i])
    
    subs <- sub_counts %>% filter(theme == th) %>% arrange(desc(mentions))
    sub_html <- if (nrow(subs)) {
      rows <- paste0("<tr><td style='padding:2px 10px 2px 0;'>", esc_html(subs$sub_theme),
                     "</td><td style='padding:2px 0;color:#555;'>", subs$mentions,
                     " (", scales::percent(subs$share, accuracy = 0.1), ")</td></tr>", collapse = "")
      paste0("<table style='font-size:13px;margin:6px 0;'>", rows, "</table>")
    } else ""
    
    qs <- quotes_list[[th]] %||% character(0)
    q_html <- if (length(qs)) {
      paste0(vapply(qs, function(q) paste0(
        "<blockquote style='margin:4px 0 4px 0;padding:4px 12px;border-left:3px solid ", grn,
        ";color:#333;font-style:italic;font-size:13px;'>", esc_html(q), "</blockquote>"),
        character(1)), collapse = "")
    } else ""
    
    paste0(
      "<div style='margin:0 0 26px 0;padding-bottom:14px;border-bottom:1px solid #eee;'>",
      "<h2 style='color:", ind, ";margin:0 0 2px 0;font-size:19px;'>", head, "</h2>",
      "<div style='font-size:12px;color:#888;margin-bottom:8px;'>", esc_html(th),
      " &nbsp;|&nbsp; ", men, " mentions (", shr, ")</div>",
      "<p style='font-size:14px;line-height:1.5;margin:6px 0;'>", summ, "</p>",
      if (nzchar(sub_html)) paste0("<div style='font-size:12px;color:#888;margin-top:8px;'>Sub-themes:</div>", sub_html) else "",
      if (nzchar(q_html)) paste0("<div style='font-size:12px;color:#888;margin-top:8px;'>Representative voices:</div>", q_html) else "",
      if (nzchar(impl)) paste0("<div style='margin-top:8px;padding:8px 12px;background:#f4f5fb;border-radius:6px;font-size:13px;'>",
                               "<b style='color:", ind, ";'>Business implication:</b> ", impl, "</div>") else "",
      "</div>"
    )
  }, character(1))
  
  paste0(header, paste(blocks, collapse = ""), "</div>")
}

# =====================================================================
# OPINION PIPELINE (sentiment-first: segment -> per-sentiment class/subclass -> message)
# =====================================================================

# ---- Stage 1: split ONE answer into sentiment-labelled segments ----
# A single answer can carry multiple sentiments; each segment is one atomic
# idea labelled positive / negative / neutral with a short rationale + evidence.
segment_sentiment_with_anthropic <- function(text, api_key, model = DEFAULT_MODEL,
                                             question_context = NULL,
                                             max_segments = MAX_SEGMENTS_PER_RESPONSE) {
  empty_seg <- tibble(segment_text = character(), sentiment = character(),
                      rationale = character(), evidence = character())
  na_pad <- c(pleasure = NA_real_, arousal = NA_real_, dominance = NA_real_)
  empty <- list(segments = empty_seg, pad = na_pad)
  if (is.null(api_key) || !nzchar(api_key)) return(empty)
  if (is.null(text) || !nzchar(str_squish(text %||% ""))) return(empty)
  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  qb <- if (nzchar(question_context)) question_context else "NONE PROVIDED"

  sys_prompt <- paste0(
    "You are an analyst reading an open-ended OPINION answer from a survey respondent.\n",
    "Your job has TWO parts.\n\n",
    "PART A - SEGMENTS: split the answer into ATOMIC SEGMENTS (one distinct idea/point each) and label the\n",
    "sentiment of EACH segment. A single answer often mixes sentiments - e.g. one part praises and\n",
    "another part criticises - so DO split it and label the parts separately.\n\n",
    "SURVEY QUESTION / CONTEXT:\n", qb, "\n\n",
    "For each segment decide:\n",
    " - sentiment: exactly one of positive, negative, neutral\n",
    "     positive  = praise, satisfaction, something the respondent likes/values\n",
    "     negative  = complaint, frustration, downside, something they want improved\n",
    "     neutral   = factual, descriptive, mixed-with-no-lean, or a definition with no evaluation\n",
    " - rationale: <=15 words explaining WHY that sentiment (cite the cue words)\n",
    " - evidence: an EXACT quote from the answer for that segment, <=20 words\n\n",
    "PART B - PAD AFFECT: rate the WHOLE answer's overall affect on the PAD model. Each dimension is a\n",
    "decimal in the range -1.0 to 1.0 (use the full range; 0 = neutral / not expressed):\n",
    " - pleasure  (valence):    -1 = very unpleasant/displeased  ... +1 = very pleasant/happy\n",
    " - arousal   (activation): -1 = very calm/passive/bored     ... +1 = very excited/agitated/activated\n",
    " - dominance (control):    -1 = feels controlled/powerless   ... +1 = feels in-control/empowered\n\n",
    "Return STRICT JSON exactly as:\n",
    "{\"segments\":[{\"segment_text\":\"\",\"sentiment\":\"positive|negative|neutral\",\"rationale\":\"\",\"evidence\":\"\"}],",
    "\"pleasure\":0.0,\"arousal\":0.0,\"dominance\":0.0,\"flag_useless\":0}\n",
    "Use AT MOST ", max_segments, " segments. segment_text is a short paraphrase (<=20 words) of that idea.\n",
    "If the answer is empty/uninformative (e.g. 'na','nothing','n/a'), set flag_useless=1, segments=[], and pleasure=arousal=dominance=0.\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
  u_prompt <- paste0("Answer:\n", text, "\n\nReturn JSON only.")

  out <- anthropic_chat_json(api_key, sys_prompt, u_prompt, model = model,
                             max_tokens = 1300L, temperature = 0, tag = "opinion_segment")
  if (is.null(out)) return(empty)

  pad <- c(
    pleasure  = clamp11(out$pleasure  %||% NA),
    arousal   = clamp11(out$arousal   %||% NA),
    dominance = clamp11(out$dominance %||% NA)
  )

  seg <- out$segments
  if (is.null(seg) || length(seg) == 0) return(list(segments = empty_seg, pad = pad))
  if (!is.data.frame(seg)) seg <- tryCatch(as_tibble(seg), error = function(e) NULL)
  if (is.null(seg) || !nrow(seg)) return(list(segments = empty_seg, pad = pad))

  seg <- as_tibble(seg) %>%
    mutate(
      segment_text = as.character(segment_text %||% ""),
      sentiment    = tolower(as.character(sentiment %||% "neutral")),
      sentiment    = if_else(sentiment %in% OPINION_SENTIMENT_LEVELS, sentiment, "neutral"),
      rationale    = as.character(rationale %||% ""),
      evidence     = as.character(evidence %||% "")
    ) %>%
    filter(nzchar(str_squish(segment_text)) | nzchar(str_squish(evidence))) %>%
    mutate(segment_text = if_else(nzchar(str_squish(segment_text)), segment_text, evidence)) %>%
    slice_head(n = max_segments)
  if (!nrow(seg)) return(list(segments = empty_seg, pad = pad))
  list(segments = seg %>% select(segment_text, sentiment, rationale, evidence), pad = pad)
}

cached_segment_sentiment <- function(text, api_key, model, question_context, max_segments) {
  key <- digest::digest(paste0("opinion_seg|", text, "|model=", model, "|maxs=", max_segments,
                               "|q=", stringr::str_squish(question_context %||% "")))
  if (!is.null(.cache_env[[key]])) return(.cache_env[[key]])
  res <- segment_sentiment_with_anthropic(text, api_key, model = model,
                                          question_context = question_context, max_segments = max_segments)
  .cache_env[[key]] <- res
  res
}

# ---- Stage 2: induce a class -> subclass framework FOR ONE sentiment bucket ----
# Returns the same shape as induce_framework_with_anthropic (theme = class,
# sub_theme = subclass) so the framework helpers can be reused.
induce_opinion_framework <- function(samples, api_key, sentiment, model = DEFAULT_MODEL,
                                     question_context = NULL, target_n = OPINION_CLASS_TARGET_DEFAULT) {
  empty <- tibble(theme = character(), theme_def = character(),
                  sub_theme = character(), sub_theme_def = character())
  if (is.null(api_key) || !nzchar(api_key)) return(empty)
  samples <- as.character(samples %||% character(0))
  samples <- samples[nzchar(str_squish(samples))]
  if (!length(samples)) return(empty)

  set.seed(42)
  if (length(samples) > CODEBOOK_SAMPLE_N) samples <- sample(samples, CODEBOOK_SAMPLE_N)
  samples <- substr(samples, 1, CODEBOOK_TEXT_TRUNC)
  joined <- paste0(seq_along(samples), ". ", samples, collapse = "\n")

  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  qb <- if (nzchar(question_context)) question_context else "NONE PROVIDED"

  lean <- switch(sentiment,
    positive = "These segments express a POSITIVE opinion. Group WHAT respondents are happy about / value / praise.",
    negative = "These segments express a NEGATIVE opinion. Group WHAT respondents are unhappy about / want improved / criticise.",
    "These segments are NEUTRAL/factual. Group the descriptive points respondents raise."
  )

  sys_prompt <- paste0(
    "You are a research analyst building a HIERARCHICAL coding framework (CLASSES and SUBCLASSES) for the\n",
    sentiment, " parts of open-ended opinion answers.\n\n",
    "SURVEY QUESTION / CONTEXT:\n", qb, "\n\n",
    lean, "\n\n",
    "From the SAMPLE segments, induce AT LEAST ", target_n, " parent CLASSES that together cover them.\n",
    "Each parent class MUST contain 2 to 5 SUBCLASSES capturing the distinct angles within it.\n",
    "Parent classes are broad and NON-OVERLAPPING; subclasses carry the specific detail.\n",
    "Each class and subclass has a short label (1-4 words) and a one-sentence definition.\n",
    "Order classes from most to least common in the sample.\n\n",
    "Return STRICT JSON exactly as:\n",
    "{\"themes\":[{\"name\":\"\",\"definition\":\"\",\"subthemes\":[{\"name\":\"\",\"definition\":\"\"}]}]}\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
  u_prompt <- paste0("Question context:\n", qb, "\n\nSentiment bucket: ", sentiment,
                     "\n\nSample segments:\n", joined, "\n\nReturn JSON only.")

  out <- anthropic_chat_json(api_key, sys_prompt, u_prompt, model = model,
                             max_tokens = 3000L, temperature = 0, tag = paste0("opinion_fw[", sentiment, "]"),
                             simplify = FALSE)
  if (is.null(out)) return(empty)
  themes <- out$themes
  if (is.null(themes) || !length(themes)) return(empty)

  rows <- list()
  for (t in themes) {
    tn <- canonical_theme(t$name %||% ""); if (!nzchar(tn)) next
    tdef <- as.character(t$definition %||% "")
    subs <- t$subthemes
    if (is.null(subs) || !length(subs)) {
      rows[[length(rows) + 1]] <- tibble(theme = tn, theme_def = tdef,
                                         sub_theme = NA_character_, sub_theme_def = NA_character_)
    } else {
      for (s in subs) {
        sn <- canonical_theme(s$name %||% ""); if (!nzchar(sn)) next
        rows[[length(rows) + 1]] <- tibble(theme = tn, theme_def = tdef,
                                           sub_theme = sn, sub_theme_def = as.character(s$definition %||% ""))
      }
    }
  }
  if (!length(rows)) return(empty)
  bind_rows(rows) %>% distinct(theme, sub_theme, .keep_all = TRUE)
}

# ---- Fuzzy snap a model-returned label to the nearest known label (base adist) ----
# Returns the matched candidate, or NA if nothing is within tolerance.
snap_to <- function(x, candidates, max_ratio = OPINION_SNAP_MAX_RATIO) {
  x <- normalise_text(x)
  candidates <- as.character(candidates %||% character(0))
  candidates <- candidates[nzchar(candidates)]
  if (!nzchar(x) || !length(candidates)) return(NA_character_)
  cn <- normalise_text(candidates)
  hit <- which(cn == x)
  if (length(hit)) return(candidates[hit[1]])
  d <- as.integer(utils::adist(x, cn))
  b <- which.min(d)
  if (length(b) && is.finite(d[b]) &&
      d[b] <= max_ratio * max(nchar(x), nchar(cn[b]), 1L)) return(candidates[b])
  NA_character_
}

# ---- Assign ONE segment to the framework, allowing an explicit "does not fit" ----
# Returns list(class, subclass, evidence, fits). The model may set fits=FALSE; we
# also fall back to fits=FALSE when its label can't be snapped to the framework.
assign_segment_to_framework <- function(text, api_key, fw_text, class_names, sub_map,
                                        model = DEFAULT_MODEL, question_context = NULL) {
  miss <- list(class = NA_character_, subclass = NA_character_, evidence = "", fits = FALSE)
  if (is.null(api_key) || !nzchar(api_key)) return(miss)
  if (is.null(text) || !nzchar(str_squish(text %||% ""))) return(miss)
  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  qb <- if (nzchar(question_context)) question_context else "NONE PROVIDED"

  sys_prompt <- paste0(
    "You are coding ONE opinion segment against a FIXED class -> subclass framework.\n\n",
    "SURVEY QUESTION / CONTEXT:\n", qb, "\n\n",
    "FRAMEWORK:\n", fw_text, "\n\n",
    "Pick the SINGLE best CLASS and, within it, the SINGLE best SUBCLASS - copying the names EXACTLY.\n",
    "IMPORTANT: only assign if the segment genuinely belongs. If NONE of the classes reasonably fit,\n",
    "set fits=false and leave class/subclass empty (do NOT force a poor match).\n",
    "Return STRICT JSON: {\"class\":\"\",\"subclass\":\"\",\"evidence\":\"\",\"fits\":true}\n",
    "evidence = a short exact quote from the segment, <=15 words.\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
  u_prompt <- paste0("Segment:\n", text, "\n\nReturn JSON only.")
  out <- anthropic_chat_json(api_key, sys_prompt, u_prompt, model = model,
                             max_tokens = 500L, temperature = 0, tag = "opinion_assign")
  if (is.null(out)) return(miss)

  fits <- out$fits
  fits <- if (is.logical(fits)) isTRUE(fits[1]) else !identical(tolower(as.character(fits %||% "true")), "false")
  raw_cls <- as.character(out$class %||% "")
  raw_sub <- as.character(out$subclass %||% "")
  ev <- as.character(out$evidence %||% "")
  if (!fits || !nzchar(str_squish(raw_cls))) return(list(class = NA_character_, subclass = NA_character_, evidence = ev, fits = FALSE))

  cls <- snap_to(raw_cls, class_names)
  if (is.na(cls)) return(list(class = NA_character_, subclass = NA_character_, evidence = ev, fits = FALSE))
  valid_subs <- sub_map[[cls]] %||% character(0)
  sub <- if (length(valid_subs) && nzchar(str_squish(raw_sub))) snap_to(raw_sub, valid_subs) else NA_character_
  list(class = cls, subclass = sub, evidence = ev, fits = TRUE)
}

cached_assign_segment <- function(text, api_key, fw_text, class_names, sub_map,
                                  model, question_context, fw_sig) {
  key <- digest::digest(paste0("opinion_assign|", text, "|model=", model, "|fw=", fw_sig,
                               "|q=", stringr::str_squish(question_context %||% "")))
  if (!is.null(.cache_env[[key]])) return(.cache_env[[key]])
  res <- assign_segment_to_framework(text, api_key, fw_text, class_names, sub_map,
                                     model = model, question_context = question_context)
  .cache_env[[key]] <- res
  res
}

# Build per-framework metadata (text, signature, class names, subclass map) once.
opinion_fw_meta <- function(fw_s, sentiment) {
  fw_text <- framework_to_text(fw_s)
  smap <- fw_s %>% filter(!is.na(sub_theme), nzchar(sub_theme)) %>%
    group_by(theme) %>% summarise(subs = list(unique(sub_theme)), .groups = "drop")
  list(fw_text = fw_text,
       fw_sig = digest::digest(paste0(sentiment, "|", fw_text)),
       class_names = canonical_theme(unique(fw_s$theme)),
       sub_map = stats::setNames(smap$subs, smap$theme),
       class_defs = fw_s %>% distinct(theme, theme_def))
}

# ---- Build the readable HTML report, organised BY SENTIMENT ----
build_opinion_report_html <- function(question, total_responses, sentiment_summary,
                                       classes_summ, subclass_counts, quotes_map,
                                       coverage = NULL) {
  ind <- "#3A3E96"; grn <- "#5CCB09"; red <- "#BA38B1"; amb <- "#FFC000"
  sent_col <- c(positive = grn, negative = red, neutral = "#3A3E96")
  header <- paste0(
    "<div style='font-family:Segoe UI,Helvetica,Arial,sans-serif;max-width:900px;'>",
    "<h1 style='color:", ind, ";margin-bottom:2px;'>Open-ended Opinion Report</h1>",
    "<div style='color:#555;font-size:14px;margin-bottom:4px;'>Question: ",
    esc_html(if (nzchar(question)) question else "(not provided)"), "</div>",
    "<div style='color:#555;font-size:13px;margin-bottom:6px;'>Based on ", total_responses,
    " responses. Each answer is split into sentiment-labelled segments; classes are built within each sentiment.</div>",
    if (!is.null(coverage)) paste0(
      "<div style='color:#555;font-size:13px;margin-bottom:14px;'>Coverage: ",
      scales::percent(coverage$classified_share %||% 0, accuracy = 0.1),
      " of segments placed in a named class (", coverage$classified %||% 0, " of ",
      coverage$total %||% 0, "); ",
      scales::percent(coverage$other_share %||% 0, accuracy = 0.1),
      " fell to '", OPINION_OTHER_LABEL, "'.</div>"
    ) else ""
  )

  # sentiment overview bar
  ov <- ""
  if (!is.null(sentiment_summary) && nrow(sentiment_summary)) {
    cells <- vapply(seq_len(nrow(sentiment_summary)), function(i) {
      s <- sentiment_summary$sentiment[i]
      lbl <- OPINION_SENTIMENT_LABELS[[s]] %||% s
      paste0("<span style='display:inline-block;margin-right:18px;'>",
             "<b style='color:", sent_col[[s]] %||% "#333", ";'>", lbl, "</b>: ",
             sentiment_summary$segments[i], " segments (",
             scales::percent(sentiment_summary$share[i], accuracy = 0.1), ")</span>")
    }, character(1))
    ov <- paste0("<div style='font-size:13px;margin-bottom:20px;padding:10px 12px;background:#f4f5fb;border-radius:6px;'>",
                 "<b>Sentiment split (of all segments):</b><br>", paste(cells, collapse = ""), "</div>")
  }

  if (is.null(classes_summ) || !nrow(classes_summ)) {
    return(paste0(header, ov, "<p>No classes produced.</p></div>"))
  }

  sent_order <- OPINION_SENTIMENT_LEVELS[OPINION_SENTIMENT_LEVELS %in% classes_summ$sentiment]
  sect <- vapply(sent_order, function(s) {
    lbl <- OPINION_SENTIMENT_LABELS[[s]] %||% s
    col <- sent_col[[s]] %||% ind
    cs <- classes_summ %>% filter(sentiment == s) %>%
      arrange(class == OPINION_OTHER_LABEL, desc(mentions))   # Other always last
    if (!nrow(cs)) return("")
    blocks <- vapply(seq_len(nrow(cs)), function(i) {
      cl   <- cs$class[i]
      men  <- cs$mentions[i]
      shr  <- scales::percent(cs$share[i], accuracy = 0.1)
      head <- esc_html(cs$headline[i])
      summ <- esc_html(cs$summary[i])
      impl <- esc_html(cs$business_implication[i])

      subs <- subclass_counts %>% filter(sentiment == s, class == cl) %>% arrange(desc(mentions))
      sub_html <- if (nrow(subs)) {
        rowsx <- paste0("<tr><td style='padding:2px 10px 2px 0;'>", esc_html(subs$subclass),
                        "</td><td style='padding:2px 0;color:#555;'>", subs$mentions,
                        " (", scales::percent(subs$share, accuracy = 0.1), ")</td></tr>", collapse = "")
        paste0("<table style='font-size:13px;margin:6px 0;'>", rowsx, "</table>")
      } else ""

      qs <- quotes_map[[paste0(s, "||", cl)]] %||% character(0)
      q_html <- if (length(qs)) {
        paste0(vapply(qs, function(q) paste0(
          "<blockquote style='margin:4px 0;padding:4px 12px;border-left:3px solid ", col,
          ";color:#333;font-style:italic;font-size:13px;'>", esc_html(q), "</blockquote>"),
          character(1)), collapse = "")
      } else ""

      paste0(
        "<div style='margin:0 0 22px 0;padding-bottom:12px;border-bottom:1px solid #eee;'>",
        "<h3 style='color:", col, ";margin:0 0 2px 0;font-size:17px;'>", head, "</h3>",
        "<div style='font-size:12px;color:#888;margin-bottom:6px;'>", esc_html(cl),
        " &nbsp;|&nbsp; ", men, " mentions (", shr, " of ", lbl, " segments)</div>",
        "<p style='font-size:14px;line-height:1.5;margin:6px 0;'>", summ, "</p>",
        if (nzchar(sub_html)) paste0("<div style='font-size:12px;color:#888;margin-top:6px;'>Subclasses:</div>", sub_html) else "",
        if (nzchar(q_html)) paste0("<div style='font-size:12px;color:#888;margin-top:6px;'>Representative voices:</div>", q_html) else "",
        if (nzchar(impl)) paste0("<div style='margin-top:8px;padding:8px 12px;background:#f4f5fb;border-radius:6px;font-size:13px;'>",
                                 "<b style='color:", col, ";'>The message:</b> ", impl, "</div>") else "",
        "</div>"
      )
    }, character(1))
    paste0("<h2 style='color:", col, ";border-bottom:2px solid ", col, ";padding-bottom:4px;margin-top:26px;'>",
           lbl, " opinions</h2>", paste(blocks, collapse = ""))
  }, character(1))

  paste0(header, ov, paste(sect, collapse = ""), "</div>")
}

# ---------- Anthropic multi-label classifier (mode-aware) ----------
classify_multi_with_anthropic <- function(text, api_key, theme_bank = character(0),
                                          allow_new_themes = TRUE,
                                          model = DEFAULT_MODEL, temperature = 0,
                                          question_context = NULL,
                                          mode = "attribute",
                                          max_themes = MAX_THEMES_PER_RESPONSE,
                                          codebook = NULL) {
  if (is.null(api_key) || !nzchar(api_key)) {
    return(fallback_multi_dynamic(text, theme_bank = theme_bank, allow_new_themes = allow_new_themes,
                                  question_context = question_context, max_themes = max_themes))
  }
  
  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  question_block <- if (nzchar(question_context)) question_context else "NONE PROVIDED"
  
  if (identical(mode, "codebook")) {
    if (is.null(codebook) || !nrow(codebook)) {
      # No codebook available -> degrade gracefully to exploratory discovery
      bank_text <- if (length(theme_bank)) paste(theme_bank, collapse = ", ") else "NONE"
      sys_prompt <- build_sys_prompt_exploratory(question_block, bank_text, allow_new_themes, max_themes)
    } else {
      theme_bank <- canonical_theme(codebook$name)
      allow_new_themes <- FALSE
      codebook_text <- paste0("- ", codebook$name, ": ", codebook$definition, collapse = "\n")
      sys_prompt <- build_sys_prompt_codebook(question_block, codebook_text, max_themes)
    }
  } else if (identical(mode, "exploratory")) {
    bank_text <- if (length(theme_bank)) paste(theme_bank, collapse = ", ") else "NONE"
    sys_prompt <- build_sys_prompt_exploratory(question_block, bank_text, allow_new_themes, max_themes)
  } else {
    bank_text <- if (length(theme_bank)) paste(theme_bank, collapse = ", ") else "NONE"
    sys_prompt <- build_sys_prompt_attribute(question_block, bank_text, allow_new_themes, max_themes)
  }
  
  u_prompt <- paste0("Survey question / analysis context:\n", question_block,
                     "\n\nComment:\n", text, "\n\nReturn JSON only (no code fences).")
  
  out <- anthropic_chat_json(api_key, sys_prompt, u_prompt, model = model,
                             max_tokens = 1500L, temperature = temperature,
                             tag = paste0("classify[", mode, "]"))
  if (is.null(out)) {
    return(fallback_multi_dynamic(text, theme_bank = theme_bank, allow_new_themes = allow_new_themes,
                                  question_context = question_context, max_themes = max_themes))
  }
  
  themes <- out$themes
  if (is.null(themes) || length(themes) == 0) themes <- tibble()
  if (!is.data.frame(themes)) themes <- tibble()
  
  if (nrow(themes) > 0) {
    themes <- as_tibble(themes) %>%
      mutate(
        theme = as.character(theme),
        sentiment = if_else(sentiment %in% SENTIMENT_LEVELS, sentiment, "neutral"),
        sentiment_score = suppressWarnings(as.integer(sentiment_score)),
        sentiment_score = if_else(is.na(sentiment_score), 0L, sentiment_score),
        sentiment_score = pmax(-5L, pmin(5L, sentiment_score)),
        is_new_theme = suppressWarnings(as.integer(is_new_theme)),
        is_new_theme = if_else(is_new_theme %in% c(0L,1L), is_new_theme, 0L),
        evidence = as.character(evidence %||% ""),
        rationale = as.character(rationale %||% "")
      )
  }
  
  flag_useless <- suppressWarnings(as.integer(out$flag_useless))
  if (!is.finite(flag_useless) || !(flag_useless %in% c(0L,1L))) flag_useless <- 0L
  
  if (nrow(themes) > 0) {
    themes <- themes %>%
      mutate(theme = map_to_existing_theme(theme, theme_bank)) %>%
      filter(!is.na(theme), theme != "") %>%
      distinct(theme, .keep_all = TRUE) %>%
      slice_head(n = max_themes)
    
    if (nrow(themes) > 0) {
      bank_norm <- normalise_text(theme_bank); theme_norm <- normalise_text(themes$theme)
      if (length(theme_bank)) {
        themes <- themes %>%
          mutate(is_new_theme = if_else(theme_norm %in% bank_norm, 0L, if_else(allow_new_themes, 1L, 0L)))
        if (!allow_new_themes) themes <- themes %>% filter(is_new_theme == 0L)
      } else {
        themes <- themes %>% mutate(is_new_theme = if_else(allow_new_themes, 1L, 0L))
      }
    }
  }
  
  discovery <- as.integer(nrow(themes) > 0 && any(themes$is_new_theme == 1L))
  new_themes <- if (nrow(themes) > 0) unique(themes$theme[themes$is_new_theme == 1L]) else character(0)
  new_themes <- new_themes[nzchar(new_themes)]
  
  list(themes = themes, flag_useless = flag_useless, discovery = discovery, new_themes = new_themes)
}

# ---------- memoized classify ----------
.cache_env <- new.env(parent = emptyenv())

cached_classify_multi <- function(text, api_key, theme_bank = character(0),
                                  use_api = TRUE, allow_new_themes = TRUE,
                                  question_context = NULL, model = DEFAULT_MODEL,
                                  mode = "attribute", max_themes = MAX_THEMES_PER_RESPONSE,
                                  codebook = NULL) {
  question_context <- stringr::str_squish(as.character(question_context %||% ""))
  cb_sig <- if (!is.null(codebook) && nrow(codebook)) paste(sort(codebook$name), collapse = "|") else "none"
  key <- digest::digest(paste0(
    "ml_dynamic|", text, "|",
    if (use_api) (api_key %||% "no_key") else "offline",
    "|model=", model, "|mode=", mode, "|maxt=", max_themes,
    "|allow_new=", allow_new_themes,
    "|bank=", paste(sort(theme_bank), collapse = "|"),
    "|cb=", cb_sig, "|question=", question_context
  ))
  if (!is.null(.cache_env[[key]])) return(.cache_env[[key]])
  
  res <- if (use_api && nzchar(api_key %||% "")) {
    classify_multi_with_anthropic(
      text = text, api_key = api_key, theme_bank = theme_bank,
      allow_new_themes = allow_new_themes, model = model,
      question_context = question_context, mode = mode, max_themes = max_themes, codebook = codebook
    )
  } else {
    fallback_multi_dynamic(text = text, theme_bank = theme_bank, allow_new_themes = allow_new_themes,
                           question_context = question_context, max_themes = max_themes)
  }
  .cache_env[[key]] <- res
  res
}

# ---------- language detection ----------
detect_lang <- function(x) {
  x <- x %||% ""; x <- str_squish(x)
  if (!nzchar(x) || nchar(x) < 20) return(NA_character_)
  if (HAS_CLD3) return(tryCatch(cld3::detect_language(x), error = function(e) NA_character_))
  NA_character_
}

# ---------- translation ----------
translate_with_anthropic <- function(text, api_key, model = DEFAULT_MODEL) {
  if (is.null(api_key) || !nzchar(api_key)) return(list(text_en = text, detected_language = NA_character_))
  sys_prompt <- paste0(
    "Translate the user's text to English.\n",
    "Return STRICT JSON with keys: detected_language, text_en.\n",
    "detected_language should be a short language code if known (e.g., 'es','fr','zh').\n",
    "If already English, return the same text. Preserve meaning; keep it concise.\n",
    "Respond with ONLY the JSON object. No prose, no markdown, no code fences."
  )
  u_prompt <- paste0("Text:\n", text, "\n\nReturn JSON only (no code fences).")
  out <- anthropic_chat_json(api_key, sys_prompt, u_prompt, model = model,
                             max_tokens = 2000L, temperature = 0, tag = "translate")
  if (is.null(out)) return(list(text_en = text, detected_language = NA_character_))
  list(text_en = out$text_en %||% text, detected_language = out$detected_language %||% NA_character_)
}

translate_cached <- function(text, api_key, do_translate = TRUE, model = DEFAULT_MODEL) {
  if (!do_translate) return(list(text_en = text, detected_language = NA_character_))
  key <- digest::digest(paste0("tr|", text, "|", api_key %||% "no_key", "|model=", model))
  if (!is.null(.cache_env[[key]])) return(.cache_env[[key]])
  res <- translate_with_anthropic(text, api_key, model = model)
  .cache_env[[key]] <- res
  res
}

# ---------- metrics ----------
theme_metrics_multi <- function(theme_df) {
  if (is.null(theme_df) || nrow(theme_df) == 0) {
    return(tibble(theme = character(), mentions = integer(), avg_sentiment = numeric(),
                  satisfied = numeric(), neutral = numeric(), need_to_improve = numeric(),
                  is_new_theme_share = numeric(), severity = numeric(), freq_norm = numeric(), priority = numeric()))
  }
  theme_df %>%
    mutate(score_01 = as.numeric(sentiment_score)/5) %>%
    group_by(theme) %>%
    summarise(mentions = n(), avg_sentiment = mean(score_01, na.rm = TRUE),
              satisfied = mean(sentiment == "satisfied"), neutral = mean(sentiment == "neutral"),
              need_to_improve = mean(sentiment == "need_to_improve"),
              is_new_theme_share = mean(is_new_theme == 1L), .groups = "drop") %>%
    mutate(severity = need_to_improve, freq_norm = mentions / sum(mentions),
           priority = freq_norm * pmax(0.2, severity))
}

trend_prep <- function(theme_df, responses_df) {
  if (is.null(theme_df) || nrow(theme_df) == 0) return(NULL)
  if (is.null(responses_df) || nrow(responses_df) == 0) return(NULL)
  out <- theme_df %>%
    inner_join(responses_df %>% select(response_id, raw_date), by = "response_id") %>%
    mutate(.dt = suppressWarnings(as_date(raw_date))) %>% filter(!is.na(.dt)) %>%
    mutate(month = floor_date(.dt, "month")) %>%
    count(month, theme, name = "n") %>% group_by(month) %>% mutate(share = n / sum(n)) %>% ungroup()
  if (nrow(out) == 0) return(NULL)
  out
}

empty_plotly <- function(msg) {
  plot_ly() %>% layout(annotations = list(list(text = msg, x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16))),
                       xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
}

# ---------- tag matrix ----------
make_tag_matrix <- function(responses_df, themes_df) {
  if (is.null(responses_df) || !nrow(responses_df) || is.null(themes_df) || !nrow(themes_df)) return(tibble())
  theme_universe <- sort(unique(as.character(themes_df$theme))); theme_universe <- theme_universe[nzchar(theme_universe)]
  long <- themes_df %>%
    inner_join(responses_df %>% select(response_id, respondent_id, response_text, extra_1, extra_2, analysis_question),
               by = c("response_id","respondent_id")) %>%
    transmute(respondent_id, response_id, response_text, extra_1, extra_2, analysis_question, tag = as.character(theme)) %>%
    filter(!is.na(tag), nzchar(tag)) %>% distinct()
  if (!nrow(long)) return(tibble())
  wide <- long %>% mutate(tmp = 1L) %>%
    tidyr::pivot_wider(names_from = tag, values_from = tmp, values_fill = 0L, values_fn = max, names_repair = "minimal")
  for (th in theme_universe) if (!(th %in% names(wide))) wide[[th]] <- 0L
  base_cols <- c("respondent_id","response_id","response_text","extra_1","extra_2","analysis_question","tag")
  out_cols <- c(base_cols, theme_universe); out_cols <- out_cols[out_cols %in% names(wide)]
  wide %>% select(all_of(out_cols))
}

# ------------------------------ UI ------------------------------
ui <- fluidPage(
  theme = bs_theme(bootwatch = "flatly"),
  shinybusy::add_busy_spinner(spin = "fading-circle", position = "top-right", timeout = 0),
  shinybusy::add_busy_bar(color = "#2C7BE5", height = "3px"),
  titlePanel("Open-ended Text Insights - Three Modes (Attribute / Exploratory / Codebook)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("file", "Upload data (.sav, .csv, .xlsx)", accept = c(".sav",".csv",".xlsx",".xls")),
      textInput("out_dir", "Output folder (auto-saved files go here)",
                value = DEFAULT_OUTPUT_DIR, placeholder = "e.g. /Users/you/Documents/SoW_outputs"),
      helpText("All CSV/HTML outputs are written here automatically each run. ",
               "Paste a full folder path ('~' is allowed); it is created if missing. ",
               "The download buttons still work independently."),
      verbatimTextOutput("out_dir_show"),
      textInput("api_key", "Anthropic API key (blank = offline fallback)",
                value = Sys.getenv("ANTHROPIC_API_KEY"), placeholder = "sk-ant-..."),
      selectInput("model", "Claude model", choices = MODEL_CHOICES, selected = DEFAULT_MODEL),
      
      radioButtons("analysis_mode", "Question type (analysis mode)", choices = MODE_CHOICES, selected = "attribute"),
      helpText("attribute = reasons + sentiment. ",
               "exploratory = narrative discovery: theme -> sub-theme, with headline/summary/quotes/implication per theme (see 'Exploratory Report' tab). ",
               "codebook = fixed taxonomy from a sample, then classify all against it (clean quant tagging). ",
               "opinion = sentiment-first: split each answer into positive/negative/neutral segments, then build class -> subclass and 'the message' within each sentiment (see 'Opinion Report' tab)."),
      conditionalPanel(
        condition = "input.analysis_mode == 'codebook'",
        numericInput("codebook_n", "Codebook: target # of dimensions", value = CODEBOOK_TARGET_DIM_DEFAULT, min = 4, max = 20, step = 1)
      ),
      conditionalPanel(
        condition = "input.analysis_mode == 'exploratory'",
        numericInput("expl_n", "Exploratory: target # of parent themes", value = 12, min = 8, max = 25, step = 1)
      ),
      conditionalPanel(
        condition = "input.analysis_mode == 'opinion'",
        numericInput("opinion_n", "Opinion: target # of classes per sentiment", value = OPINION_CLASS_TARGET_DEFAULT, min = 3, max = 15, step = 1),
        helpText("Opinion mode makes ~2 API calls per response (segment, then code), plus a message per class. ",
                 "Keep 'Max rows' modest to control cost.")
      ),
      
      textAreaInput("analysis_question", "Question being analysed (optional)", value = "", rows = 3,
                    placeholder = "Example: In your own words, what does 'wealth platform' mean to you?"),
      checkboxInput("use_question_context", "Use question context in theme discovery", TRUE),
      
      uiOutput("colpickers"),
      selectizeInput("extra1_col", "Optional extra column 1 (kept in outputs)", choices = NULL,
                     options = list(placeholder = "Select optional column")),
      selectizeInput("extra2_col", "Optional extra column 2 (kept in outputs)", choices = NULL,
                     options = list(placeholder = "Select optional column")),
      selectizeInput("date_col", "Optional date column (for trending)", choices = NULL,
                     options = list(placeholder = "Select date")),
      
      checkboxInput("translate", "Translate non-English to English (requires API key)", FALSE),
      checkboxInput("use_api", "Use Claude for labeling (if API key present)", TRUE),
      checkboxInput("lock_themes", "Lock discovered theme bank once it starts forming", FALSE),
      
      numericInput("max_rows", "Max rows to process", value = 2000, min = 50, step = 50),
      actionButton("run", "Run processing", class = "btn btn-primary"),
      br(), verbatimTextOutput("run_status"),
      hr(),
      helpText("Run one mode, switch the toggle, Run again on the same data. ",
               "Auto-saved CSVs are suffixed by mode so all runs are kept.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(id = "tabs",
                  tabPanel("Overview",
                           fluidRow(column(7, withSpinner(plotlyOutput("bubble", height = 460))),
                                    column(5, withSpinner(plotlyOutput("bars",   height = 460)))),
                           br(), withSpinner(plotlyOutput("trend",  height = 420))
                  ),
                  tabPanel("Themes",
                           conditionalPanel(
                             condition = "input.analysis_mode == 'codebook'",
                             h4("Induced codebook (codebook mode)"),
                             helpText("Stage 1 taxonomy induced from a sample. Stage 2 classifies every response against this."),
                             withSpinner(tableOutput("codebook_tbl")), hr()
                           ),
                           fluidRow(
                             column(6, h4("New themes introduced this run"), withSpinner(tableOutput("discovered_tbl"))),
                             column(6,
                                    h4("Merge themes (map one theme to another)"),
                                    helpText("Pick a theme, map it to another theme, then click Add/Update mapping."),
                                    uiOutput("merge_controls"),
                                    actionButton("add_merge", "Add / Update mapping", class = "btn btn-secondary"),
                                    actionButton("clear_merges", "Clear mappings", class = "btn btn-outline-danger"),
                                    br(), br(), h5("Current mappings"), withSpinner(tableOutput("merge_tbl"))
                             )
                           ),
                           hr(),
                           actionButton("apply_merges", "Apply mappings to outputs (plots + downloads)", class = "btn btn-primary")
                  ),
                  tabPanel("Advanced",
                           fluidRow(column(6, withSpinner(plotlyOutput("logodds", height = 450))),
                                    column(6, withSpinner(plotlyOutput("ngrams",  height = 450))))
                  ),
                  tabPanel("Data",
                           strong("Responses (1 row per response)"), div(style = "margin-top:8px;"),
                           downloadButton("dl_responses", "Download responses_enriched.csv"), br(), br(),
                           strong("Themes (many rows per response; post-merge)"), div(style = "margin-top:8px;"),
                           downloadButton("dl_themes", "Download themes_enriched.csv"), br(), br(),
                           strong("Theme metrics (post-merge)"), div(style = "margin-top:8px;"),
                           downloadButton("dl_metrics", "Download theme_metrics.csv"), br(), br(),
                           strong("Tag matrix (dynamic theme columns)"), div(style = "margin-top:8px;"),
                           downloadButton("dl_tag_matrix", "Download tag_matrix.csv"), br(), br(),
                           conditionalPanel(condition = "input.analysis_mode == 'codebook'",
                                            strong("Codebook (codebook mode)"), div(style = "margin-top:8px;"),
                                            downloadButton("dl_codebook", "Download codebook.csv"), br(), br()
                           ),
                           tableOutput("preview")
                  ),
                  tabPanel("Exploratory Report",
                           helpText("Produced by exploratory (narrative) mode: theme -> sub-theme framework with a headline, ",
                                    "summary, representative quotes and a business implication per theme."),
                           fluidRow(
                             column(6, downloadButton("dl_expl_report", "Download report (HTML)"),
                                    downloadButton("dl_expl_themes", "Themes CSV")),
                             column(6, downloadButton("dl_expl_subthemes", "Sub-themes CSV"),
                                    downloadButton("dl_expl_tags", "Response tags CSV"),
                                    downloadButton("dl_framework", "Framework CSV"))
                           ),
                           hr(),
                           withSpinner(uiOutput("expl_report")),
                           hr(),
                           h4("Framework (themes & sub-themes)"),
                           withSpinner(tableOutput("framework_tbl"))
                  ),
                  tabPanel("Opinion Report",
                           helpText("Produced by opinion (sentiment-first) mode: every answer is split into ",
                                    "positive / negative / neutral segments, then within each sentiment a class -> subclass ",
                                    "framework is built and 'the message' is written for each class."),
                           fluidRow(
                             column(6, downloadButton("dl_op_report", "Download report (HTML)"),
                                    downloadButton("dl_op_segments", "Segments CSV")),
                             column(6, downloadButton("dl_op_classes", "Classes CSV"),
                                    downloadButton("dl_op_subclasses", "Subclasses CSV"),
                                    downloadButton("dl_op_framework", "Frameworks CSV"))
                           ),
                           hr(),
                           withSpinner(uiOutput("opinion_report")),
                           hr(),
                           h4("Class / subclass frameworks (per sentiment)"),
                           withSpinner(tableOutput("opinion_framework_tbl"))
                  ),
                  tabPanel("PAD space (3D)",
                           helpText("Produced by opinion mode. Each RESPONSE is one point in PAD affect space ",
                                    "(Pleasure / Arousal / Dominance, each -1 to 1). The coloured cloud is a 3D ",
                                    "kernel-density estimate of the sample distribution; markers are individual responses."),
                           fluidRow(
                             column(3, sliderInput("pad_bw", "Density bandwidth", min = 0.10, max = 0.60,
                                                   value = PAD_BANDWIDTH_DEFAULT, step = 0.05)),
                             column(3, sliderInput("pad_grid", "Grid resolution", min = 10, max = 24,
                                                   value = PAD_GRID_N_DEFAULT, step = 2)),
                             column(3, checkboxInput("pad_show_pts", "Show response points", TRUE)),
                             column(3, checkboxInput("pad_show_iso", "Show density surfaces", TRUE))
                           ),
                           downloadButton("dl_pad_scores", "PAD scores CSV"),
                           hr(),
                           withSpinner(plotlyOutput("pad_plot", height = 620)),
                           br(),
                           h4("PAD summary (mean per dominant sentiment)"),
                           withSpinner(tableOutput("pad_summary_tbl"))
                  )
      )
    )
  )
)

# ------------------------------ SERVER ------------------------------
server <- function(input, output, session) {
  
  status_msg <- reactiveVal("Idle")
  output$run_status <- renderText(status_msg())

  # resolved (and created) output directory for all auto-saved files
  out_dir_rv <- reactive({ resolve_out_dir(input$out_dir) })
  output$out_dir_show <- renderText({
    d <- tryCatch(out_dir_rv(), error = function(e) NULL)
    if (is.null(d)) "Saving to: (default)" else paste0("Saving to: ", d)
  })
  
  responses_rv     <- reactiveVal(NULL)
  themes_raw_rv    <- reactiveVal(NULL)
  themes_final_rv  <- reactiveVal(NULL)
  tm_rv            <- reactiveVal(NULL)
  tag_matrix_rv    <- reactiveVal(NULL)
  last_mode_rv     <- reactiveVal("attribute")
  codebook_rv      <- reactiveVal(NULL)
  framework_rv     <- reactiveVal(NULL)
  expl_tags_rv     <- reactiveVal(NULL)
  expl_themes_rv   <- reactiveVal(NULL)
  expl_subthemes_rv<- reactiveVal(NULL)
  expl_quotes_rv   <- reactiveVal(NULL)
  report_html_rv   <- reactiveVal(NULL)

  # ---- opinion (sentiment-first) reactives ----
  op_segments_rv     <- reactiveVal(NULL)
  op_sent_summary_rv <- reactiveVal(NULL)
  op_classes_rv      <- reactiveVal(NULL)
  op_subclasses_rv   <- reactiveVal(NULL)
  op_framework_rv    <- reactiveVal(NULL)
  op_report_html_rv  <- reactiveVal(NULL)
  pad_scores_rv      <- reactiveVal(NULL)

  merges_rv <- reactiveVal(tibble(from = character(), to = character()))
  themes_bank_rv <- reactiveVal(DEFAULT_THEME_BANK)
  
  dat <- reactive({ req(input$file); log_msg("Loading file..."); as_tibble(read_any_file(input$file$datapath)) })
  
  output$colpickers <- renderUI({
    req(dat())
    tagList(
      selectizeInput("id_col",   "Respondent ID column", choices = NULL, options = list(placeholder = "Select ID")),
      selectizeInput("text_col", "Open-ended text column", choices = NULL, options = list(placeholder = "Select text field"))
    )
  })
  
  observeEvent(dat(), {
    cols <- names(dat())
    updateSelectizeInput(session, "id_col",   choices = cols, server = TRUE)
    updateSelectizeInput(session, "text_col", choices = cols, server = TRUE)
    updateSelectizeInput(session, "extra1_col", choices = c("", cols), server = TRUE)
    updateSelectizeInput(session, "extra2_col", choices = c("", cols), server = TRUE)
    updateSelectizeInput(session, "date_col",   choices = c("", cols), server = TRUE)
    status_msg("Dataset loaded. Select columns and click Run.")
  }, ignoreInit = TRUE)
  
  output$merge_controls <- renderUI({
    th <- themes_raw_rv()
    if (is.null(th) || !nrow(th)) return(helpText("Run processing first."))
    all_themes <- sort(unique(th$theme))
    if (!length(all_themes)) return(helpText("No themes found in this run."))
    tagList(
      selectizeInput("merge_from", "Source theme", choices = all_themes, options = list(placeholder = "Select source theme")),
      selectizeInput("merge_to", "Target theme", choices = all_themes, options = list(placeholder = "Select target theme"))
    )
  })
  
  observeEvent(input$add_merge, {
    req(input$merge_from, input$merge_to)
    m <- merges_rv() %>% filter(from != input$merge_from) %>% bind_rows(tibble(from = input$merge_from, to = input$merge_to))
    merges_rv(m); showNotification("Mapping updated.", type = "message", duration = 2)
  })
  observeEvent(input$clear_merges, {
    merges_rv(tibble(from = character(), to = character())); showNotification("Mappings cleared.", type = "message", duration = 2)
  })
  output$merge_tbl <- renderTable({ merges_rv() })
  output$codebook_tbl <- renderTable({
    cb <- codebook_rv()
    if (is.null(cb) || !nrow(cb)) return(data.frame(Message = "Run codebook mode to induce a taxonomy."))
    as.data.frame(cb)
  })
  
  save_outputs <- function(mode_tag, responses_df, themes_df, tm_df, tag_df, codebook_df = NULL) {
    outdir <- resolve_out_dir(isolate(input$out_dir))
    sfx <- function(base) file.path(outdir, sprintf("%s_%s.csv", base, mode_tag))
    write_csv(responses_df, sfx("responses_enriched"))
    write_csv(themes_df,    sfx("themes_enriched"))
    write_csv(tm_df,        sfx("theme_metrics"))
    write_csv(tag_df,       sfx("tag_matrix"))
    if (!is.null(codebook_df) && nrow(codebook_df)) write_csv(codebook_df, sfx("codebook"))
    normalizePath(outdir)
  }
  
  apply_theme_merges <- function(theme_df, merges_df) {
    if (is.null(theme_df) || !nrow(theme_df)) return(theme_df)
    if (is.null(merges_df) || !nrow(merges_df)) return(dedupe_theme_rows(theme_df))
    map <- setNames(merges_df$to, merges_df$from)
    theme_df %>%
      mutate(theme_original = theme,
             theme = if_else(theme %in% names(map), unname(map[theme]), theme),
             is_new_theme = if_else(theme_original %in% names(map), 0L, is_new_theme)) %>%
      select(-theme_original) %>% dedupe_theme_rows()
  }
  
  observeEvent(input$apply_merges, {
    th <- themes_raw_rv(); if (is.null(th)) return(NULL)
    th2 <- apply_theme_merges(th, merges_rv())
    themes_final_rv(th2); tm_rv(theme_metrics_multi(th2)); tag_matrix_rv(make_tag_matrix(responses_rv(), th2))
    save_outputs(last_mode_rv(), responses_rv(), th2, tm_rv(), tag_matrix_rv(), codebook_rv())
    showNotification("Mappings applied to outputs (and files updated).", type = "message", duration = 3)
  })
  
  output$discovered_tbl <- renderTable({
    th <- themes_raw_rv()
    if (is.null(th) || !nrow(th)) return(data.frame(Message = "Run processing first."))
    th %>% filter(is_new_theme == 1L) %>% count(theme, sort = TRUE, name = "mentions") %>% as.data.frame()
  })
  
  observeEvent(input$run, {
    log_msg("Run clicked."); showNotification("Run clicked - starting...", type = "message")
    
    df   <- isolate(dat())
    idc  <- isolate(input$id_col); txc <- isolate(input$text_col)
    exc1 <- isolate(input$extra1_col); exc2 <- isolate(input$extra2_col); dcc <- isolate(input$date_col)
    model_sel <- isolate(input$model) %||% DEFAULT_MODEL
    mode_sel  <- isolate(input$analysis_mode) %||% "attribute"
    codebook_n <- isolate(input$codebook_n) %||% CODEBOOK_TARGET_DIM_DEFAULT
    expl_n     <- isolate(input$expl_n) %||% 12
    opinion_n  <- isolate(input$opinion_n) %||% OPINION_CLASS_TARGET_DEFAULT
    max_themes_sel <- switch(mode_sel,
                             exploratory = MAX_THEMES_EXPLORATORY,
                             codebook    = MAX_THEMES_CODEBOOK,
                             opinion     = MAX_CLASSES_PER_SEGMENT,
                             MAX_THEMES_ATTRIBUTE)
    question_ctx <- if (isTRUE(isolate(input$use_question_context))) stringr::str_squish(isolate(input$analysis_question)) else ""
    
    if (is.null(df)) { showNotification("No data loaded.", type = "error"); status_msg("No data loaded."); return(NULL) }
    if (!nzchar(idc) || !nzchar(txc)) { showNotification("Please select ID and Text columns.", type = "warning"); status_msg("Missing column selections."); return(NULL) }
    if (!(idc %in% names(df) && txc %in% names(df))) { showNotification("Selected columns do not exist.", type = "error"); status_msg("Invalid column names."); return(NULL) }
    
    showNotification("Preparing responses...", type = "message"); status_msg("Preparing responses...")
    prepared <- tryCatch(prepare_single_responses(df, idc, txc, date_var = dcc, extra1_var = exc1, extra2_var = exc2),
                         error = function(e) { showNotification(paste("Prep error:", e$message), type = "error"); NULL })
    if (is.null(prepared) || !nrow(prepared)) {
      showNotification("No non-empty responses found.", type = "warning"); status_msg("No responses to process.")
      responses_rv(NULL); themes_raw_rv(NULL); themes_final_rv(NULL); tm_rv(NULL); tag_matrix_rv(NULL); return(NULL)
    }
    prepared <- prepared %>% mutate(response_id = row_number())
    if (nrow(prepared) > input$max_rows) {
      prepared <- slice_head(prepared, n = input$max_rows)
      showNotification(paste0("Capped to ", input$max_rows, " rows (cost control)."), type = "warning", duration = 5)
      log_msg("Capped rows to", input$max_rows)
    }
    
    api_key <- trimws(input$api_key)
    use_api <- isTRUE(input$use_api) && nzchar(api_key)
    do_translate <- isTRUE(input$translate) && nzchar(api_key)
    lock_themes <- isTRUE(input$lock_themes)
    
    # reset run state
    merges_rv(tibble(from = character(), to = character()))
    themes_bank_rv(DEFAULT_THEME_BANK)
    themes_raw_rv(NULL); themes_final_rv(NULL); tm_rv(NULL); tag_matrix_rv(NULL); codebook_rv(NULL)
    framework_rv(NULL); expl_tags_rv(NULL); expl_themes_rv(NULL); expl_subthemes_rv(NULL)
    expl_quotes_rv(NULL); report_html_rv(NULL)
    op_segments_rv(NULL); op_sent_summary_rv(NULL); op_classes_rv(NULL)
    op_subclasses_rv(NULL); op_framework_rv(NULL); op_report_html_rv(NULL)
    pad_scores_rv(NULL)

    mode_run <- mode_sel
    
    # ---- Stage 1: codebook induction (codebook mode only) ----
    cb <- NULL
    if (identical(mode_sel, "codebook")) {
      if (!use_api) {
        showNotification("Codebook mode needs an API key; falling back to exploratory.", type = "warning", duration = 6)
        mode_run <- "exploratory"; max_themes_sel <- MAX_THEMES_EXPLORATORY
      } else {
        status_msg("Stage 1: inducing codebook from sample...")
        showNotification("Inducing codebook (stage 1)...", type = "message", duration = 4)
        sample_src <- prepared$response_text
        if (do_translate) {
          # translate the sample first so induction reads English concepts
          n_s <- min(length(sample_src), CODEBOOK_SAMPLE_N)
          idx <- sort(sample.int(length(sample_src), n_s))
          sample_src <- vapply(sample_src[idx], function(x) translate_cached(x, api_key, TRUE, model_sel)$text_en %||% x, character(1))
        }
        cb <- tryCatch(
          induce_codebook_with_anthropic(sample_src, api_key, model = model_sel,
                                         question_context = question_ctx, target_n = as.integer(codebook_n)),
          error = function(e) { log_msg("Codebook induction error:", conditionMessage(e)); tibble(name=character(), definition=character()) }
        )
        if (is.null(cb) || !nrow(cb)) {
          showNotification("Codebook induction failed/empty; falling back to exploratory.", type = "warning", duration = 6)
          mode_run <- "exploratory"; max_themes_sel <- MAX_THEMES_EXPLORATORY; cb <- NULL
        } else {
          codebook_rv(cb)
          log_msg("Codebook induced with", nrow(cb), "dimensions.")
          showNotification(paste0("Codebook ready: ", nrow(cb), " dimensions."), type = "message", duration = 4)
        }
      }
    }
    
    # ==================================================================
    # EXPLORATORY NARRATIVE PIPELINE (theme -> sub-theme -> synthesis)
    # ==================================================================
    if (identical(mode_run, "exploratory")) {
      last_mode_rv("exploratory")
      if (!use_api) {
        showNotification("Exploratory (narrative) mode needs an API key.", type = "error", duration = 6)
        status_msg("Exploratory mode requires an API key.")
        return(NULL)
      }
      
      # ---- Stage 1: framework ----
      status_msg("Stage 1: inducing theme / sub-theme framework...")
      showNotification("Inducing theme/sub-theme framework (stage 1)...", type = "message", duration = 4)
      sample_src <- prepared$response_text
      if (do_translate) {
        n_s <- min(length(sample_src), CODEBOOK_SAMPLE_N)
        idx <- sort(sample.int(length(sample_src), n_s))
        sample_src <- vapply(sample_src[idx], function(x) translate_cached(x, api_key, TRUE, model_sel)$text_en %||% x, character(1))
      }
      fw <- tryCatch(
        induce_framework_with_anthropic(sample_src, api_key, model = model_sel,
                                        question_context = question_ctx, target_n = as.integer(expl_n)),
        error = function(e) { log_msg("Framework induction error:", conditionMessage(e)); NULL }
      )
      if (is.null(fw) || !nrow(fw)) {
        showNotification("Framework induction failed/empty. Check console / API credits.", type = "error", duration = 8)
        status_msg("Framework induction failed.")
        return(NULL)
      }
      framework_rv(fw)
      fw_text <- framework_to_text(fw)
      fw_sig  <- digest::digest(fw_text)
      theme_names <- canonical_theme(unique(fw$theme))
      theme_defs  <- fw %>% distinct(theme, theme_def)
      sub_map <- fw %>% filter(!is.na(sub_theme), nzchar(sub_theme)) %>%
        group_by(theme) %>% summarise(subs = list(unique(sub_theme)), .groups = "drop")
      sub_map <- stats::setNames(sub_map$subs, sub_map$theme)
      
      # ---- Stage 2: code every response ----
      n <- nrow(prepared)
      status_msg(sprintf("Stage 2: coding %d responses | model=%s ...", n, model_sel))
      prog <- shiny::Progress$new(session, min = 0, max = n)
      on.exit(prog$close(), add = TRUE)
      
      resp_rows <- vector("list", n); tag_rows <- vector("list", n)
      for (i in seq_len(n)) {
        prog$set(value = i, message = "Coding responses", detail = paste(i, "of", n))
        txt <- prepared$response_text[i]
        det <- detect_lang(txt)
        translated <- if (do_translate) translate_cached(txt, api_key, TRUE, model_sel) else list(text_en = txt, detected_language = det)
        txt_en <- translated$text_en %||% txt
        lang_guess <- translated$detected_language %||% det
        
        asg <- tryCatch(
          cached_code_framework(txt_en, api_key, fw_text, theme_names, sub_map,
                                model_sel, question_ctx, 2L, fw_sig),
          error = function(e) tibble(theme = character(), sub_theme = character(), evidence = character())
        )
        
        resp_rows[[i]] <- tibble(
          response_id = prepared$response_id[i], respondent_id = prepared$respondent_id[i],
          response_text = prepared$response_text[i], extra_1 = prepared$extra_1[i], extra_2 = prepared$extra_2[i],
          raw_date = prepared$raw_date[i],
          analysis_question = if (nzchar(question_ctx)) question_ctx else NA_character_,
          analysis_mode = "exploratory",
          detected_language = lang_guess %||% NA_character_, text_en = txt_en, clean_text = normalise_text(txt_en),
          flag_useless = as.integer(nrow(asg) == 0), discovery = 0L
        )
        tag_rows[[i]] <- if (!nrow(asg)) tibble() else
          asg %>% mutate(response_id = prepared$response_id[i], respondent_id = prepared$respondent_id[i])
        if (i %% 100 == 0) log_msg("Coded", i, "rows")
      }
      
      responses_enriched <- bind_rows(resp_rows)
      expl_tags <- bind_rows(tag_rows)
      if (nrow(expl_tags)) {
        expl_tags <- expl_tags %>% mutate(theme = canonical_theme(theme),
                                          sub_theme = canonical_theme(sub_theme)) %>%
          filter(nzchar(theme))
      }
      
      total_resp <- nrow(responses_enriched)
      
      # ---- counts ----
      theme_counts <- if (nrow(expl_tags)) {
        expl_tags %>% distinct(response_id, theme) %>% count(theme, name = "mentions") %>%
          mutate(share = mentions / total_resp)
      } else tibble(theme = character(), mentions = integer(), share = numeric())
      
      sub_counts <- if (nrow(expl_tags)) {
        expl_tags %>% filter(!is.na(sub_theme), nzchar(sub_theme)) %>%
          distinct(response_id, theme, sub_theme) %>% count(theme, sub_theme, name = "mentions") %>%
          mutate(share = mentions / total_resp)
      } else tibble(theme = character(), sub_theme = character(), mentions = integer(), share = numeric())
      
      # ---- Stage 3: synthesis per theme ----
      status_msg("Stage 3: writing per-theme narrative summaries...")
      showNotification("Synthesising theme narratives (stage 3)...", type = "message", duration = 4)
      themes_sorted <- theme_counts %>% arrange(desc(mentions)) %>% pull(theme)
      prog2 <- shiny::Progress$new(session, min = 0, max = max(length(themes_sorted), 1))
      on.exit(prog2$close(), add = TRUE)
      
      summ_rows <- list(); quotes_list <- list()
      for (j in seq_along(themes_sorted)) {
        th <- themes_sorted[j]
        prog2$set(value = j, message = "Synthesising themes", detail = th)
        tdef <- theme_defs$theme_def[match(th, theme_defs$theme)] %||% ""
        rids <- expl_tags %>% filter(theme == th) %>% distinct(response_id) %>% pull(response_id)
        texts <- responses_enriched %>% filter(response_id %in% rids) %>% pull(text_en)
        if (length(texts) > 50) { set.seed(7); texts <- sample(texts, 50) }
        subc <- sub_counts %>% filter(theme == th) %>% arrange(desc(mentions))
        syn <- tryCatch(
          synthesize_theme_anthropic(th, tdef, subc, texts, api_key, model = model_sel, question_context = question_ctx),
          error = function(e) list(headline = th, summary = "", quotes = character(0), business_implication = "")
        )
        men <- theme_counts$mentions[match(th, theme_counts$theme)]
        summ_rows[[j]] <- tibble(theme = th, mentions = men, share = men / total_resp,
                                 headline = syn$headline, summary = syn$summary,
                                 business_implication = syn$business_implication,
                                 quotes = paste(syn$quotes, collapse = " | "))
        quotes_list[[th]] <- syn$quotes
      }
      themes_summ <- bind_rows(summ_rows)
      
      # ---- assemble outputs ----
      subthemes_out <- sub_counts %>%
        left_join(fw %>% distinct(theme, sub_theme, sub_theme_def), by = c("theme","sub_theme")) %>%
        arrange(theme, desc(mentions))
      tags_out <- if (nrow(expl_tags)) {
        expl_tags %>% left_join(responses_enriched %>% select(response_id, response_text), by = "response_id") %>%
          select(respondent_id, response_id, theme, sub_theme, evidence, response_text)
      } else tibble()
      
      report_html <- build_exploratory_report_html(question_ctx, total_resp, themes_summ, sub_counts, quotes_list)
      
      # populate reactives (report + exploratory artifacts)
      framework_rv(fw); expl_tags_rv(tags_out); expl_themes_rv(themes_summ)
      expl_subthemes_rv(subthemes_out); expl_quotes_rv(quotes_list); report_html_rv(report_html)
      
      # also populate the standard reactives so the Data tab + bar chart still work
      themes_raw_compat <- if (nrow(expl_tags)) {
        expl_tags %>% transmute(respondent_id, response_id, theme,
                                is_new_theme = 0L, sentiment = "neutral", sentiment_score = 0L,
                                evidence = evidence, rationale = coalesce(sub_theme, "")) %>%
          dedupe_theme_rows()
      } else tibble()
      responses_rv(responses_enriched); themes_raw_rv(themes_raw_compat); themes_final_rv(themes_raw_compat)
      tm_rv(theme_metrics_multi(themes_raw_compat)); tag_matrix_rv(make_tag_matrix(responses_enriched, themes_raw_compat))
      
      # ---- save files ----
      outdir <- resolve_out_dir(isolate(input$out_dir))
      write_csv(responses_enriched, file.path(outdir, "responses_enriched_exploratory.csv"))
      write_csv(themes_summ,        file.path(outdir, "exploratory_themes.csv"))
      write_csv(subthemes_out,      file.path(outdir, "exploratory_subthemes.csv"))
      write_csv(tags_out,           file.path(outdir, "exploratory_response_tags.csv"))
      write_csv(fw,                 file.path(outdir, "exploratory_framework.csv"))
      writeLines(report_html,       file.path(outdir, "exploratory_report.html"))
      
      log_msg("Exploratory narrative complete:", nrow(themes_summ), "themes,", nrow(subthemes_out), "sub-themes.")
      showNotification(paste0("Exploratory report ready: ", nrow(themes_summ),
                              " themes. See the 'Exploratory Report' tab."), type = "message", duration = 7)
      status_msg(paste0("Done (exploratory narrative). ", nrow(themes_summ), " themes. See Exploratory Report tab."))
      return(NULL)
    }

    # ==================================================================
    # OPINION PIPELINE (segment -> per-sentiment class/subclass -> message)
    # ==================================================================
    if (identical(mode_run, "opinion")) {
      last_mode_rv("opinion")
      if (!use_api) {
        showNotification("Opinion mode needs an API key.", type = "error", duration = 6)
        status_msg("Opinion mode requires an API key.")
        return(NULL)
      }

      n <- nrow(prepared)

      # ---- Stage 1: segment + sentiment for every response ----
      status_msg(sprintf("Stage 1: segmenting & scoring sentiment for %d responses | model=%s ...", n, model_sel))
      showNotification("Stage 1: sentiment segmentation...", type = "message", duration = 4)
      prog <- shiny::Progress$new(session, min = 0, max = n)
      on.exit(prog$close(), add = TRUE)

      resp_rows <- vector("list", n); seg_rows <- vector("list", n)
      for (i in seq_len(n)) {
        prog$set(value = i, message = "Segmenting responses", detail = paste(i, "of", n))
        txt <- prepared$response_text[i]
        det <- detect_lang(txt)
        translated <- if (do_translate) translate_cached(txt, api_key, TRUE, model_sel) else list(text_en = txt, detected_language = det)
        txt_en <- translated$text_en %||% txt
        lang_guess <- translated$detected_language %||% det

        res <- tryCatch(
          cached_segment_sentiment(txt_en, api_key, model_sel, question_ctx, MAX_SEGMENTS_PER_RESPONSE),
          error = function(e) list(segments = tibble(segment_text = character(), sentiment = character(),
                                                     rationale = character(), evidence = character()),
                                   pad = c(pleasure = NA_real_, arousal = NA_real_, dominance = NA_real_))
        )
        segs <- res$segments
        pad  <- res$pad

        resp_rows[[i]] <- tibble(
          response_id = prepared$response_id[i], respondent_id = prepared$respondent_id[i],
          response_text = prepared$response_text[i], extra_1 = prepared$extra_1[i], extra_2 = prepared$extra_2[i],
          raw_date = prepared$raw_date[i],
          analysis_question = if (nzchar(question_ctx)) question_ctx else NA_character_,
          analysis_mode = "opinion",
          detected_language = lang_guess %||% NA_character_, text_en = txt_en, clean_text = normalise_text(txt_en),
          pleasure = unname(pad["pleasure"]), arousal = unname(pad["arousal"]), dominance = unname(pad["dominance"]),
          flag_useless = as.integer(nrow(segs) == 0), discovery = 0L
        )
        seg_rows[[i]] <- if (!nrow(segs)) tibble() else
          segs %>% mutate(response_id = prepared$response_id[i], respondent_id = prepared$respondent_id[i],
                          segment_id = paste0(prepared$response_id[i], "_", row_number()))
        if (i %% 100 == 0) log_msg("Segmented", i, "rows")
      }

      responses_enriched <- bind_rows(resp_rows)
      segments_all <- bind_rows(seg_rows)
      total_resp <- nrow(responses_enriched)

      if (!nrow(segments_all)) {
        showNotification("No usable sentiment segments produced.", type = "warning", duration = 6)
        status_msg("Opinion mode: no segments produced.")
        responses_rv(responses_enriched)
        return(NULL)
      }

      # sentiment split across all segments
      sentiment_summary <- segments_all %>%
        count(sentiment, name = "segments") %>%
        mutate(share = segments / sum(segments)) %>%
        arrange(match(sentiment, OPINION_SENTIMENT_LEVELS))

      # ---- Stages 2-3: ADAPTIVE per-sentiment coding ----
      # Seed a framework, code segments (snap-to-nearest OR explicit "does not fit"),
      # and whenever the unclassified pile is a fair share, induce NEW classes from it
      # and re-code. Only a tiny true residual becomes "Other". No valid segment -> NA.
      status_msg("Stages 2-3: adaptive class discovery + coding...")
      showNotification("Stages 2-3: adaptive class discovery + coding...", type = "message", duration = 4)
      buckets <- intersect(OPINION_SENTIMENT_LEVELS, unique(segments_all$sentiment))

      fw_by_sent <- list()
      coded_list <- list()

      for (s in buckets) {
        seg_s <- segments_all %>% filter(sentiment == s)
        n_s <- nrow(seg_s)
        if (!n_s) next

        fw_s <- tryCatch(
          induce_opinion_framework(seg_s$segment_text, api_key, sentiment = s, model = model_sel,
                                   question_context = question_ctx, target_n = as.integer(opinion_n)),
          error = function(e) { log_msg("Opinion framework error [", s, "]:", conditionMessage(e)); NULL }
        )
        if (is.null(fw_s) || !nrow(fw_s)) {
          coded_list[[s]] <- seg_s %>%
            transmute(sentiment = s, response_id, respondent_id, segment_id, segment_text,
                      class = OPINION_OTHER_LABEL, subclass = NA_character_, evidence)
          next
        }

        pending <- seg_s
        coded_s <- list()
        round  <- 0L
        repeat {
          round <- round + 1L
          meta   <- opinion_fw_meta(fw_s, s)
          fw_sig <- meta$fw_sig
          np <- nrow(pending)
          status_msg(sprintf("Coding %s (round %d): %d segments | %d classes",
                             s, round, np, length(meta$class_names)))
          prog_r <- shiny::Progress$new(session, min = 0, max = max(np, 1))
          fit_flags <- logical(np); cls_v <- character(np); sub_v <- character(np); ev_v <- character(np)
          for (k in seq_len(np)) {
            prog_r$set(value = k, message = paste0("Coding ", s, " (round ", round, ")"),
                       detail = paste(k, "of", np))
            a <- tryCatch(
              cached_assign_segment(pending$segment_text[k], api_key, meta$fw_text,
                                    meta$class_names, meta$sub_map, model_sel, question_ctx, fw_sig),
              error = function(e) list(class = NA_character_, subclass = NA_character_, evidence = "", fits = FALSE)
            )
            fit_flags[k] <- isTRUE(a$fits) && !is.na(a$class)
            cls_v[k] <- a$class %||% NA_character_
            sub_v[k] <- a$subclass %||% NA_character_
            ev_v[k]  <- if (nzchar(a$evidence %||% "")) a$evidence else pending$evidence[k]
          }
          prog_r$close()

          if (any(fit_flags)) {
            m <- pending[fit_flags, , drop = FALSE]
            coded_s[[length(coded_s) + 1]] <- m %>%
              mutate(class = canonical_theme(cls_v[fit_flags]),
                     subclass = canonical_theme(sub_v[fit_flags]),
                     evidence = ev_v[fit_flags]) %>%
              transmute(sentiment = s, response_id, respondent_id, segment_id, segment_text, class, subclass, evidence)
          }
          unc <- pending[!fit_flags, , drop = FALSE]

          gap_worth_it <- nrow(unc) >= max(OPINION_GAP_MIN_N, ceiling(OPINION_GAP_MIN_SHARE * n_s))
          if (!nrow(unc) || round > OPINION_MAX_GAP_ROUNDS || !gap_worth_it) { pending <- unc; break }

          status_msg(sprintf("Discovering new %s classes from %d unclassified segments...", s, nrow(unc)))
          showNotification(sprintf("Growing %s framework from %d unclassified segments...", s, nrow(unc)),
                           type = "message", duration = 3)
          new_fw <- tryCatch(
            induce_opinion_framework(unc$segment_text, api_key, sentiment = s, model = model_sel,
                                     question_context = paste0(question_ctx,
                                       " | These segments did NOT fit the existing classes; induce the NEW classes they form."),
                                     target_n = OPINION_GAP_TARGET_N),
            error = function(e) NULL
          )
          if (is.null(new_fw) || !nrow(new_fw)) { pending <- unc; break }
          fw_s <- bind_rows(fw_s, new_fw) %>% distinct(theme, sub_theme, .keep_all = TRUE)
          pending <- unc   # re-code only the leftovers against the expanded framework
        }

        # final residual -> Other (still visible + counted, never dropped)
        if (nrow(pending)) {
          coded_s[[length(coded_s) + 1]] <- pending %>%
            transmute(sentiment = s, response_id, respondent_id, segment_id, segment_text,
                      class = OPINION_OTHER_LABEL, subclass = NA_character_, evidence)
        }
        fw_by_sent[[s]] <- fw_s %>% mutate(sentiment = s)
        coded_list[[s]] <- bind_rows(coded_s)
        log_msg("Coded bucket", s, "-", nrow(coded_list[[s]]), "segments,", nrow(fw_s %>% distinct(theme)), "classes.")
      }

      coded <- bind_rows(coded_list)
      if (!nrow(coded) || !("class" %in% names(coded))) {
        showNotification("Could not code any segments. Check console / API credits.", type = "error", duration = 8)
        status_msg("Opinion mode: coding failed.")
        responses_rv(responses_enriched)
        return(NULL)
      }
      coded <- coded %>% mutate(class = if_else(!is.na(class) & nzchar(class), class, OPINION_OTHER_LABEL))

      framework_all <- if (length(fw_by_sent)) {
        bind_rows(fw_by_sent) %>%
          select(sentiment, class = theme, class_def = theme_def, subclass = sub_theme, subclass_def = sub_theme_def)
      } else tibble(sentiment = character(), class = character(), class_def = character(),
                    subclass = character(), subclass_def = character())

      # coverage: Other is counted as NOT placed in a named class
      n_total_seg <- nrow(coded)
      n_other     <- sum(coded$class == OPINION_OTHER_LABEL)
      coverage <- list(total = n_total_seg, classified = n_total_seg - n_other,
                       classified_share = (n_total_seg - n_other) / max(n_total_seg, 1),
                       other = n_other, other_share = n_other / max(n_total_seg, 1))

      # every segment now carries a class (named or Other) - none left as NA
      segments_out <- segments_all %>%
        left_join(coded %>% select(segment_id, class, subclass), by = "segment_id") %>%
        mutate(class = coalesce(class, OPINION_OTHER_LABEL)) %>%
        select(respondent_id, response_id, segment_id, sentiment, segment_text, class, subclass, rationale, evidence)

      # ---- counts within each sentiment ----
      sent_seg_totals <- coded %>% count(sentiment, name = "sent_total")
      class_counts <- coded %>%
        count(sentiment, class, name = "mentions") %>%
        left_join(sent_seg_totals, by = "sentiment") %>%
        mutate(share = mentions / pmax(sent_total, 1)) %>% select(-sent_total)
      subclass_counts <- coded %>%
        filter(!is.na(subclass), nzchar(subclass)) %>%
        count(sentiment, class, subclass, name = "mentions") %>%
        left_join(sent_seg_totals, by = "sentiment") %>%
        mutate(share = mentions / pmax(sent_total, 1)) %>% select(-sent_total)

      # ---- Stage 4: synthesize "the message" for each class ----
      status_msg("Stage 4: writing the message for each class...")
      showNotification("Stage 4: writing class messages...", type = "message", duration = 4)
      class_list <- class_counts %>% arrange(sentiment, desc(mentions))
      prog3 <- shiny::Progress$new(session, min = 0, max = max(nrow(class_list), 1))
      on.exit(prog3$close(), add = TRUE)

      # class definitions lookup (from the expanded frameworks); Other gets a fixed def
      class_def_lookup <- framework_all %>% distinct(sentiment, class, class_def)
      get_class_def <- function(sent, cl) {
        if (identical(cl, OPINION_OTHER_LABEL)) return("Segments that did not fit any induced class.")
        d <- class_def_lookup %>% filter(sentiment == sent, class == cl) %>% pull(class_def)
        if (length(d) && nzchar(d[1] %||% "")) d[1] else ""
      }

      summ_rows <- list(); quotes_map <- list()
      for (j in seq_len(nrow(class_list))) {
        s  <- class_list$sentiment[j]; cl <- class_list$class[j]
        prog3$set(value = j, message = "Writing messages", detail = paste0(s, ": ", cl))
        cdef <- get_class_def(s, cl)
        texts <- coded %>% filter(sentiment == s, class == cl) %>% pull(segment_text)
        # richer context: add the original evidence quotes for this class
        ev <- coded %>% filter(sentiment == s, class == cl) %>% pull(evidence)
        texts <- unique(c(texts, ev)); texts <- texts[nzchar(str_squish(texts))]
        if (length(texts) > 50) { set.seed(7); texts <- sample(texts, 50) }
        subc <- subclass_counts %>% filter(sentiment == s, class == cl) %>%
          arrange(desc(mentions)) %>% transmute(sub_theme = subclass, mentions)
        qctx <- paste0(question_ctx, " | Analysing the ", s, " opinions about class: ", cl)
        syn <- tryCatch(
          synthesize_theme_anthropic(cl, cdef, subc, texts, api_key, model = model_sel, question_context = qctx),
          error = function(e) list(headline = cl, summary = "", quotes = character(0), business_implication = "")
        )
        # ---- pick the 3 most informative RAW (verbatim) responses for this class ----
        raw_ids_cls <- coded %>% filter(sentiment == s, class == cl) %>% pull(response_id) %>% unique()
        raw_texts_cls <- responses_enriched %>% filter(response_id %in% raw_ids_cls) %>% pull(response_text)
        raw_examples_vec <- tryCatch(
          select_raw_examples_anthropic(raw_texts_cls, api_key, cl, cdef, s,
                                        model = model_sel, question_context = question_ctx, n = 3),
          error = function(e) head(unique(str_squish(raw_texts_cls[nzchar(str_squish(raw_texts_cls))])), 3)
        )
        raw_examples_str <- format_raw_examples(raw_examples_vec)
        men <- class_list$mentions[j]; shr <- class_list$share[j]
        summ_rows[[j]] <- tibble(sentiment = s, class = cl, class_def = cdef,
                                 mentions = men, share = shr,
                                 headline = syn$headline, summary = syn$summary,
                                 business_implication = syn$business_implication,
                                 quotes = paste(syn$quotes, collapse = " | "),
                                 raw_examples = raw_examples_str)
        quotes_map[[paste0(s, "||", cl)]] <- syn$quotes
      }
      classes_summ <- bind_rows(summ_rows)

      report_html <- build_opinion_report_html(question_ctx, total_resp, sentiment_summary,
                                                classes_summ, subclass_counts, quotes_map,
                                                coverage = coverage)

      # ---- PAD affect scores (one point per response) ----
      pad_scores <- responses_enriched %>%
        transmute(respondent_id, response_id, response_text,
                  sentiment_lean = NA_character_,
                  pleasure, arousal, dominance) %>%
        mutate(across(c(pleasure, arousal, dominance), ~ clamp11(.)))
      # tag each response with its dominant segment sentiment (for colouring the cloud)
      dom_sent <- segments_all %>%
        count(response_id, sentiment) %>%
        group_by(response_id) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
        select(response_id, dom_sentiment = sentiment)
      pad_scores <- pad_scores %>% left_join(dom_sent, by = "response_id") %>%
        mutate(sentiment_lean = coalesce(dom_sentiment, "neutral")) %>% select(-dom_sentiment)
      pad_scores_rv(pad_scores)

      # ---- populate opinion reactives ----
      op_segments_rv(segments_out); op_sent_summary_rv(sentiment_summary)
      op_classes_rv(classes_summ); op_subclasses_rv(subclass_counts)
      op_framework_rv(framework_all); op_report_html_rv(report_html)

      # ---- populate standard reactives so Overview/Data tabs keep working ----
      themes_raw_compat <- coded %>%
        mutate(.legacy_sent = opinion_to_legacy_sentiment(sentiment),
               .legacy_score = opinion_sentiment_score(sentiment)) %>%
        transmute(respondent_id, response_id,
                  theme = class, is_new_theme = 0L,
                  sentiment = .legacy_sent,
                  sentiment_score = .legacy_score,
                  evidence = evidence,
                  rationale = coalesce(subclass, "")) %>%
        dedupe_theme_rows()
      responses_rv(responses_enriched); themes_raw_rv(themes_raw_compat); themes_final_rv(themes_raw_compat)
      tm_rv(theme_metrics_multi(themes_raw_compat)); tag_matrix_rv(make_tag_matrix(responses_enriched, themes_raw_compat))

      # ---- save files ----
      outdir <- resolve_out_dir(isolate(input$out_dir))
      write_csv(responses_enriched, file.path(outdir, "responses_enriched_opinion.csv"))
      write_csv(segments_out,       file.path(outdir, "opinion_segments.csv"))
      write_csv(classes_summ,       file.path(outdir, "opinion_classes.csv"))
      write_csv(subclass_counts,    file.path(outdir, "opinion_subclasses.csv"))
      write_csv(sentiment_summary,  file.path(outdir, "opinion_sentiment_summary.csv"))
      write_csv(framework_all,      file.path(outdir, "opinion_frameworks.csv"))
      write_csv(pad_scores,         file.path(outdir, "opinion_pad_scores.csv"))
      writeLines(report_html,       file.path(outdir, "opinion_report.html"))

      log_msg("Opinion pipeline complete:", nrow(segments_all), "segments,", nrow(classes_summ), "classes.")
      showNotification(paste0("Opinion report ready: ", nrow(classes_summ),
                              " classes across ", nrow(sentiment_summary),
                              " sentiments. Files saved to ", outdir,
                              ". See the 'Opinion Report' tab."), type = "message", duration = 7)
      status_msg(paste0("Done (opinion). ", nrow(classes_summ), " classes saved to ", outdir, "."))
      return(NULL)
    }

    last_mode_rv(mode_run)
    is_codebook <- identical(mode_run, "codebook")
    
    n <- nrow(prepared)
    showNotification("Processing text (stage 2)...", type = "message", duration = 4)
    status_msg(sprintf("Processing %d rows | mode=%s | model=%s ...", n, mode_run, model_sel))
    
    prog <- shiny::Progress$new(session, min = 0, max = n)
    on.exit(prog$close(), add = TRUE)
    
    resp_rows <- vector("list", n); theme_rows <- vector("list", n)
    
    for (i in seq_len(n)) {
      prog$set(value = i, message = paste0("Processing (", mode_run, ")"), detail = paste(i, "of", n))
      status_msg(paste0("Row ", i, "/", n, " ..."))
      
      txt <- prepared$response_text[i]
      det <- detect_lang(txt)
      translated <- if (do_translate) translate_cached(txt, api_key, do_translate = TRUE, model = model_sel)
      else list(text_en = txt, detected_language = det)
      txt_en <- translated$text_en %||% txt
      lang_guess <- translated$detected_language %||% det
      clean <- normalise_text(txt_en)
      
      if (is_codebook) {
        current_bank <- canonical_theme(cb$name); allow_new_now <- FALSE
      } else {
        current_bank <- themes_bank_rv()
        allow_new_now <- if (length(current_bank) == 0) TRUE else !lock_themes
      }
      
      out <- tryCatch(
        cached_classify_multi(
          text = txt_en, api_key = api_key, theme_bank = current_bank,
          use_api = use_api, allow_new_themes = allow_new_now,
          question_context = question_ctx, model = model_sel,
          mode = mode_run, max_themes = max_themes_sel, codebook = if (is_codebook) cb else NULL
        ),
        error = function(e) fallback_multi_dynamic(txt_en, theme_bank = current_bank, allow_new_themes = allow_new_now,
                                                   question_context = question_ctx, max_themes = max_themes_sel)
      )
      
      if (!is_codebook && length(out$new_themes) > 0 && allow_new_now) {
        themes_bank_rv(unique(c(themes_bank_rv(), canonical_theme(out$new_themes))))
      }
      
      resp_rows[[i]] <- tibble(
        response_id = prepared$response_id[i], respondent_id = prepared$respondent_id[i],
        response_text = prepared$response_text[i], extra_1 = prepared$extra_1[i], extra_2 = prepared$extra_2[i],
        raw_date = prepared$raw_date[i],
        analysis_question = if (nzchar(question_ctx)) question_ctx else NA_character_,
        analysis_mode = mode_run,
        detected_language = lang_guess %||% NA_character_, text_en = txt_en, clean_text = clean,
        flag_useless = as.integer(out$flag_useless %||% 0L), discovery = as.integer(out$discovery %||% 0L)
      )
      
      th <- out$themes
      theme_rows[[i]] <- if (is.null(th) || nrow(th) == 0) tibble() else
        th %>% mutate(response_id = prepared$response_id[i], respondent_id = prepared$respondent_id[i]) %>%
        select(respondent_id, response_id, theme, is_new_theme, sentiment, sentiment_score, evidence, rationale) %>%
        dedupe_theme_rows()
      
      if (i %% 100 == 0) log_msg("Processed", i, "rows")
    }
    
    responses_enriched <- bind_rows(resp_rows)
    themes_enriched_raw <- bind_rows(theme_rows) %>% dedupe_theme_rows()
    
    responses_rv(responses_enriched); themes_raw_rv(themes_enriched_raw); themes_final_rv(themes_enriched_raw)
    tm_rv(theme_metrics_multi(themes_enriched_raw)); tag_matrix_rv(make_tag_matrix(responses_enriched, themes_enriched_raw))
    
    outpath <- save_outputs(mode_run, responses_enriched, themes_enriched_raw, tm_rv(), tag_matrix_rv(), if (is_codebook) cb else NULL)
    log_msg("Saved outputs to", outpath, "with suffix _", mode_run)
    showNotification(paste0("Results saved to ", outpath, " (suffix: _", mode_run, ")"), type = "message", duration = 6)
    
    output$preview <- renderTable({
      thf <- themes_final_rv()
      if (is.null(thf) || !nrow(thf)) return(data.frame(Message = "No theme rows produced."))
      head(thf %>%
             left_join(responses_enriched %>% select(response_id, response_text, analysis_question, discovery), by = "response_id") %>%
             select(respondent_id, response_id, theme, is_new_theme, sentiment, sentiment_score, discovery, rationale) %>%
             arrange(desc(is_new_theme), desc(abs(sentiment_score))), 25)
    })
    
    status_msg(paste0("Done (mode=", mode_run, "). Review Themes tab."))
  })
  
  # ---- Plots ----
  output$bubble <- renderPlotly({
    tm <- tm_rv(); if (is.null(tm) || !nrow(tm)) return(empty_plotly("Click 'Run processing' to generate charts."))
    plot_ly(tm, x = ~avg_sentiment, y = ~priority, type = "scatter", mode = "markers",
            size = ~mentions, sizes = c(10, 60), color = ~avg_sentiment, colors = "RdBu",
            text = ~paste0("<b>", theme, "</b><br>Mentions: ", mentions,
                           "<br>Avg sentiment: ", round(avg_sentiment, 2),
                           "<br>Neg: ", scales::percent(need_to_improve),
                           "<br>Pos: ", scales::percent(satisfied),
                           "<br>New theme share: ", scales::percent(is_new_theme_share),
                           "<br>Priority: ", round(priority, 3)), hoverinfo = "text") %>%
      layout(title = "Themes by Sentiment (post-merge)",
             xaxis = list(title = "Avg sentiment (-1 to +1)"), yaxis = list(title = "Priority (volume x severity)"))
  })
  
  output$bars <- renderPlotly({
    tm <- tm_rv(); if (is.null(tm) || !nrow(tm)) return(empty_plotly("Charts will appear here after the run."))
    top <- tm %>% arrange(desc(mentions)) %>% slice_head(n = 12)
    plot_ly(data = top, x = ~mentions, y = ~reorder(theme, mentions), type = "bar", orientation = "h",
            color = ~avg_sentiment, colors = "RdBu",
            text = ~paste0(theme, "<br>Mentions: ", mentions, "<br>Avg sentiment: ", round(avg_sentiment, 2)), hoverinfo = "text") %>%
      layout(title = "Top themes by mentions", xaxis = list(title = "Count"), yaxis = list(title = NULL))
  })
  
  output$trend <- renderPlotly({
    if (!nzchar(input$date_col)) return(empty_plotly("Select a date column (left) to see trending."))
    tr <- trend_prep(themes_final_rv(), responses_rv())
    if (is.null(tr) || !nrow(tr)) return(empty_plotly("No dated responses to trend (date parsing failed)."))
    top_now <- tr %>% filter(month == max(month)) %>% arrange(desc(share)) %>% slice_head(n = 8) %>% pull(theme)
    tr <- tr %>% filter(theme %in% top_now)
    plot_ly(tr, x = ~month, y = ~share, color = ~theme, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Trending theme share (post-merge)", yaxis = list(tickformat = ".0%"))
  })
  
  output$logodds <- renderPlotly({
    resp <- responses_rv(); th <- themes_final_rv()
    if (is.null(resp) || !nrow(resp) || is.null(th) || !nrow(th)) return(empty_plotly("Advanced views will appear after the run."))
    joined <- th %>% inner_join(resp %>% select(response_id, clean_text), by = "response_id") %>% filter(nzchar(clean_text))
    toks <- joined %>% mutate(theme = if_else(is.na(theme), "Other", theme)) %>%
      unnest_tokens(word, clean_text) %>% anti_join(stop_words, by = "word") %>% count(theme, word, name = "n")
    if (!nrow(toks)) return(empty_plotly("No tokens."))
    totals <- toks %>% group_by(theme) %>% summarise(N = sum(n), .groups = "drop")
    vocab <- n_distinct(toks$word); alpha <- 0.01
    odds <- toks %>% left_join(totals, by = "theme") %>% mutate(lo = log((n + alpha) / (N - n + alpha * (vocab - 1))))
    ref <- odds %>% group_by(word) %>% summarise(ref = mean(lo), .groups = "drop")
    comp <- odds %>% left_join(ref, by = "word") %>% mutate(delta = lo - ref) %>%
      group_by(theme) %>% slice_max(abs(delta), n = 10) %>% ungroup()
    ggplotly(ggplot(comp, aes(x = reorder(word, delta), y = delta, fill = theme)) +
               geom_col(show.legend = FALSE) + coord_flip() +
               labs(title = "Discriminative words by theme (log-odds proxy)", x = NULL, y = "Delta log-odds"))
  })
  
  output$ngrams <- renderPlotly({
    resp <- responses_rv(); th <- themes_final_rv()
    if (is.null(resp) || !nrow(resp) || is.null(th) || !nrow(th)) return(empty_plotly("Advanced views will appear after the run."))
    joined <- th %>% inner_join(resp %>% select(response_id, clean_text), by = "response_id") %>% filter(nzchar(clean_text))
    bi <- joined %>% unnest_tokens(bigram, clean_text, token = "ngrams", n = 2) %>%
      separate(bigram, c("w1","w2"), sep = " ", remove = TRUE) %>%
      filter(!w1 %in% stop_words$word, !w2 %in% stop_words$word) %>%
      unite(bigram, w1, w2, sep = " ") %>% count(theme, bigram, sort = TRUE) %>%
      group_by(theme) %>% slice_head(n = 10) %>% ungroup()
    if (!nrow(bi)) return(empty_plotly("No bigrams."))
    ggplotly(ggplot(bi, aes(x = n, y = tidytext::reorder_within(bigram, n, theme), fill = theme)) +
               geom_col(show.legend = FALSE) + tidytext::scale_y_reordered() +
               facet_wrap(~theme, scales = "free_y") +
               labs(title = "Top bigrams by theme", x = "Count", y = NULL))
  })
  
  # ---- Downloads ----
  output$dl_responses <- downloadHandler(
    filename = function() sprintf("responses_enriched_%s.csv", last_mode_rv()),
    content = function(file) { d <- responses_rv(); if (is.null(d)) d <- tibble(Message = "Run processing first."); readr::write_csv(d, file) })
  output$dl_themes <- downloadHandler(
    filename = function() sprintf("themes_enriched_%s.csv", last_mode_rv()),
    content = function(file) { d <- themes_final_rv(); if (is.null(d)) d <- tibble(Message = "Run processing first."); readr::write_csv(d, file) })
  output$dl_metrics <- downloadHandler(
    filename = function() sprintf("theme_metrics_%s.csv", last_mode_rv()),
    content = function(file) { d <- tm_rv(); if (is.null(d)) d <- tibble(Message = "Run processing first."); readr::write_csv(d, file) })
  output$dl_tag_matrix <- downloadHandler(
    filename = function() sprintf("tag_matrix_%s.csv", last_mode_rv()),
    content = function(file) { d <- tag_matrix_rv(); if (is.null(d)) d <- tibble(Message = "Run processing first."); readr::write_csv(d, file) })
  output$dl_codebook <- downloadHandler(
    filename = function() sprintf("codebook_%s.csv", last_mode_rv()),
    content = function(file) { d <- codebook_rv(); if (is.null(d) || !nrow(d)) d <- tibble(Message = "No codebook (run codebook mode)."); readr::write_csv(d, file) })
  
  # ---- Exploratory (narrative) outputs ----
  output$expl_report <- renderUI({
    h <- report_html_rv()
    if (is.null(h) || !nzchar(h)) return(helpText("Run exploratory (narrative) mode to generate the report."))
    HTML(h)
  })
  output$framework_tbl <- renderTable({
    fw <- framework_rv()
    if (is.null(fw) || !nrow(fw)) return(data.frame(Message = "Run exploratory mode to induce a framework."))
    as.data.frame(fw)
  })
  output$dl_expl_report <- downloadHandler(
    filename = function() "exploratory_report.html",
    content = function(file) {
      h <- report_html_rv(); if (is.null(h)) h <- "<p>Run exploratory mode first.</p>"; writeLines(h, file)
    })
  output$dl_expl_themes <- downloadHandler(
    filename = function() "exploratory_themes.csv",
    content = function(file) { d <- expl_themes_rv(); if (is.null(d)) d <- tibble(Message = "Run exploratory mode first."); readr::write_csv(d, file) })
  output$dl_expl_subthemes <- downloadHandler(
    filename = function() "exploratory_subthemes.csv",
    content = function(file) { d <- expl_subthemes_rv(); if (is.null(d)) d <- tibble(Message = "Run exploratory mode first."); readr::write_csv(d, file) })
  output$dl_expl_tags <- downloadHandler(
    filename = function() "exploratory_response_tags.csv",
    content = function(file) { d <- expl_tags_rv(); if (is.null(d)) d <- tibble(Message = "Run exploratory mode first."); readr::write_csv(d, file) })
  output$dl_framework <- downloadHandler(
    filename = function() "exploratory_framework.csv",
    content = function(file) { d <- framework_rv(); if (is.null(d)) d <- tibble(Message = "Run exploratory mode first."); readr::write_csv(d, file) })

  # ---- Opinion (sentiment-first) outputs ----
  output$opinion_report <- renderUI({
    h <- op_report_html_rv()
    if (is.null(h) || !nzchar(h)) return(helpText("Run opinion mode to generate the report."))
    HTML(h)
  })
  output$opinion_framework_tbl <- renderTable({
    fw <- op_framework_rv()
    if (is.null(fw) || !nrow(fw)) return(data.frame(Message = "Run opinion mode to build class frameworks."))
    as.data.frame(fw)
  })
  output$dl_op_report <- downloadHandler(
    filename = function() "opinion_report.html",
    content = function(file) { h <- op_report_html_rv(); if (is.null(h)) h <- "<p>Run opinion mode first.</p>"; writeLines(h, file) })
  output$dl_op_segments <- downloadHandler(
    filename = function() "opinion_segments.csv",
    content = function(file) { d <- op_segments_rv(); if (is.null(d)) d <- tibble(Message = "Run opinion mode first."); readr::write_csv(d, file) })
  output$dl_op_classes <- downloadHandler(
    filename = function() "opinion_classes.csv",
    content = function(file) { d <- op_classes_rv(); if (is.null(d)) d <- tibble(Message = "Run opinion mode first."); readr::write_csv(d, file) })
  output$dl_op_subclasses <- downloadHandler(
    filename = function() "opinion_subclasses.csv",
    content = function(file) { d <- op_subclasses_rv(); if (is.null(d)) d <- tibble(Message = "Run opinion mode first."); readr::write_csv(d, file) })
  output$dl_op_framework <- downloadHandler(
    filename = function() "opinion_frameworks.csv",
    content = function(file) { d <- op_framework_rv(); if (is.null(d)) d <- tibble(Message = "Run opinion mode first."); readr::write_csv(d, file) })

  # ---- PAD 3D density ----
  output$pad_plot <- renderPlotly({
    ps <- pad_scores_rv()
    if (is.null(ps) || !nrow(ps)) return(empty_plotly("Run opinion mode to compute PAD scores."))
    pts <- ps %>% filter(!is.na(pleasure), !is.na(arousal), !is.na(dominance))
    if (!nrow(pts)) return(empty_plotly("No usable PAD scores (all responses were flagged uninformative)."))

    sent_col <- c(positive = "#5CCB09", negative = "#BA38B1", neutral = "#3A3E96")
    bw   <- input$pad_bw   %||% PAD_BANDWIDTH_DEFAULT
    gn   <- input$pad_grid %||% PAD_GRID_N_DEFAULT

    p <- plot_ly()

    if (isTRUE(input$pad_show_iso) && nrow(pts) >= 5) {
      g <- tryCatch(pad_density_grid(pts %>% select(pleasure, arousal, dominance), grid_n = gn, h = bw),
                    error = function(e) NULL)
      if (!is.null(g) && any(g$dens > 0)) {
        mx <- max(g$dens)
        p <- p %>% add_trace(
          type = "isosurface",
          x = g$px, y = g$ay, z = g$dz, value = g$dens,
          isomin = mx * 0.15, isomax = mx,
          surface = list(count = 3, fill = 0.9),
          caps = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE)),
          opacity = 0.30, colorscale = "YlGnBu", showscale = TRUE,
          colorbar = list(title = "Density")
        )
      }
    }

    if (isTRUE(input$pad_show_pts)) {
      htxt <- paste0("P: ", round(pts$pleasure, 2),
                     " | A: ", round(pts$arousal, 2),
                     " | D: ", round(pts$dominance, 2),
                     "<br>", substr(pts$response_text %||% "", 1, 120))
      p <- p %>% add_trace(
        type = "scatter3d", mode = "markers",
        x = pts$pleasure, y = pts$arousal, z = pts$dominance,
        marker = list(size = 3, opacity = 0.6,
                      color = unname(sent_col[pts$sentiment_lean]) ),
        text = htxt, hoverinfo = "text", name = "responses"
      )
    }

    p %>% layout(
      title = "PAD affect space - response-level density",
      scene = list(
        xaxis = list(title = "Pleasure", range = c(-1, 1)),
        yaxis = list(title = "Arousal",  range = c(-1, 1)),
        zaxis = list(title = "Dominance",range = c(-1, 1)),
        aspectmode = "cube"
      )
    )
  })

  output$pad_summary_tbl <- renderTable({
    ps <- pad_scores_rv()
    if (is.null(ps) || !nrow(ps)) return(data.frame(Message = "Run opinion mode to compute PAD scores."))
    ps %>% filter(!is.na(pleasure)) %>%
      group_by(sentiment_lean) %>%
      summarise(responses = n(),
                mean_pleasure = round(mean(pleasure, na.rm = TRUE), 3),
                mean_arousal = round(mean(arousal, na.rm = TRUE), 3),
                mean_dominance = round(mean(dominance, na.rm = TRUE), 3),
                .groups = "drop") %>%
      bind_rows(ps %>% filter(!is.na(pleasure)) %>%
                  summarise(sentiment_lean = "ALL", responses = n(),
                            mean_pleasure = round(mean(pleasure, na.rm = TRUE), 3),
                            mean_arousal = round(mean(arousal, na.rm = TRUE), 3),
                            mean_dominance = round(mean(dominance, na.rm = TRUE), 3))) %>%
      as.data.frame()
  })

  output$dl_pad_scores <- downloadHandler(
    filename = function() "opinion_pad_scores.csv",
    content = function(file) { d <- pad_scores_rv(); if (is.null(d)) d <- tibble(Message = "Run opinion mode first."); readr::write_csv(d, file) })
}

shinyApp(ui, server)
