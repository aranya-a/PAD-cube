# ============================================================================
#  Sentiment -> Theme hierarchy  (sunburst + treemap)
#  Inner ring = sentiment, outer ring = class/theme.
#  Slice size = mentions. Label shows % of all mentions.
#  Hover shows the full theme summary.
#  Data file: sentiment_themes.csv (sentiment, class, mentions, summary)
# ============================================================================
library(readxl)
## ---- packages --------------------------------------------------------------
if (!"plotly" %in% rownames(installed.packages())) install.packages("plotly")
library(plotly)

font_family <- "Roboto Light, Roboto, Helvetica Neue, sans-serif"

## ---- 1. load data ----------------------------------------------------------
d <- read_xlsx('/Users/Aranya/Work/Work --surface/Raiz /Open Text/Q3/New with responses/Clasees with raw responses.xlsx')
d$mentions <- as.numeric(d$mentions)


total <- sum(d$mentions)

## ---- 2. helper: wrap long summary text for readable hover ------------------
wrap_html <- function(x, width = 72) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "<br>"),
         character(1), USE.NAMES = FALSE)
}

## ---- 3. build the node table (roots + leaves) ------------------------------
# capitalise sentiment for display
cap <- function(s) paste0(toupper(substr(s,1,1)), substr(s,2,nchar(s)))

# ---- leaves (one per theme). ids must be UNIQUE because "Other /
#      Unclassified" repeats across sentiments -> id = sentiment + class ------
sent_tot <- tapply(d$mentions, d$sentiment, sum)

leaf <- data.frame(
  id      = paste(d$sentiment, d$class, sep = " | "),
  label   = d$class,
  parent  = cap(d$sentiment),
  value   = d$mentions,
  sentiment = d$sentiment,
  pct_all = 100 * d$mentions / total,                       # % of everything
  pct_sen = 100 * d$mentions / sent_tot[d$sentiment],       # % within sentiment
  summary = d$summary,
  stringsAsFactors = FALSE
)

# ---- roots (one per sentiment) --------------------------------------------
root <- data.frame(
  id      = cap(names(sent_tot)),
  label   = cap(names(sent_tot)),
  parent  = "",
  value   = as.numeric(sent_tot),
  sentiment = names(sent_tot),
  pct_all = 100 * as.numeric(sent_tot) / total,
  pct_sen = 100,
  summary = paste0(as.numeric(sent_tot), " mentions across ",
                   tapply(d$class, d$sentiment, length)[names(sent_tot)],
                   " themes"),
  stringsAsFactors = FALSE
)

nodes <- rbind(root, leaf)

## ---- 4. colours by sentiment ----------------------------------------------
pal <- c(negative = "#3A3E96", neutral = "#18B8ED", positive = "#5CCB09")
node_col <- pal[nodes$sentiment]
# make the root slices a touch darker so the two rings read as a hierarchy
node_col[nodes$parent == ""] <- c(negative="#3A3E96", neutral="#18B8ED",
                                   positive="#5CCB09")[nodes$sentiment[nodes$parent==""]]

## ---- 5. custom hover: mentions, %s, then wrapped summary -------------------
hover <- paste0(
  "<b>", nodes$label, "</b><br>",
  nodes$value, " mentions &#183; ", sprintf("%.1f%%", nodes$pct_all), " of all<br>",
  ifelse(nodes$parent == "", "",
         paste0(sprintf("%.1f%%", nodes$pct_sen), " of ", nodes$parent, "<br>")),
  "<br>", wrap_html(nodes$summary)
)

## ============================================================================
## SUNBURST
## ============================================================================
sb <- plot_ly(
  type = "sunburst",
  ids = nodes$id, labels = nodes$label, parents = nodes$parent,
  values = nodes$value, branchvalues = "total",
  marker = list(colors = node_col, line = list(color = "#ffffff", width = 1)),
  text = hover, hoverinfo = "text",
  texttemplate = "%{label}<br>%{percentRoot:.1%}",
  insidetextorientation = "radial",
  insidetextfont = list(family = font_family, size = 12, color = "#ffffff"),
  sort = TRUE
) %>%
  layout(title = list(text = NULL,
                      font = list(family = font_family, size = 18)),
         font = list(family = font_family),
         margin = list(t = 60, l = 0, r = 0, b = 0))
sb   # htmlwidgets::saveWidget(sb, "sentiment_sunburst.html", selfcontained = TRUE)

## ============================================================================
## TREEMAP  (same data, rectangular — often easier to read the small themes)
## ============================================================================
tm <- plot_ly(
  type = "treemap",
  ids = nodes$id, labels = nodes$label, parents = nodes$parent,
  values = nodes$value, branchvalues = "total",
  marker = list(colors = node_col, line = list(color = "#ffffff", width = 1)),
  text = hover, hoverinfo = "text",
  texttemplate = "%{label}. %{value}  (%{percentRoot:.1%})",
  insidetextfont = list(family = font_family, size = 14, color = "#ffffff"),
  tiling = list(packing = "squarify"),
  sort = TRUE
) %>%
  layout(title = list(text = "Themes by sentiment (size = mentions)",
                      font = list(family = font_family, size = 18)),
         font = list(family = font_family),
         margin = list(t = 60, l = 0, r = 0, b = 0))
tm   # htmlwidgets::saveWidget(tm, "sentiment_treemap.html", selfcontained = TRUE)

# NOTE: %{percentRoot} = share of ALL mentions; %{percentParent} = share within
# the sentiment. Swap it into texttemplate if you prefer the within-sentiment %.
htmlwidgets::saveWidget(sb, "sb_sentiment.html", selfcontained = TRUE)
htmlwidgets::saveWidget(tm, "tm_sentiment.html", selfcontained = TRUE)
getwd()
setwd('/Users/Aranya/Work/Work --surface/Raiz /Open Text/Q3/Opinion Based/Opus/')





# ============================================================================
#  Sentiment -> Theme hierarchy  (sunburst + treemap)
#  Inner ring = sentiment, outer ring = class/theme.
#  Slice size = mentions. Label shows % of all mentions.
#  Hover shows the full theme summary.
#  Data file: sentiment_themes.csv (sentiment, class, mentions, summary)
# ============================================================================

## ---- packages --------------------------------------------------------------
if (!"plotly" %in% rownames(installed.packages())) install.packages("plotly")
library(plotly)

font_family <- "Roboto Light, Roboto, Helvetica Neue, sans-serif"

## ---- 1. load data ----------------------------------------------------------
d <- read.csv('/Users/Aranya/Work/Work --surface/Raiz /Open Text/Q80/Repeat/opinion_classes.csv', stringsAsFactors = FALSE,
              encoding = "UTF-8", check.names = FALSE)
d$mentions <- as.numeric(d$mentions)


d <- d %>% 
  filter(mentions>4)
total <- sum(d$mentions)
## ---- 2. helper: wrap long summary text for readable hover ------------------
wrap_html <- function(x, width = 72) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "<br>"),
         character(1), USE.NAMES = FALSE)
}

## ---- 3. build the node table (roots + leaves) ------------------------------
cap <- function(s) paste0(toupper(substr(s,1,1)), substr(s,2,nchar(s)))
sent_tot <- tapply(d$mentions, d$sentiment, sum)

# leaves: ids must be UNIQUE because "Other / Unclassified" repeats
leaf <- data.frame(
  id        = paste(d$sentiment, d$class, sep = " | "),
  label     = d$class,
  parent    = cap(d$sentiment),
  value     = d$mentions,
  sentiment = d$sentiment,
  pct_all   = 100 * d$mentions / total,
  pct_sen   = 100 * d$mentions / sent_tot[d$sentiment],
  summary   = d$summary,
  raw_examples = d$raw_examples,
  stringsAsFactors = FALSE
)

root <- data.frame(
  id        = cap(names(sent_tot)),
  label     = cap(names(sent_tot)),
  parent    = "",
  value     = as.numeric(sent_tot),
  sentiment = names(sent_tot),
  pct_all   = 100 * as.numeric(sent_tot) / total,
  pct_sen   = 100,
  summary   = paste0(as.numeric(sent_tot), " mentions across ",
                     tapply(d$class, d$sentiment, length)[names(sent_tot)], " themes"),
  raw_examples = "",
  stringsAsFactors = FALSE
)

nodes <- rbind(root, leaf)

## ---- 4. colours by sentiment ----------------------------------------------
pal <- c(negative = "#3A3E96", neutral = "#18B8ED", positive = "#5CCB09")
node_col <- pal[nodes$sentiment]
node_col[nodes$parent == ""] <- c(negative="#3A3E96", neutral="#18B8ED",
                                  positive="#5CCB09")[nodes$sentiment[nodes$parent==""]]

## ---- 5. custom hover -------------------------------------------------------
examples_html <- function(x, width = 72) {
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return("")
    lines <- strsplit(s, "\n", fixed = TRUE)[[1]]
    lines <- lines[nzchar(trimws(lines))]
    paste(vapply(lines, function(ln)
      paste(strwrap(ln, width = width), collapse = "<br>"), character(1)),
      collapse = "<br>")
  }, character(1), USE.NAMES = FALSE)
}

hover <- paste0(
  "<b>", nodes$label, "</b><br>",
  nodes$value, " mentions &#183; ", sprintf("%.1f%%", nodes$pct_all), " of all<br>",
  ifelse(nodes$parent == "", "",
         paste0(sprintf("%.1f%%", nodes$pct_sen), " of ", nodes$parent, "<br>")),
  "<br>",
  ifelse(nzchar(nodes$raw_examples),
         paste0("<br><br><b>Example responses:</b><br>", examples_html(nodes$raw_examples)),
         "")
)
## ---- layout tuned to KILL the surrounding white space ----------------------
# 1) no plotly title  -> the slide headline already labels the chart
# 2) all margins = 0   -> wheel expands to the edges
# 3) square width=height -> a circular sunburst can only fill a square, so a
#    square canvas removes the left/right white bands. When embedding, size the
#    PowerPoint / Web Viewer box square too (equal width & height).
tight <- function(p) {
  layout(p,
         showlegend    = FALSE,
         font          = list(family = font_family),
         margin        = list(t = 0, l = 0, r = 0, b = 0),
         width         = 900, height = 900,
         paper_bgcolor = "rgba(0,0,0,0)",
         plot_bgcolor  = "rgba(0,0,0,0)")
}

## ============================================================================
## SUNBURST
## ============================================================================
sb <- plot_ly(
  type = "sunburst",
  ids = nodes$id, labels = nodes$label, parents = nodes$parent,
  values = nodes$value, branchvalues = "total",
  marker = list(colors = node_col, line = list(color = "#ffffff", width = 1)),
  text = hover, hoverinfo = "text",
  texttemplate = "%{label}<br>%{percentRoot:.1%}",
  insidetextorientation = "radial",
  insidetextfont = list(family = font_family, size = 12, color = "#ffffff"),
  sort = TRUE
) %>% tight()
sb   # htmlwidgets::saveWidget(sb, "sentiment_sunburst.html", selfcontained = TRUE)

## ============================================================================
## TREEMAP  (rectangular — fills a wide box edge-to-edge, no circle gaps)
##  -> if your slide box stays wide, the treemap wastes NO space at all.
## ============================================================================
tm <- plot_ly(
  type = "treemap",domain = list(x=c(0,0.8),y=c(0,1)),
  ids = nodes$id, labels = nodes$label, parents = nodes$parent,
  values = nodes$value, branchvalues = "total",
  marker = list(colors = node_col, line = list(color = "#ffffff", width = 1)),
  text = paste0("<b>", nodes$label, "</b>"), hoverinfo = "text", #was text for hoverinfo
  texttemplate = "%{label}. %{value}  (%{percentRoot:.1%})",
  insidetextfont = list(family = font_family, size = 14, color = "#ffffff"),
  tiling = list(packing = "squarify"),
  sort = TRUE
) %>% layout(          # treemap is rectangular: let it fill the wide box
  showlegend = FALSE, font = list(family = font_family),
  margin = list(t = 0, l = 0, r = 0, b = 0),autosize=T,
  paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>% 
  config(responsive=T)


tm <- htmlwidgets::onRender(tm, "
function(el, x, data){
  el.style.position = 'relative';
  var panel = document.createElement('div');
  panel.style.cssText =
    'position:absolute; top:0; right:0; width:20%; height:100%; overflow:auto;'
    + 'padding:12px 14px; box-sizing:border-box; background:#fff;'
    + 'border-left:1px solid #ddd; font-family:Roboto,Helvetica,Arial,sans-serif;'
    + 'font-size:13px; line-height:1.45; color:#222;';
  panel.innerHTML = '<em>Hover a tile to see the full responses.</em>';
  el.appendChild(panel);

  var map = {};
  for (var i = 0; i < data.ids.length; i++) map[data.ids[i]] = data.full[i];

  el.on('plotly_hover', function(d){
    var txt = map[d.points[0].id];
    if (txt != null) panel.innerHTML = txt;
  });
}
", data = list(ids = nodes$id, full = hover))
tm
# TIP: %{percentRoot}=share of ALL mentions; %{percentParent}=share within
# the sentiment. Swap in texttemplate if you prefer within-sentiment %.
htmlwidgets::saveWidget(sb, "/Users/Aranya/Work/Work --surface/Raiz /Open Text/Q40/Q40_fitted_sb_sentiment.html", selfcontained = TRUE)
htmlwidgets::saveWidget(tm, "/Users/Aranya/Work/Work --surface/Raiz /Open Text/Q40/Q40_raw_response_fitted_tm_sentiment.html", selfcontained = TRUE)






hover_nm <- paste0(
  "<b>", nodes$label, "</b><br>",
   sprintf("%.1f%%", nodes$pct_all), " of all<br>",
  ifelse(nodes$parent == "", "",
         paste0(sprintf("%.1f%%", nodes$pct_sen), " of ", nodes$parent, "<br>")),
  "<br>", 
  ifelse(nzchar(nodes$raw_examples),
         paste0("<br><br><b>Example responses:</b><br>", examples_html(nodes$raw_examples)),
         "")
)
tm_no_mention <- plot_ly(
  type = "treemap",domain = list(x=c(0,0.8),y=c(0,1)),
  ids = nodes$id, labels = nodes$label, parents = nodes$parent,
  values = nodes$value, branchvalues = "total",
  marker = list(colors = node_col, line = list(color = "#ffffff", width = 1)),
  text = paste0("<b>", nodes$label, "</b>"), hoverinfo = "text",
  texttemplate = "%{label}<br>  (%{percentRoot:.1%})",
  insidetextfont = list(family = font_family, size = 13, color = "#ffffff"),
  tiling = list(packing = "squarify"),
  sort = TRUE
) %>% layout(          # treemap is rectangular: let it fill the wide box
  showlegend = FALSE, font = list(family = font_family),
  margin = list(t = 0, l = 0, r = 0, b = 0),autosize = T,
  paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>% 
  config(responsive = T)
tm_no_mention   # htmlwidgets::saveWidget(tm, "sentiment_treemap.html", selfcontained = TRUE)

tm_no_mention <- htmlwidgets::onRender(tm_no_mention, "
function(el, x, data){
  el.style.position = 'relative';
  var panel = document.createElement('div');
  panel.style.cssText =
    'position:absolute; top:0; right:0; width:20%; height:100%; overflow:auto;'
    + 'padding:12px 14px; box-sizing:border-box; background:#fff;'
    + 'border-left:1px solid #ddd; font-family:Roboto,Helvetica,Arial,sans-serif;'
    + 'font-size:13px; line-height:1.45; color:#222;';
  panel.innerHTML = '<em>Hover a tile to see the full responses.</em>';
  el.appendChild(panel);

  var map = {};
  for (var i = 0; i < data.ids.length; i++) map[data.ids[i]] = data.full[i];

  el.on('plotly_hover', function(d){
    var txt = map[d.points[0].id];
    if (txt != null) panel.innerHTML = txt;
  });
}
", data = list(ids = nodes$id, full = hover_nm))
tm_no_mention

htmlwidgets::saveWidget(tm_no_mention, "/Users/Aranya/Work/Work --surface/Raiz /Open Text/Q80/Q80_sidepanel_nm_2.html", selfcontained = TRUE)
