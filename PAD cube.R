# ============================================================================
#  PAD Emotion Cube  —  interpretable 3D visualisation of Pleasure/Arousal/
#  Dominance scores (opinion_pad_scores.csv)
#
#  Produces THREE views, all interactive (open in browser / RStudio Viewer):
#    (1) Labelled 3D scatter, coloured by nearest "emotion prototype"
#    (2) 3D kernel-density isosurface  (where respondents cluster)
#    (3) 3x3x3 "Rubik's cube" of voxels, each labelled with its emotion + count
#
#  The trick that makes it INTERPRETABLE (not just a point cloud) is the
#  EMOTION PROTOTYPE table: named emotions placed at canonical PAD coordinates
#  (Mehrabian's PAD octant model). Every respondent is assigned to the nearest
#  prototype, so a region can be read as "joy", "skeptical", "confused", etc.
#  Edit that table to match your own emotion vocabulary.
# ============================================================================

## ---- 0. Packages -----------------------------------------------------------
pkgs <- c("plotly", "dplyr", "readr", "ks", "misc3d")   # misc3d only for view 2
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
library(plotly); library(dplyr); library(readr)

## ---- 1. Load & clean -------------------------------------------------------
# adjust the path to wherever the file lives
df <- read_csv('/Users/Aranya/Work/Work --surface/Raiz /Open Text/Q3/Opinion Based/Opus/opinion_pad_scores.csv', show_col_types = FALSE) %>%
  rename(P = pleasure, A = arousal, D = dominance) %>%
  filter(!is.na(P), !is.na(A), !is.na(D)) %>%
  mutate(across(c(P, A, D), as.numeric))

cat(sprintf("Loaded %d respondents with complete PAD scores\n", nrow(df)))

## ---- 2. Emotion prototypes (EDIT THIS TABLE) -------------------------------
# Coordinates on a -1..1 scale for Pleasure, Arousal, Dominance.
# These are the "anchors": whichever anchor a respondent is closest to (in 3D
# Euclidean distance) becomes their emotion label.
prototypes <- tribble(
  ~emotion,       ~P,    ~A,    ~D,     ~color,
  "Joy / Elated",  0.8,   0.6,   0.5,   "#F4B400",  # happy, energised, in control
  "Excited",       0.6,   0.8,   0.4,   "#BA38B1",
  "Content",       0.6,  -0.3,   0.4,   "#5CCB09",  # pleasant, calm, secure
  "Hopeful",       0.5,   0.2,   0.3,   "#18B8ED",
  "Neutral",       0.0,   0.0,   0.0,   "#9E9E9E",
  "Skeptical",    -0.2,   0.1,   0.3,   "#3A3E96",  # mild displeasure, guarded, in control
  "Confused",     -0.2,   0.3,  -0.4,   "#0066FF",  # unpleasant, aroused, low control
  "Anxious",      -0.4,   0.6,  -0.5,   "#AB47BC",
  "Frustrated",   -0.5,   0.5,   0.4,   "#7030A0",  # displeased, aroused, wants control
  "Bored",        -0.3,  -0.5,  -0.3,   "#78909C",
  "Disappointed", -0.6,  -0.2,  -0.3,   "#455A64"
)

## ---- 3. Assign each respondent to nearest prototype ------------------------
assign_emotion <- function(P, A, D, proto) {
  d <- sqrt((proto$P - P)^2 + (proto$A - A)^2 + (proto$D - D)^2)
  proto$emotion[which.min(d)]
}
df$emotion <- mapply(assign_emotion, df$P, df$A, df$D,
                     MoreArgs = list(proto = prototypes))
df$emotion <- factor(df$emotion, levels = prototypes$emotion)
pal <- setNames(prototypes$color, prototypes$emotion)

print(sort(table(df$emotion), decreasing = TRUE))

## ============================================================================
## VIEW 1 — Labelled 3D scatter coloured by emotion
## Jitter identical points slightly so overlapping scores don't hide density.
## ============================================================================
set.seed(1)
plot_df <- df %>%
  mutate(Pj = P + rnorm(n(), 0, .015),
         Aj = A + rnorm(n(), 0, .015),
         Dj = D + rnorm(n(), 0, .015))

v1 <- plot_ly(plot_df,
              x = ~Pj, y = ~Aj, z = ~Dj,
              color = ~emotion, colors = pal,
              type = "scatter3d", mode = "markers",
              marker = list(size = 3, opacity = 0.55),
              text = ~paste0("<b>", emotion, "</b><br>P=", P, " A=", A, " D=", D),
              hoverinfo = "text") %>%
  # drop the emotion prototypes in as big diamonds so labels are visible
  add_trace(data = prototypes, x = ~P, y = ~A, z = ~D,
            type = "scatter3d", mode = "markers+text",
            marker = list(size = 8, symbol = "diamond",
                          color = ~color, line = list(color = "black", width = 1)),
            text = ~emotion, textposition = "top center",
            textfont = list(size = 14), inherit = FALSE,
            showlegend = FALSE, hoverinfo = "skip") %>%
  layout(title = NULL,
         scene = list(
           xaxis = list(title = "Pleasure",  range = c(-0.75, 0.96)),
           yaxis = list(title = "Arousal",   range = c(-0.4, 0.75)),
           zaxis = list(title = "Dominance", range = c(-0.75, 0.8))))
v1   # print / save with: htmlwidgets::saveWidget(v1, "view1_scatter.html")


## ============================================================================
## VIEW 2 — 3D kernel-density isosurface (the actual "3D density plot")
## Shows nested shells enclosing the densest 25% / 50% / 75% of respondents.
## ============================================================================
library(ks)
H   <- Hpi(df[, c("P","A","D")])                 # bandwidth
fit <- kde(df[, c("P","A","D")], H = H,
           gridsize = 40, xmin = c(-1,-1,-1), xmax = c(1,1,1))

g  <- fit$eval.points
vol <- fit$estimate
# density thresholds enclosing 75/50/25% of the probability mass
lv <- contourLevels(fit, cont = c(75, 50, 25))

v2 <- plot_ly() %>%
  add_trace(type = "isosurface",
            x = rep(g[[1]], times = 40*40),
            y = rep(rep(g[[2]], each = 40), times = 40),
            z = rep(g[[3]], each = 40*40),
            value = as.vector(vol),
            isomin = min(lv), isomax = max(vol),
            surface = list(count = 3),
            opacity = 0.35, colorscale = "Viridis",
            caps = list(x = list(show = FALSE),
                        y = list(show = FALSE),
                        z = list(show = FALSE))) %>%
  layout(title = "PAD space — kernel-density shells (where respondents cluster)",
         scene = list(xaxis = list(title = "Pleasure"),
                      yaxis = list(title = "Arousal"),
                      zaxis = list(title = "Dominance")))
v2   # htmlwidgets::saveWidget(v2, "view2_density.html")

## ============================================================================
## VIEW 3 — 3x3x3 "Rubik's cube"
## Each axis split into Low / Mid / High -> 27 cells. Every cell is drawn as a
## translucent cube whose OPACITY = how many respondents fall in it, and is
## LABELLED with (a) the emotion of the prototype nearest the cell centre and
## (b) the respondent count. This is the most "at-a-glance readable" view.
## ============================================================================
brks <- c(-1, -1/3, 1/3, 1)
lab  <- c("Low", "Mid", "High")
cut3 <- function(x) cut(x, brks, labels = lab, include.lowest = TRUE)


showtext::showtext_auto()
font_family <- "Roboto Light"

# label each occupied cell with nearest emotion prototype
cell_emotion <- function(cx, cy, cz) {
  d <- sqrt((prototypes$P-cx)^2 + (prototypes$A-cy)^2 + (prototypes$D-cz)^2)
  prototypes$emotion[which.min(d)]
}

centers <- c(Low = -2/3, Mid = 0, High = 2/3)
half    <- 1/3                       # half-width of each cube

# build the cell table: counts -> PERCENTAGE, cube centre, emotion, label
cells <- df %>%
  mutate(bx = cut3(P), by = cut3(A), bz = cut3(D)) %>%
  count(bx, by, bz, name = "n") %>%
  mutate(
    pct = 100 * n / sum(n),
    cx  = centers[as.character(bx)],
    cy  = centers[as.character(by)],
    cz  = centers[as.character(bz)],
    emo = mapply(cell_emotion, cx, cy, cz),
    # single-line label -> no <br>/newline artefact, no overlap
    label = sprintf("%s  %.1f%%", emo, pct)
  ) %>% 
  filter(pct>=0.5)

# helper: 8 corners + 12 triangular faces of one cube for mesh3d
cube_mesh <- function(cx, cy, cz, h) {
  x <- cx + h*c(-1,-1, 1, 1,-1,-1, 1, 1)
  y <- cy + h*c(-1, 1, 1,-1,-1, 1, 1,-1)
  z <- cz + h*c(-1,-1,-1,-1, 1, 1, 1, 1)
  i <- c(0,0,0,0,4,4,0,0,1,1,2,3)
  j <- c(1,2,3,4,5,7,1,4,5,6,6,7)
  k <- c(2,3,4,1,6,6,5,5,6,7,7,4)
  list(x=x,y=y,z=z,i=i,j=j,k=k)
}

# --- draw the translucent cubes (hover uses <br>, which IS supported there) --
v3 <- plot_ly()
maxn <- max(cells$n)
for (r in seq_len(nrow(cells))) {
  m <- cube_mesh(cells$cx[r], cells$cy[r], cells$cz[r], half*0.92)
  v3 <- add_trace(v3, type = "mesh3d",
                  x=m$x, y=m$y, z=m$z, i=m$i, j=m$j, k=m$k,
                  facecolor = rep(pal[[cells$emo[r]]], 12),
                  opacity = 0.15 + 0.6*(cells$n[r]/maxn),   # denser cell = more solid
                  flatshading = TRUE, showscale = FALSE,
                  hoverinfo = "text",
                  text = sprintf("%s | %s-P %s-A %s-D<br>%.1f%% of respondents (n=%d)",
                                 cells$emo[r], cells$bx[r], cells$by[r], cells$bz[r],
                                 cells$pct[r], cells$n[r]))
}

# --- ONE combined text trace: single-line labels, Roboto Light -------------
v3 <- add_trace(v3, type = "scatter3d", mode = "text",
                x = cells$cx, y = cells$cy, z = cells$cz,
                text = cells$label,
                textposition = "middle center",
                textfont = list(size = 16, color = "#111111", family = "roboto light"),
                showlegend = FALSE, hoverinfo = "skip")

v3 <- layout(v3,
             title = list(text = NULL,
                          font = list(family = "Roboto light")),
             font  = list(family = font_family),
             scene = list(
               xaxis = list(title="Pleasure",  tickvals=centers, ticktext=names(centers)),
               yaxis = list(title="Arousal",   tickvals=centers, ticktext=names(centers)),
               zaxis = list(title="Dominance", tickvals=centers, ticktext=names(centers))))
v3   # htmlwidgets::saveWidget(v3, "view3_rubik.html")

## ---- 4. (optional) export a table of who's in each emotion ------------------
# write_csv(df %>% count(emotion, sort = TRUE), "emotion_counts.csv")
# write_csv(df, "respondents_with_emotion.csv")



htmlwidgets::saveWidget(v3, "Q3_pad_cube.html", selfcontained = TRUE)
getwd()
setwd('/Users/Aranya/Work/Work --surface/Raiz /Open Text/Q40/')



















#############################################################################
########################## PES instead of PAD ###############################
#############################################################################


# ============================================================================
#  PES Emotion Cube  —  interpretable 3D visualisation of
#  Positivity / Emotional-intensity / Sense-of-control scores
#  (opinion_pad_scores.csv)
#
#  NOTE ON NAMING:
#    The source CSV/Excel still stores the raw columns as
#      pleasure / arousal / dominance   (leave the data file untouched).
#    Everything this script PRODUCES uses the professional PES vocabulary:
#      Pleasure   -> Positivity          (P)
#      Arousal    -> Emotional Intensity (E)
#      Dominance  -> Sense of Control    (S)
#
#  Produces THREE views, all interactive (open in browser / RStudio Viewer):
#    (1) Labelled 3D scatter, coloured by nearest "emotion prototype"
#    (2) 3D kernel-density isosurface  (where respondents cluster)
#    (3) 3x3x3 "Rubik's cube" of voxels, each labelled with its emotion + count
#
#  The trick that makes it INTERPRETABLE (not just a point cloud) is the
#  EMOTION PROTOTYPE table: named emotions placed at canonical PES coordinates
#  (Mehrabian's PAD octant model, relabelled). Every respondent is assigned to
#  the nearest prototype, so a region can be read as "joy", "skeptical",
#  "confused", etc. Edit that table to match your own emotion vocabulary.
# ============================================================================
## ---- 0. Packages -----------------------------------------------------------
pkgs <- c("plotly", "dplyr", "readr", "ks", "misc3d")   # misc3d only for view 2
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
library(plotly); library(dplyr); library(readr)
## ---- 1. Load & clean -------------------------------------------------------
# adjust the path to wherever the file lives
# The raw columns in the file stay pleasure/arousal/dominance; we rename them
# on read to the PES variables P (Positivity), E (Emotional Intensity),
# S (Sense of Control).
df <- read_csv('/Users/Aranya/Work/Work --surface/Raiz /Open Text/Q3/Opinion Based/Opus/opinion_pad_scores.csv', show_col_types = FALSE) %>%
  rename(P = pleasure, E = arousal, S = dominance) %>%
  filter(!is.na(P), !is.na(E), !is.na(S)) %>%
  mutate(across(c(P, E, S), as.numeric))
cat(sprintf("Loaded %d respondents with complete PES scores\n", nrow(df)))
## ---- 2. Emotion prototypes (EDIT THIS TABLE) -------------------------------
# Coordinates on a -1..1 scale for Positivity, Emotional Intensity, Sense of
# Control. These are the "anchors": whichever anchor a respondent is closest to
# (in 3D Euclidean distance) becomes their emotion label.
prototypes <- tribble(
  ~emotion,       ~P,    ~E,    ~S,     ~color,
  "Joy / Elated",  0.8,   0.6,   0.5,   "#F4B400",  # positive, energised, in control
  "Excited",       0.6,   0.8,   0.4,   "#BA38B1",
  "Content",       0.6,  -0.3,   0.4,   "#5CCB09",  # positive, calm, secure
  "Hopeful",       0.5,   0.2,   0.3,   "#18B8ED",
  "Neutral",       0.0,   0.0,   0.0,   "#9E9E9E",
  "Skeptical",    -0.2,   0.1,   0.3,   "#3A3E96",  # mild negativity, guarded, in control
  "Confused",     -0.2,   0.3,  -0.4,   "#0066FF",  # negative, intense, low control
  "Anxious",      -0.4,   0.6,  -0.5,   "#AB47BC",
  "Frustrated",   -0.5,   0.5,   0.4,   "#7030A0",  # negative, intense, wants control
  "Bored",        -0.3,  -0.5,  -0.3,   "#78909C",
  "Disappointed", -0.6,  -0.2,  -0.3,   "#455A64"
)
## ---- 3. Assign each respondent to nearest prototype ------------------------
assign_emotion <- function(P, E, S, proto) {
  d <- sqrt((proto$P - P)^2 + (proto$E - E)^2 + (proto$S - S)^2)
  proto$emotion[which.min(d)]
}
df$emotion <- mapply(assign_emotion, df$P, df$E, df$S,
                     MoreArgs = list(proto = prototypes))
df$emotion <- factor(df$emotion, levels = prototypes$emotion)
pal <- setNames(prototypes$color, prototypes$emotion)
print(sort(table(df$emotion), decreasing = TRUE))
## ============================================================================
## VIEW 1 — Labelled 3D scatter coloured by emotion
## Jitter identical points slightly so overlapping scores don't hide density.
## ============================================================================
set.seed(1)
plot_df <- df %>%
  mutate(Pj = P + rnorm(n(), 0, .015),
         Ej = E + rnorm(n(), 0, .015),
         Sj = S + rnorm(n(), 0, .015))
v1 <- plot_ly(plot_df,
              x = ~Pj, y = ~Ej, z = ~Sj,
              color = ~emotion, colors = pal,
              type = "scatter3d", mode = "markers",
              marker = list(size = 3, opacity = 0.55),
              text = ~paste0("<b>", emotion, "</b><br>P=", P, " E=", E, " S=", S),
              hoverinfo = "text") %>%
  # drop the emotion prototypes in as big diamonds so labels are visible
  add_trace(data = prototypes, x = ~P, y = ~E, z = ~S,
            type = "scatter3d", mode = "markers+text",
            marker = list(size = 8, symbol = "diamond",
                          color = ~color, line = list(color = "black", width = 1)),
            text = ~emotion, textposition = "top center",
            textfont = list(size = 14), inherit = FALSE,
            showlegend = FALSE, hoverinfo = "skip") %>%
  layout(title = NULL,
         scene = list(
           xaxis = list(title = "Positivity",          range = c(-0.75, 0.96)),
           yaxis = list(title = "Emotional Intensity", range = c(-0.4, 0.75)),
           zaxis = list(title = "Sense of Control",    range = c(-0.75, 0.8))))
v1   # print / save with: htmlwidgets::saveWidget(v1, "view1_scatter.html")
## ============================================================================
## VIEW 2 — 3D kernel-density isosurface (the actual "3D density plot")
## Shows nested shells enclosing the densest 25% / 50% / 75% of respondents.
## ============================================================================
library(ks)
H   <- Hpi(df[, c("P","E","S")])                 # bandwidth
fit <- kde(df[, c("P","E","S")], H = H,
           gridsize = 40, xmin = c(-1,-1,-1), xmax = c(1,1,1))
g  <- fit$eval.points
vol <- fit$estimate
# density thresholds enclosing 75/50/25% of the probability mass
lv <- contourLevels(fit, cont = c(75, 50, 25))
v2 <- plot_ly() %>%
  add_trace(type = "isosurface",
            x = rep(g[[1]], times = 40*40),
            y = rep(rep(g[[2]], each = 40), times = 40),
            z = rep(g[[3]], each = 40*40),
            value = as.vector(vol),
            isomin = min(lv), isomax = max(vol),
            surface = list(count = 3),
            opacity = 0.35, colorscale = "Viridis",
            caps = list(x = list(show = FALSE),
                        y = list(show = FALSE),
                        z = list(show = FALSE))) %>%
  layout(title = "PES space — kernel-density shells (where respondents cluster)",
         scene = list(xaxis = list(title = "Positivity"),
                      yaxis = list(title = "Emotional Intensity"),
                      zaxis = list(title = "Sense of Control")))
v2   # htmlwidgets::saveWidget(v2, "view2_density.html")
## ============================================================================
## VIEW 3 — 3x3x3 "Rubik's cube"
## Each axis split into Low / Mid / High -> 27 cells. Every cell is drawn as a
## translucent cube whose OPACITY = how many respondents fall in it, and is
## LABELLED with (a) the emotion of the prototype nearest the cell centre and
## (b) the respondent count. This is the most "at-a-glance readable" view.
## ============================================================================
brks <- c(-1, -1/3, 1/3, 1)
lab  <- c("Low", "Mid", "High")
cut3 <- function(x) cut(x, brks, labels = lab, include.lowest = TRUE)
showtext::showtext_auto()
font_family <- "Roboto Light"
# label each occupied cell with nearest emotion prototype
cell_emotion <- function(cx, cy, cz) {
  d <- sqrt((prototypes$P-cx)^2 + (prototypes$E-cy)^2 + (prototypes$S-cz)^2)
  prototypes$emotion[which.min(d)]
}
centers <- c(Low = -2/3, Mid = 0, High = 2/3)
half    <- 1/3                       # half-width of each cube
# build the cell table: counts -> PERCENTAGE, cube centre, emotion, label
cells <- df %>%
  mutate(bx = cut3(P), by = cut3(E), bz = cut3(S)) %>%
  count(bx, by, bz, name = "n") %>%
  mutate(
    pct = 100 * n / sum(n),
    cx  = centers[as.character(bx)],
    cy  = centers[as.character(by)],
    cz  = centers[as.character(bz)],
    emo = mapply(cell_emotion, cx, cy, cz),
    # single-line label -> no <br>/newline artefact, no overlap
    label = sprintf("%s  %.1f%%", emo, pct)
  ) %>%
  filter(pct>=0.5)
# helper: 8 corners + 12 triangular faces of one cube for mesh3d
cube_mesh <- function(cx, cy, cz, h) {
  x <- cx + h*c(-1,-1, 1, 1,-1,-1, 1, 1)
  y <- cy + h*c(-1, 1, 1,-1,-1, 1, 1,-1)
  z <- cz + h*c(-1,-1,-1,-1, 1, 1, 1, 1)
  i <- c(0,0,0,0,4,4,0,0,1,1,2,3)
  j <- c(1,2,3,4,5,7,1,4,5,6,6,7)
  k <- c(2,3,4,1,6,6,5,5,6,7,7,4)
  list(x=x,y=y,z=z,i=i,j=j,k=k)
}
# --- draw the translucent cubes (hover uses <br>, which IS supported there) --
v3 <- plot_ly()
maxn <- max(cells$n)
for (r in seq_len(nrow(cells))) {
  m <- cube_mesh(cells$cx[r], cells$cy[r], cells$cz[r], half*0.92)
  v3 <- add_trace(v3, type = "mesh3d",
                  x=m$x, y=m$y, z=m$z, i=m$i, j=m$j, k=m$k,
                  facecolor = rep(pal[[cells$emo[r]]], 12),
                  opacity = 0.15 + 0.6*(cells$n[r]/maxn),   # denser cell = more solid
                  flatshading = TRUE, showscale = FALSE,
                  hoverinfo = "text",
                  text = sprintf("%s | %s-P %s-E %s-S<br>%.1f%% of respondents (n=%d)",
                                 cells$emo[r], cells$bx[r], cells$by[r], cells$bz[r],
                                 cells$pct[r], cells$n[r]))
}
# --- ONE combined text trace: single-line labels, Roboto Light -------------
v3 <- add_trace(v3, type = "scatter3d", mode = "text",
                x = cells$cx, y = cells$cy, z = cells$cz,
                text = cells$label,
                textposition = "middle center",
                textfont = list(size = 16, color = "#111111", family = "roboto light"),
                showlegend = FALSE, hoverinfo = "skip")
v3 <- layout(v3,
             title = list(text = NULL,
                          font = list(family = "Roboto light")),
             font  = list(family = font_family),
             scene = list(
               xaxis = list(title="Positivity",          tickvals=centers, ticktext=names(centers)),
               yaxis = list(title="Emotional Intensity", tickvals=centers, ticktext=names(centers)),
               zaxis = list(title="Sense of Control",    tickvals=centers, ticktext=names(centers))))
v3   # htmlwidgets::saveWidget(v3, "view3_rubik.html")
## ---- 4. (optional) export a table of who's in each emotion ------------------
# write_csv(df %>% count(emotion, sort = TRUE), "emotion_counts.csv")
# write_csv(df, "respondents_with_emotion.csv")
htmlwidgets::saveWidget(v3, "Q3_pes_cube.html", selfcontained = TRUE)
getwd()
setwd('/Users/Aranya/Work/Work --surface/Raiz /Open Text/Q3/')
