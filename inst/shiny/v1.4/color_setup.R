##----------------------------------------------------------------------------##
## Color management.
##----------------------------------------------------------------------------##
# Low-saturation qualitative palette for categorical groups (clusters, samples,
# ...) when the user has NOT picked colours in the Color management tab. It
# replaces the old high-saturation flatuicolors sets so plots read as calm and
# publication-like. Warm/cool balanced, aligned to the app --c-blue / --c-amber
# accents; no neon primaries. 12 base hues.
#
# Users can still override any group colour with the colour picker; this is only
# the default fallback (see reactive_colors() below).
default_colorset_base <- c(
  "#4c72a6", # muted blue    (aligns with --c-blue)
  "#dd8452", # terracotta    (aligns with --c-amber)
  "#6e9e6b", # sage green
  "#b5606a", # dusty rose
  "#9d6fae", # muted violet (nudged lighter/warmer to clear ΔE>=20 from the blue)
  "#8c7b6b", # warm taupe
  "#c99bc4", # soft lilac
  "#7e9bb0", # slate blue
  "#c2a25a", # muted gold
  "#5fa8a3", # dusty teal
  "#a98b78", # clay
  "#9aa06b" # olive
)

## Build n visually distinct qualitative colours from the base palette. For
## n <= length(base) we take the first n base hues (hand-tuned, best contrast).
## For n > length(base) we interpolate across the whole base ring with
## colorRampPalette so a data set with many clusters still gets n *valid*
## colours instead of NAs — the old `default_colorset[seq_along(...)]` slicing
## silently returned NA past 40 groups.
cerebro_group_colors <- function(n) {
  n <- max(0L, as.integer(n))
  if (n == 0L) {
    return(character(0))
  }
  if (n <= length(default_colorset_base)) {
    return(default_colorset_base[seq_len(n)])
  }
  grDevices::colorRampPalette(default_colorset_base)(n)
}

# Kept for backward compatibility: any code that still indexes a flat vector
# gets a long-enough, low-saturation set instead of the old flatuicolors.
default_colorset <- cerebro_group_colors(40)

# Cell-cycle phases: low-saturation four-colour set matching the app palette.
# G1 = calm blue, S = muted gold, G2M = muted brick (was tomato #e74c3c),
# "-"/unknown = warm grey.
cell_cycle_colorset <- setNames(
  c("#4c72a6", "#c2a25a", "#c05b5b", "#9a9aa0"),
  c("G1", "S", "G2M", "-")
)

##----------------------------------------------------------------------------##
## Assign colors to groups.
##----------------------------------------------------------------------------##
reactive_colors <- reactive({
  req(data_set())
  ## get cell meta data
  meta_data <- getMetaData()
  colors <- list()
  ## go through all groups
  for (group_name in getGroups()) {
    ## if color selection from the "Color management" tab exist, assign those
    ## colors, otherwise assign colors from default colorset
    if (
      !is.null(input[[paste0(
        'color_',
        group_name,
        '_',
        getGroupLevels(group_name)[1]
      )]])
    ) {
      for (group_level in getGroupLevels(group_name)) {
        ## it seems that special characters are not handled well in input/output
        ## so I replace them with underscores using gsub()
        colors[[group_name]][group_level] <- input[[paste0(
          'color_',
          group_name,
          '_',
          gsub(group_level, pattern = '[^[:alnum:]]', replacement = '_')
        )]]
      }
    } else {
      colors[[group_name]] <- cerebro_group_colors(
        length(getGroupLevels(group_name))
      )
      names(colors[[group_name]]) <- getGroupLevels(group_name)
      if ('N/A' %in% getGroupLevels(group_name)) {
        colors[[group_name]][which(
          names(colors[[group_name]]) == 'N/A'
        )] <- '#898989'
      }
    }
  }
  ## go through columns with cell cycle info
  if (length(getCellCycle()) > 0) {
    for (column in getCellCycle()) {
      ## if color selection from the "Color management" tab exist, assign those
      ## colors, otherwise assign colors from cell cycle colorset
      if (
        !is.null(input[[paste0(
          'color_',
          column,
          '_',
          unique(as.character(meta_data[[column]]))[1]
        )]])
      ) {
        for (state in unique(as.character(meta_data[[column]]))) {
          ## it seems that special characters are not handled well in input/output
          ## so I replace them with underscores using gsub()
          colors[[column]][state] <- input[[paste0(
            'color_',
            column,
            '_',
            gsub(state, pattern = '[^[:alnum:]]', replacement = '_')
          )]]
        }
      } else {
        colors[[
          column
        ]] <- cell_cycle_colorset[seq_along(unique(as.character(meta_data[[
          column
        ]])))]
        names(colors[[column]]) <- unique(as.character(meta_data[[column]]))
      }
    }
  }
  return(colors)
})
