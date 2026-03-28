##----------------------------------------------------------------------------##
## Tab: Immune Repertoire server (unified TCR/BCR)
##
## Uses getImmuneRepertoire() which returns data from the unified
## immune_repertoire field, or falls back to merging legacy bcr_data + tcr_data.
## Chain choices are auto-detected from the CTgene column content.
##----------------------------------------------------------------------------##

local({

  has_scRepertoire <- function() {
    requireNamespace("scRepertoire", quietly = TRUE)
  }

  safeRenderPlot <- function(expr, plot_name = "unknown") {
    tryCatch({
      expr
    }, error = function(e) {
      message("[IR ERROR] Plot '", plot_name, "' failed: ", e$message)
      plot.new()
      text(0.5, 0.5, paste("Error in", plot_name, ":\n", e$message), cex = 0.8)
    })
  }

  ## ---- Detect chain types present in data -------------------------------- ##
  detect_chains <- function(data) {
    if (is.null(data) || !is.list(data) || length(data) == 0) return(character(0))
    # Sample up to 3 elements for efficiency
    sample_dfs <- data[seq_len(min(length(data), 3))]
    all_ct <- unlist(lapply(sample_dfs, function(df) {
      if ("CTgene" %in% names(df)) as.character(df$CTgene) else character(0)
    }))
    chains <- character(0)
    if (any(grepl("TRA", all_ct)))  chains <- c(chains, "TRA")
    if (any(grepl("TRB", all_ct)))  chains <- c(chains, "TRB")
    if (any(grepl("TRG", all_ct)))  chains <- c(chains, "TRG")
    if (any(grepl("TRD", all_ct)))  chains <- c(chains, "TRD")
    if (any(grepl("IGH", all_ct)))  chains <- c(chains, "IGH")
    if (any(grepl("IGK", all_ct)))  chains <- c(chains, "IGK")
    if (any(grepl("IGL", all_ct)))  chains <- c(chains, "IGL")
    chains
  }

  ## ---- Reactive: repertoire data ---------------------------------------- ##
  ir_data <- reactive({
    req(!is.null(data_set()))
    data <- getImmuneRepertoire()
    if (is.null(data) || !is.list(data) || length(data) == 0) return(NULL)
    data
  })

  ## ---- Reactive: parameters --------------------------------------------- ##
  ir_params <- reactive({
    gb <- input$ir_groupBy
    if (is.null(gb) || gb == "") gb <- NULL
    list(
      cloneCall = input$ir_cloneCall,
      chain     = input$ir_chain,
      groupBy   = gb
    )
  })

  ## ---- Reactive: number of groups for faceted plots --------------------- ##
  n_groups <- reactive({
    gb <- ir_params()$groupBy
    if (is.null(gb)) return(1L)
    data <- ir_data()
    if (is.null(data)) return(1L)
    lvls <- unique(unlist(lapply(data, function(df) {
      if (gb %in% names(df)) unique(as.character(df[[gb]])) else character(0)
    })))
    max(1L, length(lvls))
  })

  ## ---- Dynamic gene parameter for vizGenes/percentGeneUsage ------------- ##
  default_gene_family <- reactive({
    chains <- detect_chains(ir_data())
    tcr_chains <- intersect(chains, c("TRA", "TRB", "TRG", "TRD"))
    bcr_chains <- intersect(chains, c("IGH", "IGK", "IGL"))
    if (length(tcr_chains) > 0 && "TRB" %in% tcr_chains) return("TRBV")
    if (length(tcr_chains) > 0) return(paste0(tcr_chains[1], "V"))
    if (length(bcr_chains) > 0 && "IGH" %in% bcr_chains) return("IGHV")
    if (length(bcr_chains) > 0) return(paste0(bcr_chains[1], "V"))
    "TRBV"
  })

  ## ---- Resolve chain: for functions that don't accept "both" ------------ ##
  specific_chain <- reactive({
    ch <- input$ir_chain
    if (is.null(ch) || ch == "both") {
      chains <- detect_chains(ir_data())
      if ("TRB" %in% chains) return("TRB")
      if (length(chains) > 0) return(chains[1])
      return("TRB")
    }
    ch
  })

  ## ---- Count unique genes for dynamic plot height ----------------------- ##
  n_genes <- reactive({
    data <- ir_data()
    if (is.null(data)) return(0L)
    gene_family <- default_gene_family()
    # Gather all gene values across samples
    all_genes <- unique(unlist(lapply(data, function(df) {
      # CTgene has format like "TRBV1.TRBJ2" — extract the gene family portion
      ct <- as.character(df$CTgene)
      ct <- ct[!is.na(ct)]
      # Split by "." and keep segments matching the gene family prefix
      segments <- unlist(strsplit(ct, "[._]"))
      segments[grepl(paste0("^", gene_family), segments, ignore.case = TRUE)]
    })))
    length(all_genes)
  })

  ir_plot_height <- function(facet_mode = c("none", "grid", "wrap")) {
    facet_mode <- match.arg(facet_mode)
    n <- n_genes()
    ng <- n_groups()
    base_h <- max(450, min(n * 25, 2500))
    if (ng <= 1 || facet_mode == "none") return(base_h)
    if (facet_mode == "grid") {
      # facet_grid(Group ~ .): each group stacked vertically
      return(base_h * ng)
    }
    # facet_wrap: ggplot default ncol = ceiling(sqrt(n))
    ncol <- ceiling(sqrt(ng))
    nrow <- ceiling(ng / ncol)
    base_h * nrow
  }

  ## ---- Tab change: update cloneCall choices ----------------------------- ##
  observeEvent(input$ir_tabs, {
    req(has_scRepertoire())
    tab <- input$ir_tabs
    if (tab %in% c("Length", "K-mer")) {
      updateSelectInput(session, "ir_cloneCall",
        choices = c("nt", "aa"),
        selected = if (input$ir_cloneCall %in% c("nt", "aa")) input$ir_cloneCall else "aa"
      )
    } else if (tab %in% c("Gene usage", "vizGenes", "percentGenes",
                           "percentVJ", "AA %", "Entropy")) {
      updateSelectInput(session, "ir_cloneCall", choices = NULL, selected = NULL)
    } else {
      updateSelectInput(session, "ir_cloneCall",
        choices = c("gene", "nt", "aa", "strict"),
        selected = input$ir_cloneCall
      )
    }
    shinyjs::toggleElement(id = "ir_scatter_x", anim = TRUE,
      condition = tab == "Scatter" && n_samples() >= 2)
    shinyjs::toggleElement(id = "ir_scatter_y", anim = TRUE,
      condition = tab == "Scatter" && n_samples() >= 2)
    shinyjs::toggleElement(id = "ir_compare_samples", anim = TRUE,
      condition = tab == "Compare" && n_samples() >= 2)
  })

  ## ---- Settings UI ------------------------------------------------------ ##
  output$ir_settings_UI <- renderUI({
    req(has_scRepertoire())
    data <- ir_data()
    if (is.null(data)) {
      return(div(class = "alert alert-warning",
        "No immune repertoire data available. Import data with TCR/BCR annotations first."))
    }

    available_samples <- names(data)
    chains_present <- detect_chains(data)
    ## Build grouped chain choices: All / TCR / BCR
    tcr_present <- intersect(chains_present, c("TRA", "TRB", "TRG", "TRD"))
    bcr_present <- intersect(chains_present, c("IGH", "IGK", "IGL"))
    chain_choices <- list("All" = "both")
    if (length(tcr_present) > 0)
      chain_choices[["TCR"]] <- as.list(setNames(tcr_present, tcr_present))
    if (length(bcr_present) > 0)
      chain_choices[["BCR"]] <- as.list(setNames(bcr_present, bcr_present))

    all_groups <- getGroups()
    data_cols <- names(data[[1]])
    available_groups <- c(NULL, intersect(all_groups, data_cols))

    tagList(
      tags$style("#ir_chain + .selectize-control .selectize-dropdown-content { max-height: none; }"),
      fluidRow(
        column(6, selectInput("ir_cloneCall", "Clone call:",
          choices = c("gene", "nt", "aa", "strict"), selected = "gene")),
        column(6, selectInput("ir_groupBy", "Group by:",
          choices = c("None" = "", available_groups), selected = ""))
      ),
      fluidRow(
        column(6, selectInput("ir_chain", "Chain:",
          choices = chain_choices, selected = "both"))
      ),
      if (length(available_samples) >= 2) {
        tagList(
          fluidRow(
            column(6, selectInput("ir_scatter_x", "Sample 1 (Scatter):",
              choices = available_samples, selected = available_samples[1])),
            column(6, selectInput("ir_scatter_y", "Sample 2 (Scatter):",
              choices = available_samples, selected = available_samples[2]))
          ),
          fluidRow(
            column(12, selectInput("ir_compare_samples",
              "Samples for Compare (select >= 2):",
              choices = available_samples, multiple = TRUE,
              selected = available_samples[1:2]))
          )
        )
      }
    )
  })

  ## ---- Reactive: number of samples -------------------------------------- ##
  n_samples <- reactive({
    data <- ir_data()
    if (is.null(data)) 0L else length(data)
  })

  ## ---- Visualizations UI ------------------------------------------------ ##
  output$ir_visualizations_UI <- renderUI({
    req(has_scRepertoire())
    data <- ir_data()
    if (is.null(data)) {
      return(div(class = "alert alert-warning",
        "No immune repertoire data available."))
    }

    ## Always-available tabs
    tabs <- list(
      tabPanel("Abundance",    shinycssloaders::withSpinner(plotOutput("ir_plot_clonalAbundance",          height = 450))),
      tabPanel("Diversity",    shinycssloaders::withSpinner(plotOutput("ir_plot_clonalDiversity",          height = 450))),
      tabPanel("Homeostasis",  shinycssloaders::withSpinner(plotOutput("ir_plot_clonalHomeostasis",        height = 450))),
      tabPanel("Length",       shinycssloaders::withSpinner(plotOutput("ir_plot_clonalLength",             height = 450))),
      tabPanel("Proportion",   shinycssloaders::withSpinner(plotOutput("ir_plot_clonalProportion",         height = 450))),
      tabPanel("Quant",        shinycssloaders::withSpinner(plotOutput("ir_plot_clonalQuant",              height = 450))),
      tabPanel("Rarefaction",  shinycssloaders::withSpinner(uiOutput("ir_ui_clonalRarefaction"))),
      tabPanel("Gene usage",   shinycssloaders::withSpinner(uiOutput("ir_ui_percentGeneUsage"))),
      tabPanel("vizGenes",     shinycssloaders::withSpinner(uiOutput("ir_ui_vizGenes"))),
      tabPanel("percentGenes", shinycssloaders::withSpinner(uiOutput("ir_ui_percentGenes"))),
      tabPanel("percentVJ",    shinycssloaders::withSpinner(uiOutput("ir_ui_percentVJ"))),
      tabPanel("AA %",         shinycssloaders::withSpinner(uiOutput("ir_ui_percentAA"))),
      tabPanel("Entropy",      shinycssloaders::withSpinner(plotOutput("ir_plot_positionalEntropy",        height = 450))),
      tabPanel("Property",     shinycssloaders::withSpinner(uiOutput("ir_ui_positionalProperty"))),
      tabPanel("K-mer",        shinycssloaders::withSpinner(uiOutput("ir_ui_percentKmer")))
    )

    ## Tabs requiring >= 2 samples
    if (n_samples() >= 2) {
      multi_tabs <- list(
        tabPanel("Compare",  shinycssloaders::withSpinner(plotOutput("ir_plot_clonalCompare",              height = 450))),
        tabPanel("Overlap",  shinycssloaders::withSpinner(plotOutput("ir_plot_clonalOverlap",              height = 450))),
        tabPanel("Scatter",  shinycssloaders::withSpinner(plotOutput("ir_plot_clonalScatter",              height = 450))),
        tabPanel("SizeDist", shinycssloaders::withSpinner(plotOutput("ir_plot_clonalSizeDistribution",     height = 450)))
      )
      tabs <- c(tabs, multi_tabs)
    }

    do.call(tabsetPanel, c(list(id = "ir_tabs"), tabs))
  })

  ##------------------------------------------------------------------------##
  ## Plot renderers
  ##------------------------------------------------------------------------##

  output$ir_plot_clonalAbundance <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::clonalAbundance(data, cloneCall = pars$cloneCall,
        group.by = pars$groupBy),
      "clonalAbundance")
  })

  output$ir_plot_clonalCompare <- renderPlot({
    req(has_scRepertoire())
    req(!is.null(input$ir_compare_samples) && length(input$ir_compare_samples) >= 2)
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::clonalCompare(data,
        cloneCall = pars$cloneCall, chain = pars$chain,
        group.by = pars$groupBy, samples = input$ir_compare_samples,
        top.clones = 5, graph = "alluvial", proportion = TRUE,
        exportTable = FALSE, palette = "inferno"),
      "clonalCompare")
  })

  output$ir_plot_clonalDiversity <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::clonalDiversity(data,
        cloneCall = pars$cloneCall, chain = pars$chain,
        group.by = pars$groupBy, metric = "shannon", n.boots = 100,
        exportTable = FALSE, palette = "inferno"),
      "clonalDiversity")
  })

  output$ir_plot_clonalHomeostasis <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::clonalHomeostasis(data,
        cloneCall = pars$cloneCall, chain = pars$chain,
        group.by = pars$groupBy,
        exportTable = FALSE, palette = "inferno"),
      "clonalHomeostasis")
  })

  output$ir_plot_clonalLength <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::clonalLength(data,
        cloneCall = pars$cloneCall, chain = pars$chain,
        group.by = pars$groupBy,
        exportTable = FALSE, palette = "inferno"),
      "clonalLength")
  })

  output$ir_plot_clonalOverlap <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::clonalOverlap(data,
        cloneCall = pars$cloneCall, chain = pars$chain,
        group.by = pars$groupBy, method = "overlap",
        exportTable = FALSE, palette = "inferno"),
      "clonalOverlap")
  })

  output$ir_plot_clonalProportion <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::clonalProportion(data,
        cloneCall = pars$cloneCall, chain = pars$chain,
        group.by = pars$groupBy,
        clonalSplit = c(10, 100, 1000, 10000, 30000, 1e+05),
        exportTable = FALSE, palette = "inferno"),
      "clonalProportion")
  })

  output$ir_plot_clonalQuant <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::clonalQuant(data,
        cloneCall = pars$cloneCall, chain = pars$chain,
        group.by = pars$groupBy, scale = FALSE,
        exportTable = FALSE, palette = "inferno"),
      "clonalQuant")
  })

  output$ir_ui_clonalRarefaction <- renderUI({
    n_boots <- input$ir_rarefaction_boots
    if (is.null(n_boots)) n_boots <- 5
    tagList(
      sliderInput("ir_rarefaction_boots", "Bootstrap iterations:",
        min = 3, max = 50, value = n_boots, step = 1),
      shinycssloaders::withSpinner(plotOutput("ir_plot_clonalRarefaction", height = "450px"))
    )
  })

  output$ir_plot_clonalRarefaction <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    n_boots <- input$ir_rarefaction_boots
    if (is.null(n_boots)) n_boots <- 5
    safeRenderPlot(
      scRepertoire::clonalRarefaction(data,
        cloneCall = pars$cloneCall, chain = pars$chain,
        group.by = pars$groupBy,
        plot.type = 1, hill.numbers = 0, n.boots = n_boots,
        exportTable = FALSE, palette = "inferno"),
      "clonalRarefaction")
  })

  output$ir_plot_clonalScatter <- renderPlot({
    req(has_scRepertoire())
    req(!is.null(input$ir_scatter_x) && !is.null(input$ir_scatter_y))
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::clonalScatter(data,
        cloneCall = pars$cloneCall, chain = pars$chain,
        group.by = pars$groupBy,
        x.axis = input$ir_scatter_x, y.axis = input$ir_scatter_y,
        dot.size = "total", graph = "proportion",
        exportTable = FALSE, palette = "inferno"),
      "clonalScatter")
  })

  output$ir_plot_clonalSizeDistribution <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::clonalSizeDistribution(data,
        cloneCall = pars$cloneCall, group.by = pars$groupBy,
        method = "ward.D2",
        exportTable = FALSE),
      "clonalSizeDistribution")
  })

  output$ir_ui_percentGeneUsage <- renderUI({
    h <- ir_plot_height("none")
    shinycssloaders::withSpinner(plotOutput("ir_plot_percentGeneUsage", height = paste0(h, "px")))
  })

  output$ir_plot_percentGeneUsage <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::percentGeneUsage(data,
        chain = pars$chain, gene = default_gene_family(),
        group.by = pars$groupBy,
        summary.fun = "percent", plot.type = "heatmap",
        exportTable = FALSE, palette = "inferno"),
      "percentGeneUsage")
  })

  output$ir_ui_vizGenes <- renderUI({
    h <- ir_plot_height("none")
    shinycssloaders::withSpinner(plotOutput("ir_plot_vizGenes", height = paste0(h, "px")))
  })

  output$ir_plot_vizGenes <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::vizGenes(data,
        x.axis = default_gene_family(), y.axis = NULL,
        group.by = pars$groupBy,
        plot = "heatmap", summary.fun = "count",
        exportTable = FALSE, palette = "inferno"),
      "vizGenes")
  })

  output$ir_ui_percentGenes <- renderUI({
    h <- ir_plot_height("none")
    shinycssloaders::withSpinner(plotOutput("ir_plot_percentGenes", height = paste0(h, "px")))
  })

  output$ir_plot_percentGenes <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::percentGenes(data,
        chain = specific_chain(), gene = "Vgene",
        group.by = pars$groupBy, summary.fun = "percent",
        exportTable = FALSE, palette = "inferno"),
      "percentGenes")
  })

  output$ir_ui_percentVJ <- renderUI({
    h <- ir_plot_height("wrap")
    shinycssloaders::withSpinner(plotOutput("ir_plot_percentVJ", height = paste0(h, "px")))
  })

  output$ir_plot_percentVJ <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::percentVJ(data,
        chain = specific_chain(),
        group.by = pars$groupBy, summary.fun = "percent",
        exportTable = FALSE, palette = "inferno"),
      "percentVJ")
  })

  output$ir_ui_percentAA <- renderUI({
    ng <- n_groups()
    # facet_grid(group ~ .): ~200px per group, minimum 400
    h <- max(400, ng * 200)
    shinycssloaders::withSpinner(plotOutput("ir_plot_percentAA", height = paste0(h, "px")))
  })

  output$ir_plot_percentAA <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::percentAA(data,
        chain = pars$chain, group.by = pars$groupBy,
        aa.length = 20,
        exportTable = FALSE, palette = "inferno"),
      "percentAA")
  })

  output$ir_plot_positionalEntropy <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    safeRenderPlot(
      scRepertoire::positionalEntropy(data,
        chain = pars$chain, group.by = pars$groupBy,
        aa.length = 20, method = "norm.entropy",
        exportTable = FALSE, palette = "inferno"),
      "positionalEntropy")
  })

  ## ---- Positional Property: facet count per method ---------------------- ##
  ## Requires immApex; most methods also need the Peptides package.
  all_property_facets <- c(
    atchleyFactors = 5, crucianiProperties = 3, FASGAI = 6,
    kideraFactors = 10, MSWHIM = 3, ProtFP = 8,
    stScales = 8, tScales = 5, VHSE = 8, zScales = 5
  )

  available_property_methods <- reactive({
    resolver <- tryCatch(
      getFromNamespace(".aa.property.matrix", "immApex"),
      error = function(e) NULL)
    if (is.null(resolver)) return(all_property_facets["atchleyFactors"])
    ok <- vapply(names(all_property_facets), function(m) {
      tryCatch({ resolver(m); TRUE }, error = function(e) FALSE)
    }, logical(1))
    all_property_facets[ok]
  })

  output$ir_ui_positionalProperty <- renderUI({
    avail <- available_property_methods()
    method <- input$ir_property_method
    if (is.null(method) || !method %in% names(avail)) method <- names(avail)[1]
    n_facets <- avail[[method]]
    if (is.null(n_facets)) n_facets <- 5
    # ~120px per facet row, minimum 450
    h <- max(450, n_facets * 120)
    tagList(
      selectInput("ir_property_method", "Property method:",
        choices = names(avail), selected = method),
      shinycssloaders::withSpinner(plotOutput("ir_plot_positionalProperty", height = paste0(h, "px")))
    )
  })

  output$ir_plot_positionalProperty <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    method <- input$ir_property_method
    if (is.null(method)) method <- "atchleyFactors"
    safeRenderPlot(
      scRepertoire::positionalProperty(data,
        chain = pars$chain, group.by = pars$groupBy,
        method = method,
        exportTable = FALSE, palette = "inferno"),
      "positionalProperty")
  })

  output$ir_ui_percentKmer <- renderUI({
    top_m <- input$ir_kmer_top_motifs
    if (is.null(top_m)) top_m <- 30
    h <- max(450, top_m * 20)
    tagList(
      sliderInput("ir_kmer_top_motifs", "Top motifs:",
        min = 10, max = 100, value = top_m, step = 5),
      shinycssloaders::withSpinner(plotOutput("ir_plot_percentKmer", height = paste0(h, "px")))
    )
  })

  output$ir_plot_percentKmer <- renderPlot({
    req(has_scRepertoire())
    data <- ir_data(); req(!is.null(data))
    pars <- ir_params()
    top_m <- input$ir_kmer_top_motifs
    if (is.null(top_m)) top_m <- 30
    safeRenderPlot(
      scRepertoire::percentKmer(data,
        chain = pars$chain, cloneCall = pars$cloneCall,
        group.by = pars$groupBy,
        motif.length = 3, min.depth = 3, top.motifs = top_m,
        exportTable = FALSE, palette = "inferno"),
      "percentKmer")
  })

})
