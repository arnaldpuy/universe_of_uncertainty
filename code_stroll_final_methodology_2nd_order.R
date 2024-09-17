## ----setup, include=FALSE---------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "tikz", cache = TRUE)


## ----preliminary, warning=FALSE, message=FALSE------------------------------------------------------

# LOAD PACKAGES ################################################################

library(sensobol)
load_packages(c("haven", "tidyverse", "data.table", "margins", "miceadds",
                "doParallel", "foreach", "cowplot", "benchmarkme", "sandwich", 
                "countrycode"))

# THEME FOR PLOTTING ###########################################################

theme_AP <- function() {
  theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA), 
          strip.background = element_rect(fill = "white"), 
          legend.margin = margin(0.5, 0.1, 0.1, 0.1),
          legend.box.margin = margin(0.2,-4,-7,-7), 
          plot.margin = margin(3, 4, 0, 4), 
          legend.text = element_text(size = 8), 
          axis.title = element_text(size = 10),
          legend.key.width = unit(0.4, "cm"), 
          legend.key.height = unit(0.4, "cm"), 
          legend.title = element_text(size = 9)) 
}


## ----data-------------------------------------------------------------------------------------------

# READ DATA AND STANDARDIZE ####################################################

data <- readRDS("df.rds")
setDT(data)

# Columns with migration data  -------------------------------------------------

cols <- c("foreignpct", "migstock_wb", "migstock_un", "migstock_oecd", "netmigpct")

# Standardize migration data ---------------------------------------------------

data[, (cols) := lapply(.SD, function(x) as.numeric(scale(as.numeric(x)))), .SDcols = cols]

# TURN DOTS AND EMPTY CELLS INTO NA ############################################

data[data == "."] <- NA
data[data == ""] <- NA

# IDENTIFY NUMERIC COLUMNS AND TRANSFORM #######################################

numeric_columns <- sapply(data, is.numeric)
numeric_columns <- names(numeric_columns[numeric_columns])
data[, (numeric_columns):= lapply(.SD, as.numeric), .SDcols = (numeric_columns)]
data[, iso_country:= as.numeric(iso_country)]

to.numeric <- c("gdp_wb", "gdp_twn", "gni_wb", "gini_wid", "ginim_dolt", "top10_wid", 
                "al_ethnic", "dpi_tf", "wdi_empprilo", "mcp", "mignet_un")

data[, (to.numeric):= lapply(.SD, as.numeric), .SDcols = (to.numeric)]

# IDENTIFY THE PROPORTION OF MISSING VALUES PER COLUMN AND YEAR ################

data[, lapply(.SD, function(x) sum(is.na(x)) / nrow(.SD)), year]

# COUNTRY VARIABLE #############################################################

country <- unique(data$country)
setDT(data)
data[, country_cl:= countrycode(country, "country.name", "continent")]
data[, country_cl:= ifelse(country_cl == "Oceania" | country_cl == "Asia", "Asia_Oceania", 
                           ifelse(country_cl == "Americas", "America", country_cl))]

# DEFINE VARIABLES #############################################################

# Dependent variable (logit and linear) ----------------------------------------

Y.vector <- c("jobs", "unemployed", "reduce_income_diff", "old_age_care", "housing", 
              "health", "jobs_c", "unemp_c", "incdiff_c", "oldage_c", "housing_c", 
              "health_c", "latent6", "latent4") 

# Policy variables -------------------------------------------------------------

X <- c("foreignpct", "migstock_un", "netmigpct", "migstock_oecd")         

# Controls variables -----------------------------------------------------------

controls.ind = c("female", "education", "income", "iso_country", "year_t_minus_1",
                 "al_ethnic","socx_oecd", "country")     

# Controls country -------------------------------------------------------------

controls.country <- c("America - Asia_Oceania", "Asia_Oceania - Europe", 
                      "America - Europe", "America - Asia_Oceania - Europe")

# Controls age  ----------------------------------------------------------------

controls.age <- c("age", "age_sq", "no")    

# Controls gini  ---------------------------------------------------------------

controls.gini <- c("ginid_solt", "ginim_dolt")             

# Robust standard errors -------------------------------------------------------

se.vec <- c("no", "country")

# Yes / no vector --------------------------------------------------------------
yes.no.vec <- c("yes", "no")    


## ----sample_matrix, dependson="data"----------------------------------------------------------------

# DEFINE SAMPLE MATRIX #########################################################

# Settings ---------------------------------------------------------------------

matrices <- c("A", "B", "AB")
order <- "second"
N <- 2^13
params <- c("Y", "X1", controls.ind, "age", "SE", "bootstrap", "GINI")

# Sample matrix ----------------------------------------------------------------

mat <- data.table(sobol_matrices(matrices = matrices, params = params, N = N, 
                                 order = order))

# To remove -------------------------------------------------------------------

to.remove <- "country"
specific.columns <- setdiff(controls.ind, to.remove)

# Transform to appropriate columns ---------------------------------------------

mat[, Y:= floor(Y * 14) + 1]
mat[, Y:= Y.vector[Y]]
mat[, X1:= floor(X1 * length(X)) + 1]
mat[, X1:= X[X1]]
mat[, (specific.columns):= lapply(.SD, function(x) ifelse(x > 0.5, "yes", "no")), .SDcols = (specific.columns)]
mat[, country:= floor(country * length(controls.country)) + 1]
mat[, country:= controls.country[country]]
mat[, age:= floor(age * length(controls.age)) + 1]
mat[, age:= controls.age[age]]
mat[, SE:= floor(SE * length(se.vec)) + 1]
mat[, SE:= se.vec[SE]]
mat[, bootstrap:= floor(bootstrap * length(yes.no.vec)) + 1]
mat[, bootstrap:= yes.no.vec[bootstrap]]
mat[, GINI:= floor(GINI * length(controls.gini)) + 1]
mat[, GINI:= controls.gini[GINI]]

# DEFINE MODEL #################################################################

# Function to create the glm formula -------------------------------------------

formula_fun <- function(mat) {
  
  formula = paste0(mat[, Y], " ~ ", mat[,X1], collapse = " ")
  
  # Conditional on age ---------------------------------------------------------
  
  formula <- switch(mat[, age],
                    "age" = paste0(formula, " + age"),
                    "age_sq" = paste0(formula, " + age + age_sq"),
                    formula)
  
  # Add GINI -------------------------------------------------------------------
  
  formula <- paste0(formula, " + ", paste0(mat[, GINI], collapse = " + "))
  
  # List of additional variables to include if they are marked as "yes" --------
  
  additional_vars <- c("female", "education", "income", "year_t_minus_1", "iso_country",
                       "al_ethnic", "socx_oecd", "country")
  
  # Search variables marked as "yes" in mat ------------------------------------
 
  included_vars <-  names(mat[, ..additional_vars])[which(mat[, ..additional_vars] == "yes")]
  
  # Append included variables to the formula -----------------------------------
  
  if (length(included_vars) > 0) {
    
    formula <- paste0(formula, " + ", paste(included_vars, collapse = " + "))
  }

  return(formula)

}

# Wrap up function -------------------------------------------------------------

full_fun <- function(data, mat, Y.vector) {
  
  formula <- as.formula(formula_fun(mat = mat))
  
  # Country filtering ----------------------------------------------------------
  
  countries_to_filter <- switch(as.character(mat[, country]),
                                "America - Asia_Oceania" = c("America", "Asia_Oceania"),
                                "Asia_Oceania - Europe" = c("Asia_Oceania", "Europe"),
                                "America - Europe" = c("America", "Europe"),
                                "America - Asia_Oceania - Europe" = c("America", "Asia_Oceania", "Europe"),
                                NULL)
  
  if (!is.null(countries_to_filter)) {
    
    data <- data[country_cl %in% countries_to_filter]
  }
    
  # Bootstrap ------------------------------------------------------------------
  
   if (mat[, bootstrap] == "yes") {
    
     data <- data[sample(1:nrow(data), nrow(data), replace = TRUE), ]
    
   }
  
  # Determine model family and cluster variable --------------------------------
  
  family <- if (mat[, Y] %in% Y.vector[1:6]) "binomial" else "gaussian"
  
  cluster_var <- switch(as.character(mat[, SE]),
                        "no" = NULL,
                        "country" = "country",
                        "year")
  
  # Use only complete cases ----------------------------------------------------
  
  cols_formula <- all.vars(formula)
  
  if(!is.null(cluster_var)) {
    
    colnames_selected <- c(cols_formula, cluster_var)
    
  } else {
    
    colnames_selected <- cols_formula
  }
  
  data <- data[, ..colnames_selected][complete.cases(data[, ..colnames_selected])]
  
  # Execute model --------------------------------------------------------------
  
  if (family == "binomial") {
    
    if (is.null(cluster_var)) {
      
      model <- glm(formula, family = binomial, data = data)
      out <- margins(model, variables = mat[, X1])
      
    } else {
      
      model <- glm.cluster(data = data[, ..cols_formula], formula = formula, 
                           cluster = data[[cluster_var]], family = "binomial")
      out <- margins(model$glm_res, variables = mat[, X1], vcov = model$vcov)
      
    }
    
  } else {
    
    if (is.null(cluster_var)) {
      
      model <- lm(formula = formula, data = data)
      out <- margins(model, variables = mat[, X1])
      
    } else {
      
      model <- lm.cluster(data = data[, ..cols_formula], formula = formula, 
                          cluster = data[[cluster_var]])
      out <- margins(model$lm_res, variables = mat[, X1], vcov = model$vcov)
      
    }
  }
  
  out <- as.numeric(summary(out)[c("AME", "lower", "upper")])
  
  return(out)
}


## ----parallel, dependson="sample_matrix"------------------------------------------------------------

# PARALLEL COMPUTING ###########################################################

# Define parallel computing ----------------------------------------------------

cl <- makeCluster(floor(detectCores() * 0.75))
registerDoParallel(cl)

# Run the simulations ----------------------------------------------------------

out <-  foreach(i = 1:nrow(mat), 
                .packages = c("margins", "miceadds", "data.table"), 
                .combine = "rbind") %dopar% {
                  
                  full_fun(data = data, mat = mat[i, ], Y.vector = Y.vector)
                  
                }

# Stop the parallel backend ----------------------------------------------------

stopCluster(cl)


## ----arrange_data, dependson="parallel"-------------------------------------------------------------

# ARRANGE DATA #################################################################

out.dt <- data.table(out) %>%
  setnames(., colnames(.), c("AME", "low", "high")) 

AME.dt <- out.dt[1:(2 * N), ]

# EXPORT DATA ##################################################################

full.dt <- cbind(mat, out.dt)
fwrite(full.dt, "full.dt.csv")


## ----plot_uncertainty, dependson="arrange_data", dev = "pdf", fig.height=2, fig.width=3.7-----------

# PLOT UNCERTAINTY #############################################################

tmp <- out.dt[order(AME.dt)] %>%
  .[, Model:= .I] %>%
  .[, color:= ifelse(low < 0 & high < 0, "negative", 
                     ifelse(low < 0 & high > 0, "includes zero", "positive"))] %>%
  .[, color:= factor(color, levels = c("negative", "includes zero", "positive"))]

tmp[, .N, color] %>%
  .[, total.n:= nrow(tmp)] %>%
  .[, fraction:= N / total.n] %>%
  print()

# Calculate 2.5% and 97.5% quantiles -------------------------------------------

quantiles <- quantile(AME.dt$AME, c(0.025, 0.975))
quantiles

plot.uncertainty <- tmp %>%
  na.omit() %>%
  ggplot(., aes(x = Model, y = AME, color = color)) +
  geom_point(size = 0.3) +
  geom_errorbar(aes(ymin = low, ymax = high), size = 0.5) +
  scale_color_manual(values = c("orange", "grey", "blue"), name = "") +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "Models Ordered by AME", y = "AME") +
  theme_AP() + 
  theme(legend.position = "top", 
        axis.title.x = element_text(size = 8), 
        axis.text.x = element_text(size = 6.5)) 

plot.uncertainty


## ----plot_scatter, dependson="arrange_data", dev = "pdf"--------------------------------------------

# PLOT SCATTERPLOT #############################################################

plot_scatter(data = mat, N = N, Y = out, params = params) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = "", y = "AMS") + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))


## ----sa, dependson="parallel", dev = "pdf", fig.height=2.5, fig.width=4.5---------------------------

# SENSITIVITY ANALYSIS #########################################################

# Define setting ---------------------------------------------------------------

first <- "saltelli"
total <- "jansen"
R <- 10^3

# Modify Y for y in the params vector for notation consistency -----------------

params[1] <- "y"

# Run SA -----------------------------------------------------------------------

ind <- sobol_indices(matrices = matrices, params = params, N = N, Y = out.dt$AME, 
                     boot = TRUE, R = R, first = first, total = total, order = order)

ind

# Calculate sum of first and total-order indices -------------------------------
# (only indices >0.05 are considered)

ind$results[sensitivity %in% c("Si", "Sij")] %>%
  .[original > 0.05, sum(original)]

# Plot Sobol' indices ----------------------------------------------------------

plot(ind)

plot.sobol <- plot(ind) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
  theme(legend.position = "top", 
        legend.margin = margin(0.5, 0.1, 0.1, 0.1),
        legend.box.margin = margin(0.2,-4,-7,-7), 
        plot.margin = margin(3, 4, 0, 4), 
        legend.text = element_text(size = 8), 
        legend.key.width = unit(0.4, "cm"), 
        legend.key.height = unit(0.4, "cm"), 
        legend.title = element_text(size = 9), 
        axis.text.x = element_text(size = 6.2)) 
  

plot.sobol

plot.second <- plot(ind, order = order) +
  theme(axis.text.x = element_text(size = 6.5))

# Export sensitivity indices -----------------------------

fwrite(ind$results, "ind.csv")


## ----merge_plots, dependson=c("sa", "plot_uncertainty"), fig.width=4.5, fig.height=4, dev = "pdf"----

# ADD ORIGINAL BREZNAU ET AL PLOT ###############################################

original.plot<- cowplot::ggdraw() + cowplot::draw_image("pnas.2203150119fig01.png", scale = 0.9)

original.plot

p2 <- plot_grid(plot.uncertainty, plot.sobol, ncol = 1, labels = c("b", "c"))

p2



## ----all_together, dependson="merge_plots", fig.width=3.7, fig.height=6.5, dev = "pdf", fig.cap="Global sensitivity analysis. a) Distribution of estimated Average Marginal Effects (AME) in @breznau2022. Reproduced with permission. b) Distribution of AME after a global sensitivity analysis. c) Sobol' first ($S_i$) and total-order ($T_i$) effects. d) Second-order ($S_{ij}$) effects. All error bars show the 95\\% confidence intervals after bootstrapping the Sobol' indices using the normal method. The red, horizontal dashed line is the approximation error threshold and is at 0.05. We only show pairwise interactions whose ontribution to the output variance is higher than the approximation error."----

# MERGE ALL PLOTS ###############################################################

plot_grid(original.plot, p2, plot.second, ncol = 1, labels = c("a", "", "d"),
          rel_heights = c(0.37, 0.63))



## ----session_info-----------------------------------------------------------------------------------

# SESSION INFORMATION ##########################################################

sessionInfo()

## Return the machine CPU
cat("Machine:     "); print(get_cpu()$model_name)

## Return number of true cores
cat("Num cores:   "); print(detectCores(logical = FALSE))

## Return number of threads
cat("Num threads: "); print(detectCores(logical = FALSE))

