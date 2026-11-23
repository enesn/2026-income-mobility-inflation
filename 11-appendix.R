## ==================================================================================================#
# Relative Income Mobility around an Inflationary Shock
# May 2025
# EI
## ==================================================================================================#

# ==================================================================================================#
# Table A1, Mobility in table
# ==================================================================================================#


step_header("11 | Appendix: Tables A1-A7")

step_note("Table A1: the Fig 2 mobility shares as a table")

# The shares themselves are the ones 06-class-mobility.R plots as Fig 2; this only reshapes them.
mobility_of_class_shares %>%
  mutate(mobility = str_to_title(mobility)) %>%
  select(type, income_year, mobility, p) %>%
  mutate(p = 100 * p) %>%
  pivot_wider(
    names_from = mobility,
    values_from = p
  ) %>%
  relocate(Upward, Downward, Immobile, .after = income_year) %>%
  arrange(type, income_year) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    digits = 1,
    caption = "Income mobility shares by class and year"
  ) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::collapse_rows(columns = 1, latex_hline = "major") %>%
  kableExtra::add_header_above(
    c(" " = 1, "Year" = 1, "Mobility states" = 3)
  ) %>%
  save_tex("outputs-included/tableA1-mobility-shares.tex")


# ==================================================================================================#
# Table A2, Summary statistics
# ==================================================================================================#
# --------------  
# Panel A: All
# --------------
  step_note("Table A2, panel A: rank changes and class shares over the whole sample")

  all_rank_changes <- class_mobility_ext %>%
    filter(income_year > 2011) %>%
    mutate(delta_rank = deciles2 - decile_prev1) %>% 
    filter(!is.na(delta_rank)) %>% 
    left_join(
      read_xlsx("other-input-data/evds_inflation_fxrate.xlsx") %>% 
        mutate(
          Date = as.Date(paste0(Date, "-01")),
          inflation = as.numeric(inflation),
          fxchange = as.numeric(fxchange)
        ) %>%   
        filter(year(Date) < 2024) %>% mutate(income_year = year(Date)) %>% group_by(income_year) %>% summarise(annual_inflation = mean(inflation))
    ) %>%
    distinct(HKIMLIK, income_year,  .keep_all = T) %>%
    mutate(
      pensioner = as.numeric(type == "Pensioner"),
      laborer = as.numeric(type == "Laborer"),
      employer = as.numeric(type == "Employer"),
      self_employer = as.numeric(type == "Self-employer"),
      transfer_dependent = as.numeric(type == "Transfer-Dependent"),
      financier = as.numeric(type == "Financier"),
      rentier = as.numeric(type == "Rentier")
    ) %>%
    select(
      delta_rank,
      pensioner,
      laborer,
      employer,
      self_employer,
      transfer_dependent,
      financier,
      rentier
    ) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.data.frame()
  
  #Latex table panel A:
  stargazer::stargazer(
   as.data.frame(all_rank_changes),
    summary = TRUE,
    type = "latex",
    out = "outputs-included/tableA2-panelA-summary-stats.tex"
  )
# --------------
# Panel B: Labor
# --------------
  step_note("Table A2, panel B: the labor market variables behind Table 3")

   lmarket_descriptive <- lmarket_panel %>%
    filter(income_year > 2011) %>%
    mutate(delta_rank = deciles2 - decile_prev1) %>%
    mutate(delta_aroundmw_mean = around_minimumwage_wmean - around_minimumwage_wmean_prev1) %>% 
    mutate(delta_jtjt_wmean = jtjt_wmean - jtjt_wmean_prev1) %>% 
    mutate(delta_informality_wmean = informality_wmean - informality_wmean_prev1) %>%
    mutate(delta_worked_months_wmean = worked_months_wmean - worked_months_wmean_prev1) %>% 
    mutate(delta_working_hour_wmean = working_hour_wmean - working_hour_wmean_prev1) %>%
    filter(!is.na(delta_jtjt_wmean)) %>%
    select(HKIMLIK, 
      delta_rank,
      delta_aroundmw_mean,
      delta_jtjt_wmean, 
      delta_informality_wmean, 
      delta_worked_months_wmean,
      delta_working_hour_wmean,
      informality_wmean_prev1,
      around_minimumwage_wmean_prev1
    ) 
  stargazer::stargazer(
    as.data.frame(lmarket_descriptive),
    summary = TRUE,
    out = "outputs-included/tableA2-panelB-labor-summary-stats.tex"
  )

# ==================================================================================================#
# Tables A3-A4, Full coefficients for the model presented in Fig 4
# ==================================================================================================#

step_note("Tables A3-A4: the full coefficient sets behind Fig 4")

# HC1 standard errors and p-values, passed explicitly so stargazer cannot pair them with the wrong
# coefficient (see hc1_se() in _setup.R); star cutoffs as in Tables 1-3, A5 and A6.
fig4_models <- list(m2, m2upper, m2bottom)

stargazer::stargazer(
  fig4_models,
  se           = lapply(fig4_models, hc1_se),
  p            = lapply(fig4_models, hc1_p),
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char    = c("+", "*", "**", "***"),
  notes        = "HC1 standard errors in parentheses. + p$<$0.1; * p$<$0.05; ** p$<$0.01; *** p$<$0.001",
  notes.append = FALSE,
  out = "outputs-included/tableA3A4-fig4-full-coefficients.tex"
)

# ==================================================================================================#
# Tables A5-A6, Full coefficients for Tables 1 and 3
# ==================================================================================================#
# Table 1 in 10-baseline-models.R folds the forty origin-decile dummies into a single "X" at the foot
# and leaves out the unadjusted cell-mean regression; Table 3 comes out of stargazer with its raw
# variable names down the side. The two tables below are those models with nothing suppressed and with
# readable row labels, so a reader can check any coefficient the text tables only summarise. Table 2
# has no appendix counterpart: it already prints every coefficient it estimates.
#
# The model objects, the row-label helper tidy_table1_name and table1_gof all come from
# 10-baseline-models.R, which runs immediately before this script in the same workspace.

# modelsummary writes a stars note of its own under every table; these tables carry the note in their
# own minipage instead, as Tables 1 and 2 do.
stars_note_option <- getOption("modelsummary_stars_note")
options(modelsummary_stars_note = FALSE)

# --------------------------------------------------------------------------------------------------
# Table A5: Table 1 with the origin-decile dummies shown
# --------------------------------------------------------------------------------------------------

step_note("Table A5: the full coefficient set behind Table 1, decile dummies included")

# Table 1's own renamer leaves the decile dummies untouched, because it never has to print them.
tidy_tableA5_name <- function(x) {
  x %>%
    tidy_table1_name() %>%
    str_replace_all(fixed("as.factor(decile_prev1)"), "Origin decile ")
}

tableA5_body <-
  modelsummary::modelsummary(
    list(
      "(3)"  = pi_x_class,
      "(4)"  = did_post2021,
      "(5)"  = hhlevel_fe,
      # The unadjusted collapse is estimated in 10 but not printed there; it belongs next to the
      # adjusted one, since the difference between the two columns is exactly what the origin-decile
      # residualisation does to the cell means.
      "(6a)" = did_post2021_cellmeans,
      "(6)"  = did_post2021_cellmeans_adj
    ),
    # The same standard errors as Table 1, plus HC1 for the unadjusted collapse that only this table
    # prints.
    vcov        = unname(c(table1_vcov[c("(3)", "(4)", "(5)")],
                           list("(6a)" = hc1(did_post2021_cellmeans)),
                           table1_vcov["(6)"])),
    stars       = TRUE,
    coef_rename = tidy_tableA5_name,
    gof_map     = table1_gof,
    output      = "tinytable"
  ) %>%
  tinytable::theme_latex(
    environment_table = FALSE,
    resize_width      = 1,
    resize_direction  = "down"
  ) %>%
  tinytable::save_tt("latex")

tableA5_note <- paste(
  c(
    "\\textit{Notes:} The models of Table \\ref{tab:class-inflation-interactions}.",
    "Heteroskedasticity-robust (HC1) standard errors in parentheses; column (5) clustered by household.",
    "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001."
  ),
  collapse = "\n"
)

paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Relative Income Mobility by Class: Full Coefficients Behind Table\n",
  "         \\ref{tab:class-inflation-interactions}}\n",
  "\\label{tab:appendix-class-inflation-full}\n",
  tableA5_body,
  "\n\n\\begin{minipage}{\\linewidth}\n",
  "\\footnotesize\n",
  "\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0.5em}\n",
  tableA5_note,
  "\n\\end{minipage}\n",
  "\\end{table}\n"
) %>%
  save_tex("outputs-included/tableA5-table1-full-coefficients.tex")

# --------------------------------------------------------------------------------------------------
# Table A6: Table 3 with readable labels
# --------------------------------------------------------------------------------------------------

step_note("Table A6: the Table 3 three-way interaction model with readable row labels")

# The labor market model carries three-way interactions of eight variables, so its terms come out of
# lm() as colon-joined strings of column names. Each piece is translated on its own and the pieces are
# joined back with a times sign, which keeps a three-way term readable and does not depend on the order
# in which lm() happened to write the variables.
tableA6_labels <- c(
  "(Intercept)"                    = "Intercept",
  "delta_worked_months_wmean"      = "$\\Delta$ Months worked",
  "delta_jtjt_wmean"               = "$\\Delta$ Job-to-job move",
  "delta_informality_wmean"        = "$\\Delta$ Informality",
  "delta_aroundmw_mean"            = "$\\Delta$ Around minimum wage",
  "delta_working_hour_wmean"       = "$\\Delta$ Weekly hours",
  "hh_size_wmean"                  = "Household size",
  "around_minimumwage_wmean_prev1" = "Around minimum wage ($t-1$)",
  "informality_wmean_prev1"        = "Informality ($t-1$)",
  "post_2021"                      = "Post-2021",
  "lower_decile"                   = "Bottom 7 deciles"
)

tidy_tableA6_name <- function(x) {
  vapply(
    str_split(x, fixed(":")),
    function(parts) {
      renamed <- unname(tableA6_labels[parts])
      renamed <- ifelse(is.na(renamed), parts, renamed)
      renamed <- str_replace(renamed, fixed("as.factor(decile_prev1)"), "Origin decile ")
      paste(renamed, collapse = " $\\times$ ")
    },
    character(1)
  )
}

tableA6_note <- paste(
  c(
    "\\textit{Notes:} The model of Table \\ref{tab:labor-market-adjustment}, printed in full.",
    "Heteroskedasticity-robust (HC1) standard errors in parentheses.",
    "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001."
  ),
  collapse = "\n"
)

# Table 3's stargazer output reports the observations and three R2-type statistics; the same four are
# named here so the appendix column can be compared with it line for line.
tableA6_gof <- tribble(
  ~raw,            ~clean,           ~fmt,
  "nobs",          "Observations",   0,
  "r.squared",     "R$^2$",          3,
  "adj.r.squared", "Adjusted R$^2$", 3,
  "rmse",          "RMSE",           3
)

# The model has forty-odd coefficients, which is more than a single column of estimates can hold on one
# page. Rather than let the table run over a page break — a multipage table cannot sit inside the table
# float the rest of the paper's tables use, and loses its opening rows there — the coefficients are cut
# in half and the two halves printed side by side, so the table stays one ordinary float.
tableA6_cells <-
  modelsummary::modelsummary(
    list("$\\Delta$ Decile" = lm_model_3wayinteraction),
    # As in Table 3. modelsummary computes HC1 itself here and matches it to the coefficients by name,
    # so the aliased lower_decile term is simply left out.
    vcov        = "HC1",
    stars       = TRUE,
    coef_rename = tidy_tableA6_name,
    gof_map     = tableA6_gof,
    output      = "data.frame"
  )

# Two lines per coefficient: the estimate against its label, the standard error under it against none.
tableA6_estimates <-
  tableA6_cells %>%
  filter(part == "estimates") %>%
  transmute(
    label = ifelse(statistic == "estimate", term, ""),
    value = .data[["$\\Delta$ Decile"]]
  )

# The cut is made at an even number of lines, so an estimate is never left in one column with its
# standard error in the other.
tableA6_cut  <- 2 * ceiling(nrow(tableA6_estimates) / 4)
tableA6_left  <- tableA6_estimates[seq_len(tableA6_cut), ]
tableA6_right <- tableA6_estimates[-seq_len(tableA6_cut), ]

# A blank line is appended to the shorter half so the two columns can be set next to each other.
tableA6_pad <- nrow(tableA6_left) - nrow(tableA6_right)
if (tableA6_pad > 0) {
  tableA6_right <- bind_rows(tableA6_right, tibble(label = rep("", tableA6_pad), value = ""))
}

# The fit statistics belong to the model rather than to either half, so they sit under the left column
# with the right one left blank.
tableA6_fit <-
  tableA6_cells %>%
  filter(part == "gof") %>%
  transmute(label = term, value = .data[["$\\Delta$ Decile"]], label2 = "", value2 = "")

tableA6_panel <-
  bind_cols(tableA6_left, tableA6_right, .name_repair = "unique_quiet") %>%
  setNames(c("label", "value", "label2", "value2")) %>%
  bind_rows(tableA6_fit)

# Duplicate column names are what the table wants to print, so they are set after every join is done.
names(tableA6_panel) <- c(" ", "$\\Delta$ Decile", " ", "$\\Delta$ Decile")

tableA6_body <-
  tinytable::tt(tableA6_panel, align = "lclc") %>%
  # The rule separates the coefficients from the fit statistics that follow them.
  tinytable::style_tt(i = nrow(tableA6_panel) - nrow(tableA6_fit) + 1, line = "t", line_width = 0.05) %>%
  tinytable::theme_latex(
    environment_table = FALSE,
    resize_width      = 1,
    resize_direction  = "down"
  ) %>%
  tinytable::save_tt("latex")

paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Labor market adjustment of laborer households: full coefficients behind\n",
  "         Table \\ref{tab:labor-market-adjustment}}\n",
  "\\label{tab:appendix-labor-market-full}\n",
  tableA6_body,
  "\n\n\\begin{minipage}{\\linewidth}\n",
  "\\footnotesize\n",
  "\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0.5em}\n",
  tableA6_note,
  "\n\\end{minipage}\n",
  "\\end{table}\n"
) %>%
  save_tex("outputs-included/tableA6-table3-full-coefficients.tex")

# --------------------------------------------------------------------------------------------------
# Table A7: the Table 1-3 models with HC1 and with household-clustered standard errors
# --------------------------------------------------------------------------------------------------

step_note("Table A7: Tables 1-3 re-estimated with household-clustered standard errors, beside HC1")

# The panel follows each household for up to four years, so a household contributes several rows to
# the pooled models of Table 1 (columns 3-4), Table 2 and Table 3. The MacKinnon, Nielsen & Webb (2023)
# test finds that this dependence does not call for clustering in those models (see the inference note
# in _setup.R). This table lets a reader check the consequence directly: each model is printed twice,
# once with the HC1 standard errors of the main text and once clustered by household, so the question
# is simply whether a coefficient keeps its stars. Column (5) of Table 1 is left out because it is
# already clustered by household, and column (6) because its unit is a class-year cell.
#
# vcovCL() recovers each model's household identifiers from the data the model was fitted on, so the
# clusters line up with the estimation sample row for row; type = "HC1" applies the clustered analogue
# of the HC1 small-sample adjustment, G/(G - 1) x (n - 1)/(n - k).
household_cluster <- function(model) sandwich::vcovCL(model, cluster = ~HKIMLIK, type = "HC1")
household_count   <- function(model) {
  dplyr::n_distinct(expand.model.frame(model, ~HKIMLIK, na.expand = FALSE)$HKIMLIK)
}

# One panel: every model in `models` twice, HC1 then clustered, showing only the coefficients named in
# `coef_map` (raw name = row label). Built through a data frame, as Table A6 is, so the two columns of a
# model can share the header "HC1" / "Clustered" under the model's own name.
tableA7_panel <- function(models, coef_map) {
  doubled <- unlist(lapply(models, function(m) list(m, m)), recursive = FALSE)
  names(doubled) <- paste0("m", seq_along(doubled))
  vcovs <- unlist(lapply(models, function(m) list(hc1(m), household_cluster(m))), recursive = FALSE)

  cells <- modelsummary::modelsummary(
    doubled,
    vcov     = unname(vcovs),
    stars    = TRUE,
    coef_map = coef_map,
    gof_map  = tribble(~raw, ~clean, ~fmt, "nobs", "Observations", 0),
    output   = "data.frame"
  )

  estimates <- cells %>%
    filter(part == "estimates") %>%
    mutate(term = ifelse(statistic == "estimate", term, "")) %>%
    select(term, all_of(names(doubled)))

  fit <- cells %>% filter(part == "gof") %>% select(term, all_of(names(doubled)))
  households <- c(
    term = "Households (clusters)",
    setNames(rep(as.character(vapply(models, household_count, integer(1))), each = 2),
             names(doubled))
  )

  panel <- bind_rows(estimates, fit, as_tibble_row(households))
  names(panel) <- c(" ", rep(c("HC1", "Clustered"), length(models)))

  tinytable::tt(panel, align = paste0("l", strrep("c", 2 * length(models)))) %>%
    tinytable::group_tt(j = setNames(lapply(seq_along(models), function(i) 2 * i + 0:1),
                                     names(models))) %>%
    # The rule separates the coefficients from the sample sizes under them.
    tinytable::style_tt(i = nrow(estimates) + 1, line = "t", line_width = 0.05) %>%
    tinytable::theme_latex(
      environment_table = FALSE,
      resize_width      = 1,
      resize_direction  = "down"
    ) %>%
    tinytable::save_tt("latex")
}

# The coefficients each table is read for: the class interactions in Tables 1 and 2, the three-way
# interactions in Table 3. The labels are the ones the main-text tables print.
label_terms <- function(terms, renamer) setNames(renamer(terms), terms)

table1_terms <- c(
  grep(":log\\(annual_inflation\\)$", names(coef(pi_x_class)), value = TRUE),
  grep(":as\\.factor\\(post_2021\\)1$", names(coef(did_post2021)), value = TRUE)
)
table2_terms <- grep(":as\\.factor\\(post_2021\\)1$", names(coef(mix_change_post)), value = TRUE)
table3_terms <- names(coef(lm_model_3wayinteraction))[
  str_count(names(coef(lm_model_3wayinteraction)), ":") == 2
]

tableA7_bodies <- list(
  "Panel A: Table \\ref{tab:class-inflation-interactions}, columns (3) and (4)" =
    tableA7_panel(list("(3)" = pi_x_class, "(4)" = did_post2021),
                  label_terms(table1_terms, tidy_table1_name)),
  "Panel B: Table \\ref{tab:income-mix-response}" =
    tableA7_panel(list("Any change"        = mix_change_post,
                       "To no dominant"    = mix_change_to_no_dominant,
                       "To earned income"  = mix_change_to_earned,
                       "To other unearned" = mix_change_to_unearned),
                  label_terms(table2_terms, tidy_coef_name)),
  "Panel C: Table \\ref{tab:labor-market-adjustment}, three-way interactions" =
    tableA7_panel(list("$\\Delta$ Decile" = lm_model_3wayinteraction),
                  label_terms(table3_terms, tidy_tableA6_name))
)

tableA7_note <- paste(
  c(
    "\\textit{Notes:} Each model is shown twice: with the heteroskedasticity-robust (HC1) standard errors",
    "of the main text, and with standard errors clustered by household. Only the coefficients the main",
    "text interprets are printed; the other regressors are as in the original tables. Column (5) of",
    "Table \\ref{tab:class-inflation-interactions} is already clustered by household and column (6) is",
    "estimated on class-year cells, so neither is repeated here.",
    "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001."
  ),
  collapse = "\n"
)

paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Tables \\ref{tab:class-inflation-interactions}--\\ref{tab:labor-market-adjustment} with\n",
  "         HC1 and household-clustered standard errors}\n",
  "\\label{tab:appendix-cluster-comparison}\n",
  paste0("\\textit{", names(tableA7_bodies), "}\n\n", unlist(tableA7_bodies),
         collapse = "\n\n\\medskip\n"),
  "\n\n\\begin{minipage}{\\linewidth}\n",
  "\\footnotesize\n",
  "\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0.5em}\n",
  tableA7_note,
  "\n\\end{minipage}\n",
  "\\end{table}\n"
) %>%
  save_tex("outputs-included/tableA7-hc1-vs-clustered.tex")

options(modelsummary_stars_note = stars_note_option)
