# Análise de Custo-Efetividade da Vacinação contra VSR
# Autora: Natacha Cerchiari

# --- 0. SETUP ---

# Carrega as bibliotecas necessárias para a análise.
library(dplyr)
library(tibble)

# --- 1. PARÂMETROS DE ENTRADA ---

# Parâmetros Globais
cohort <- 2536281
daly_moderate <- 0.051
daly_severe <- 0.133
duration_illness_hosp <- 10
duration_illness_outpatient <- 5
days_in_year <- 365.25

# Parâmetros de Custo Direto
outpatient_cost <- 13.45
outpatient_cost_ec <- 17.30
pcr_hcv <- 2.66 * 96.00
gal_adj <- 56254 / 45357
pcr_cost <- pcr_hcv * gal_adj
pcr_cost_dollar <- pcr_cost / 4.99

# Parâmetros de Custo Indireto
trips_per_consultation <- 2
transport_cost_per_trip <- 0.9299
avg_consultations_per_patient <- 1.5
mother_mean_daily_salary <- 23.9238

# Parâmetros específicos Nirsevimab (Intervenção)
nirsevimab_coverage <- 0.8826
nirsevimab_dose_price <- 255.15
nirsevimab_administration_cost <- 2.59
nirsevimab_wastage_rate <- 0.05
nirsevimab_cost <- nirsevimab_dose_price * (1 + nirsevimab_wastage_rate) + nirsevimab_administration_cost

# Parâmetros específicos Vacina (Comparador)
vaccine_coverage <- 0.5
vaccine_dose_price <- 39.08
vaccine_administration_cost <- 2.59
vaccine_wastage_rate <- 0.05
vaccine_cost <- vaccine_dose_price * (1 + vaccine_wastage_rate) + vaccine_administration_cost

# NOMES DE COLUNAS DE EFETIVIDADE:
# vaccine_eff_hosp: efetividade da vacina em evitar hospitalização
# vaccine_eff_lrti: efetividade da vacina em evitar doença respiratória aguda baixa (LRTI)
# nirsevimab_eff_hosp: efetividade do nirsevimab em evitar hospitalização
# nirsevimab_eff_lrti: efetividade do nirsevimab em evitar LRTI

# Parâmetros por Faixa Etária
age_group_params <- tribble(
  ~age_group, ~prop, ~hosp_rate, ~outpatient_rate, ~lethality, ~vaccine_eff_hosp, ~vaccine_eff_lrti, ~inpatient_cost, ~nirsevimab_eff_hosp, ~nirsevimab_eff_lrti,
  "0-3-months-old", 0.260518452, 0.02768, 0.09382, 0.0088, 0.697, 0.576, 585.2959519, 0.7641, 0.701,
  "3-6-months-old", 0.26290344, 0.02095, 0.07065, 0.0074, 0.4276, 0.4105, 426.0104609, 0.7641, 0.701,
  "6-12-months-old", 0.476578108, 0.01083, 0.07347, 0.0040, 0, 0, 318.7895391, 0, 0
) %>%
  mutate(
    # Proporção do período em que a mãe não está em licença maternidade
    proportion_no_maternity_leave = case_when(
      age_group == "0-3-months-old"  ~ 0,
      age_group == "3-6-months-old"  ~ 2/3,
      age_group == "6-12-months-old" ~ 1,
      TRUE ~ 0
    ),
    # Número de comparecimentos da mãe em casos de internação
    mother_inpatient_visits = case_when(
      age_group == "0-3-months-old"  ~ 5.59,
      age_group == "3-6-months-old"  ~ 5.05,
      age_group == "6-12-months-old" ~ 4.49,
      TRUE ~ 0
    )
  ) %>%
  # Calcula os custos diretos não médicos por caso
  mutate(
    outpatient_transport_cost = avg_consultations_per_patient * trips_per_consultation * transport_cost_per_trip,
    inpatient_transport_cost = (mother_inpatient_visits + avg_consultations_per_patient) * trips_per_consultation * transport_cost_per_trip
  ) %>%
  
  # Calcula os custos indiretos por caso
  mutate(
    outpatient_salary_loss = duration_illness_outpatient * mother_mean_daily_salary * proportion_no_maternity_leave,
    inpatient_salary_loss = duration_illness_hosp * mother_mean_daily_salary * proportion_no_maternity_leave,
  )

# Função para calcular YLLs (Years of Life Lost) descontados - expectativa de vida: 76.4 anos
calculate_discounted_yll <- function(discount_rate = 0.05, years = 76, final_year_factor = 0.4) {
  time_periods <- 0:(years - 1)
  discounts <- (1 / (1 + discount_rate))^time_periods
  sum_discounts <- sum(discounts)
  final_discount <- (1 / (1 + discount_rate))^years
  total_discounted_value <- sum_discounts + final_discount * final_year_factor
  return(total_discounted_value)
}

discounted_yll <- calculate_discounted_yll()

# --- 2. FUNÇÕES AUXILIARES ---

# Calcula os custos totais (diretos e indiretos) e os DALYs para um subgrupo populacional,
# considerando a efetividade de uma intervenção na redução de casos.
calculate_outcomes <- function(population, params, effectiveness_hosp = 0, effectiveness_lrti = 0) {
  hosp_cases <- population * params$hosp_rate * (1 - effectiveness_hosp)
  outpatient_cases <- population * params$outpatient_rate * (1 - effectiveness_lrti)
  # cat(sprintf("Out_cases %s (%s): %.2f\n", intervencao, params$age_group, outpatient_cases))

  deaths <- hosp_cases * params$lethality
  hosp_cured <- hosp_cases - deaths
  # cat(sprintf("deaths %s (%s): %.2f\n", intervencao, params$age_group, deaths))

  inpatient_cost_total <- hosp_cases * (params$inpatient_cost + pcr_cost_dollar + outpatient_cost_ec + params$inpatient_salary_loss + params$inpatient_transport_cost)
  outpatient_cost_total <- outpatient_cases * (outpatient_cost + params$outpatient_salary_loss + params$outpatient_transport_cost)
  total_cost <- inpatient_cost_total + outpatient_cost_total

  daly_hosp_cured <- hosp_cured * daly_severe * (duration_illness_hosp / days_in_year)
  daly_outpatient <- outpatient_cases * daly_moderate * (duration_illness_outpatient / days_in_year)
  daly_death_morbidity <- deaths * daly_severe * (duration_illness_hosp / days_in_year)
  daly_death_yll <- deaths * discounted_yll
  # cat(sprintf("YLL: %.2f\n", daly_death_yll))
  daly_deaths <- daly_death_morbidity + daly_death_yll
  
  total_dalys <- daly_hosp_cured + daly_outpatient + daly_deaths

  return(list(cost = total_cost, dalys = total_dalys))
}

# Executa um cenário completo para uma intervenção. A função calcula os custos
# totais (custos da intervenção + custos médicos) e os DALYs totais para a
# coorte inteira, considerando os indivíduos tratados e não tratados.
run_scenario <- function(coverage, intervention_cost, effectiveness_hosp_col, effectiveness_lrti_col) {
  results_by_age <- age_group_params %>%
    rowwise() %>%
    mutate(
      age_group_population = cohort * prop,
      # Resultados para o grupo que recebe a intervenção
      treated_outcomes = list(calculate_outcomes(
        population = age_group_population * coverage,
        params = cur_data(),
        effectiveness_hosp = .data[[effectiveness_hosp_col]],
        effectiveness_lrti = .data[[effectiveness_lrti_col]]
      )),
      # Resultados para o grupo que NÃO recebe a intervenção
      untreated_outcomes = list(calculate_outcomes(
        population = age_group_population * (1 - coverage),
        params = cur_data(),
        effectiveness_hosp = 0,
        effectiveness_lrti = 0
      ))
    ) %>%
    ungroup()

  # Soma os custos totais da doença (diretos e indiretos)
  total_disease_cost <- sum(sapply(results_by_age$treated_outcomes, `[[`, "cost")) +
    sum(sapply(results_by_age$untreated_outcomes, `[[`, "cost"))

  # Calcula o custo da própria intervenção
  total_intervention_cost <- cohort * coverage * intervention_cost

  # Custo total para o cenário
  total_scenario_cost <- total_disease_cost + total_intervention_cost

  # Soma os DALYs
  total_scenario_dalys <- sum(sapply(results_by_age$treated_outcomes, `[[`, "dalys")) +
    sum(sapply(results_by_age$untreated_outcomes, `[[`, "dalys"))

  return(list(cost = total_scenario_cost, dalys = total_scenario_dalys))
}

# --- 3. ANÁLISE DE CENÁRIOS ---

# Aqui realizamos os cálculos para os dois cenários: Nirsevimab e Vacina.

# 3.1. Cenário Nirsevimab (Intervenção)
nirsevimab_scenario_results <- run_scenario(
  coverage = nirsevimab_coverage,
  intervention_cost = nirsevimab_cost,
  effectiveness_hosp_col = "nirsevimab_eff_hosp",
  effectiveness_lrti_col = "nirsevimab_eff_lrti"
)

# 3.2. Cenário Vacina (Comparador)
vaccine_scenario_results <- run_scenario(
  coverage = vaccine_coverage,
  intervention_cost = vaccine_cost,
  effectiveness_hosp_col = "vaccine_eff_hosp",
  effectiveness_lrti_col = "vaccine_eff_lrti"
)

# --- 4. RESULTADOS ---

# Finalmente, calculamos os resultados incrementais e a ICER.

incremental_cost <- nirsevimab_scenario_results$cost - vaccine_scenario_results$cost
incremental_dalys_averted <- vaccine_scenario_results$dalys - nirsevimab_scenario_results$dalys

icer <- if (incremental_dalys_averted > 0) {
  incremental_cost / incremental_dalys_averted
} else {
  NA
}

# Exibição de resultados:

cat("--- Resultados da Análise de Custo-Efetividade: Nirsevimab vs. Vacina ---\n\n")
cat(sprintf("Custo Total (Nirsevimab): $%.2f\n", nirsevimab_scenario_results$cost))
cat(sprintf("Custo Total (Vacina):     $%.2f\n", vaccine_scenario_results$cost))
cat(sprintf("Custo Incremental:        $%.2f\n\n", incremental_cost))

cat(sprintf("DALYs Totais (Nirsevimab): %.2f\n", nirsevimab_scenario_results$dalys))
cat(sprintf("DALYs Totais (Vacina):     %.2f\n", vaccine_scenario_results$dalys))
cat(sprintf("DALYs Evitados:            %.2f\n\n", incremental_dalys_averted))

if (!is.na(icer)) {
  cat(sprintf("ICER (Custo por DALY evitado): $%.2f\n", icer))
} else {
  cat("ICER: Não foi possível calcular (Nirsevimab não evitou mais DALYs que a Vacina).\n")
}
