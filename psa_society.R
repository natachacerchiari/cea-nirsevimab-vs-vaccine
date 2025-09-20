# Análise de Custo-Efetividade da Vacinação contra VSR
# Autora: Natacha Cerchiari

# --- 0. SETUP ---

# Carrega as bibliotecas necessárias para a análise.
library(dplyr)
library(tibble)
library(ggplot2)



calculate_icer_components <- function(ne_malrti, ne_hospitalization, nirsevimab_coverage, inpatient_rate_0_3m, 
                                inpatient_rate_3_6m, inpatient_rate_6_12m, outpatient_rate_0_3m,
                                outpatient_rate_3_6m, outpatient_rate_6_12m, inpatient_cost_0_3m,
                                inpatient_cost_3_6m, inpatient_cost_6_12m, outpatient_cost,
                                outpatient_cost_ec, daly_moderate, daly_severe, mother_mean_daily_salary) {

  # Função para calcular YLLs (Years of Life Lost) descontados - expectativa de vida: 76.4 anos
  calculate_discounted_yll <- function(discount_rate = 0.05, years = 76, final_year_factor = 0.4) {
    time_periods <- 0:(years - 1)
    discounts <- (1 / (1 + discount_rate))^time_periods
    sum_discounts <- sum(discounts)
    final_discount <- (1 / (1 + discount_rate))^years
    total_discounted_value <- sum_discounts + final_discount * final_year_factor
    return(total_discounted_value)
  }

  # Calcula os custos e os DALYs para um subgrupo populacional,
  # considerando a efetividade de uma intervenção na redução de casos.
  calculate_outcomes <- function(population, params, effectiveness_hosp = 0, effectiveness_lrti = 0) {
    hosp_cases <- population * params$hosp_rate * (1 - effectiveness_hosp)
    outpatient_cases <- population * params$outpatient_rate * (1 - effectiveness_lrti)
    
    deaths <- hosp_cases * params$lethality
    hosp_cured <- hosp_cases - deaths
    
    inpatient_cost_total <- hosp_cases * (params$inpatient_cost + pcr_cost_dollar + outpatient_cost_ec + params$inpatient_transport_cost + params$inpatient_salary_loss)
    outpatient_cost_total <- outpatient_cases * (outpatient_cost + params$outpatient_transport_cost + params$outpatient_salary_loss)
    total_cost <- inpatient_cost_total + outpatient_cost_total
    
    daly_hosp_cured <- hosp_cured * daly_severe * (duration_illness_hosp / days_in_year)
    daly_outpatient <- outpatient_cases * daly_moderate * (duration_illness_outpatient / days_in_year)
    daly_death_morbidity <- deaths * daly_severe * (duration_illness_hosp / days_in_year)
    daly_death_yll <- deaths * discounted_yll
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
    
    # Soma os custos da doença
    total_cost <- sum(sapply(results_by_age$treated_outcomes, `[[`, "cost")) +
      sum(sapply(results_by_age$untreated_outcomes, `[[`, "cost"))
    
    # Calcula o custo da própria intervenção
    total_intervention_cost <- cohort * coverage * intervention_cost
    
    # Custo total para o cenário
    total_scenario_cost <- total_cost + total_intervention_cost
    
    # Soma os DALYs
    total_scenario_dalys <- sum(sapply(results_by_age$treated_outcomes, `[[`, "dalys")) +
      sum(sapply(results_by_age$untreated_outcomes, `[[`, "dalys"))
    
    return(list(cost = total_scenario_cost, dalys = total_scenario_dalys))
  }

  # Parâmetros Globais
  cohort <- 2536281
  daly_moderate <- 0.051
  daly_severe <- 0.133
  duration_illness_hosp <- 10
  duration_illness_outpatient <- 5
  days_in_year <- 365.25
  
  # Parâmetros de Custo
  outpatient_cost <- outpatient_cost
  outpatient_cost_ec <- outpatient_cost_ec
  pcr_hcv <- 2.66 * 96.00
  gal_adj <- 56254 / 45357
  pcr_cost <- pcr_hcv * gal_adj
  pcr_cost_dollar <- pcr_cost / 4.99
  
  # Parâmetros específicos Nirsevimab (Intervenção)
  nirsevimab_coverage <- nirsevimab_coverage
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
  
  # Parâmetros de Custo Direto Não Médico 
  trips_per_consultation <- 2
  transport_cost_per_trip <- 0.9299
  avg_consultations_per_patient <- 1.5
  
  
  
  # Parâmetros de Custo Indireto
  mother_mean_daily_salary <- mother_mean_daily_salary
  
  # Parâmetros por Faixa Etária
  age_group_params <- tribble(
    ~age_group, ~prop, ~hosp_rate, ~outpatient_rate, ~lethality, ~ve_hospitalization, ~ve_malrti, ~inpatient_cost, ~ne_hospitalization, ~ne_malrti,
    "0-3-months-old", 0.260518452, inpatient_rate_0_3m, outpatient_rate_0_3m, 0.0088, 0.697, 0.576, inpatient_cost_0_3m, ne_hospitalization, ne_malrti,
    "3-6-months-old", 0.26290344, inpatient_rate_3_6m, outpatient_rate_3_6m, 0.0074, 0.4276, 0.4105, inpatient_cost_3_6m, ne_hospitalization, ne_malrti,
    "6-12-months-old", 0.476578108, inpatient_rate_6_12m, outpatient_rate_6_12m, 0.0040, 0, 0, inpatient_cost_6_12m, ne_hospitalization, ne_malrti
  )%>%
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
  
  discounted_yll <- calculate_discounted_yll()

  # 3.1. Cenário Nirsevimab (Intervenção)
  nirsevimab_scenario_results <- run_scenario(
    coverage = nirsevimab_coverage,
    intervention_cost = nirsevimab_cost,
    effectiveness_hosp_col = "ne_hospitalization",
    effectiveness_lrti_col = "ne_malrti"
  )
  
  # 3.2. Cenário Vacina (Comparador)
  vaccine_scenario_results <- run_scenario(
    coverage = vaccine_coverage,
    intervention_cost = vaccine_cost,
    effectiveness_hosp_col = "ve_hospitalization",
    effectiveness_lrti_col = "ve_malrti"
  )
  
  incremental_cost <- nirsevimab_scenario_results$cost - vaccine_scenario_results$cost
  incremental_dalys_averted <- vaccine_scenario_results$dalys - nirsevimab_scenario_results$dalys

  return(list(incremental_cost=incremental_cost,
              incremental_dalys_averted=incremental_dalys_averted
        ))
}


set.seed(42)
n_iteracoes <- 1000
resultados <- data.frame(
  x = numeric(n_iteracoes),
  y = numeric(n_iteracoes)
)

# Loop através dos valores
for (i in 1:n_iteracoes) {
  ne_malrti <- rbeta(1, shape1=25.486, shape2=11.794)
  ne_hospitalization <- rnorm(1, mean=0.7641, sd= 1.824)
  nirsevimab_coverage <- rbeta(1, shape1=3.939, shape2=0.525)
  inpatient_rate_0_3m <- rlnorm(1, meanlog=-3.633932, sdlog=0.3695873)
  inpatient_rate_3_6m <- rlnorm(1, meanlog=-3.881946, sdlog=0.284551)
  inpatient_rate_6_12m <- rlnorm(1, meanlog=-4.492599, sdlog=0.2042144)
  outpatient_rate_0_3m <- rlnorm(1, meanlog=-2.353393, sdlog=0.4034967)
  outpatient_rate_3_6m <- rlnorm(1, meanlog=-2.719342, sdlog=0.6914414)
  outpatient_rate_6_12m <- rlnorm(1, meanlog=-2.626787, sdlog=0.4159427)
  inpatient_cost_0_3m <- rlnorm(1, meanlog=6.339864, sdlog=0.1303139)
  inpatient_cost_3_6m <- rlnorm(1, meanlog=6.022195, sdlog=0.1303119)
  inpatient_cost_6_12m <- rlnorm(1, meanlog=5.732261, sdlog=0.1303193)
  outpatient_cost <- rlnorm(1, meanlog=2.566759, sdlog=0.1302139)
  outpatient_cost_ec <- rlnorm(1, meanlog=2.818745, sdlog=0.1302757)
  daly_moderate <- rbeta(1, shape1=21.451, shape2=399.158)
  daly_severe <- rbeta(1, shape1=22.221, shape2=144.479)
  mother_mean_daily_salary <- rnorm(1, mean= 5.8498, sd=0.8966)
  
  
  icer_components <- calculate_icer_components(ne_malrti, ne_hospitalization, nirsevimab_coverage, inpatient_rate_0_3m, 
                                     inpatient_rate_3_6m, inpatient_rate_6_12m, outpatient_rate_0_3m,
                                     outpatient_rate_3_6m, outpatient_rate_6_12m, inpatient_cost_0_3m,
                                     inpatient_cost_3_6m, inpatient_cost_6_12m, outpatient_cost,
                                     outpatient_cost_ec, daly_moderate, daly_severe, mother_mean_daily_salary)

  incremental_cost <- icer_components$incremental_cost
  incremental_dalys_averted <- icer_components$incremental_dalys_averted
  
  # cat(sprintf("Iteração %d: incremental_cost = %.2f, incremental_dalys_averted = %.2f, ICER = %.2f\n",
  #            i, incremental_cost, incremental_dalys_averted, incremental_cost/incremental_dalys_averted))
 
  resultados$x[i] <- incremental_dalys_averted
  resultados$y[i] <- incremental_cost
}

# 1. Criar o gráfico e guardar em uma variável
meu_grafico <- ggplot(resultados, aes(x = x, y = y)) +
  geom_point(color = "darkblue", alpha = 0.4) +
  labs(x = "Incremental DALYs Averted", y = "Incremental Cost") +
  geom_abline(
    intercept = 0,
    slope = 8016.03,
    color = "darkgray",
    linewidth = 1
  ) +
  annotate("text", 
           x = max(resultados$x) * 0.99,
           y = (8016.03 * max(resultados$x) * 0.65) - (max(resultados$y) * 0.225),
           label = paste("US$", 8016.03,"/DALY"),
           hjust = 1.1, vjust = -0.5,  # Ajuste fino da posição
           color = "darkgray",
           size = 5,
           fontface = "bold") +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1)  # Números inteiros
  ) +
  theme(panel.background = element_rect(fill = "white"))

# 2. "Imprimir" o gráfico para visualizá-lo
print(meu_grafico)