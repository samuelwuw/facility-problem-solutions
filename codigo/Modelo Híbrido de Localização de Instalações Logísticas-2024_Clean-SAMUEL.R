#     Modelo Hibrido de Localizacao de Instalacoes Logisticas 
#         ----- FACILITY LOCATION PROBLEM (FLP) -----
#          WU, S. W. A.; VALLIM FILHO, A. R. A. - 2024
# 

# FASE 1: >>>>> GERACAO DE PROTOTIPOS (CANDIDATOS) <<<<<
# O modelo gera Prototipos por meio de uma rede SOM,
#   ===>>> PROTOTIPOS = NEURONIOS do SOM
# Os Prototipos serao Locais CANDIDATOS a Instalacoes Logisticas
#   ===>>> PROTOTIPOS = NEURONIOS do SOM = CANDIDATOS

# FASE 2: >>>>> RESOLUCAO do FLP <<<<<
# Os Prototipos serao Dados de Entrada de um Modelo MILP
# O modelo MILP vai encontrar uma Solucao Otima para o FLP
# Na Solucao Otima os melhores Locais Candidatos sao selecionados
# Solucao Otima contem:
#  ==>> Candidatos Descartados e Ativados
#  ==>> Alocacao dos Pontos de Demanda a cada Candidato Ativo
#  ==>> Valor da Funcao Objetivo

# >>>>> LIMPA MEMORIA <<<<<
rm(list = ls())
# >>>>> Define Raiz de Geracao de Nos. Aleatorios para Sorteios
set.seed(1234)

setwd("C:/Users/samue/Documents/Mestrados/defesa")

library(kohonen)
require(kohonen)
library(RSNNS)
somFunc <- kohonen::som

df <- read.csv2('codigo/databases/UCHOA-109.csv', header = TRUE, sep = ";")
# str(df)

# ----- Data Frame (DF) sem a coluna de ID -----
Data_Points <- df[2:4]
str(Data_Points)


# ---------------------------------------------------------------
#     ---------------- MODELO SOM --------------------
# ---------------------------------------------------------------

# ---------------------------------------------------------------
#   ---------- PREPARACAO: NORMALIZACAO dos DADOS -----------
#          ----- Normalizacao do DF "Data_Points" -----
# ---------------------------------------------------------------
data_train_matrix <- as.matrix(normalizeData(Data_Points, type = "norm")) 
# ---------------------------------------------------------------
# ---------------------------------------------------------------

# ---- Atribuicao de Rotulos as Colunas da Matriz Normalizada ----
# colnames(data_train_matrix) <- c("lat", "lng", "Demand")
colnames(data_train_matrix) <- c("X", "Y", "Demanda")

# ---------------- ARQUITETURA do SOM ---------------------
Dim_X <- 3
Dim_Y <- 4
#                 SOM Dim_X x Dim_Y, Hexagonal
# som_grid <- somgrid(xdim = 3, ydim = 4, topo="hexagonal") 
#                 SOM Dim_X x Dim_Y, Hexagonal
som_grid <- somgrid(xdim = Dim_X, ydim = Dim_Y, topo="hexagonal") 

# ---------------- PROCESSAMENTO do SOM ---------------------
som_model <- somFunc(data_train_matrix, 
                 grid=som_grid,  
                 rlen=500, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 radius = 5)

#     ------------ NORMALIZACAO dos Prototypes ------------
# Prototypes ==>> Posicao dos Neuronios (Dim_X x Dim_Y neuronios)
Prototypes_Norm <- as.data.frame(som_model$codes)
# View(Prototypes_Norm) # NORMALIZADOS

# ---------------------------------------------------------------
#     ------------ DESNORMALIZACAO dos Prototypes ------------
# ---------------------------------------------------------------
# Usa Funcao de Desnormalizacao ==>> "denormalizeData()"
Prototypes_DeNorm <- as.data.frame(denormalizeData(Prototypes_Norm, getNormParameters(data_train_matrix)))
colnames(Prototypes_DeNorm) <- c("X", "Y", "Demanda")
#View(Prototypes_DeNorm)
# ---------------------------------------------------------------
# ---------------------------------------------------------------


# ---------------------------------------------------------------
#     ------------ PLOTS of SOM Model ------------
# ---------------------------------------------------------------
plot(som_model, type="changes")

#quantidade de amostras mapeadas em cada node (centroide)
plot(som_model, type="count", main = "node counts")

plot(som_model, type="dist.neighbours", main = "SOM neighbour distances") 

plot(som_model, type="codes", main = "codes") 

# Mostra a Alocacao de Pontos aos Neuronios
som_model$unit.classif 

# ---------------------------------------------------------------
# ---------------------------------------------------------------

# -------------- INICIALIZACAO ------------------
m <- Dim_X * Dim_Y # No de Neuronios - usado em Prototype locations
n <- dim(Data_Points)[1] # No de Pontos
D <- 0
#         ----- MEDIAS DES_NORMALIZADAS -----
x_mean_Desnorm <- mean(Prototypes_DeNorm[,1]) # media x dos Prototypes  
y_mean_Desnorm <- mean(Prototypes_DeNorm[,2]) # media y dos Prototypes

#         ----- MEDIAS NORMALIZADAS -----
# x_mean <- mean(Prototypes_Norm[,1]) # media NORMALIZADA x dos Prototypes  
# y_mean <- mean(Prototypes_Norm[,2]) # media NORMALIZADA y dos Prototypes


# ------------------------------------------------------------
#      -------- INICIALIZACAO de VETORES --------
# ------------------------------------------------------------
# Vetor com Distâncias entre os Pontos de Demanda e os Prototipos
# e Prototypes (CANDIDATES)) para sua Media 
PointsDistanceVector <- c()

#Vetor com Distâncias entre Prototypes posic a Media dos Prototypes
PrototypeDistanceVector <- c() # NORMALIZADAS
PrototypeDistanceVector_DeNorm <- c() # DES_NORMALIZADAS

#Vector with Costs based in Distance
Prototype_costPerSquareMeter <- c()

PointsCostVector <- c()
PrototypeCostVector <- c()
# ------------------------------------------------------------
# ------------------------------------------------------------

# FUNCAO para Calcular DISTANCIAS (custo do transporte)
#  entre o PONTO de Demanda e o seu PROTOTIPO (CANDIDATO)
distanc <- function(Xc, Yc, Xw, Yw){
  distance <- sqrt((Xw-Xc)**2+(Yw-Yc)**2)
  return(distance)
}

# ----- DISTANCIA DES_NORMALIZADAS para MEDIA - com Coords Euclidianas -----
for(val in 1:m){
  D_DeNorm <- distanc(Prototypes_DeNorm$X[[val]], Prototypes_DeNorm$Y[[val]], 
               x_mean_Desnorm, y_mean_Desnorm)
  PrototypeDistanceVector_DeNorm[val] <- D_DeNorm 
}
# View(PrototypeDistanceVector)
# PrototypeDistanceVector_DeNorm  # ==>> DISTANCIAS DES_NORMALIZADAS
Average_Dist <- sum(PrototypeDistanceVector_DeNorm)/m
Average_Dist


# ----- DISTANCIAS NORMALIZADAS para MEDIA - com Coords Euclidianas -------
# for(val in 1:m){
#   D <- distanc(Prototypes_Norm$X[[val]], Prototypes_Norm$Y[[val]], 
#                x_mean, y_mean)
#   PrototypeDistanceVector[val] <- D 
# }
# View(PrototypeDistanceVector)
# PrototypeDistanceVector  # ==>> DISTANCIAS NORMALIZADAS

# ---------- QUARTIS de DISTANCIAS para as MEDIAS ----------
#          ----- QUARTIS DES_NORMALIZADOS -----
# quartile1 <- quantile(PrototypeDistanceVector_DeNorm, 0.25)
# quartile2 <- quantile(PrototypeDistanceVector_DeNorm, 0.5) 
# quartile3 <- quantile(PrototypeDistanceVector_DeNorm, 0.75) 
# 
# # -------- CUSTOS por Metro Quadrado --------
# # ----- CRIACAO de VETOR com Custos/m2 por PROTOTIPO -----
# for(val in 1:m){
#   if(PrototypeDistanceVector_DeNorm[val] <= quartile1){
#     Prototype_costPerSquareMeter[val] <- 2000 
#   } 
#   if(PrototypeDistanceVector_DeNorm[val] > quartile1 && PrototypeDistanceVector_DeNorm[val] <= quartile2){
#     Prototype_costPerSquareMeter[val] <- 1500
#   } 
#   if(PrototypeDistanceVector_DeNorm[val] > quartile2 && PrototypeDistanceVector_DeNorm[val] <= quartile3){
#     Prototype_costPerSquareMeter[val] <- 1000
#   } 
#   if(PrototypeDistanceVector_DeNorm[val] > quartile3 ){
#     Prototype_costPerSquareMeter[val] <- 500
#   } 
# } 
# Prototype_costPerSquareMeter
# df_Prototype_costPerSquareMeter <- as.data.frame(Prototype_costPerSquareMeter)
# View(df_Prototype_costPerSquareMeter)

# Data Frame "Point Locations": Coords, Neuronio e Demanda
#          ----- DADOS DESNORMALIZADOS -----

# >>>>> "PONTOS de DEMANDA" - Atributos: X, Y, Neuronio de Alocacao e Demanda
Points_Locations <- data.frame(
  id = 1:n,
  x = Data_Points[,1],
  y = Data_Points[,2],
  localiz = as.matrix(som_model$unit.classif), # Ptos vs Neuronios
  Demand = Data_Points[,3]
)
# View(Points_Locations) # X, Y, Neuronio, Demanda
Average_Demand <- sum(Data_Points[,3])/m
Average_Demand
#sum(Data_Points[,3])


    
    #  ----- Comparacao de Metadados -----
# str(Data_Points)
# str(Points_Locations) # X, Y, Neuronio, Demanda

# -------- Montagem de Demanda Total por PROTOTIPO --------
#           ----- DEMANDA DESNORMALIZADA -----
#   Soma DEMANDA (DESNORMALIZADA) de cada PROTOTIPO
Protot_Demand <- vector(length = m)
for(i in 1:m){
  for(j in 1:n){
    if(Points_Locations$localiz[j] == i){
      Protot_Demand[i] <- Protot_Demand[i] + Points_Locations$Demand[j]
      
    }
  }
}

# ===>>> Se Demanda = 0 ==>> Demanda = Infinito!!
# for(i in 1:m){
#   if(Protot_Demand[i] == 0){
#     Protot_Demand[i] <- 999999999}
#}
# Protot_Demand
length(Protot_Demand)

# ---------- Calculo de SIZE e COST por PROTOTIPO (CANDIDATE) ----------
# --- Inicializa Vetores ---
Prototype_costs <- vector(length = m)
Prototype_size  <- vector(length = m)

# Define Densidade de UNIDADES/m2 ==>> UNIDADE = ton, m3, etc.
# ........... ATENCAO!!!...... REVISAR!!!!!
sq_meter_per_Unit <- 8    # Vide Planilha de Densidades ==>> 8,3m2/ton
Cost_per_sq_meter <- 380  # Vide Planilha de Densidades
Cost_per_Unit <- 42.50    # Vide Planilha de Densidades ==>> 42,50/ton

# ----- Calculo de CUSTO FIXO por Prototipo (CANDIDATO)
#           ----- DADOS DESNORMALIZADOS -----
for(i in 1:m){
  Prototype_size[i] <- (Protot_Demand[i] * sq_meter_per_Unit)
  Prototype_costs[i] <- (Protot_Demand[i] * Cost_per_Unit
                      + Prototype_size[i]* Cost_per_sq_meter)
}

# Dados de Prototypes (com "dist_to_mean" DES_NORMALIZADA)

# >>>>> "PROTOTIPOS" <<<<<
#        Atributos: X, Y, Dist p Media, $/m2, Demanda, Area e Custo

Prototype_id <- m # No Total de Neuronios

Prototype_locations <- data.frame(
  id = 1:Prototype_id,
  x = Prototypes_DeNorm$X,
  y = Prototypes_DeNorm$Y,
  dist_to_mean = PrototypeDistanceVector_DeNorm, #dist of each Prototype to all Prototypes mean
  # cost_per_square_meter = Prototype_costPerSquareMeter, #cost based on dist_to_mean quartiles (line 162)
  total_Demand = Protot_Demand,
  Prototype_size = Prototype_size, #size based on Demand 
  Prototype_costs = Prototype_costs #cost based on Prototype_size and cost_per_square_meter
)
# View(Prototype_locations)  # X, Y, Dist_to_Mean, cost/m2, Demanda, Size, Cost
# Average_total_Demand <- sum(Protot_Demand)/m
# Average_total_Demand
# Average_Protot_Size <- sum(Prototype_size)/m
# Average_Protot_Size
# Average_Protot_Cost <- sum(Prototype_costs)/m
# Average_Protot_Cost
# Fixed_Protot_Cost <- (Average_total_Demand * Cost_per_Unit
#                       + Average_Protot_Size* Cost_per_sq_meter)
# 
Tot_Demand <- sum(Protot_Demand)
Tot_Demand
Tot_Facilities_size <- Tot_Demand * sq_meter_per_Unit
Tot_Facilities_size
p_max <- 0.2*n
Fixed_Facility_Dem_Cost <- Tot_Demand/(p_max)*Cost_per_Unit
Fixed_Facility_Size_Cost <- Tot_Facilities_size/(p_max) * Cost_per_sq_meter
Fixed_Facility_Cost <- Fixed_Facility_Dem_Cost + Fixed_Facility_Size_Cost
Fixed_Facility_Dem_Cost
Fixed_Facility_Size_Cost
Fixed_Facility_Cost

# --------- Com DISTANCIA GEOGRAFICA (HAVERSINE) --------
# BIBLIOTECAS Haversine
# library(pracma)
# require(pracma)

# transportcost_func <- function(i, j) {
#   Point <- Points_Locations[i, ]
#   Prototype <- Prototype_locations[j, ]
#   # calcula o custo de transporte com Dist. Haversine: 
#   return(haversine(c(Point$x, Point$y), c(Prototype$x, Prototype$y)) 
#          * (2.5/25) * (Prototype$Prototype_size * 12/0.3))
# }



#   ----- MATRIZ de CUSTOS de TRANSPORTE: PONTOS-CANDIDATOS -----

#     --- FUNCAO para CALCULAR OS CUSTOS de TRANSPORTE ---
#        --------- Com DISTANCIAS EUCLIDIANAS --------
Custo_Unitario_de_Transporte <- 0.36225  # <<<=== [ $/ton.km ]
# fonte: ANTT 2024 - Carga Geral(item 5) - Lotação de Alto Desempenho - 2 eixos
# R$/km = 2,898... Capac = 8t ==>>> $/t.km - 2,898/8 = 0,3625
transportcost_func <- function(i, j) {
  Point <-  Points_Locations[i, ]
  Prototype <- Prototype_locations[j, ]
  # calcula o custo de transporte com Dist. Euclidiana
  # return(sqrt((Point$x-Prototype$x)**2+(Point$y-Prototype$y)**2) 
  #        * (2.5/25) * (Prototype$Prototype_size * 12/0.3))
  return(sqrt((Point$x-Prototype$x)**2+(Point$y-Prototype$y)**2) 
         * (Custo_Unitario_de_Transporte) * (Point$Demand))
}

# --- CRIA MATRIZ COM CUSTOS TOTAIS de TRANSPORTE: PONTOS-CANDIDATOS ---
# Calcula Custo de Transporte de cada Ponto a todos os Candidatos
# Usa Funcao "transport_cost_func" para esse Calculo
# E gera a Matriz de Custos chamada de "transport_cost"
#   ----- Com DISTANCIAS EUCLIDIANAS - DES_NORMALIZADAS -----

transportCostMatrixFact <- function(){
  transport_cost <- matrix(nrow = n, ncol = m)
  for(row in 1:n){
    for(col in 1:m){
      transport_cost[row, col] <- transportcost_func(row, col)
    }
  }
  return(transport_cost)
}

# -----------------------------------------------------------
#  --- CUSTO de TRANSPORTE USADO no MODELO MILP ---
# -----------------------------------------------------------
# --- Transformacao de Matriz de Custos em DF ---
transport_cost <- as.data.frame(transportCostMatrixFact())
# View(transport_cost)
# summary(transport_cost)
# str(transport_cost)
# -----------------------------------------------------------
# -----------------------------------------------------------



# ------------------------------------------------------
# -------------- MONTAGEM DO MODELO MILP --------------
# ------------------------------------------------------
library(ompr)
library(magrittr)
# masked functions: and, mod, or

model_MIP <- MIPModel() %>%
  
  # ===>>> VARIABLES DEFINITION: x[i,j]={0,1} and y[j]={0,1} ----------
  
  # x[i,j] = 1, iff Demand Point i gets assigned to Prototype j
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # y[j] = 1, iff Prototype j is built (active)
  add_variable(y[j], j = 1:m, type = "binary") %>%
  
  # set_objective(sum_expr(transportcost_func(i, j) * x[i, j], i = 1:n, j = 1:m) +  #trocar por transport_cost[i,j]
  #                 sum_expr(Prototype_costs[j] * y[j], j = 1:m), "min") %>%        #trocar por Prototype_costs[j]

  # ===>>> OBJECTIVE FUNCTION -------------
    # set_objective(sum_expr(transport_cost[i, j] * x[i, j], i = 1:n, j = 1:m) +    #trocar por transport_cost[i,j]
    #               sum_expr(Prototype_costs[j] * y[j], j = 1:m), "min") %>%      #trocar por Prototype_costs[j]

    # set_objective(sum_expr(transport_cost[i, j] * x[i, j], i = 1:n, j = 1:m) +
    #                 sum_expr(Average_Protot_Cost * y[j], j = 1:m), "min") %>%

   # set_objective(sum_expr(transport_cost[i, j] * x[i, j], i = 1:n, j = 1:m) +
   #                sum_expr(Fixed_Protot_Cost * y[j], j = 1:m), "min") %>%
  
  set_objective(sum_expr(transport_cost[i, j] * x[i, j], i = 1:n, j = 1:m) +
                  sum_expr(Fixed_Facility_Cost * y[j], j = 1:m), "min") %>%
  
  # ===>>> CONSTRAINTS -------------
# Every Demand Point needs to be assigned to a Prototype
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n) %>% 
  
# If a Demand Point is assigned to a Prototype, the Prototype must be Active
  add_constraint(x[i,j] <= y[j], i = 1:n, j = 1:m)

model_MIP

# ------------------------------------------------------
#  --------------- RODAR O MODELO MILP ---------------
# ------------------------------------------------------
library(ompr.roi)
library(ROI.plugin.glpk)
result <- solve_model(model_MIP, with_ROI(solver = "glpk", verbose = TRUE))
# ------------------------------------------------------
# ------------------------------------------------------


# ------------------------------------------------------
#  ------------ SOLUCAO OTIMA - RESULTADOS ------------
# ------------------------------------------------------
result$objective_value # Valor da Funcao Objetivo
result$solution        # Valor das Variaveis Binarias: x e Y
result$status          # Atingiu Solucao Otima (S/N).. Sucesso
result$model           # Caracterisiticas do MOdelo : MILP Model
# OBS: A variavel Y define os Candidatos escolhidos para Facilities
# ------------------------------------------------------
# ------------------------------------------------------


# --------------------------------------------------------------------
# --------------------------------------------------------------------
#     ----- SOLUCAO OTIMA: Facilities, Alocacao e Custos -----
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# ===>>> Yj ==>> Candidates_Selection = List of Selected Candidates
Y_j <- vector(length = m)
for(i in 1:m){
  Y_selected <- result$solution[m*n + i]
  Y_j[i] <- Y_selected
}
Y_j # Yj

Y_j_DF <- as.data.frame(Y_j)
View(Y_j_DF)
str(Y_j_DF)

#Teste
Y_j_DF$Y_j[5]

# ===>>> p = # of Active Candidates 
p=0
for (i in 1:m) {
  p <- p + Y_j[i]
}
p

# ===>>> CUSTO TOTAL de Implantacao das FACILITIES 
Facilities_Opt_Cost=0
for (i in 1:m) {
  Facilities_Opt_Cost <- Facilities_Opt_Cost + Fixed_Facility_Cost * Y_j[i] # Candidates_Selection[i]
  
  # Facilities_Opt_Cost <- Facilities_Opt_Cost + Prototype_locations$Prototype_costs[i] * Candidates_Selection_DF$Candidates_Selection[i]
}
Facilities_Opt_Cost 


# ===>>> X_ij = Matriz de Alocacao "Pontos-Facilities"
X_ij <- matrix(result$solution, ncol = m, nrow=n, byrow=T)
dim(X_ij)
dim(transport_cost)

X_ij[1,5]
X_ij[5,1]
X_ij[2,5]
X_ij[,5]

# ===>>> CUSTO TOTAL de TRANSPORTE: "Pontos-Facilities" 
Transp_Opt_Cost=0
for (i in 1:n) {
  for (j in 1:m){
    Transp_Opt_Cost <- Transp_Opt_Cost + transport_cost[i, j] * X_ij[i, j]
  } 
}
Transp_Opt_Cost

# ===>>> CUSTO TOTAL da OPERACAO = Custo de TRANSPORTE + Custo de FACILITIES
Total_Opt_Cost <- Transp_Opt_Cost + Facilities_Opt_Cost
Total_Opt_Cost

# ===>> *** cHECAGEM ***: CUSTO TOTAL da OPERACAO via MODELO
Total_Opt_Cost_Model <- result$objective_value
Total_Opt_Cost_Model


# ===>>> PESOS % dos CUSTOS
Weight_Facilities_Opt_Cost <- Facilities_Opt_Cost/Total_Opt_Cost*100
Weight_Facilities_Opt_Cost

Weight_Transp_Opt_Cost <- Transp_Opt_Cost/Total_Opt_Cost*100
Weight_Transp_Opt_Cost
# --------------------------------------------------------------------
# --------------------------------------------------------------------



# --------- GERAR MATRIZ COMPLETA DE SOLUCOES; Xij + Yj ---------

# ===>>> X_ij = Matriz de Alocacao "Pontos-Facilities"
# ===>>> Y_j = Vetor de Candidatos Selecionados

#         ----------------- Unir X_ij e Y_j  ----------------- 
#               ---------- Criar Matriz Xij_Yj ---------- 
Xij_Yj <- X_ij
dim(Xij_Yj)
Xij_Yj =  rbind(Xij_Yj, Y_j)
dim(Xij_Yj)

# Teste
Xij_Yj[110,]

# Append de uma Linha a mais na Matriz com o Valor da F. Objetivo
# O valor fica repetido em todas as posições da Linha
K_N = 0
c<- c()
for(Col in 1:N_Col){
  K_N <- K_N + 1
  c <- append(c, result$objective_value, after = K_N)}
c

Xij_Yj_FObj <- Xij_Yj
dim(Xij_Yj_FObj)
F_Obj <- c
Xij_Yj_FObj =  rbind(Xij_Yj_FObj, F_Obj)
dim(Xij_Yj_FObj)

# ----- Gravar Xij_Yj_FObj em CSV
setwd("D:/SCRIPTS_R/Mestrado PPGCA/Samuel/Results_2024")
write.csv(Xij_Yj_FObj, "Solution_Test_Xij_Yj_FObj.csv")
Data_SolutionXij_Yj_FObj<- read.csv("Solution_Test_Xij_Yj_FObj.csv", sep = ",", header = T)
str(Data_SolutionXij_Yj_FObj)
setwd("D:/SCRIPTS_R/Mestrado PPGCA/Samuel/UCHOA")

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------




#     -------------------- OUTRA ALTERNATIVA ----------------------------
#      ---------- NAO E NECESSARIO RODAR ESTA ALTERNATIVA -----------
#            -------------- VETOR de SOLUCOES --------------
#                 ------------ Xij + Yj ------------
#           ---------- Total de Posicoes = n*m = m ----------
Candidates_Solution <- c()
K_N <- 0
Lsup <- n*m + m
for (i in 1:Lsup) {
  if (i==1)  {K_N <- K_N + i }
  else       {K_N <- K_N + i + 1}
  Candidates_Solution<- append(Candidates_Solution, result$solution[i], after = K_N)
}

# Teste... Print de Yj ultimas posicoes do vetor
length(Candidates_Solution)
Candidates_Solution[1309:1320]

setwd("D:/SCRIPTS_R/Mestrado PPGCA/Samuel/Results_2024")
write.csv(Candidates_Solution, "Solution_Test113.csv")
Data_Solution1<- read.csv("Solution_Test113.csv", sep = ",", header = T)
str(Data_Solution1)
setwd("D:/SCRIPTS_R/Mestrado PPGCA/Samuel/UCHOA")

# -----------------------------------------------------------------------

# ---------------------------------------------------------------------------
#     ------------------ GERACAO de MATRIZ de SOLUCOES ------------------
# ---------------------------------------------------------------------------
Solution_Matrix <- matrix(nrow = n+1, ncol = m)
dim(Solution_Matrix)
  
# N_Variables <- n*m + m
N_Variable <- 0
N_Row <- n + 1
N_Col <- m
for(Row in 1:N_Row){
  for(Col in 1:N_Col){
  N_Variable <- N_Variable + 1
  Solution_Matrix[Row, Col] <- result$solution[N_Variable]}}
  
# Append de uma Linha a mais na Matriz com o Valor da F. Objetivo
# O valor fica repetido em todas as posições da Linha
K_N = 0
c<- c()
for(Col in 1:N_Col){
  K_N <- K_N + 1
  c <- append(c, result$objective_value, after = K_N)}
c

F_Obj <- c
Solution_Matrix =  rbind(Solution_Matrix, F_Obj)
dim(Solution_Matrix)

# Teste
Solution_Matrix[N_Row+1,]
Solution_Matrix[111,]
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# ------------------ GRAVACAO de MATRIZ de SOLUCOES em CSV ------------------
# ---------------------------------------------------------------------------
setwd("D:/SCRIPTS_R/Mestrado PPGCA/Samuel/Results_2024")
write.csv(Solution_Matrix, "Solution_Matrix10011.csv")
Matrix_Solution10011<- read.csv("Solution_Matrix10011.csv", sep = ",", header = T)
str(Matrix_Solution10011)
setwd("D:/SCRIPTS_R/Mestrado PPGCA/Samuel/UCHOA")
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------




 
#########################################################################
##########################     PLOTS     ################################
#           ---------- Nao estao Funcionando -----------
#                >>>>>>>> PRECISA AJUSTAR !!!!! <<<<<<<<
#########################################################################

library(ggplot2)
require(ggplot2)

x11()
# ------------------------------------------------------
# -------- PLOT dos PONTOS.... nao funcionou!! --------
# ------------------------------------------------------
grid_size <- 0
#principal PLOT
p <- ggplot(customer_locations, aes(x, y)) +
  geom_point() +
  geom_point(data = Prototype_locations, color = "red", alpha = 0.5, shape = 17) +
  scale_x_continuous(limits = c(-25, -2)) +
  scale_y_continuous(limits = c(-53, -33)) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), panel.grid = element_blank())
p + ggtitle("Prototype Location Problem",
            "Black dots are customers. Light red triangles show potential Prototypes locations.")
# ------------------------------------------------------
# ------------------------------------------------------


# ------------------------------------------------------
# ----------- PLOTS.... nao funcionou!! --------------
# ------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
matching <- result %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%  
  select(i, j)

x11()

#add the assignments to the previous plot
plot_assignment <- matching %>% 
  inner_join(customer_locations, by = c("i" = "id")) %>% 
  inner_join(Prototype_locations, by = c("j" = "id"))
customer_count <- matching %>% group_by(j) %>% summarise(n = n()) %>% rename(id = j)

###### problema com fixed cost (o custo fixo deste código varia)
#armazéns escolhidos
plot_Prototypes <- Prototype_locations %>%
  mutate(costs = Prototype_costs) %>%
  inner_join(customer_count, by = "id") %>%
  filter(id %in% unique(matching$j))

p + 
  geom_segment(data = plot_assignment, aes(x = x.y, y = y.y, xend = x.x, yend = y.x)) + 
  geom_point(data  = plot_Prototypes, color = "red", size = 3, shape = 17) +
  ggrepel::geom_label_repel(data  = plot_Prototypes, 
                            aes(label = paste0("fixed costs:", costs, "; customers: ", n )), 
                            size = 3, nudge_y = 20) + 
  ggtitle(paste0("Cost optimal Prototype Locations and customer assignment"),
          "Big red triangles show Prototypes that will be built, light red are unused Prototype 
          locations. Dots represent customers served by the respective Prototypes.")

#fixed costs for setting up the 4 Prototypes:
sum(Prototype_costs[unique(matching$j)])











#           ------------------- SEM USO ----------------------

#################################################################################################################
######################################## OBJETIVOS ##############################################################
#################################################################################################################

#1)
# Somar as populações das 77 cidades de delaware
# Dividir a população de cada cidade pelo total somado (dplyr package)
# Pegar o resultado de cada divisão (77 indices), e multiplica pela população real (google) de delaware (var realPop)
# O resultado será a população aproximada de cada cidade

#2)
# Depois vamos estabelecer um valor de m² de armazém por habitante (1m² por habitante)
# Multiplica esse valor pela população de cada cidade = tamanho de cada armazén na cidade
# multiplicar pelos custos por M² que já estão no data frame

#3)
# adicionar 2 colunas ao Prototype Locations:
# uma será o tamanho du cluster ( a área de armazén = população * parmetro p)
# a outra coluna custo total será o custo do armazén, que será a área do armazén * custo por m²

#4)
# melhorar vetor de custo de transporte, adicionando o custo de cada cidade para todos os armazéns (16), 
# para assim vermos quais armazéns são os melhores
# tentar recriar função de "transport cost" do Prototype Locations

#5)
#pegar a soma da coluna "i" do vetor de custo de transporte, e a "i" linha do custo fixo do vetor de armazéns.

#6)
#Usar o modelo MIP do script warehouse.R, trocando a função "transportcost()" pelo valor i
#no data frame "transport_cost", e trocar
# (Problema no resultado do modelo mip)

# 7) 
# calcular a matriz com os dados originais (77 por 77), e outra com os dados normalizados.
# tirar a média geral das duas matrizes, e divide a média dos dados originais pela média dos dados normalizados.
# usar esse valor para montar a matriz "transport_cost", multiplicando a dist por esse valor
# (esperar um valor alto)

# 8) montar a matriz de transport_cost (237)



fra <- c(df$lat[328], df$lng[328])
ord <- c(df$lat[250], df$lng[250])

dis <- haversine(fra, ord)
fprintf('Flight distance Frankfurt-Chicago is %8.3f km.\n', dis)

