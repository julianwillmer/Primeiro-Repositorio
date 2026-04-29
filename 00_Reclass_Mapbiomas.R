# ============================================================================
# SCRIPT: CONVERSÃO DO MAPBIOMAS DO NÍVEL 3 PARA O NÍVEL 2
# ============================================================================
# Autor: (seu nome)
# Data: (data atual)
# Descrição: 
#   - Reclassifica o raster do MapBiomas (coleção 10) de nível 3 para nível 2.
#   - Gera gráficos usando ggplot2 e plotagem nativa do terra.
#   - Produz estatísticas de área por classe.
# ============================================================================

# Carregar pacotes necessários
library(terra)      # Manipulação de rasters
library(ggplot2)    # Gráficos avançados

# Definir diretório de trabalho (ajuste conforme necessário)
setwd("D:/Priorizacoes/Priorizacao_JW/Fluxo2026/Dados/")

# --------------------------------------------------------------------------
# 1. FUNÇÃO PRINCIPAL: CONVERTER NÍVEL 3 -> NÍVEL 2
# --------------------------------------------------------------------------
# Recebe o caminho de um arquivo raster do MapBiomas (nível 3) e retorna um
# raster reclassificado para as categorias do nível 2, conforme tabela de 
# correspondência definida.
mapbiomas_level3_to_level2 <- function(raster_file) {
  
  # Carrega o raster de entrada
  rast <- rast(raster_file)
  
  # Matriz de reclassificação: 
  # Cada linha: [código_original, código_destino]
  # Os códigos de nível 3 são mapeados para os códigos de nível 2
  reclass_matrix <- matrix(c(
    # Florestas (já estão no nível 2, mantêm o mesmo código)
    3, 3,   # Formação Florestal -> Formação Florestal
    4, 4,   # Formação Savânica -> Formação Savânica
    5, 5,   # Mangue -> Mangue
    6, 6,   # Floresta Inundável -> Floresta Inundável
    49, 49, # Apicum (Floresta Paludosa) -> Apicum
    
    # Vegetação Herbácea e Arbustiva (consolida várias classes)
    11, 10, # Wetland -> Vegetação Herbácea/Arbustiva
    12, 10, # Campo Alagado -> Vegetação Herbácea/Arbustiva
    32, 10, # Planície Hipersalina -> Vegetação Herbácea/Arbustiva
    29, 10, # Afloramento Rochoso -> Vegetação Herbácea/Arbustiva
    50, 10, # Apicum Herbáceo -> Vegetação Herbácea/Arbustiva
    
    # Agropecuária (consolida pastagem, agricultura anual e perene)
    15, 13, # Pastagem -> Agropecuária (Pastagem)
    18, 14, # Agricultura -> Agropecuária (Agricultura)
    19, 14, # Lavoura Temporária -> Agropecuária (Agricultura)
    39, 14, # Soja -> Agropecuária (Agricultura)
    20, 14, # Cana-de-açúcar -> Agropecuária (Agricultura)
    40, 14, # Arroz -> Agropecuária (Agricultura)
    62, 14, # Algodão -> Agropecuária (Agricultura)
    41, 14, # Outras Lavouras Temporárias -> Agropecuária (Agricultura)
    36, 14, # Lavoura Perene -> Agropecuária (Agricultura)
    46, 14, # Café -> Agropecuária (Agricultura)
    47, 14, # Citros -> Agropecuária (Agricultura)
    35, 14, # Dendezeiro -> Agropecuária (Agricultura)
    48, 14, # Outras Lavouras Perenes -> Agropecuária (Agricultura)
    9,  14, # Silvicultura -> Agropecuária (Agricultura)
    21, 14, # Mosaico de Usos -> Agropecuária (Agricultura)
    
    # Área não vegetada (consolida áreas urbanas, mineração, etc.)
    23, 22, # Praia, Duna e Areal -> Área não vegetada
    24, 22, # Área Urbana -> Área não vegetada
    30, 22, # Mineração -> Área não vegetada
    75, 22, # Usina Solar Fotovoltaica -> Área não vegetada
    25, 22, # Outras Áreas não Vegetadas -> Área não vegetada
    
    # Água (consolida rios, lagos, oceanos e aquicultura)
    33, 26, # Rio, Lago e Oceano -> Água
    31, 26, # Aquicultura -> Água
    
    # Não observado (mantém o mesmo código)
    27, 27  # Não Observado -> Não Observado
  ), ncol = 2, byrow = TRUE)
  
  # Aplica a reclassificação
  rast_level2 <- classify(rast, reclass_matrix)
  
  return(rast_level2)
}

# --------------------------------------------------------------------------
# 2. PALETA DE CORES E NOMES PARA AS CLASSES DO NÍVEL 2
# --------------------------------------------------------------------------
# Paleta de cores (código da classe -> cor em hex)
level2_colors <- c(
  "3"  = "#1f8d49",  # Formação Florestal
  "4"  = "#7dc975",  # Formação Savânica
  "5"  = "#04f8fd",  # Mangue
  "6"  = "#007785",  # Floresta Inundável
  "49" = "#02d659",  # Apicum Florestado
  "10" = "#d6bc74",  # Vegetação Herbácea/Arbustiva
  "13" = "#edde8e",  # Pastagem
  "14" = "#ffefc3",  # Agricultura (lavouras)
  "22" = "#d4271e",  # Área não vegetada
  "26" = "#2532e4",  # Água
  "27" = "#ffffff"   # Não Observado
)

# Nomes descritivos das classes (código -> nome legível)
level2_class_names <- c(
  "3"  = "Formação Florestal",
  "4"  = "Formação Savânica",
  "5"  = "Mangue",
  "6"  = "Floresta Inundável", 
  "49" = "Apicum Florestado",
  "10" = "Vegetação Herbácea/Arbustiva",
  "13" = "Pastagem",
  "14" = "Agricultura",
  "22" = "Área não vegetada",
  "26" = "Água",
  "27" = "Não Observado"
)

# --------------------------------------------------------------------------
# 3. FUNÇÕES DE PLOTAGEM
# --------------------------------------------------------------------------

# 3.1 Plotagem com ggplot2 (mais flexível)
plot_mapbiomas_level2 <- function(rast_level2, title = "MapBiomas Coleção 10 - Nível 2") {
  
  # Converte o raster para data.frame (x, y, classe)
  rast_df <- as.data.frame(rast_level2, xy = TRUE)
  colnames(rast_df) <- c("x", "y", "class")
  rast_df$class <- as.factor(rast_df$class)
  
  # Identifica as classes presentes no raster
  present_classes <- levels(rast_df$class)
  present_colors <- level2_colors[present_classes]
  present_names <- level2_class_names[present_classes]
  
  # Cria o gráfico
  ggplot() +
    geom_raster(data = rast_df, aes(x = x, y = y, fill = class)) +
    scale_fill_manual(
      values = present_colors,
      labels = present_names,
      name = "Cobertura do Solo"
    ) +
    coord_equal() +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right",
      legend.text = element_text(size = 8)
    ) +
    ggtitle(title)
}

# 3.2 Plotagem usando a função nativa do terra (mais rápida para rasters grandes)
plot_mapbiomas_level2_terra <- function(rast_level2, title = "MapBiomas Coleção 10 - Nível 2") {
  # Define a tabela de cores baseada nas classes presentes
  coltab(rast_level2) <- level2_colors[as.character(sort(unique(values(rast_level2))))]
  plot(rast_level2, main = title)
}

# --------------------------------------------------------------------------
# 4. FUNÇÕES UTILITÁRIAS: SALVAR E SUMARIZAR
# --------------------------------------------------------------------------

# Salva o raster reclassificado em disco
save_level2_raster <- function(rast_level2, output_path) {
  writeRaster(rast_level2, output_path, overwrite = TRUE)
  cat("Raster nível 2 salvo em:", output_path, "\n")
}

# Gera tabela de frequência (área) das classes no nível 2
get_level2_summary <- function(rast_level2) {
  # Contagem de pixels por classe
  freq_table <- freq(rast_level2)
  
  # Adiciona os nomes das classes
  freq_table$class_name <- level2_class_names[as.character(freq_table$value)]
  
  # Calcula percentual de área (assumindo que cada pixel tem a mesma área)
  total_pixels <- sum(freq_table$count)
  freq_table$percentage <- round((freq_table$count / total_pixels) * 100, 2)
  
  return(freq_table)
}

# --------------------------------------------------------------------------
# 5. FUNÇÃO DE WORKFLOW COMPLETO (OPCIONAL)
# --------------------------------------------------------------------------
# Executa todas as etapas: conversão, plotagem, salvamento e sumário
process_mapbiomas_to_level2 <- function(input_path, output_path = NULL, plot_result = TRUE) {
  
  # Passo 1: Converter para nível 2
  cat("Convertendo MapBiomas do nível 3 para o nível 2...\n")
  level2_raster <- mapbiomas_level3_to_level2(input_path)
  
  # Passo 2: Plotar resultado (se solicitado)
  if (plot_result) {
    cat("Gerando plot...\n")
    print(plot_mapbiomas_level2(level2_raster))
  }
  
  # Passo 3: Salvar raster (se um caminho de saída foi fornecido)
  if (!is.null(output_path)) {
    save_level2_raster(level2_raster, output_path)
  }
  
  # Passo 4: Exibir estatísticas resumidas
  cat("Gerando estatísticas resumidas...\n")
  summary_stats <- get_level2_summary(level2_raster)
  print(summary_stats)
  
  # Retorna o raster reclassificado (para uso posterior)
  return(level2_raster)
}

# --------------------------------------------------------------------------
# 6. EXEMPLO DE USO (DESCOMENTE PARA EXECUTAR)
# --------------------------------------------------------------------------
 result <- process_mapbiomas_to_level2(
   input_path = "./LULC/Mapbiomas_Ori_261125.tif",
   output_path = "./LULC/Mapbiomas_reclass_lv2_22042026.tif",
   plot_result = TRUE
 )
