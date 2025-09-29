# scripts/bootstrap.R
# But: restaurer les packages R et créer l'environnement conda "rpy"
# Usage: source("scripts/bootstrap.R")

message("▶ Préparation R (renv + reticulate)")
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")

# Restaure les dépendances R si renv.lock existe, sinon crée un lock minimal
if (file.exists("renv.lock")) {
  renv::restore(prompt = FALSE)
} else {
  renv::init(bare = TRUE)
  renv::snapshot(prompt = FALSE)
}

# Assure-toi d'avoir un conda disponible (Miniconda si besoin)
if (is.na(reticulate::conda_binary())) {
  message("▶ Installation de Miniconda (aucun conda détecté)")
  reticulate::install_miniconda()
}

conda <- reticulate::conda_binary()

# (Re)crée l'environnement conda à partir de environment.yml si présent
if (file.exists("environment.yml")) {
  message("▶ Création/maj de l'environnement conda 'rpy' depuis environment.yml")
  try(system2(conda, c("env", "remove", "-n", "rpy", "-y"), stdout = TRUE, stderr = TRUE), silent = TRUE)
  status <- system2(conda, c("env", "create", "-f", "environment.yml"), stdout = TRUE, stderr = TRUE)
  if (status != 0) {
    stop("Échec de la création de l'env conda 'rpy'. Regarde la sortie ci-dessus.")
  }
} else {
  message("▶ environment.yml introuvable — création minimale de 'rpy'")
  reticulate::conda_create("rpy", packages = c("python=3.11"))
  reticulate::conda_install("rpy", packages = c("pandas", "geopandas", "requests", "numpy"),
                            channel = "conda-forge")
}

message("▶ Test rapide de Python dans 'rpy'")
reticulate::use_condaenv("rpy", required = TRUE)
reticulate::py_run_string("import sys; print('Python:', sys.version)")
reticulate::py_run_string("import pandas, geopandas, requests, numpy; print('OK: modules chargés')")

message("\n✅ Préparation terminée. Dans RStudio :")
message("   - Redémarre la session (Session > Restart R)")
message("   - library(reticulate); py_config() doit pointer vers l'env 'rpy'")
