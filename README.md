# Challenge_Open_Data_M1

## 📦 Prérequis

Avant d’installer ce projet, assure-toi d’avoir :

1. **R & RStudio**
   - R ≥ 4.2 (idéalement 4.3+)
   - RStudio ≥ 2023.0

2. **Anaconda ou Miniconda**
   - Anaconda (complet) ou Miniconda (plus léger)
   - Téléchargement : https://docs.conda.io/en/latest/miniconda.html

3. **Git (optionnel)**
   - Utile pour cloner le repo avec `git clone`
   - Sinon tu peux télécharger les fichiers individuellement ou en ZIP

✅ Les dépendances R et Python seront installées automatiquement via `scripts/bootstrap.R` :
- **R** : packages gérés avec `renv`
- **Python** : environnement conda `rpy` avec `pandas`, `geopandas`, `requests`, `numpy`

---

## 🚀 Installation (sans Git)

1. Télécharge **tous les fichiers du repo** (bouton vert *Code > Download ZIP* ou individuellement depuis GitHub).  
2. Décompresse / place les fichiers dans un dossier, puis ouvre ce dossier dans **RStudio**.  
3. Dans la console R, lance :  
   ```r
   source("scripts/bootstrap.R")
   ```  
4. Redémarre R (`Session > Restart R`)  
5. Vérifie la config Python :  
   ```r
   library(reticulate); py_config()
   ```  
6. Ouvre et exécute :  
   ```
   notebooks/demo_r_plus_python.Rmd
   ```  

✅ Tu peux maintenant utiliser **R + Python** ensemble dans RStudio (l’environnement `rpy` est créé automatiquement).
