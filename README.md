# Challenge_Open_Data_M1

## Installation (sans Git)

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
