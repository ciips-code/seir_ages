@echo off
:Begin
cd/
git clone https://github.com/MinCiencia/Datos-COVID19
cd datos-covid19
"C:\Program Files\R\R-4.0.2\bin\Rscript.exe" -e source('https://github.com/ciips-code/seir_ages/raw/829695655b6edf3caec99de56ff9f4acf599632e/functions/updateChile.R')
:End
cmd /k