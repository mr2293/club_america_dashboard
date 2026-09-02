# Calculadora de Sesión (R / Shiny)

Modelo predictivo de carga física para sesiones de entrenamiento.
Basado en datos históricos de Catapult.

Port a R/Shiny de la versión original en Python (FastAPI + JS), para que
viva en el mismo lenguaje/stack que tus otras herramientas.

## Inicio rápido

### 1. Instalar R

Si no lo tienes, descárgalo de https://www.r-project.org/
(versión 4.1 o superior)

### 2. Instalar dependencias

Abre una consola de R en la carpeta del proyecto y ejecuta:

```r
install.packages(c("shiny", "readxl", "dplyr"))
```

### 3. Colocar tus datos

Copia tu archivo `stats_df.xlsx` (generado desde tu script de procesamiento
de datos en R / Catapult API) en la carpeta `data/`:

```
session-calculator-r/
├── data/
│   └── stats_df.xlsx    ← aquí
├── R/
├── app.R
└── ...
```

Si el archivo ya trae una columna `match_day` (p. ej. "MD", "MD-2", "MD+1"),
la app la usa directamente para agrupar las sesiones. Si no la trae, la
calcula ella misma a partir de la fecha y la distancia al partido más
cercano (ver `compute_match_days()` en `R/engine.R`).

Los valores del tag DayCode que no son un offset estándar (MD-N / MD+N) se
normalizan así (ver `normalize_match_day()` en `R/engine.R`):

- **"Game"** (amistoso de jugadores sin/con pocos minutos en el MD) se
  agrupa dentro de **MD+1 (Compensatorio)**.
- **"Other"** se mantiene como su propio botón, **"Otro"**, en vez de
  descartarse.
- **"Tuesday"**, vacío o `NA` se descarta (no se puede asignar a un día del
  microciclo).

### 4. Ejecutar

```bash
Rscript run.R
```

o desde RStudio, abre `app.R` y haz clic en "Run App".

Se abrirá automáticamente en tu navegador en http://localhost:8000

## Actualizar datos

1. Genera un nuevo `stats_df.xlsx` desde tu script de R
2. Reemplaza el archivo en `data/`
3. Reinicia la app (Ctrl+C y `Rscript run.R` de nuevo)

## Agregar nuevas actividades

Si aparecen nuevos `period_name` en Catapult que no están mapeados:

```bash
Rscript R/categories.R --unmapped data/stats_df.xlsx
```

Edita `R/categories.R` para agregar los nombres faltantes (`EXACT_MAP` o
`KEYWORD_MAP`).

## Estructura del proyecto

- `R/categories.R` — mapea `period_name` crudos de Catapult a categorías canónicas.
- `R/engine.R` — carga el xlsx, calcula match days (MD-N / MD+N), y construye
  el modelo estadístico (media, desviación, IC 95% por minuto).
- `R/mod_activity_row.R` — módulo Shiny para cada fila de actividad en el
  planificador (selección de tipo, duración, barras de métricas con IC).
- `app.R` — UI y servidor Shiny: selector de día de partido, filas de
  actividades dinámicas, y panel de carga total.
