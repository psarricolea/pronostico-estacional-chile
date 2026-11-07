# Pronóstico estacional · Chile

Visor web del pronóstico estacional de **temperatura máxima, temperatura mínima y precipitación** para Chile, a partir del multi-modelo **Copernicus C3S** (10 sistemas, grilla de 1°). Muestra la anomalía respecto de lo normal y el tercil más probable para los próximos cuatro meses y dos trimestres.

Visor: https://psarricolea.github.io/pronostico-estacional-chile/

## Cómo se actualiza

La rutina `.github/workflows/pronostico-mensual.yml` corre sola el **día 11 de cada mes**, porque C3S publica la nueva inicialización el día 10. Si ese día falla (por ejemplo, porque la cola del CDS está saturada), reintenta los días 12 y 13.

En cada corrida, `R/pronostico_mensual.R`:

1. Descarga del CDS, con `ecmwfr`, el pronóstico de la última inicialización y el hindcast 1993–2016 de cada sistema.
2. Calcula, por sistema, los terciles del hindcast y la fracción de miembros del pronóstico que cae en cada tercil. En Tmax y Tmin, el hindcast se lleva antes al nivel del año actual según su tendencia lineal.
3. Promedia los sistemas con igual peso y reescribe `data.js` y las imágenes de `img/`.

`index.html` no se modifica en la rutina.

### Correrla a mano

En **Actions → Pronóstico estacional mensual → Run workflow**. Se puede indicar el año y el mes de inicialización, y marcar *forzar* para recalcular una inicialización que ya está publicada.

### Configuración (una sola vez)

- **Secreto `CDS_API_KEY`** (Settings → Secrets and variables → Actions) con el token personal del CDS. El token nunca debe quedar escrito en el script, porque el repositorio es público.
- Aceptar la licencia del dataset `seasonal-monthly-single-levels` en la web del CDS.

## Notas

- Precipitación: anomalía en % de la lluvia normal. Las zonas donde la lluvia normal del período es menor a 5 mm/mes quedan sin color, porque ahí los terciles no tienen sentido.
- Es un producto propio y no reemplaza el pronóstico oficial de la Dirección Meteorológica de Chile (DMC).
