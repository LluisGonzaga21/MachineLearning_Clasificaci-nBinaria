# MachineLearning_ClasificaciónBinaria
Se ha realizado un estudio de la tasa de mortalidad por enfermedad cardíaca mediante la elaboración de distintos modelos predictivos basados en Machine Learning, como redes neuronales, Random Forest o XGBOOST, entre otros.

En el directorio se adjuntan los datos usados. Proceden de la investigación que está llevando a cabo la Organización Mundial de la Salud para el estudio y prevención de las enfermedades cardiovasculares.

El objetivo del presente trabajo es encontrar el mejor modelo predictivo, es decir, aquel que sea capaz de predecir con mayor fiablidad si un paciente, dados uns datos, experimentará muerte debido a un problema cardiovascular.

Descripción de archivos:
- TareaMonica.R: es el archivo que contiene todas las líneas de código usadas para el desarrollo de los distintos modelos predictivos, su tuneo, evaluación y comparación mediante métodos gráficos con los demás.
- mifem.csv: contiene todos los datos procedentes del estudio de la OMS. La variable dependiente es de tipo binaria, categórica.
- cruzadas: Los archivos en formato .R que comienzan por "cruzada" o "cruazadas" son un conjunto de funciones propias en que nos hemos apoyado para la realización de la validación cruzada repetida de los distintos modelos.
