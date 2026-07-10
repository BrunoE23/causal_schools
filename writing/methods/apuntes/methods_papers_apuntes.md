# Apuntes de papers metodologicos

Fecha de lectura: 2026-06-30

Estos apuntes resumen los tres papers en `writing/methods/papers/` y conectan cada uno con las revisiones metodologicas del paper de escuelas causales.

Fuentes:

- `writing/methods/papers/hoee_methods (1).pdf`
- `writing/methods/papers/HOLE_EBchapter.pdf`
- `writing/methods/papers/rss_teachers_cjc.pdf`

Las extracciones completas de texto usadas para lectura quedaron en `tmp/pdfs/`.

## 1. Angrist, Hull, and Walters - Methods for Measuring School Effectiveness

Referencia local: `writing/methods/papers/hoee_methods (1).pdf`

Este handbook chapter es la referencia principal para escribir la parte de identificacion con lotteries, assignment risk, centralized assignment, value-added validation, y risk-controlled VAM.

### Idea general

El capitulo define school effectiveness como el efecto causal de asistir a una escuela, o conjunto de escuelas, sobre outcomes de estudiantes.
El problema central es selection bias: diferencias entre escuelas pueden reflejar student ability, family background, preferencias, o redes, no solo causal school quality.
La solucion moderna usa variacion aleatoria en school assignment, especialmente lotteries y centralized assignment.

### Single-school lottery IV

Para una escuela o sector con lottery offer `Z_i` y asistencia `D_i`, el IV basico estima el efecto causal de attendance usando el offer como instrumento.
Con efectos constantes, el estimador Wald es reduced form dividido por first stage.
Con efectos heterogeneos, el estimador tiene interpretacion LATE: efecto para compliers inducidos por la oferta.

El capitulo enfatiza que una buena tabla IV debe reportar:

- reduced form o second stage
- first stage
- risk controls
- balance checks
- attrition or outcome observation checks

### Assignment risk

En lotteries estratificadas, assignment is random only within risk sets.
Assignment risk es la probabilidad condicional de recibir oferta.
Si grupos tienen distinta probabilidad de oferta, comparar winners y losers sin controlar risk sets puede introducir selection bias.

El estimador recomendado es 2SLS con risk set controls.
Esto agrega efectos especificos por risk set en una media ponderada.
Los pesos dependen del tamano del risk set, de la varianza de assignment dentro del risk set, y del first stage.

Covariates adicionales no son necesarias para identificacion una vez controlado assignment risk, pero suelen aumentar precision si predicen outcomes.

### Multiple schools and sectors

Cuando hay varias escuelas dentro de un sector, el paper distingue entre:

- estimar un tratamiento comun de sector
- estimar efectos separados de escuelas
- usar multiple instruments o un any-offer instrument

La pregunta "compared to what?" es central.
Para interpretar efectos de un sector, hay que describir los counterfactual destinies de compliers, porque lottery losers pueden ir a distintos tipos de escuelas.

Esto importa para nuestro paper porque el margen SAE no identifica "el efecto de una escuela cualquiera" sin especificar la alternativa.
Nuestro estimand es mas estrecho: efecto causal de moverse a lo largo de un scalar school-value index en el margen inducido por ofertas SAE.

### Centralized assignment

La seccion sobre Deferred Acceptance es directamente relevante para SAE.
En DA, la asignacion depende de preferencias, prioridades, capacities, y tie-breakers.
Conditional on full applicant type, offers are ignorable por equal treatment of equals, pero el type es de alta dimension.

La solucion es controlar propensity scores de assignment:

`p_ij = Pr(Z_i = j | theta_i)`

Estos scores resumen assignment risk inducido por preferencias y prioridades.
Pueden obtenerse con formulas large-market o por simulacion, redibujando tie-breakers muchas veces y manteniendo fijo todo lo no aleatorio.

Para un school `j`, la especificacion puede usar el offer residualizado:

`Z_ij - p_ij`

o, equivalentemente, controlar por `p_ij` y usar `Z_ij`.
Por Frisch-Waugh-Lovell, controlar por propensity score o recentrar el instrumento son formas equivalentes de aislar la variacion aleatoria condicional.

### School-characteristic IV

El capitulo extiende centralized assignment a tratamientos definidos por caracteristicas de la escuela.
Si `C_j` es una caracteristica de la escuela, el tratamiento puede ser:

`C_{d(i)} = sum_j C_j D_ij`

y el instrumento:

`C_{z(i)} = sum_j C_j Z_ij`

La especificacion debe controlar:

`sum_j C_j p_ij`

Este punto es la base mas directa para nuestro expected-VA risk control.
En nuestro paper, `C_j` es el value-added observacional del outcome `m`.
Entonces:

- tratamiento endogeno: value-added de la escuela asistida
- instrumento: value-added de la escuela ofrecida
- risk control: expected value-added bajo las assignment probabilities

Esto conecta formalmente nuestro `expected_VA_i = sum_s p_is V_s` con la literatura de centralized assignment, no solo con una conveniencia computacional.

### Observational VAM

La seccion de VAM parte de un modelo de efectos constantes por escuela:

`Y_i = sum_j beta_j D_ij + X_i lambda + eta_i`

El supuesto de VAM observacional es selection-on-observables:

`E[D_ij eta_i] = 0`

En palabras: condicional en los controles, los estudiantes no deben estar sorting across schools en dimensiones no observadas que afectan outcomes.

El capitulo tambien discute gains models y lagged achievement controls.
Para achievement, lagged outcomes ayudan mucho.
Para outcomes de largo plazo o no-achievement, lagged outcomes del mismo objeto muchas veces no existen.
Esto justifica el rol de lotteries para validar VAM en outcomes como field choice o institutional quality.

### Empirical Bayes en VAM

Los OLS school fixed effects son ruidosos.
EB shrinkage puede reducir MSE de predicciones de school quality.
Pero el uso de shrinkage depende del objetivo:

- para predecir individual school quality, posterior means pueden ser utiles
- para describir la distribucion de efectos latentes, los posteriors shrinked no tienen la misma varianza que los efectos verdaderos
- para regressions con VA como variable, hay que distinguir si el VA esta en RHS o LHS

El capitulo anticipa las advertencias mas desarrolladas en Walters y Rose-Schellenberg-Shem-Tov.

### Testing VAM validity with lotteries

Esta es una seccion clave para nuestro paper.
Si el VAM es valido, los residuals del VAM observacional deben ser ortogonales a los offers ajustados por risk:

`E[(Z_il - p_il) nu_i] = 0`

El test de Angrist et al. puede descomponerse en:

- forecast bias test
- overidentification test

El forecast coefficient viene de una IV regression de `Y_i` sobre la prediccion VAM de la escuela asistida, instrumentada por ofertas/risk-adjusted offers.
El benchmark natural es forecast coefficient igual a 1.
Si es menor que 1, el VAM observacional predice causal effects pero sobrestima el tamano.
Si es 0, no tiene predictive causal content.

Nuestro `theta` es muy cercano a este forecast/pass-through coefficient.
Una diferencia es que nuestro tratamiento escalar es outcome-specific school VA, instrumentado con offered-school VA, y controlamos expected VA en la assignment portfolio.

### Bias correction, IV VAM, and RC VAM

El capitulo presenta tres formas de usar lotteries para mejorar VAM:

- combinar OLS VAM y IV VAM con EB, tradeoff bias-variance
- IV VAM para casos con undersubscription, usando lower-dimensional school characteristics
- risk-controlled VAM, que agrega funciones de assignment probabilities al control set de un VAM observacional

Nuestro paper actual no esta estimando individual-school causal VAM con IV VAM.
Tampoco esta usando RC VAM como main object.
Nuestro ejercicio principal es validacion/pass-through de value-added indices usando lottery-induced movement along those indices.

### Relevancia para el manuscrito

Este paper deberia anclar:

- la definicion de assignment risk
- por que simulated assignment probabilities son controles suficientes en centralized assignment
- por que `expected_VA_i` es el control correcto cuando el tratamiento es una caracteristica escalar de la escuela
- la interpretacion de `theta` como forecast/pass-through coefficient
- la advertencia de que el paper no estima un efecto causal separado de cada escuela
- la distincion entre validating observational VA y constructing causal school report cards

## 2. Walters - Empirical Bayes Methods in Labor Economics

Referencia local: `writing/methods/papers/HOLE_EBchapter.pdf`

Este capitulo es la referencia principal para pensar que hacer con school effects ruidosos, shrinkage, distribuciones latentes, rankings, y regresiones que usan value-added estimates.

### Idea general

EB methods son utiles cuando hay muchos parametros unit-specific, como schools, teachers, firms, neighborhoods, doctors, judges, o hospitals.
El problema comun es que cada unidad tiene una estimacion ruidosa de un parametro verdadero.
EB usa la estructura comun de muchas unidades para estimar una prior/mixing distribution y luego construir posterior predictions para cada unidad.

El capitulo insiste en que EB puede servir para tres objetivos distintos:

- describir heterogeneidad en efectos verdaderos
- predecir el parametro de una unidad especifica
- tomar decisiones o clasificar unidades

Estos objetivos no usan necesariamente el mismo objeto estadistico.

### La receta EB

La receta basica tiene tres pasos:

1. Estimation: estimar `theta_hat_j` y su standard error `s_j` para cada unidad.
2. Deconvolution: estimar la distribucion latente `G` de los parametros verdaderos `theta_j`.
3. Shrinkage: usar `G_hat` como prior para formar posterior distributions o posterior means.

En el caso normal-normal, el posterior mean es una combinacion ponderada de `theta_hat_j` y la media de la prior.
El peso en `theta_hat_j` es mayor cuando la estimacion es precisa o cuando la varianza latente es grande.

### James-Stein y compound decision

Shrinkage puede mejorar MSE agregado incluso fuera del modelo normal exacto.
La intuicion de compound decision es que el investigador enfrenta muchos problemas paralelos, no uno solo.
Cuando hay muchas unidades similares, aceptar un poco de bias hacia la media puede reducir mucho variance y MSE agregado.

Esto es util para report cards o decision rules, pero no implica que todo analisis de VA deba usar posterior means.

### Tres objetos distintos

Una advertencia central:

- efectos verdaderos latentes: `theta_j`
- estimates insesgados pero ruidosos: `theta_hat_j`
- posterior means shrinked: `theta_star_j`

La distribucion de posterior means no reproduce la distribucion de efectos verdaderos.
En particular, la varianza de los posterior means suele ser menor que la varianza latente.
Por lo tanto, no conviene describir dispersion de school effects usando simplemente la varianza de estimates shrinked.

Para nuestro paper, esto importa al describir la distribucion de value-added.
Si mostramos dispersion de VA, hay que ser claro si es dispersion de fixed effects estimados, de estimates shrinked, o de efectos latentes bias-corrected.

### Shrinkage and regression

Esta es probablemente la advertencia mas importante para nuestras revisiones:

`shrinkage on the right fixes bias, but shrinkage on the left causes bias`

Si un estimate ruidoso entra como variable explicativa, classical measurement error attenua el slope.
Usar shrinkage en el RHS puede ayudar a reducir ese attenuation bias.

Pero si el estimate shrinked entra como variable dependiente, shrinkage introduce attenuation porque el posterior mean esta movido hacia la media.

Esto importa si usamos school VA en regresiones posteriores:

- VA como RHS treatment/index: shrinkage puede tener sentido dependiendo del objetivo
- VA como LHS o para describir heterogeneidad: shrinkage puede distorsionar el objeto

El capitulo tambien enfatiza que poner posterior means en regresiones no resuelve automaticamente measurement-error problems.
El uso correcto depende del estimand.

### Long regression vs residual means

En VA, se puede:

- estimar una long regression con group fixed effects y controls juntos
- partial out controls primero y luego promediar residuals por unidad

Walters recomienda usualmente la long regression si los group effects pueden estar correlacionados con los controls.
Esto calza con nuestro enfoque de estimar school fixed effects en una regresion individual-level con controles, en vez de residualizar de manera separada y despues promediar.

### Multivariate EB and combining estimators

El capitulo extiende EB a:

- multiples outcomes por unidad
- covariates de unidad
- multiples estimadores del mismo parametro
- combinaciones de estimadores sesgados y no sesgados

La discusion de combining estimators conecta con Angrist et al. cuando se combinan VAM observacionales precisos pero potencialmente biased con estimates quasi-experimentales menos precisos pero mas creibles.
Nuestro paper actual no implementa esta combinacion.
Pero sirve para explicar que nuestro objetivo es validation/pass-through, no construir posterior causal school quality estimates para cada escuela.

### Precision dependence

Muchos modelos EB asumen independencia entre effect sizes y standard errors.
Esto puede fallar si, por ejemplo, escuelas grandes o con estimates mas precisos tambien son sistematicamente mejores o peores.
Walters discute tests y modelos para precision-dependence.

Para nuestro paper, esto sugiere cautela al interpretar dispersion de school VA cuando la precision depende de school size o composition.
Tambien aconseja no usar shrinkage mecanicamente sin revisar si precision y effect size estan relacionados.

### Nonparametric EB and bias-corrected variance

El capitulo presenta metodos no parametricos para estimar la mixing distribution:

- NPMLE
- log-spline deconvolution
- bias-corrected variance components

La idea importante para nosotros es que, si el objetivo es describir heterogeneidad latente, hay que corregir la varianza por sampling error.
No basta con mirar la varianza de estimates observados ni la varianza de posterior means.

### Ranking and decision rules

Walters advierte contra la "league table mentality".
Si el objetivo es rankear unidades o seleccionar unidades extremas, posterior means, posterior probabilities, q-values, y ranking intervals responden preguntas distintas.

Nuestro paper no debe sonar como un ranking/report-card exercise de escuelas.
El foco actual es si los indices de VA tienen contenido causal en el margen de SAE, no producir una tabla de mejores y peores escuelas.

### Relevancia para el manuscrito

Este capitulo deberia anclar:

- por que los school fixed effects son ruidosos
- por que EB shrinkage es util para prediccion pero no automaticamente para distribuciones o regressions
- por que conviene distinguir estimates, latent effects, y posterior means
- por que no conviene describir nuestro paper como ranking individual de escuelas
- por que el ejercicio orthogonal/math-STEM debe preocuparse por sampling error covariance

## 3. Rose, Schellenberg, and Shem-Tov - The Effects of Teacher Quality on Adult Criminal Justice Contact

Referencia local: `writing/methods/papers/rss_teachers_cjc.pdf`

Este paper es la referencia mas cercana para nuestro ejercicio de value-added multidimensional y orthogonal VA.
Aunque estudia teachers y criminal justice contact, el punto metodologico central aplica a schools y outcomes multiples.

### Idea general

El paper desarrolla metodos EB-free para estimar efectos multidimensionales de teachers.
El objetivo no es predecir el efecto de cada teacher individual, sino estimar la variance-covariance structure de los efectos latentes.

La motivacion es que los EB posteriors son utiles para prediccion, pero sus covariances no identifican necesariamente las covariances de efectos verdaderos.
Esto es especialmente importante cuando se pregunta si efectos en test scores estan relacionados con efectos en outcomes no testeados o de largo plazo.

### Efectos causales y observacionales

El paper distingue:

- efectos causales de teachers
- efectos observacionales/projection effects
- bias o sorting no observado

Bajo su Assumption 1, los efectos observacionales coinciden con efectos causales.
Bajo una assumption mas debil, las varianzas observacionales pueden ser lower bounds de varianzas causales, pero las covarianzas pueden diferir dependiendo de como se correlaciona el bias entre outcomes.

Esto es util para nuestro paper porque el math/STEM covariance object puede no tener interpretacion causal pura si los VA observacionales tienen residual selection.
Nuestro IV/pass-through luego valida si esos indices tienen causal predictive content.

### Parametros de interes

El paper se enfoca en la variance-covariance matrix de efectos.

- La varianza mide cuanta heterogeneidad hay en effects sobre un outcome.
- La covarianza/correlacion mide si las unidades que mejoran un outcome tambien mejoran otro.

Este es exactamente el tipo de pregunta detras de "es STEM VA simplemente math VA?".
No queremos solo estimates individuales de schools.
Queremos saber la relacion entre dimensiones de school value.

### Estimacion EB-free de variance-covariance

Los fixed effects estimados son ruidosos.
La varianza naive de estimates observados sobrestima la varianza latente porque incluye sampling error.
La covarianza naive tambien puede estar sesgada si hay correlated sampling error entre outcomes.

El paper usa productos cross-year/cross-cohort para estimar covarianzas de efectos latentes, evitando same-year sampling error covariance.
Equivalentemente, puede verse como tomar covarianza de efectos estimados y restar una correction por correlated sampling variation.

Este es el fundamento de nuestro ejercicio:

`stem_va_orth_math = stem_va - phi * math_va`

donde `phi` deberia estimarse usando una covarianza math-STEM que intenta capturar latent signal, no covarianza mecanica de ruido.

### Relacion entre efectos de corto y largo plazo

El paper muestra que se puede usar la variance-covariance matrix para estimar regressions "infeasibles" entre efectos verdaderos.
Por ejemplo, el slope de efectos long-run sobre efectos short-run es:

`Cov(alpha_C, alpha_A) / Var(alpha_A)`

Esto es conceptualmente parecido a proyectar STEM VA sobre math VA y quedarse con el componente orthogonal.

El paper tambien muestra que test-score effects pueden ser casi orthogonal a criminal justice effects, mientras behavior effects predicen criminal justice outcomes.
El paralelo para nuestro paper es: math VA puede no resumir STEM enrollment VA.

### Validacion

El paper valida sus efectos con varios ejercicios:

- omitted variable tests
- forecast unbiasedness tests using instruments
- checks that teacher effects are not really school effects
- sensitivity to assumptions about cross-year independence and drift
- robustness across many control specifications

Esto sugiere que nuestro paper deberia separar:

- la estimacion de VA observacional
- la validacion causal via SAE
- el ejercicio orthogonal como una prueba de multidimensionalidad, no como prueba unica de mecanismos

### Appendix B: covariance of EB posteriors

Este apendice es muy importante.
Muestra que la covariance de EB posterior means no identifica la covariance de efectos latentes.
El bias puede venir de:

- shrinkage factors que atenun covariances
- correlated sampling error entre outcomes

El signo del bias puede ser ambiguo.
En simulaciones calibradas al paper, covariances de EB posteriors pueden estar muy sesgadas e incluso tener signo incorrecto.
El problema persiste aun con muestras grandes por teacher si hay correlated sampling error.

Para nuestro paper, esto justifica no usar simplemente la correlacion entre EB-shrunken school VA estimates para decir que math y STEM school effects son orthogonal.
La relacion entre dimensiones debe construirse con un metodo que corrija sampling error covariance o use productos cross-cohort.

### Inference

El paper usa representaciones U-statistic para standard errors de variance/covariance estimators.
Para objetos mas complicados, usa parametric bootstrap sobre la sampling variance-covariance estimada.

Si nuestro ejercicio orthogonal se vuelve central, convendria documentar claramente como se calculan standard errors o uncertainty para el projection coefficient y la tabla IV.

### Relevancia para el manuscrito

Este paper deberia anclar:

- la motivacion para value-added multidimensional
- la frase de que effects on one dimension need not proxy effects on another
- la decision de estimar math/STEM covariance con un metodo EB-free
- la advertencia contra usar covariance de EB posteriors
- la interpretacion de orthogonal STEM VA como dimension no explicada por math VA
- la cautela de que "orthogonal to math VA" no identifica por si solo un canal causal especifico

## Implicancias para nuestro paper

### Como deberia escribirse la metodologia principal

La metodologia puede presentarse como una version de school-characteristic IV en centralized assignment.
Para cada outcome `m`, construir `V_s^m`.
Luego definir:

- `A_i^m = V_{D_i}^m`: value-added de la escuela asistida
- `O_i^m = V_{Z_i}^m`: value-added de la escuela ofrecida
- `E_i^m = sum_s p_is V_s^m`: expected value-added bajo assignment risk

La ecuacion IV estima si movement induced by offers toward higher `V_s^m` causa aumento en `Y_i^m`.
El parametro `theta^m` puede describirse como pass-through o forecast coefficient.

### Que no deberiamos decir

No decir que el paper estima un efecto causal separado de cada escuela.
El estimand principal es el efecto de moverse a lo largo de un indice escalar de school value.

No decir que el expected-VA control es un shortcut ad hoc.
Es el analog de controlar `sum_j C_j p_ij` para una caracteristica escalar de escuelas en centralized assignment.

No describir la distribucion de school VA como distribucion de efectos causales latentes si son fixed effects observacionales ruidosos.
Hay que ser claro si se reportan estimated VA measures, student-weighted distributions, o latent variance-corrected objects.

No usar covariances de EB posterior means como evidencia directa sobre covariances latentes entre math y STEM.
Si se habla de orthogonal VA, hay que anclarlo en el metodo EB-free/cross-cohort.

No vender el ejercicio orthogonal como identificacion definitiva del mecanismo.
Muestra que STEM VA contiene una dimension no explicada por math VA; no dice por si solo si el canal es counseling, information, confidence, peers, preparation, or expectations.

### Posibles cambios utiles al manuscrito

1. En la seccion de assignment risk, citar y explicar explicitamente el caso de school-characteristic IV:
   `C_{z(i)}` instruments `C_{d(i)}`, controlling for `sum_j C_j p_ij`.
2. Renombrar o conectar `theta` con "forecast coefficient" de Angrist-Hull-Walters.
3. Aclarar que el paper valida observational VA on average along the lottery margin.
4. Incluir una breve nota de que el paper no aplica EB shrinkage para producir report cards.
5. En orthogonal VA, explicar por que la covariance math-STEM se estima con cross-cohort/EB-free logic y no con covariance de posterior means.
6. Separar "multidimensional school value" de "mechanism" para no sobrerreclamar.

### Preguntas pendientes para revision

- Debemos llamar `theta` principalmente "pass-through coefficient", "forecast coefficient", o ambos?
- Queremos explicar explicitamente por que no usamos EB shrinkage en las main VA measures?
- El table/figure de VA distribution deberia decir mas claramente que reporta estimated observational VA, not latent causal school effect distribution?
- El orthogonal VA section deberia incluir mas detalles sobre cross-cohort covariance correction en el texto principal o dejarlo para appendix?
- Queremos presentar expected-VA controls como main identification object y dejar wide probability controls solo como robustness/legacy?
