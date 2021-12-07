Para deixarmos claro as notações que iremos adotar adiante vamos definir que:
  
  + Temos $k = `r k`$ grupos e para cada grupo $n =  `r n`$. Denotamos as observações dentro de cada grupo de $n_{ij}$ em que $i$ é a *i-ésima* observação ($i: 1 a `r n`$) do *j-ésimo* grupo ($j: 1 a `r k`$). Neste exemplo, o número de observações em cada grupo é o mesmo ($n_1 = n_2 = n_3 = n$). Deste modo temos o total de observações dados por $N = k \times n = n_1 + n_2 + n_3 = `r N`$.
  
  + A média de cada grupo será denotada por $\overline{Y}_j$ ($Y_1 = `r Ymeans[1]`$ - grupo A; $Y_2 = `r Ymeans[2]`$ - grupo B; $Y_3 = `r Ymeans[3]`$ - grupo C).
  
  + Vamos denotar por $\overline{\overline{Y}}$ como a **Grande Média**, isto é a média geral de todas as observações independente do grupo de origem.
  
  $\overline{\overline{Y}} = \sum_{j = 1}^{k}\sum_{i = 1}^{n}\frac{Y_{ij}}{N} = \frac{\overline{Y_1} + \overline{Y_2} + \overline{Y_3}}{3} = `r Yg`$