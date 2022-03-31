#	Exercicios	Capitulo	4

#	Exercicio	1	- Encontre	e	faça	a	correção	do	erro	na	instrução	abaixo:

write.table(mtcars,	file	=	"mtcars2.txt",	sep	=	"|",	col.names	=	NA,	qmethod	=	"double")

#	Exercicio	2	- Encontre	e	faça	a	correção	do	erro	na	instrução	abaixo:

df_iris	<- read_csv("iris.csv",	col_types	=	list(
  Sepal.Length	=	col_double(),
  Sepal.Width	=	col_double(),
  Petal.Length	=	col_double(),
  Petal.Width	=	col_double(),
  Species	=	col_factor(c("setosa",	"versicolor",	"virginica"))
))

#	Exercicio	3	- Encontre	e	faça	a	correção	do	erro	na	instrução	abaixo:

df2	<- read.xlsx("UrbanPop.xlsx",	sheetIndex	=	1)

#	Exercicio	4	- Encontre	e	faça	a	correção	do	erro	na	instrução	abaixo:
df_sono	<- read_csv("C:/RFundamentos/parte 3/sono.csv")
sleepData	<- select(df_sono,	nome,	sono_total)
df_sono	%>%	
  select(nome,	cidade,	sono_total)	%>%
  arrange(cidade,	sono_total)	
head(df_sono)

#	Exercicio	5	- Encontre	e	faça	a	correção	do	erro	na	instrução	abaixo:

set.seed(1)
df3	<- data.frame(
  participante	=	c("p1", "p2",	"p3",	"p4",	"p5",	"p6"),	
  info	=	c("g1m",	"g1m",	"g1f",	"g2m",	"g2m",	"g2m"),
  day1score	=	rnorm(n	=	6,	mean	=	80,	sd	=	15),	
  day2score	=	rnorm(n	=	6,	mean	=	88,	sd	=	8)
)

df3	%>%
  gather(day,	score,	c(day1score,	day2score))	%>%
  separate(col	=	info,	into	=	c("group",	"gender"),	sep	=	2)	%>%
  ggplot(aes(x	=	day,	y	=	score))	+
  geom_point()	+	
  facet_wrap(~	group)	+
  geom_smooth(method	=	"lm",	aes(group	=	1),	se	=	F)