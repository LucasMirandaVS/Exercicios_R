#	Exercício	1	- Crie	uma	função	que	receba	e	vetores	como	parâmetro,	converta-os	em	um	
# dataframe	e	imprima
vetor1 <- c("cu", "buceta", "pinto murcho", "natimortos", 18)
vetor2 <- c(21:30)

myfunc2 <- function(a, b) {
  df <- as.data.frame(c(a, b))
  print(df)
}
myfunc2(vetor1, vetor2)

#	Exercício	2	- Crie	uma	matriz	com	4	linhas	e	4	colunas	preenchida	com	números	inteiros	e	
# calcule	a	media	de	cada	linha
matriz <- matrix(c(1:16), nr = 4, nc = 4, byrow =T)
matriz

apply(matriz, 1, mean)

#	Exercício	3	- Considere	o	dataframe	abaixo.	Calcule	a	media	por	disciplina
escola	<- data.frame(Aluno	=	c('Alan',	'Alice',	'Alana',	'Aline',	'Alex',	'Ajay'),
                     Matemática	=	c(90,	80,	85,	87,	56,	79),
                     Geografia	=	c(100,	78,	86,	90,	98,	67),
                     Química	=	c(76,	56,	89,	90,	100,	87))

escola$Media = NA
escola

escola$Media = apply(escola[, c(2, 3, 4)], 1, mean)
escola

#	Exercício	4	- Cria	uma	lista	com	3	elementos,	todos	numéricos	e	calcule	a	soma	de	todos	os	
# elementos	da	lista
lista1 <- list(c(1:10),2,c(30:45))
lista1

sum(unlist(lista1))

# SOLUÇÃO DO CURSO: do.call(sum, lista1)

#	Exercício	5	- Transforme	a	lista	anterior	um	vetor
vetor.da.lista <- unlist(lista1)
vetor.da.lista
class(vetor.da.lista)

#	Exercício	6	- Considere	a	string	abaixo.	Substitua	a	palavra	textos	por	frases
str	<- c("Expressoes",	"regulares",	"em	linguagem	R",	
         "permitem	a	busca	de	padroes",	"e	exploracao	de	textos",
         "podemos	buscar	padroes	em	digitos",
         "como	por	exemplo",
         "10992451280")

sub("textos", "frases", str)

#	Exercício 7	- Usando	o	dataset	mtcars,	crie	um	gráfico com	ggplot	do	tipo	scatter	plot.	Use	as	
# colunas	disp	e	mpg	nos	eixos	x	e	y	respectivamente
library(ggplot2)
ggplot(data = mtcars, aes(x = disp, y = mpg)) + geom_point()


#	Exercício 8	- Usando	o	exemplo	anterior,	explore	outros	tipos	de	gráficos
ggplot(data = mtcars, aes(x = wt, y = qsec)) + geom_point()
ggplot(data = mtcars, aes(x = cyl, y = hp)) + geom_point()
ggplot(data = mtcars, aes(x = drat, y = vs)) + geom_point()
ggplot(data = mtcars, aes(x = gear, y = carb)) + geom_point()
