# Contsruindo uam calculador apenas com as funções do base R

print("Escolhas possíveis")
print("1. Adição")
print('2. Subtração')
print("3. Divisão")
print('4. Multiplicação')

escolha <- as.integer(readline(prompt = "Escolha sua operação: "))

num1 <- as.double(readline(prompt = "Digite o primeiro número: "))
num2 <- as.double(readline(prompt = "Digite o segundo número: "))

resultado <- switch(escolha, (num1+num2), (num1-num2),
                    (num1/num2), (num1*num2))

print(paste("o resultado é: ", resultado))
