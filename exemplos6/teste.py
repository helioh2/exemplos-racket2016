
print("Hello world")

x = 2
print(x)

y = 10
print(x+y*10-4)

if y > 7:
    print("ok")
else:
    print("nao")

while y>=0:
    print(y)
    y -= 1

# soma: Numero Numero -> Numero
def soma(n1, n2):
    return n1+n2

resultado = soma(2,7)

# somatorio : List[Numero] -> Numero
def somatorio_v1(lst):
    soma = 0
    for item in lst:
        #soma = soma + item
        soma += item
    return soma

print(somatorio_v1([1,2,3,4,100]))


def somatorio_v2(lst):
    soma = 0
    for i in range(len(lst)):
        soma += lst[i]
    return soma

print(somatorio_v2([1,2,3,4,100]))

import operator
def somatorio_v3(lst):
    return reduce(operator.add,lst)


print(somatorio_v3([1,2,3,4,100]))















    
    



