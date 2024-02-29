import random
i=0
while i<10:
    i+=1
    x = random.randint(1, 100)
    y = random.randint(1, 100)
    total = x+y

    sum_input = int(input('{}+{}='.format(x,y)))

    if (sum_input == total):
        print("Correct!")
    else:
        print("No, the correct answer is", total)




