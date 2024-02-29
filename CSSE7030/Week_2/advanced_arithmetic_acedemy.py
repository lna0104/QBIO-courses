import random
i=0
while i<10:
    i+=1
    operators = ['+', '-', '*', '//', '**', '%']
    op = random.choice(operators)

    x = random.randint(10, 99)

    if op == '**':
        y = random.randint(0, 3)
    else:
        y = random.randint(10, 99)

    cal_result = eval('{}{}{}'.format(x, op, y))
    input_result = int(input('{}{}{}='.format(x,op,y)))
    if (input_result == cal_result):
        print("Correct!")
    else:
        print("No, the correct answer is", cal_result)




