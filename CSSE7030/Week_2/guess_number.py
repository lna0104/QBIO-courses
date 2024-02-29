import random
print("Try to guess the number I am thinking of between 1 and 100.")

true_number = random.randint(1, 100)

while True:
    guess_number = int(input("Please enter your guess: "))
    if (guess_number == -1):
        print("The number you were trying to guess was", true_number)
        break
    if (guess_number == true_number):
        print("Congratulations! You have guessed correctly.")
        break
    print("Sorry that is not correct.")