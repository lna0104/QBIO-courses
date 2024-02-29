import random


def average_three_numbers(number_a, number_b, number_c):
    """ Function that takes in three arguments,
    and returns the average of the three arguments
    
    Parameters:
        number_a: the first number
        number_b: the second number
        number_c: the third number

    Return:
        float: The average of the three numbers
        
    """
    return (number_a + number_b + number_c) / 3


def input_number():
    """ Function that asks the user to input a number
    between 0 and 10, and prints "Error" if the user
    enters a number greater than 10.
    """
    number = float(input("Please enter a number between 0 and 10: "))

    if number > 10:
        print("Error")


def sum_even(n):
    """ Function that returns the sum of all even numbers between 1
    and the number n
    
    Parameters:
        n: all even numbers between 1 and this number will be summed

    Return:
        int: The sum of all even numbers between 1 and n

    """
    total = 0  
    for m in range(2, n + 1, 2):
        total = total + m
    return total


def sum_even2(n):
    """ Function that returns the sum of all even numbers between 1
    and the number n
    
    Parameters:
        n: all even numbers between 1 and this number will be summed

    Return:
        int: The sum of all even numbers between 1 and n

    """
    total = 0
    count = 1

    while count <= n:
        if count % 2 == 0:
            total += count
        count += 1

    return total 


def move(position, coin_toss_result):
    """ Function that increments the position by 1 if the
    coin_toss_result is a head, and decrements the position by 1
    if the coin_toss_result is a tail

    Parameters:
        position: The position
        coin_toss_result: The result of the coin toss

    Return:
        int: The position
        
    """
    if coin_toss_result == 'h':
        position = position + 1
    else:
        position = position - 1

    return position

