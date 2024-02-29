"""
A set of simple arithmetic functions.

Todo:
    subtraction & division functions.
"""

__author__ = "Richard Thomas"
__email__ = "richard.thomas@uq.edu.au"
__date__ = "08/03/2021"
__copyright__ = "Copyright, University of Queensland"


def add(x: int, y: int) -> int :
    """Add two values 'x' and 'y' and return the result."""
    return x + y


def multiply(x: int, y: int) -> int :
    """Multiply two numbers 'x' and 'y' and return the result.

    Uses add(x, y) function to perform multiplication.
    This is not efficient but enforces arithmetic multiplication.

    Preconditions:
        x >= 0
    """
    result = 0
    while x > 0 :
        result = add(result, y)
        x = x - 1
    return result


def integer_type(n) :  
    """Return if n is 'positive', 'zero' or 'negative'."""  
    if n > 0 :  
        result = 'positive'  
    elif n < 0 :  
        result = 'negative'  
    else :  
        result = 'zero'  
    return result  


def factorial(n) :
    """Calculate and return the factorial of 'n'.

    Calculates the product of all positive integers less than or equal to 'n'.

    Parameters:
        n (int): A positive integer or 0.

    Return:
        int: Result of factorial calculation.

    Preconditions:
        n >= 0

    Examples:
        >>> factorial(0)
        1
        >>> factorial(1)
        1
        >>> factorial(2)
        2
        >>> factorial(3)
        6
        >>> factorial(4)
        24
    """
    result = 1
    while n >= 1 :
        result = result * n
        n = n - 1
    return result

def substract(x: int, y: int) -> int:
    return x - y

def division(x: int, y: int) -> int:
    result = 0
    while x > 0:
        x = substract(x, y)
        result = result + 1
    return result

