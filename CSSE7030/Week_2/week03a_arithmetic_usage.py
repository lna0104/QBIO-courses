"""Simple demonstration of the arithmetic module."""

__author__ = "Richard Thomas"
__email__ = "richard.thomas@uq.edu.au"
__date__ = "08/03/2021"
__copyright__ = "Copyright, University of Queensland"


import week03a_arithmetic


def sum_to(n) :
    """Return the sum of numbers from 1 to 'n' inclusive.

    Preconditions:
        n >= 0
    """
    total = 0
    increment = 1
    while n >= increment :
        total = week03a_arithmetic.add(total, increment)
        increment += 1
    return total


def main() :
    """Main program logic."""
    end_of_range = input("\nEnter a positive integer to sum up to: ")
    end_of_range = int(end_of_range)
    while end_of_range > 0 :
        result = sum_to(end_of_range)
        print("The sum of 1 to", end_of_range, "is", result)
        end_of_range = input("\nEnter a positive integer to sum up to: ")
        end_of_range = int(end_of_range)


if __name__ == "__main__" :
    main()
