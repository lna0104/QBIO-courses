"""Simulate pirate coin toss game.

Pirate moves one step forward when coin is heads.
Pirate moves one step backwards when coin is tails.
Pirate cannot step off the beginning of the plank and back into ship.
Pirate falls into water when they step off the end of the plank.

Calculate the average number of coin tosses that are needed for a pirate to
walk off the end of the plank, for a specified plank length.

Algorithm:
    1 : toss a coin
    2 : add one to total
    3 : move pirate according to rules
    4 : has pirate fallen in the water?
    5 : if no go to 1
    6 : if yes and no more trials - compute average
    7 : if no - add one to num of trials - go to 1

Coin toss values could be "head" or "tail", 'h' or 't', True or False, or 1 or 0.
Commonly we would use 1 or 0 as it can be viewed as a binary value.
(That is value that can only be 1 or 0.)
"""

__author__ = "Richard Thomas"
__email__ = "richard.thomas@uq.edu.au"
__date__ = "10/03/2021"
__copyright__ = "Copyright, University of Queensland"


import random


HEAD = 1
TAIL = 0


def toss() -> int:
    """Return a head or tail (head = 1, tail = 0)."""
    return random.randint(0, 1)


def move(position: int, coin: int) -> int :
    """Return the new position of the pirate given current position
       and coin toss.

    Pirate cannot step back into boat from the plank, so a tail when pirate is
    at the start of the plank returns the pirate staying at the same position.

    Parameters:
        position: Current position of pirate on plank.
        coin: Coin toss result (head = 1, tail = 0).

    Return: New position of pirate on plank.

    Preconditions:
        TAIL <= coin <= HEAD and position >= 0
    """
    if coin == HEAD :
        position += 1
    elif position > 0 :  # TAIL and not at beginning of plank.
        position -= 1
    # else TAIL and at the beginning of plank, so position already equals 0.

    return position


def trial(plank_length: int) -> int :
    """Return the number of coin tosses needed for pirate to walk off plank.

    Parameters:
        plank_length: Length of the plank.

    Return: Number of coin tosses until pirate walked off plank.

    Preconditions:
        plank_length > 0
    """
    num_tosses = 0
    position = 0
    # Pirate walks off plank when position == plank_length.
    while position < plank_length :
        position = move(position, toss())
        num_tosses += 1
    return num_tosses


def get_average(total_trials: int, plank_length: int) -> int :
    """Return the average number of coin tosses across several trials.

    Parameters:
        total_trials: Total number of trials to try.
        plank_length: Length of the plank.

    Return: Average number of coin tosses until pirate walked off plank.

    Preconditions:
        total_trials > 0 and plank_length > 0
    """
    num_trials = 0
    total_tosses = 0
    while num_trials < total_trials :
        num_trials += 1
        total_tosses += trial(plank_length)

    return total_tosses / num_trials


def main() -> None:
    another_try = 'y'
    
    while another_try == 'y' :
        print()
        print("Simulation program that experimentally demonstrates the number",
              "of coin tosses required for a 'pirate' to walk off the end",
              "of a plank of a specified length.")
        print()
        num_trials = int(input("Please enter the number of trials to attempt: "))
        plank_length = int(input("Please enter the length of the plank: "))
        
        average_tosses = get_average(num_trials, plank_length)

        print()
        print("For a plank of", plank_length, "steps, it would require",
              average_tosses, "coin tosses for the 'pirate' to walk off the",
              "end of the plank.")
        print()
        another_try = input("Do you want to try again (Y/N)? ")
        another_try = another_try.lower()


if __name__ == "__main__" :
    main()
