"""Simple simulation of a banking application."""

__author__ = "Richard Thomas"
__date__ = "03/03/2021"
__copyright__ = "The University of Queensland"


# Banking options that the user may choose to do.
BALANCE_QUERY = '1'
TRANSFER_BETWEEN_ACCOUNTS = '2'
BPAY_PAYMENT = '3'
QUIT = '4'

# User's accounts initialised with current balances.
# Balances are stored as cents, to avoid rounding errors with floats.
savings = 10000
high_interest = 100000
mastercard = -10000
CENTS_IN_DOLLAR = 100

# Choice entered by the user.
user_choice = ""

while user_choice != QUIT :
    # Display the banking simulation menu.
    print("\nDo you wish to")
    print("  ", BALANCE_QUERY, ") See your account balances", sep="")
    print("  ", TRANSFER_BETWEEN_ACCOUNTS,
          ") Transfer money between your accounts", sep="")
    print("  ", BPAY_PAYMENT, ") Make a BPay payment", sep="")
    print("  ", QUIT, ") Quit", sep="")
    user_choice = input("Please enter your choice: ")

    # Display the user's account balances.
    if user_choice == BALANCE_QUERY :
        print("\nYour account balances are:")
        print("  $", savings / CENTS_IN_DOLLAR,
              " in your Savings Account", sep='')
        print("  $", high_interest / CENTS_IN_DOLLAR,
              " in your High Interest Savings Account", sep='')
        print("  $", mastercard / CENTS_IN_DOLLAR,
              " in your Mastercard Account", sep='')

    # Make a BPay payment.
    elif user_choice == BPAY_PAYMENT :
        bpay_payee = input("\nPlease enter BPay code of payee: ")
        print("\nSelect the account from which you wish to pay")
        from_account = input("  1) Savings,  2) High Interest > ")
        if from_account == '1' :
            amount = input("Please enter the amount, in cents, to pay: ")
            amount = int(amount)
            # Code to look up BPay Payee's account from their BPay code,
            # and then to transfer money to their account.
            savings -= amount
        elif from_account == '2' :
            amount = input("Please enter the amount, in cents, to pay: ")
            amount = int(amount)
            # Code to look up BPay Payee's account from their BPay code,
            # and then to transfer money to their account.
            high_interest -= amount
        else :
            print("That is not a valid account option")

    # Transfer money between the user's accounts.   
    elif user_choice == TRANSFER_BETWEEN_ACCOUNTS :
        print("\nSelect the account from which you wish to pay")
        from_account = input("  1) Savings,  2) High Interest > ")
        if from_account == '1' :
            print("\nSelect the account to which you wish to transfer")
            to_account = input("  1) High Interest,  2) Mastercard > ")
            if to_account == 1:
                amount = input("Please enter the amount to transfer: ")
                amount = int(amount)
                savings -= amount
                high_interest += amount
            elif to_account == 2:
                amount = input("Please enter the amount to transfer: ")
                amount = int(amount)
                savings -= amount
                mastercard += amount
            else :
                print("That is not a valid account option") 
        elif from_account == '2' :
            print("\nSelect the account to which you wish to transfer")
            to_account = input("  1) Savings,  2) Mastercard > ")
            if to_account == 1:
                amount = input("Please enter the amount to transfer: ")
                amount = int(amount)
                high_interest -= amount
                savings += amount
            
            elif to_account == 2:
                amount = input("Please enter the amount to transfer: ")
                amount = int(amount)
                high_interest -= amount
                mastercard += amount
            else :
                print("That is not a valid account option")
        else :
            print("That is not a valid account option")


    # Check if the user entered an invalid menu option.
    elif user_choice != QUIT :
        print(user_choice, "is not a valid option.")
