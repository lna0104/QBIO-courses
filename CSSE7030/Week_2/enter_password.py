true_password = "Nln0104"
while True:
    enter_password = input("Please enter the password: ")
    if enter_password == "secret":
        break
    if enter_password == true_password:
        print("You have entered the correct password.")
        break
    else: 
        print("Sorry, the password is incorrect. Please try again.")