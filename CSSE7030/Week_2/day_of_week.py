input_year = int(input("Please enter a year: "))
input_month = int(input("Please enter a month: "))
input_day = int(input("Please enter a date: "))

import datetime
ans = datetime.date(input_year, input_month, input_day)
print (ans.strftime("%A"))s