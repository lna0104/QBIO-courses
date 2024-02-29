#
# Program to calculate a person's tax
#
deduction_clothes=100
deduction_car=200
deduction_dependents=300
#
gross_income=50000
tax_free_threshold=18000
tax_rate=0.25
#
taxable_income=gross_income-tax_free_threshold-(deduction_clothes+deduction_car+deduction_dependents)
tax_payable=tax_rate*taxable_income
print('Tax payable=', tax_payable,' Taxable_income=',taxable_income, sep='')
