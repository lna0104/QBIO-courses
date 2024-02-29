#
# Program to calculate a person's tax
#
deduction_clothes=float(input('Enter deduction for clothes: '))
deduction_car=float(input('Enter deduction for car: '))
deduction_dependents=float(input('Enter deduction for dependents: '))
#
gross_income=float(input('Enter Gross Income: '))
tax_free_threshold=float(input('Enter tax free threshold: '))
tax_rate=float(input('Enter tax rate: '))
#
taxable_income=gross_income-tax_free_threshold-(deduction_clothes+deduction_car+deduction_dependents)
tax_payable=tax_rate*taxable_income
print('Tax payable=', tax_payable,' Taxable_income=',taxable_income, sep='')
