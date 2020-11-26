'''
Program finds how many months it takes to save for a downpayment with a certain percentage of 
monthly salary and an annual salary raise every 6 months. Every month, investments are raised which 
increases savings by a certain percentage
'''

#inputs to put in
total_cost = float(input("Enter Cost of Dream House: "))
annual_salary = float(input("Enter Annual Salary: "))
portion_saved = float(input("Enter Portion of Salary to Be Saved as Decimal: "))
semi_annual_raise = float(input("Enter Semi-Annual Pay Raise as Decimal: "))

#constants
portion_down_payment = 0.25 * total_cost
current_savings = 0
rate =  0.04
month = 0
monthly_salary = annual_salary / 12


while current_savings < portion_down_payment:
    #increases month counter by 1
    month += 1
    #if month is divisible by 6 and not 0, semi annual salary increase
    if month % 6 == 0 and month != 0:
        annual_salary *= (1 + semi_annual_raise)
        monthly_salary = annual_salary / 12
    #investments increase savings
    current_savings += (current_savings * (rate / 12))
    #percent of monthly salary is added to savings
    current_savings += (monthly_salary * portion_saved)
    
print("It will take", month, "months to pay the downpayment")
print(current_savings)

