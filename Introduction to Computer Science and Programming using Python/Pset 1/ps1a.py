# !/usr/bin/env python3
# -*- coding: utf-8 -*-

''' 
Program finds out how many months it takes to day a downpayment based on total cost of dream house,
annual salary, and portion saved.

Every month, you invest money to increase current savings by a monthly rate and add a certain percentage
of monthly salary to help pay for 
'''

#inputs to put in
total_cost = float(input("Enter Cost of Dream House: "))
annual_salary = float(input("Enter Annual Salary: "))
portion_saved = float(input("Enter Portion of Salary to Be Saved as Decimal: "))

#constants
portion_down_payment = 0.25 * total_cost
current_savings = 0
rate =  0.04
month = 0
monthly_salary = annual_salary / 12

while current_savings < portion_down_payment:
    #increases month counter by 1
    month += 1
    #investments increase savings
    current_savings == (current_savings * (rate / 12))
    #percent of monthly salary is added to savings
    current_savings += (monthly_salary * portion_saved)
    
print("It will take", month, "months to pay the downpayment")
