
'''
Program uses bisectional search to find the best rate in order to save for a downpayment.
Includes a semi-annual salary raise and investments to increase savings
'''

#inputs to put in
annual_salary = float(input("Enter Annual Salary: "))

#constants
standard_as = annual_salary
total_cost = 1000000
portion_down_payment = 0.25 * total_cost
current_savings = 0
rate =  0.04
month = 0
monthly_salary = annual_salary / 12
semi_annual_raise = 0.07
high = 1
low = 0
portion_saved = (high + low) / 2
bisection_search = 0
epsilon = 0.01

#checks to make sure rate is close enough
while current_savings - portion_down_payment <= epsilon:
    #if more than 100% of monthly salary needs to go into savings to reach goal, prints not possible
    if portion_saved >= 1:
        print("Not Possible")
        break
    month += 1
    #increases annual salary every 6 months
    if month % 6 == 0 and month != 0:
        annual_salary *= (1 + semi_annual_raise)
        monthly_salary = annual_salary / 12
    current_savings += (current_savings * (rate / 12))
    current_savings += (monthly_salary * portion_saved)
    #if rate is too high, makes rate lower
    if current_savings > portion_down_payment and month < 36:
        high = portion_saved
        portion_saved = (high + low) / 2
        month = 0
        current_savings = 0
        bisection_search += 1
        annual_salary = standard_as
    #if rate is too low, makes rate higher
    elif current_savings < portion_down_payment and month >= 36:
        low = portion_saved
        portion_saved = (high + low) / 2
        month = 0
        current_savings = 0
        bisection_search += 1
        annual_salary = standard_as
        
print("Best Rate:", portion_saved)
print("Number of Bisection Searches:", bisection_search)

