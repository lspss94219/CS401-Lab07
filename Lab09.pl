% ============================================================
% CS401/501 Programming Paradigms - Lab09
% Employee Management System in Prolog
% Author: [Student Name]
% Date: 22 March 2026
% ============================================================

% ------------------------------------------
% Part 2: Employee Facts
% ------------------------------------------
% Each employee is represented by:
% employee(Name, Position, Department, Salary).

employee(john,    manager,  sales, 70000).
employee(susan,   engineer, it,    85000).
employee(mike,    technician, it,  60000).
employee(lisa,    manager,  hr,    75000).
employee(paul,    engineer, sales, 90000).

% ------------------------------------------
% Rule 1: higher_salary(X, Y)
% True if X has a higher salary than Y.
% ------------------------------------------
higher_salary(X, Y) :-
    employee(X, _, _, SalaryX),   % Get salary of X
    employee(Y, _, _, SalaryY),   % Get salary of Y
    SalaryX > SalaryY.            % Compare

% ------------------------------------------
% Rule 2: same_department(X, Y)
% True if X and Y work in the same department,
% and they are not the same person.
% ------------------------------------------
same_department(X, Y) :-
    employee(X, _, Dept, _),      % Department of X
    employee(Y, _, Dept, _),      % Department of Y
    X \= Y.                       % Ensure they are different people

% ------------------------------------------
% Query 3: Find all employees who are managers.
% We define a predicate managers/1 that returns a list of manager names.
% ------------------------------------------
managers(List) :-
    findall(Name, employee(Name, manager, _, _), List).

% Example query (commented):
% ?- managers(Managers).
% Expected: Managers = [john, lisa].

% ------------------------------------------
% Query 4: List employees in the IT department.
% Predicate it_employees/1 returns a list of names working in IT.
% ------------------------------------------
it_employees(List) :-
    findall(Name, employee(Name, _, it, _), List).

% Example query (commented):
% ?- it_employees(IT_List).
% Expected: IT_List = [susan, mike].

% ------------------------------------------
% Part 3: Filter high earners
% high_earners(Threshold, List) unifies List with the names of
% employees whose salary is greater than Threshold.
% ------------------------------------------
high_earners(Threshold, List) :-
    findall(Name, (employee(Name, _, _, Salary), Salary > Threshold), List).

% Sample run (commented):
% ?- high_earners(80000, List).
% List = [susan, paul].

% End of file