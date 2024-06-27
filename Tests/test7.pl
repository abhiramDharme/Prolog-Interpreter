# write prolog code below

% Test for the case where the input is a list of numbers

% Test case 1
student("Alice", 1).
student("Bob", 2).
student("Charlie", 3).
student("David", 4).
student("Eve", 5).

linear_search([], _, _) :- false.
linear_search([X|_], X, 1).
linear_search([_|T], X, Y) :- linear_search(T, X, Z), Y is Z + 1.
