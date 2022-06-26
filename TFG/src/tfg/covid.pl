%!read confirmedCase(X, Y) as: X is a confirmed case of Y
%!read person(X) as: X is a person
%!read diagnosed(X, Y) as: X is diagnosed with Y
%!read tests(X, Y) as: X has taken a test for Y
%!read testedPositive(X, Y) as: X has tested positive for Y
%!read not(tested_positive(X, Y)) as: X has not tested positive for Y
%!read symptoms(X, Z) as: X has symptoms of Z
%!read not symptoms(X, Z) as: X does not have symptoms of Z
%!read closeContact(X, Z) as: X had close contact with Z
%!read positivePCR(X, Y) as: X has tested positive on PCR for Y
%!read positiveAT(X, Y) as: X has tested positive on an antigen test for Y
%!read lineControl(X, Y) as: control line (C) showed up in X's test for Y
%!read lineTest(X, Y) as: test line (T) showed up in X's test for Y
%!read goodConditionAT(X, Y) as: the antigen test for Y that X has used was in good conditions
%!read not (goodConditionAT(X, Y)) as: the antigen test for Y that X has used was not in good conditions
%!read hospital(X) as: X has been to the hospital
%!read arn(X, Y) as: Y ARN found in X's blood
%!read expiredAT(X, Y) as: the antigen test for Y that X has used is expired
%!read contagious(X, Y) as: X can spread the Y virus
%!read highViralLoad(X, Y) as: X has high viral load of Y
%!read negativeAT(X, Y) as: X has tested negative on an antigen test for Y
%!read disease(Y) as: Y is a well-known disease
%!read quarantine(X, Y) as: X has to keep in quarantine for Y
%!read risk(X, Y) as: X has a higher risk of severe illness from Y

disease(covid19).
person(john).
person(mike).
person(elon).
person(amber).
person(laura).
hospital(elon).
hospital(mike).
arn(mike, covid19).
arn(elon, covid19).
risk(elon, covid19).
risk(john, covid19).
symptoms(mike,covid19).
symptoms(laura, covid19).
closeContact(amber, elon).
lineTest(mike, covid19).
expiredAT(john, covid19).
expiredAT(amber, covid19).
lineControl(mike, covid19).
highViralLoad(mike, covid19).
goodConditionAT(mike, covid19).

confirmedCase(X, Y):-
  person(X),
  diagnosed(X, Y).

diagnosed(X, Y) :-
  tests(X, Y),
  testedPositive(X, Y).

tests(X, Y) :-
  risk(X, Y),
  disease(Y).

tests(X, Y) :-
  symptoms(X, Y),
  disease(Y).

tests(X, Y) :-
  closeContact(X, Z),
  testedPositive(Z, Y),
  disease(Y),
  not symptoms(X, Y).

testedPositive(X, Y) :- 
  positivePCR(X, Y). 
 
testedPositive(X, Y) :- 
 positiveAT(X, Y).

positiveAT(X, Y) :-
  lineControl(X, Y),
  lineTest(X, Y),
  goodConditionAT(X, Y).

positivePCR(X, Y) :-
  hospital(X),
  arn(X, Y).
 
negativeAT(X, Y) :-
 expiredAT(X, Y),
  not symptoms(X, Y).

negativeAT(X, Y) :-
  disease(Y),
  not goodConditionAT(X, Y).

quarantine(X, Y) :-
  contagious(X, Y),
  testedPositive(X, Y),
  disease(Y).

quarantine(X, Z) :- 
   closeContact(X, Y),
   confirmedCase(Y, Z).

contagious(X, Y) :-
  symptoms(X, Y),
   highViralLoad(X, Y).

% Since tests have a failure rate, it would be
% convenient in the future to study possibilities like this one but with
% different odds: positive_AT(X) :- fake_positive_AT(X).

% query: run([confirmedCase(mike, covid19)])
% query:  run([confirmedCase(elon, covid19)])
% query: run([quarantine(mike, covid19)])
% query: run([quarantine(amber, covid19)])
% query: run([tests(john, covid19)])

