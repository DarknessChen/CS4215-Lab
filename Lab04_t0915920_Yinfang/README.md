This code supports the Programming Language Implementation course
It contains a bunch of APIs for computing various semantics described in the course.
Examples for using these APIs are in the corresponding test packages.

In this tutorial, we will be practicing type inference and simple evaluation of an expression language. 
To finish this tutorial, you need to:

1) compiler all the code and run all the test cases.
Upon cloning the project, simply do
```
sbt test
```
Alternatively, you may import the code into IntelliJ as an Scala `sbt`. Make sure that you have the Scala and Antlr plugins loaded before doing that. Then you can just run the tests (methods in classes whose names have the suffix 'Test') individually.
Each test is an example of how the language APIs can be used.

2) fill up all the pattern matching blanks in 2 files:
 -- /src/main/scala/edu/nus/comp/pli/epl/bigstepsemantics/EplBigStepSemantics.scala
 -- /src/main/scala/edu/nus/comp/pli/epl/staticsemantics/EplStaticSemantics.scala
 To ease your work, all the blanks are marked with comment "//add your code"
 
3) after programming, all the test cases are expected to be passed. 

(Note: the AST of this expression language is defined in 
/src/main/antlr4/epl/ExpressionParser.g4)

---------------------------------------------------------------- * 
Origin repository: https://github.com/razvanvoicu/pli
Revised by:        Yahui Song  (e0210374@u.nus.edu)  
Date:              08/2/2019                            
Purpose:           Tutorial for CS4215 (2018/2019 sem2) 
---------------------------------------------------------------- * 
Submission: please submit a compressed folder which is renamed with: "Lab04_<ID>_<Name>"
eg. Lab04_A12345678_Yahui.zip

Deadline: 22/02/2019 23:59pm