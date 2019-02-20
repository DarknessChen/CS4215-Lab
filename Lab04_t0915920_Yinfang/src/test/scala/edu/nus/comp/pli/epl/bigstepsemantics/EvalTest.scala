package edu.nus.comp.pli.epl.bigstepsemantics

import org.scalatest.FlatSpec
import edu.nus.comp.pli.epl.parser.EplParser.parse
import edu.nus.comp.pli.epl.bigstepsemantics.EplBigStepSemantics.{eval, EvaluationError, ResultBool, ResultInt}

/**
  *
  */

class EvalTest extends FlatSpec {

  "1) The evaluator" should "evaluate an int" in {
    val input = "123"
    val output = eval(parse(input))
    val expected = ResultInt(123)
    assert(output == expected)
  }

  "2) The evaluator" should "evaluate an bool" in {
    val input = "true"
    val output = eval(parse(input))
    val expected = ResultBool(true)
    assert(output == expected)
  }
  "3) The evaluator" should "evaluate an bool" in {
    val input = "false"
    val output = eval(parse(input))
    val expected = ResultBool(false)
    assert(output == expected)
  }
  "4) The evaluator" should "evaluate an error" in {
    val input = "1 + true "
    val output = eval(parse(input))
    val expected = EvaluationError
    assert(output == expected)
  }
  "5) The evaluator" should "evaluate an error" in {
    val input = "1+-true"
    val output = eval(parse(input))
    val expected = EvaluationError
    assert(output == expected)
  }
  "6) The evaluator" should "evaluate an error" in {
    val input = "1--true"
    val output = eval(parse(input))
    val expected = EvaluationError
    assert(output == expected)
  }

  "7) The evaluator" should "evaluate an bool" in {
    val input = "1+2*3-4 <= 5"
    val output = eval(parse(input))
    val expected = ResultBool(true)
    assert(output == expected)
  }

  "8) The evaluator" should "evaluate an int" in {
    val input = "(1+2)*(4-3)"
    val output = eval(parse(input))
    val expected = ResultInt(3)
    assert(output == expected)
  }

  "9) The evaluator" should "evaluate an bool" in {
    val input = "1 <= 2 & 4 == 5"
    val output = eval(parse(input))
    val expected = ResultBool(false)
    assert(output == expected)
  }

  //

  "10) The evaluator" should "evaluate an int" in {
    val input = "2^3"
    val output = eval(parse(input))
    val expected = ResultInt(8)
    assert(output == expected)
  }

  "11) The evaluator" should "evaluate a bool" in {
    val input = "false == (!true) & false | (2+3 ==4)"
    val output = eval(parse(input))
    val expected = ResultBool(false)
    assert(output == expected)
  }

  "12) The evaluator" should "evaluate a bool" in {
    val input = "false == 10 > 6"
    val output = eval(parse(input))
    val expected = ResultBool(false)
    assert(output == expected)
  }

  "13) The evaluator" should "evaluate a bool" in {
    val input = "false == !true & false"
    val output = eval(parse(input))
    val expected = ResultBool(false)
    assert(output == expected)
  }

  "14) The evaluator" should "evaluate a bool" in {
    val input = "10 == 3 + 7"
    val output = eval(parse(input))
    val expected = ResultBool(true)
    assert(output == expected)
  }

  "15) The evaluator" should "evaluate an int" in {
    val input = "1+2*3+4"
    val output = eval(parse(input))
    val expected = ResultInt(11)
    assert(output == expected)
  }
  "16) The evaluator" should "evaluate an int" in {
    val input = "(1+2)*3"
    val output = eval(parse(input))
    val expected = ResultInt(9)
    assert(output == expected)
  }
  "17) The evaluator" should "evaluate an int" in {
    val input = "(1+2)*(4-3)"
    val output = eval(parse(input))
    val expected = ResultInt(3)
    assert(output == expected)
  }
  "18) The evaluator" should "evaluate an int" in {
    val input = "---5"
    val output = eval(parse(input))
    val expected = ResultInt(-5)
    assert(output == expected)
  }
  "19) The evaluator" should "evaluate an int" in {
    val input = "--5"
    val output = eval(parse(input))
    val expected = ResultInt(5)
    assert(output == expected)
  }

  "20) The evaluator" should "evaluate a bool" in {
    val input = "false == (!true) | false & (2+3 ==4)"
    val output = eval(parse(input))
    val expected = ResultBool(true)
    assert(output == expected)
  }
  "21) The evaluator" should "evaluate an int" in {
    val input = "if true then 2 else 3"
    val output = eval(parse(input))
    val expected = ResultInt(2)
    assert(output == expected)
  }
  "22) The evaluator" should "evaluate an int" in {
    val input = "if (1+2*3-4 <= 5) then 2^3 else ---5"
    val output = eval(parse(input))
    val expected = ResultInt(8)
    assert(output == expected)
  }

  "23) The evaluator" should "evaluate an int" in {
    val input = "(2+3)*4/2-1"
    val output = eval(parse(input))
    val expected = ResultInt(9)
    assert(output == expected)
  }

  "24) The evaluator" should "evaluate a bool" in {
    val input = "if (1+2*3-4 <= 5) then true else false"
    val output = eval(parse(input))
    val expected = ResultBool(true)
    assert(output == expected)
  }

  "25) The evaluator" should "evaluate an error" in {
    val input = "2^true"
    val output = eval(parse(input))
    val expected = EvaluationError
    assert(output == expected)
  }

  //////////////////////////////////////

}
