package edu.nus.comp.pli.epl.staticsemantics

import edu.nus.comp.pli.epl.parser.EplParser.parse
import edu.nus.comp.pli.epl.staticsemantics.EplStaticSemantics._
import org.scalatest.FlatSpec

class StaticSemanticsTest extends FlatSpec {

  "1) The type inference" should "infer an int" in {
    assert(
      inferType(parse("123")) == ResultTypeInt
    )
  }

  "2) The type inference algo" should "infer a bool" in {
    assert(
      inferType(parse("true")) == ResultTypeBool
    )
  }

  "3) The type inference algo" should "infer a bool" in {
    assert(
      inferType(parse("false")) == ResultTypeBool
    )
  }
  "4) The type inference algo" should "infer an error" in {
    assert(
      inferType(parse("1 + true ")) == ResultTypeError
    )
  }
  "5) The type inference algo" should "infer an error" in {
    assert(
      inferType(parse("1+-true")) == ResultTypeError
    )
  }

  "6) The type inference algo" should "infer an error" in {
    assert(
      inferType(parse("1--true")) == ResultTypeError
    )
  }

  "7) The type inference algo" should "infer a bool" in {
    assert(
      inferType(parse("1+2*3-4 <= 5")) == ResultTypeBool
    )
  }
  "8) The type inference algo" should "infer an int" in {
    assert(
      inferType(parse("(1+2)*(4-3)")) == ResultTypeInt
    )
  }

  "9) The type inference algo" should "infer a bool" in {
    assert(
      inferType(parse("1 <= 2 & 4 == 5")) == ResultTypeBool
    )
  }

  "10) The type inference algo" should "infer an int" in {
    assert(
      inferType(parse("2^3")) == ResultTypeInt
    )
  }
  "11) The type inference algo" should "infer a bool" in {
    assert(
      inferType(parse("false == (!true) & false | (2+3 ==4)")) == ResultTypeBool
    )
  }

  "12) The type inference algo" should "infer a bool" in {
    assert(
      inferType(parse("false == 10 > 6")) == ResultTypeBool
    )
  }
  "13) The type inference algo" should "infer a bool" in {
    assert(
      inferType(parse("false == !true & false")) == ResultTypeBool
    )
  }

  "14) The type inference algo" should "infer a bool" in {
    assert(
      inferType(parse("10 == 3 + 7")) == ResultTypeBool
    )
  }
  "15) The type inference algo" should "infer an int" in {
    assert(
      inferType(parse("1+2*3+4")) == ResultTypeInt
    )
  }

  "16) The type inference algo" should "infer an int" in {
    assert(
      inferType(parse("(1+2)*3")) == ResultTypeInt
    )
  }
  "17) The type inference algo" should "infer an int" in {
    assert(
      inferType(parse("(1+2)*(4-3)")) == ResultTypeInt
    )
  }

  "18) The type inference algo" should "infer an int" in {
    assert(
      inferType(parse("---5")) == ResultTypeInt
    )
  }

  "19) The type inference algo" should "infer an int" in {
    assert(
      inferType(parse("--5")) == ResultTypeInt
    )
  }


  "20) The type inference algo" should "infer a bool" in {
    assert(
      inferType(parse("false == (!true) | false & (2+3 ==4)")) == ResultTypeBool
    )
  }

  "21) The type inference algo" should "infer an int" in {
    assert(
      inferType(parse("if true then 2 else 3")) == ResultTypeInt
    )
  }

  "22) The type inference algo" should "infer an int" in {
    assert(
      inferType(parse("if (1+2*3-4 <= 5) then 2^3 else ---5")) == ResultTypeInt
    )
  }

  "23) The type inference algo" should "infer an int" in {
    assert(
      inferType(parse("(2+3)*4/2-1")) == ResultTypeInt
    )
  }
  "24) The type inference algo" should "infer a bool" in {
    assert(
      inferType(parse("if (1+2*3-4 <= 5) then true else false")) == ResultTypeBool
    )
  }
  "25) The type inference algo" should "infer an error" in {
    assert(
      inferType(parse("2^true")) == ResultTypeError
    )
  }
}
