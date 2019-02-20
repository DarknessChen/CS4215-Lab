package edu.nus.comp.pli.epl.bigstepsemantics

import edu.nus.comp.pli.epl.parser.EplAST.{Minus, _}

object EplBigStepSemantics {

  sealed trait Result
  case class ResultInt(value: Int) extends Result
  case class ResultBool(value: Boolean) extends Result
  case object EvaluationError extends Result

  def eval(ast: Expression): Result = ast match {
    case Num(value) => ResultInt(value)
    case Bool(value) => ResultBool(value)
    case Ury(Minus, expr) => eval(expr) match {
      case ResultInt(value) => ResultInt(- value)
      case _ => EvaluationError
    }
    case Ury(Not, expr) => eval(expr) match {
      case ResultBool(value) => ResultBool(!value)
      case _ => EvaluationError
    }
    case Bin(Add, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      case (ResultInt(vLeft), ResultInt(vRight)) => ResultInt(vLeft + vRight)
      case _ => EvaluationError
    }
    case Bin(Sub, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultInt(valueLeft), ResultInt(valueRight)) => ResultInt(valueLeft - valueRight)
      case _ => EvaluationError
    }
    case Bin(Mul, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultInt(valueLeft), ResultInt(valueRight)) => ResultInt(valueLeft * valueRight)
      case _ => EvaluationError
    }
    case Bin(Div, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultInt(valueLeft), ResultInt(valueRight)) => ResultInt(valueLeft / valueRight)
      case _ => EvaluationError
    }

    case Bin(Pow, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultInt(valueLeft), ResultInt(valueRight)) => ResultInt(scala.math.pow(valueLeft, valueRight).toInt)
      case _ => EvaluationError
    }
    case Bin(Eq, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultInt(valueLeft), ResultInt(valueRight)) => ResultBool(valueLeft == valueRight)
      case (ResultBool(valueLeft), ResultBool(valueRight)) => ResultBool(valueLeft == valueRight)
      case _ => EvaluationError
    }
    case Bin(NEq, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultInt(valueLeft), ResultInt(valueRight)) => ResultBool(valueLeft != valueRight)
      case _ => EvaluationError
    }
    case Bin(Lt, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      case (ResultInt(valueLeft), ResultInt(valueRight)) => ResultBool(valueLeft < valueRight)
      case _ => EvaluationError
    }
    case Bin(Lte, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultInt(valueLeft), ResultInt(valueRight)) => ResultBool(valueLeft <= valueRight)
      case _ => EvaluationError
    }
    case Bin(Gt, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultInt(valueLeft), ResultInt(valueRight)) => ResultBool(valueLeft > valueRight)
      case _ => EvaluationError
    }
    case Bin(Gte, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultInt(valueLeft), ResultInt(valueRight)) => ResultBool(valueLeft >= valueRight)
      case _ => EvaluationError
    }
    case Bin(And, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultBool(valueLeft), ResultBool(valueRight)) => ResultBool(valueLeft & valueRight)
      case _ => EvaluationError
    }
    case Bin(Or, exprLeft, exprRight) => (eval(exprLeft), eval(exprRight)) match {
      //add your code
      case (ResultBool(valueLeft), ResultBool(valueRight)) => ResultBool(valueLeft | valueRight)
      case _ => EvaluationError
    }
    case IfElse(cond, ifbranch, elsebranch) => (eval(cond),eval(ifbranch),eval(elsebranch)) match {
      //add your code
      case (ResultBool(true), ResultBool(valueLeft), ResultBool(_)) => ResultBool(valueLeft)
      case (ResultBool(true), ResultInt(valueLeft), ResultInt(_)) => ResultInt(valueLeft)
      case (ResultBool(false), ResultInt(_), ResultInt(valueRight)) => ResultInt(valueRight)
      case (ResultBool(false), ResultInt(_), ResultInt(valueRight)) => ResultInt(valueRight)
      case _ => EvaluationError
    }

  }
}
