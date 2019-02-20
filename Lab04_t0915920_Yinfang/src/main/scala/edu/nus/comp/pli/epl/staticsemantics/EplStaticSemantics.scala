package edu.nus.comp.pli.epl.staticsemantics

import edu.nus.comp.pli.epl.parser.EplAST._

/**
  * @author rvoicu
  *
  */
object EplStaticSemantics {
  sealed trait ResultType
  case object ResultTypeInt extends ResultType
  case object ResultTypeBool extends ResultType
  case object ResultTypeError extends ResultType

  def inferType(ast: Expression): ResultType = ast match {
    case Num(_) => ResultTypeInt
    case Bool(_) => ResultTypeBool
    case Ury(Minus, expr) => inferType(expr) match {
      case ResultTypeInt => ResultTypeInt
      case _ => ResultTypeError
    }
    case Ury(Not, expr) => inferType(expr) match {
      case ResultTypeBool => ResultTypeBool
      case _ => ResultTypeError
    }
    case Bin(Add, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      case (ResultTypeInt, ResultTypeInt) => ResultTypeInt
      case _ => ResultTypeError
    }
    case Bin(Sub, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeInt, ResultTypeInt) => ResultTypeInt
      case _ => ResultTypeError
    }
    case Bin(Mul, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeInt, ResultTypeInt) => ResultTypeInt
      case _ => ResultTypeError
    }
    case Bin(Div, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeInt, ResultTypeInt) => ResultTypeInt
      case _ => ResultTypeError
    }
    case Bin(Pow, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeInt, ResultTypeInt) => ResultTypeInt
      case _ => ResultTypeError
    }
    case Bin(Eq, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeInt, ResultTypeInt) => ResultTypeBool
      case (ResultTypeBool, ResultTypeBool) => ResultTypeBool
      case _ => ResultTypeError
    }
    case Bin(NEq, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeInt, ResultTypeInt) => ResultTypeBool
      case _ => ResultTypeError
    }
    case Bin(Lt, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      case (ResultTypeInt, ResultTypeInt) => ResultTypeBool
      case _ => ResultTypeError
    }
    case Bin(Lte, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeInt, ResultTypeInt) => ResultTypeBool
      case _ => ResultTypeError
    }
    case Bin(Gt, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeInt, ResultTypeInt) => ResultTypeBool
      case _ => ResultTypeError
    }
    case Bin(Gte, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeInt, ResultTypeInt) => ResultTypeBool
      case _ => ResultTypeError
    }
    case Bin(And, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeBool, ResultTypeBool) => ResultTypeBool
      case _ => ResultTypeError
    }
    case Bin(Or, exprLeft, exprRight) => (inferType(exprLeft), inferType(exprRight)) match {
      //add your code
      case (ResultTypeBool, ResultTypeBool) => ResultTypeBool
      case _ => ResultTypeError
    }
    case IfElse(cond, ifbranch, elsebranck) => (inferType(cond), inferType(ifbranch), inferType(elsebranck)) match {
      //add your code
      case (ResultTypeBool, ResultTypeBool, ResultTypeBool) => ResultTypeBool
      case (ResultTypeBool, ResultTypeInt, ResultTypeInt) => ResultTypeInt
      case _ => ResultTypeError
    }
  }
}
