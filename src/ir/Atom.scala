package ir

sealed trait VariableType
object VariableInt extends VariableType
object VariableBoolean extends VariableType
object VariableCallout extends VariableType

sealed trait BinopType

sealed trait BinopIntType extends BinopType
object BinopPlus extends BinopIntType
object BinopMinus extends BinopIntType
object BinopTimes extends BinopIntType
object BinopDiv extends BinopIntType
object BinopMod extends BinopIntType

sealed trait BinopBooleanType extends BinopType
object BinopGT extends BinopBooleanType
object BinopLT extends BinopBooleanType
object BinopGTE extends BinopBooleanType
object BinopLTE extends BinopBooleanType
object BinopEQ extends BinopBooleanType
object BinopNEQ extends BinopBooleanType
object BinopAND extends BinopBooleanType
object BinopOR extends BinopBooleanType

sealed trait AssignType
object AssignEQ extends AssignType
object AssignPlusEQ extends AssignType
object AssignMinusEQ extends AssignType
