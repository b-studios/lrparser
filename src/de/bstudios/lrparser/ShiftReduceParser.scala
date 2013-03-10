package de.bstudios.lrparser

trait ShiftReduceParser[Terminal] {
  
  sealed abstract class Action
  case class Shift(state: State) extends Action
  case class Reduce(production: Int) extends Action
  case object Accept extends Action
  
  
  /**
   * The type of the nonterminals
   * ----------------------------
   * Probably just use Symbol
   */
  type Nonterminal
  
  /**
   * Production Table
   * ----------------
   * Maps an indice of a production to a tuple # -> (how many to pop, what nonterminal to use for `goto`)
   * My production table starts with index 0, so all reduce actions have to be decremented by one
   */
  def productionTable: List[(Int, Nonterminal)]
  
  /**
   * Parse Table
   * -----------
   * Contains the actions, that guide the parser
   */
  def parseTable: List[Map[Terminal, Action]]
  
   
  /**
   * Goto Table
   * ----------
   * After a reduction step the goto table tells us where to continue, depending on the topmost state on the stack
   */
  def gotoTable: List[Map[Nonterminal, State]]
  
  /**
   * End Of Stream
   * -------------
   * We need a custom endOfStream Token
   */
  def endOfStream: Terminal
  
  type State = Int
  private val startState: State = 0
  
  def parse(input: List[Terminal]): Boolean = {
    
    var restInput = input
    val states = scala.collection.mutable.Stack[State](startState)
    
    
    
    while(true) {
      
      val currentState = states.head

      // If the stream does not end with EOS, we will fix that.
      val nextToken = restInput match {
        case Nil => endOfStream
        case first :: _ => first
      }
      
      // EOF is currently encoded as the empty string
      // No viable path
      if (!(parseTable(currentState) contains nextToken))
        sys error "Could not parse input. Got '%s' but expected: %s".format(nextToken, parseTable(currentState).keys mkString ", ")
      
      parseTable(currentState)(nextToken) match {
        case Accept => return true
        case Shift(state) => {
          states push state
          restInput = restInput.tail
        }
        case Reduce(prod) => productionTable(prod) match {
          case (toPop, there) => {
            for ( i <- 0 until toPop) 
              states.pop
            
            // there is a serious error!
            if( !(gotoTable(states.head) contains there) )
              sys error "In State %s - Trying to Reduce to %s: Cannot goto %s because there is no transition. Stack %s".format(currentState, prod, there, states)
              
            states push gotoTable(states.head)(there)
          }
        }
      }      
    }    
    true   
  }
}

object demoParser extends ShiftReduceParser[String] {
  
  type Nonterminal = Symbol
  
  def endOfStream = "$"
  
  def productionTable = List(
    (3, 'E), // 0: E → E + T
    (1, 'E), // 1: E → T
    (3, 'T), // 2: T → T * F
    (1, 'T), // 3: T → F
    (3, 'F), // 4: F → ( E )
    (1, 'F)  // 5: F → id
  )
  
  def parseTable = List(
     
    //  |------ id -------|------- + -------|------- * -------|------- ( -------|------- ) -------|------- $ -------|
    Map( "id" -> Shift(5),                                     "(" -> Shift(4)                                       ),  // 0
    Map(                   "+" -> Shift(6),                                                        "$"  -> Accept    ),  // 1
    Map(                   "+" -> Reduce(1), "*" -> Shift(7),                    ")" -> Reduce(1), "$"  -> Reduce(1) ),  // 2
    Map(                   "+" -> Reduce(3), "*" -> Reduce(3),                   ")" -> Reduce(3), "$"  -> Reduce(3) ),  // 3
    Map( "id" -> Shift(5),                                     "(" -> Shift(4)                                       ),  // 4
    Map(                   "+" -> Reduce(5), "*" -> Reduce(5),                   ")" -> Reduce(5), "$"  -> Reduce(5) ),  // 5
    Map( "id" -> Shift(5),                                     "(" -> Shift(4)                                       ),  // 6
    Map( "id" -> Shift(5),                                     "(" -> Shift(4)                                       ),  // 7
    Map(                   "+" -> Shift(6),                                      ")" -> Shift(11)                    ),  // 8
    Map(                   "+" -> Reduce(0), "*" -> Shift(7),                    ")" -> Reduce(0), "$"  -> Reduce(0) ),  // 9
    Map(                   "+" -> Reduce(2), "*" -> Reduce(2),                   ")" -> Reduce(2), "$"  -> Reduce(2) ),  // 10
    Map(                   "+" -> Reduce(4), "*" -> Reduce(4),                   ")" -> Reduce(4), "$"  -> Reduce(4) )   // 11
  )
  
  def gotoTable = List(
    Map( 'E -> 1, 'T -> 2, 'F ->  3 ), // 0
    Map(                            ), // 1
    Map(                            ), // 2
    Map(                            ), // 3
    Map( 'E -> 8, 'T -> 2, 'F ->  3 ), // 4
    Map(                            ), // 5
    Map(          'T -> 9, 'F ->  3 ), // 6
    Map(                   'F -> 10 ), // 7
    Map(                            ), // 8
    Map(                            ), // 9
    Map(                            ), // 10
    Map(                            )  // 11
  )
  
  def test = parse(List("id", "+", "id", "$"))
  
}