package de.bstudios.lrparser
package shiftreduce

import grammar.{Terminal, Nonterminal, Grammar, Production}
import parsetable.ParseTable

sealed abstract class Action
case class Shift(state: Int) extends Action
case class Reduce(production: Int) extends Action
case object Accept extends Action

trait Parser {
  
  /**
   * End Of Stream
   * -------------
   * We need a custom endOfStream Token
   */
  def endOfStream: Terminal
  
  /**
   * Parsetable
   * ----------
   * Every parser needs to be fed with a previously generated parse table
   */
  def parseTable: ParseTable
  
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
      if (!(parseTable.actions(currentState) contains nextToken))
        sys error "Could not parse input. Got '%s' but expected: %s".format(nextToken, parseTable.actions(currentState).keys mkString ", ")
      
      parseTable.actions(currentState)(nextToken) match {
        case Accept => return true
        case Shift(state) => {
          states push state
          restInput = restInput.tail
        }
        case Reduce(prod_index) => parseTable.grammar.prods(prod_index) match {
          case prod@Production(head_nonterminal, body) => {
            
            for ( i <- 0 until body.length ) 
              states.pop
            
            // there is a serious error!
            if( !(parseTable.gotos(states.head) contains head_nonterminal) )
              sys error "In State %s - Trying to Reduce to %s".format(currentState, head_nonterminal)
              
            states push parseTable.gotos(states.head)(head_nonterminal)
          }
        }
      }      
    }    
    true   
  }
}

object demoParser extends Parser {
  
  import grammar.implicits._
  
  implicit def stringAction2terminalAction(pair: (String, Action)): (Terminal, Action) = 
    (Terminal(pair._1), pair._2)
    
  implicit def symbolState2nonterminalState(pair: (Symbol, State)): (Nonterminal, State) = 
    (Nonterminal(pair._1.name), pair._2)
  
  def endOfStream = "$"
  
  def parseTable = ParseTable(    
      
    /**
     * The Grammar
     */
    grammar.example,
      
    /**
     * Action Table
     */
    List(
     
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
    ),
    
    /**
     * Goto Table
     */
    List(
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
  ) 
  
  def test = parse(List("id", "+", "id", "$"))
}