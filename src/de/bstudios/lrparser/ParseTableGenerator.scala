package de.bstudios.lrparser
package parsetable

import scala.collection.mutable
import grammar.{SententialForm, Nonterminal, GrammarSymbol, Grammar, Terminal, Production, Epsilon, EOS}
import shiftreduce.{Action, Shift, Reduce, Accept}

trait ParseTableGenerator {
  
  // we require items to store some productions and to tell us, what the next symbol is
  type Item <: {
    def production: Production
    def next: Option[GrammarSymbol]
  }
  
  case class State(items: Set[Item]) {
    override def toString = "State (%s) %s".format(items.size, items.mkString(", "))
  }
  
  /**
   * CLOSURE(As)
   */
  def closure(items: Set[Item])(implicit grammar: Grammar): Set[Item]
  
  /**
   * GOTO(As, T)
   */
  def goto(items: Set[Item], symbol: GrammarSymbol)(implicit grammar: Grammar): Set[Item]
  
  def nextItemSet(state: State, symbol: GrammarSymbol)(implicit grammar: Grammar) =
    goto(closure(state.items), symbol)
  
  /**
   * LR AUTOMATON
   */
  def statesOfLRAutomaton(states: Set[State])(implicit grammar: Grammar): Set[State] = {
  
    val newStates = states ++ (for {
      state <- states
      symbol <- grammar.grammarSymbols
      
      val trans = nextItemSet(state, symbol)
      if !trans.isEmpty
    } yield State(trans))
    
    if (newStates == states)
      states
    else
     statesOfLRAutomaton(newStates)   
  }
  
  /**
   * Action Table
   */
  def buildActionTable(states: List[State], startProd: Production)(implicit grammar: Grammar) = {
    
    // The action table is accessed by index
    val actionTable = states.map( (state) => mutable.Map[Terminal, Action]() )
    
    def stateIndex(state: State) =
      states indexOf state
    
    def productionIndex(prod: Production) =
      grammar.prods indexOf prod
    
    for {
      state <- states
      item  <- closure(state.items)
      val prod = item.production
    } item.next match {
      case Some(t:Terminal) => {
        val nextState = State(nextItemSet(state, t))       
        actionTable(stateIndex(state)) put (t, Shift(stateIndex(nextState)))
      }
        
      // It's reducable
      case None if prod != startProd => for(follower <- grammar follow prod.head) {
        println("  Follower: " + follower)
        actionTable(stateIndex(state)) put (follower, Reduce(productionIndex(prod)))
      }
        
      // It's the start rule - we're done
      case None if prod == startProd => actionTable(stateIndex(state)) put (EOS, Accept)
          
      // Nothing to do here
      case _ => 
    }
    
    // make it immutable again
    actionTable.map { Map() ++ _ }
  }
  
  def buildGotoTable(states: List[State])(implicit grammar: Grammar) = {
    
    /**
     * Goto Table
     */
    // Like the action table, the goto table is accessed by index
    val gotoTable = states.map( (state) => mutable.Map[Nonterminal, Int]() )
    
    /**
     * For each nonterminal, check whether goto(state1, nonterminal) = state2
     * Create entry gotoTable[1, nonterminal] = 2
     * 
     * Both: `Shift` and `Goto` could be created on the fly inside of the LR0 Automaton creation process
     */
    for {
      state       <- states
      nonterminal <- grammar.nonterminals
      val nextSet = nextItemSet(state, nonterminal)
      if !nextSet.isEmpty
      val nextStateIndex = states indexOf State(nextSet)
    } gotoTable(states indexOf state) put (nonterminal, nextStateIndex)
    
    gotoTable.map { Map() ++ _ }
  }
  
  def newItem(prod: Production): Item
    
  def generate(implicit grammar: Grammar): ParseTable = {
    
    val startProduction = Production(Nonterminal("__START__"), List(grammar.start))
    val startItem = newItem(startProduction)
    val startState = State(Set(startItem))
    
    val states = statesOfLRAutomaton(Set(startState)).toList
    
    // debug output
    for ( (state, i) <- states.zipWithIndex ) {
      printf("State %2d:\n", i)
      for(item <- state.items) {
        printf("  %s\n", item)
      }
      print("\n")
    }
    
    // convert mutable Maps to immutable ones
    ParseTable(
        states indexOf startState,
        grammar,
        buildActionTable(states, startProduction),
        buildGotoTable(states)
    )    
  }
}