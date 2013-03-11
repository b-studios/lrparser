package de.bstudios.lrparser
package parsetable

import grammar.{SententialForm, Nonterminal, GrammarSymbol, Grammar, Terminal, Production, Epsilon, EOS}
import shiftreduce.{Action, Shift, Reduce, Accept}


case class ParseTable(
    
    /**
     * Start State
     * -----------
     * The index of the state to start parsing with
     */
    startState: Int,
    
    /**
     * Production Table
     * ----------------
     * Maps an index of a production to a tuple # -> (how many to pop, what nonterminal to use for `goto`)
     * My production table starts with index 0, so all reduce actions have to be decremented by one
     */
    grammar: Grammar,
    
    /**
     * Action Table
     * -------------
     * Contains the actions like `shift` and `reduce`, that guide the parsers behavior
     * 
     * 	 Shift  : Consumes the next input and jumps to specified state (index into states)
     *   Reduce : Reduces the stack input by using the specified production (index into productions)    
     */
    actions: List[Map[Terminal, Action]],
    
    /**
     * Goto Table
     * ----------
     * After a reduction step the goto table tells us where to continue, depending on the topmost state on the stack
     */
    gotos: List[Map[Nonterminal, Int]]
) {
  
  /**
   * Outputs a table like:
   *      | id |
   *  ----+----+ 
   *    0 | s3 |
   *  
   * Every column should have a size of terminal.name.lenth but at least 3 characters
   * 
   */
  private def actionTableToString = {
    
    
    // they always have to be in the same order
    val terminals = (grammar.terminals + EOS).toList
    
    var table = "    |"
      
    // print header
    table += (for {
      terminal <- terminals
      val colwidth = terminals.size.max(3)
    } yield (" %" + colwidth.toString + "s ").format(terminal.name)) mkString "|"
    
    
    val separator = "\n----+" + (for {
      terminal <- terminals
      val colwidth = terminals.size.max(3)
    } yield ("-" * (colwidth + 2))).mkString("+") + "\n"
    
    
    table += (for ((state, i) <- actions.zipWithIndex) yield {
      
      // separator and first column followed by all other columns
       separator + ("%3d |".format(i)) + (for (terminal <- terminals; val colwidth = terminals.size.max(3)) yield {
        if (!(state contains terminal)) {
          " " * (colwidth + 2)
        } else (" %" + colwidth.toString + "s ").format(state(terminal) match {
          case Accept => "acc"
          case Shift(s) => "s%2d".format(s)
          case Reduce(s) => "r%2d".format(s)
        }) 
      }).mkString("|")
    }).mkString("")  
    
    table
  }
  
  private def gotoTableToString = {
    
    val nonterminals = grammar.nonterminals.toList
    
    var table = "    |"
      
    // print header
    table += (for {
      nonterminal <- nonterminals
      val colwidth = nonterminal.name.size.max(3)
    } yield (" %" + colwidth.toString + "s ").format(nonterminal.name)) mkString "|"
    
    
    val separator = "\n----+" + (for {
      nonterminal <- nonterminals
      val colwidth = nonterminal.name.size.max(3)
    } yield ("-" * (colwidth + 2))).mkString("+") + "\n"
    
    
    table += (for ((goto, i) <- gotos.zipWithIndex) yield {
      
      // separator and first column followed by all other columns
       separator + ("%3d |".format(i)) + (for (nonterminal <- nonterminals; val colwidth = nonterminal.name.size.max(3)) yield {
        if (!(goto contains nonterminal)) {
          " " * (colwidth + 2)
        } else (" %" + colwidth.toString + "d ").format(goto(nonterminal))
      }).mkString("|")
    }).mkString("")  
    
    table
  }
  
  override def toString = """
ParseTable
==========
StartState: %s

Grammar
-------
%s
    
Action Table
------------
%s
    
Goto Table
----------
%s
""".format(startState, grammar, actionTableToString, gotoTableToString)
}




trait ParseTableGenerator {
  
  case class State(items: Set[Item]) {
    override def toString = "State (%s) %s".format(items.size, items mkString ", ")
  }
  
  case class Item(production: Production, position: Int) {
    
      /**
       * This item can no longer be incremented
       */
      def reducable = position == production.size
      
      /**
       * The next grammar symbol following the dot
       */
      def next = if(production.size == position) None else Some(production.body(position))	  
      
      override def toString = 
        "%s → %s · %s".format(
            production.head, 
            production.body.slice(0, position) mkString " ", 
            production.body.slice(position, production.body.size) mkString " "
        )
  }
  
  def generate(grammar: Grammar): ParseTable = {
    
    import scala.collection.mutable
        
    /**
     * CLOSURE(As)
     */
    def closure(items: Set[Item]): Set[Item] = {
     
      val newItems = items.foldLeft(items)( (old, item) => item.next match {
        case Some(n:Nonterminal) => old ++ grammar.productionsFor(n).map {
          (prod) => Item(prod, 0)
        }
        case _ => old
      })
      
      if (newItems == items)
        items
      else
        closure(newItems)
    }
    
    /**
     * GOTO(As, T)
     */
    def goto(items: Set[Item], symbol: GrammarSymbol) = items.filter( (item) => item.next match {
      case Some(rule) => rule == symbol
      case _ => false
    }).map { (item) =>
      Item(item.production, item.position + 1)
    }
    
    
    def gotoState(prevState: State, symbol: GrammarSymbol): Option[State] = {      
      val nextItems = goto(closure(prevState.items), symbol)
      if (nextItems.isEmpty)
        None
      else
        Some(State(nextItems))  
    }
      
    /**
     * LR0AUTOMATON
     */
    def canonicalSetOfLR0(states: Set[State]): Set[State] = {
	    
	    val newStates = states ++ (for {
	      state <- states
	      symbol <- grammar.grammarSymbols
	      
	      val trans = goto(closure(state.items), symbol)
	      if !trans.isEmpty
	    } yield State(trans))
	    
	    if (newStates == states)
	      states
	    else
	      canonicalSetOfLR0(newStates)   
    }
    
    /**
     * FIRST(A)
     */    
    // initialize every nonterminal with an empty set
    val firstSets = Map(grammar.nonterminals.toList.map(
      (nonterm) => (nonterm, mutable.Set[Terminal]()) 
    ):_*)    
    
    def buildFirstSets(): Unit = {
      
      var modified = false
      
      def trackingChanges(key: Nonterminal)(changes: (mutable.Set[Terminal]) => Unit) {
        val set = firstSets(key)
        val sizeBefore = set.size
        changes(set)
        modified = modified || (set.size != sizeBefore)
      }
      
      grammar.prods.foreach {
        
        // for each nonterminal: if epsilon was in every previous nonterminal, then
        // add rules of that nonterminal to the head's first set
        case Production(head, body) => body.foldLeft(true) { (allEpsilon, sym) =>
          
          if (allEpsilon) sym match {
            case t:Terminal => {
              trackingChanges(head) { _ += t }
              false
            }
            case n:Nonterminal => {
              trackingChanges(head) { _ ++= (firstSets(n) - Epsilon) }
              firstSets(n) contains Epsilon                  
            }
          
          // Not all previous have contained epsilon, so we skip the remainder
          } else {
            false
          }
        
        // Check if epsilon was in all components, then add it to this head
        } match {
          case true => trackingChanges(head) { _ += Epsilon }
          case false =>
        }
      }
      
      if (modified)
        buildFirstSets()
    }    
    // Immediately build them
    buildFirstSets()
    
    def first(symbols: List[GrammarSymbol]): Set[Terminal] = 
      // caution: we use 2 aggregators here: (allEpsilon, collectedTerminals)
      symbols.foldLeft[(Boolean, Set[Terminal])]( (true, Set.empty) ) { (old, sym) => (old, sym) match {
        // there has been a previous set without epsilon
        case ((false, _), _) => old
        case ((true, set), t:Terminal) => (false, set + t)
        case ((true, set), n:Nonterminal) => (firstSets(n) contains Epsilon, set ++ (firstSets(n) - Epsilon))
      }
    // In the end: check for epsilon
    } match {
      case (true, set)  => set + Epsilon
      case (false, set) => set
    }
    
    /**
     * FOLLOW(A)
     */    
    // initialize every nonterminal with an empty set
    val followSets = Map(grammar.nonterminals.toList.map(
      (nonterm) => (nonterm, mutable.Set[Terminal]()) 
    ):_*)    
    
    // put EOS in Startrule
    followSets(grammar.start) += EOS
    
    def buildFollowSets(): Unit = {
      
      var modified = false
      
      def trackingChanges(key: Nonterminal)(changes: (mutable.Set[Terminal]) => Unit) {
        val set = followSets(key)
        val sizeBefore = set.size
        changes(set)
        modified = modified || (set.size != sizeBefore)
      }      
      
      grammar.prods.foreach {      
        
        // now we go from right to left and check if all right hand side rules contain epsilon
        case Production(head, body) => {
        
          def followSetOfString(restString: List[GrammarSymbol]): Unit = restString match {
            case (n:Nonterminal) :: rest => {
              val firstOfRest = first(rest)            
              trackingChanges(n) { _ ++= (firstOfRest - Epsilon) }
                            
              if ( (first(rest) contains Epsilon) || rest == Nil)
                trackingChanges(n) { _ ++= followSets(head) }
              
              // continue on rest
              followSetOfString(rest)
            }
            case (t:Terminal) :: rest => followSetOfString(rest)
            case _ =>
          }
          followSetOfString(body)
        }
      }
      
      if (modified)
        buildFollowSets()            
    }
    // immediately build follow sets
    buildFollowSets()
    
    // debug output
    // println("FirstSets")
    // println(firstSets)
    
    // println("FollowSets")
    // println(followSets)
    
    val startProduction = Production(Nonterminal("__START__"), List(grammar.start))
    val startState = State(Set(Item(startProduction, 0) ))
    
    val states = canonicalSetOfLR0(Set(startState)).toList
    
    // println("Canonical Set of LR0")
    // for(state <- states) {
    //   println(state + "\n")
    // }
    
    
    
    /**
     * Action Table
     */
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
        val nextState = State(goto(closure(state.items), t))        
        actionTable(stateIndex(state)) put (t, Shift(stateIndex(nextState)))
      }
        
      // It's reducable
      case None if prod != startProduction => for(follower <- followSets(prod.head)) {
        actionTable(stateIndex(state)) put (follower, Reduce(productionIndex(prod)))
      }
      
        
      // It's the start rule - we're done
      case None if prod == startProduction => actionTable(stateIndex(state)) put (EOS, Accept)
          
      // Nothing to do here
      case _ =>
    }
    
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
      val nextSet = goto(closure(state.items), nonterminal)
      if !nextSet.isEmpty
      val nextStateIndex = stateIndex(State(nextSet))
    } gotoTable(stateIndex(state)) put (nonterminal, nextStateIndex)
    
   
    
    // convert mutable Maps to immutable ones
    ParseTable(
        stateIndex(startState),
        grammar,
        actionTable.map { Map() ++ _ },
        gotoTable.map { Map () ++ _}
    )    
  }  
}