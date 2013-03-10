package de.bstudios.lrparser
package parsetable

import grammar.{SententialForm, Nonterminal, GrammarSymbol, Grammar, Terminal, Production, Epsilon, EOS}
import shiftreduce.{Action, Shift, Reduce, Accept}


case class ParseTable(
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
)


trait ParseTableGenerator {
  
  case class State(items: Set[Item])
  
  case class Item(production: Production, position: Int) {
    
      /**
       * This item can no longer be incremented
       */
      def reducable = position == production.size
      
      /**
       * The next grammar symbol following the dot
       */
      def next = if(production.size == position) None else Some(production.body(position))	    
  }
  
  def generate(grammar: Grammar): ParseTable = {
    
    import scala.collection.mutable
    
    val states = mutable.LinkedList[State]()
    
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
    println(firstSets)
    
    println("FollowSets")
    println(followSets)
    
    // dummy output
    ParseTable(Grammar(Nonterminal("Start"), List()), List(), List())    
  }
    
  
  
  
  
   
  /*
  def goto(what: GrammarSymbol): Option[Item] = next match {
    case Some(rule) if rule == what => Some(Item(production, state + 1)) 
    case _ => None
  }*/
  
  
  /*
  case class ItemSet(items: Set[Item]) {
    def closure(grammar: Grammar): Set[Item] = {
      
    }
  }
  
  def closure(itemset: Set[Item], grammar: Grammar): Set[Item] = {

    val newItems = items.foldLeft(items) {
	      
	      (old, item) => item.next match {
	        case Some(head:Nonterminal) => old ++ grammar.productionsFor(head).map {
	          (prod) => Item(n, prod.body, 0)
	        }
	        case _ => old
	      }
	    }
	    
	    if (newItems == items)
	      items
	    else
	      closure(newItems, grammar)
	  }
	  
	  def goto(items: Set[Item], what: GrammarSymbol, grammar: Grammar) = 
	    closure(
	        items.map { (item) => item.goto(what) }.foldRight[Set[Item]](Set.empty) { 
	          (item, set) => item match {
	            case Some(i) => set + i
	            case _ => set
	          }
	        })
    
	  def canonicalSetOfLR(itemSets: Set[Set[Item]], grammar: Grammar): Set[Set[Item]] = {
	    
	    val newItemSets = itemSets ++ (for {
	      set <- itemSets
	      sym <- grammar.grammarSymbols
	      val trans = goto(set, sym)
	      if trans != Set.empty
	    } yield trans)
	    
	    if (newItemSets == itemSets)
	      itemSets
	    else
	      canonicalSetOfLR(newItemSets)   
    }*/
  
}