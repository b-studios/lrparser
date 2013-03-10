package de.bstudios.lrparser
package parsetable

import grammar.{SententialForm, Nonterminal, GrammarSymbol, Grammar, Terminal, Production, Epsilon}
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
  
  case class Item(production: SententialForm, state: Int) {
    
      /**
       * This item can no longer be incremented
       */
      def reducable = state == production.size
      
      /**
       * The next grammar symbol following the dot
       */
      def next = if(production.size == state) None else Some(production(state))	    
  }
  
  def generate(grammar: Grammar): ParseTable = {
    
    import scala.collection.mutable
    
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
    
    def first(symbol: GrammarSymbol) = symbol match {
      case t:Terminal => Set(t)
      case n:Nonterminal => grammar.productionsFor(n)      
    }
    
    buildFirstSets()
    println(firstSets)
    
    ParseTable(Grammar(List()), List(), List())
    
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