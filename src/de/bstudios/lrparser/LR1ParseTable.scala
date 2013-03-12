package de.bstudios.lrparser
package parsetable

import grammar.{SententialForm, Nonterminal, GrammarSymbol, Grammar, Terminal, Production, Epsilon, EOS}
import shiftreduce.{Action, Shift, Reduce, Accept}

trait LR1ParseTableGenerator extends ParseTableGenerator {
  
  /**
   * Delta: A new lookahead Terminal is added to each item
   */
  case class LR1Item(production: Production, position: Int, lookahead: Terminal) {
    
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
       
     // the rest after the `next` symbol
     def rest = 
       production.body.slice(position + 1, production.body.size)
       
  }
  
  type Item = LR1Item
  
  
  /**
   * CLOSURE(As)
   * 
   * Delta: Now for every terminal in the `first` set a new item is created
   */
  def closure(items: Set[Item])(implicit grammar: Grammar): Set[Item] = {
     
    val newItems = items.foldLeft(items)( (old, item) => item.next match {
      case Some(n:Nonterminal) => old ++ (for {
        prod <- grammar productionsFor n
        terminal <- grammar first (item.rest :+ item.lookahead) 
      } yield LR1Item(prod, 0, terminal))
      case _ => old
    })
      
    if (newItems == items)
      items
    else
      closure(newItems)
  }
    
  /**
   * GOTO(As, T)
   * 
   * Nothing big changed - only lookahead is copied, too
   */
  def goto(items: Set[Item], symbol: GrammarSymbol)(implicit grammar: Grammar) = 
    items.filter( (item) => item.next match {
      case Some(rule) => rule == symbol
      case _ => false
    }).map { (item) =>
      LR1Item(item.production, item.position + 1, item.lookahead)
    }
 
  def newItem(prod: Production): Item = LR1Item(prod, 0, EOS)
}