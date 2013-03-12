package de.bstudios.lrparser
package parsetable

import grammar.{SententialForm, Nonterminal, GrammarSymbol, Grammar, Terminal, Production, Epsilon, EOS}
import shiftreduce.{Action, Shift, Reduce, Accept}


trait LR0ParseTableGenerator extends ParseTableGenerator {
  
  case class LR0Item(production: Production, position: Int) {
    
      /**
       * This item can no longer be incremented
       */
      def reducable = position == production.size
      
      /**
       * The next grammar symbol following the dot
       */
      def next = if (production.size == position) None else Some(production.body(position))	  
      
      override def toString = 
        "%s → %s · %s".format(
            production.head, 
            production.body.slice(0, position) mkString " ", 
            production.body.slice(position, production.body.size) mkString " "
        )
  }
  
  type Item = LR0Item
  
  /**
   * CLOSURE(As)
   */
  def closure(items: Set[Item])(implicit grammar: Grammar): Set[Item] = {
    
    val newItems = items.foldLeft(items)( (old, item) => item.next match {
      case Some(n:Nonterminal) => old ++ grammar.productionsFor(n).map {
        (prod) => LR0Item(prod, 0)
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
  def goto(items: Set[Item], symbol: GrammarSymbol)(implicit grammar: Grammar): Set[Item] = 
    items.filter( (item) => item.next match {
      case Some(rule) => rule == symbol
      case _ => false
    }).map { (item) =>
      LR0Item(item.production, item.position + 1)
    }
  
  def newItem(prod: Production): Item = LR0Item(prod, 0)
}