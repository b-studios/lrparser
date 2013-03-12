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
     *  Shift  : Consumes the next input and jumps to specified state (index into states)
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