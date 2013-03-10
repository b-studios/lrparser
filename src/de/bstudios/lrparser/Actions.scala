package de.bstudios.lrparser


/**
 * Expr   ::= Expr + Term { Add(0,2) }     <--- Here we access nonterminals, that already are transformed to Returntype
 *          | Term
 * 
 * Term   ::= Term * Factor { Mul(0,1) }
 *          | Factor
 * 
 * Factor ::= ( Expr ) { 0 }
 *          | id { Id(0) }                  <--- Here we access a terminal
 * 
 * 
 * 
 * Production('Expr, List['Expr, "+", 'Term]) { (_, results) => results match {
 * 	case e :: t :: Nil => Add(e, t) 
 * }}
 * 
 * Production('Factor, List["id"]) { (input, _) => Id(input(0)) }
 */
/*
object actions {

  
  trait Grammar[Terminal, R] {
	  
    abstract class GrammarSymbol
    case class Nonterminal(name: Symbol) extends GrammarSymbol 
    
    // what should the action get as input? Consumed input + tree nodes
	  case class Production[T](contents: List[GrammarSymbol], action: (List[Terminal], List[R]) => R)
	  
  }
	  
}*/