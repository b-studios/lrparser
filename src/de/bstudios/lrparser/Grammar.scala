package de.bstudios.lrparser

object grammar {
    
  /**
   * GrammarSymbols that the body of a production might contain
   */
  sealed abstract class GrammarSymbol {
    def name: String
    override def toString = name
  }
  case class Nonterminal(name: String) extends GrammarSymbol
  case class Terminal(name: String) extends GrammarSymbol
  case object Epsilon extends Terminal("__epsilon__")
  case object EOS extends Terminal("__eos__")
  
  type SententialForm = List[GrammarSymbol] 
    
  /**
   * Later on add semantic actions here
   */
  case class Production(head: Nonterminal, body: List[GrammarSymbol]) {
    override def toString = "%s → %s" format (head, body mkString " ")
    def size = body.size
  }
  
  
  case class Grammar(start: Nonterminal, prods: List[Production]) {
    override def toString =
      "Grammar {\n%s\n}".format(prods.zipWithIndex.map { (p) => "%3d: %s".format(p._2, p._1) } mkString "\n")
  
    // TODO analyse where this is used and maybe outfactor it.
    def grammarSymbols = prods.foldLeft[Set[GrammarSymbol]](Set.empty) { 
      (old, prod) =>
      
      // collect:
      // 1. The nonterminal head 
      // 2. all symbols in the production body
      old + prod.head ++ prod.body.toSet
    }
    
    def nonterminals = grammarSymbols.foldLeft[Set[Nonterminal]](Set.empty) {
      (old, sym) => sym match {
        case n:Nonterminal => old + n
        case _ => old
      }
    }
    
    def terminals = grammarSymbols.foldLeft[Set[Terminal]](Set.empty) {
      (old, sym) => sym match {
        case t:Terminal => old + t
        case _ => old
      }
    }
  
    def productionsFor(head: Nonterminal): List[Production] = 
      prods.filter( _.head == head )
  }
  
  object implicits {
    implicit def sym2Nonterminal(s: Symbol): Nonterminal = Nonterminal(s.name)
    implicit def string2Terminal(s: String): Terminal = Terminal(s)  
  }
  
  val example = Grammar(Nonterminal("E"), List(
    Production(Nonterminal("E"), 
      Nonterminal("E") :: Terminal("+") :: Nonterminal("T") :: Nil ),
      
    Production(Nonterminal("E"), 
      Nonterminal("T") :: Nil ),
      
    Production(Nonterminal("T"),
      Nonterminal("T") :: Terminal("*") :: Nonterminal("F") :: Nil ),
      
    Production(Nonterminal("T"),
      Nonterminal("F") :: Nil ),
      
    Production(Nonterminal("F"),
      Terminal("(") :: Nonterminal("E") :: Terminal(")") :: Nil),
      
    Production(Nonterminal("F"),
      Terminal("id") :: Nil)
  ))  
}