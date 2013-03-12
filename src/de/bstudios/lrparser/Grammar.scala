package de.bstudios.lrparser

import scala.collection.mutable

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
  type SymbolSets = Map[Nonterminal, mutable.Set[Terminal]]
  
  /**
   * Later on add semantic actions here
   */
  case class Production(head: Nonterminal, body: List[GrammarSymbol]) {
    override def toString = "%s → %s" format (head, body mkString " ")
    def size = body.size
  }
  
  
  case class Grammar(start: Nonterminal, prods: List[Production]) {
    
    /**
     * Initialize FIRST and FOLLOW sets
     */
    val firstSets = Map(nonterminals.toList.map(
      (nonterm) => (nonterm, mutable.Set[Terminal]()) 
    ):_*)
    buildFirstSets
    
    val followSets = Map(nonterminals.toList.map(
      (nonterm) => (nonterm, mutable.Set[Terminal]()) 
    ):_*)
    
    // put EOS in Startrule
    followSets(start) += EOS
    
    buildFollowSets
    
    
    
    
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
    
      
    /**
     * FIRST(A)
     */    
    def buildFirstSets: Unit = {
      
      var modified = false
      
      def trackingChanges(key: Nonterminal)(changes: (mutable.Set[Terminal]) => Unit) {
        val set = firstSets(key)
        val sizeBefore = set.size
        changes(set)
        modified = modified || (set.size != sizeBefore)
      }
      
      prods.foreach {
        
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
        buildFirstSets
    }
    
    
    /**
     * FOLLOW(A)
     * 
     * depends on the already calculated firstSets
     */    
    def buildFollowSets: Unit = {
      
      var modified = false
     
      def trackingChanges(key: Nonterminal)(changes: (mutable.Set[Terminal]) => Unit) {
        val set = followSets(key)
        val sizeBefore = set.size
        changes(set)
        modified = modified || (set.size != sizeBefore)
      }
      
      prods.foreach {
          
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
        buildFollowSets
    }
    
    def follow(symbol: Nonterminal) = followSets(symbol)
    
    /**
     * FIRST( multiple symbols )
     */
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
  }
  
  object implicits {
    implicit def sym2Nonterminal(s: Symbol): Nonterminal = Nonterminal(s.name)
    implicit def string2Terminal(s: String): Terminal = Terminal(s)  
  }
  
  /**
   * Dragonbook p. 251
   */
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
  
  
  /**
   * Dragonbook p. 261
   */
  val example2 = Grammar(Nonterminal("S"), List(
    Production(Nonterminal("S"),
      Nonterminal("B") :: Nonterminal("B") :: Nil),
      
    Production(Nonterminal("B"),
      Terminal("a") :: Nonterminal("B") :: Nil),
      
    Production(Nonterminal("B"),
      Terminal("b") :: Nil)
  ))
  
  /**
   * Wikipedia http://en.wikipedia.org/wiki/LR_parser
   */
  val example3 = Grammar(Nonterminal("E"), List(
    Production(Nonterminal("E"),
      Terminal("a") :: Nonterminal("E") :: Nil),
      
    Production(Nonterminal("E"),
      Terminal("a") :: Nil)
  ))
}