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
      "Grammar {%s}".format(prods mkString "\n")
  
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
 
  
  /*
object grammar {
   val demoGrammar = Grammar(List(
   Production( Nonterminal("E"), 
        Nonterminal("E") :: Terminal("+") :: Nonterminal("T") :: Nil ),
        
    Production( Nonterminal("E"), 
        Nonterminal("T") :: Nil ),
        
    Production( Nonterminal("T"),
        Nonterminal("T") :: Terminal("*") :: Nonterminal("F") :: Nil ),
        
    Production( Nonterminal("T"),
        Nonterminal("F") :: Nil ),
        
    Production( Nonterminal("F"),
        Terminal("(") :: Nonterminal("E") :: Terminal(")") :: Nil),
        
        Production( Nonterminal("F"),
        Terminal("id") :: Nil)
  ))
}*/
  

/*
object grammar {
  

  type SententialForm = List[Rule]
  
  class Grammar(val prods: Map[Nonterminal, Set[SententialForm]]) {
    override def toString = "Grammar { %s }".format(prods mkString ", ")
    
    // maybe refactor this to LRParser
    def grammarSymbols = prods.flatMap { (prod) =>
      
      // The nonterminal ++ the list of rules
      Set(prod._1) ++ prod._2.flatMap { 
        (sentence) => sentence.foldLeft[Set[Rule]](Set.empty)( (old, rule) => old + rule )
      }
    }
  }
  object Grammar {
    def apply(prods: (Nonterminal, Set[SententialForm])*): Grammar = new Grammar( Map(prods.toList:_*) )
  }
    
  abstract class Rule
  case class Nonterminal(name: Symbol) extends Rule
  case class Terminal(name: String) extends Rule
  
  val g = Grammar (
    // Nonterminal('Start) -> Set(
    //   Nonterminal('E) :: List()
    // ),
    Nonterminal('E) -> Set(
      Nonterminal('E) :: Terminal("+") :: Nonterminal('T) :: List(),
      Nonterminal('T) :: List()
    ),
    Nonterminal('T) -> Set(
      Nonterminal('T) :: Terminal("*") :: Nonterminal('F) :: List(),
      Nonterminal('F) :: List()
    ),
    Nonterminal('F) -> Set(
      Terminal("(") :: Nonterminal('E) :: Terminal(")") :: List(),
      Terminal("id") :: List()
    )
  )
  
  val testinput = List(Terminal("id"), Terminal("+"),Terminal("id"), Terminal("*"), Terminal("id"))
  
  class LRParser(grammar: Grammar, start: Nonterminal) {
	  
    
    //Here `A → · B C D` is represented as Item(A, List('B, 'C, 'D), 0)
    case class Item(nonterminal: Nonterminal, production: SententialForm, state: Int) {
	    def reducable = state == production.size
	    def next = if(production.size == state) None else Some(production(state))
	    def goto(what: Rule): Option[Item] = next match {
	      case Some(rule) if rule == what => Some(Item(nonterminal, production, state + 1)) 
	      case _ => None
	    }
	  }  
       
	  def closure(items: Set[Item]): Set[Item] = {
	    
	    val newItems = items.foldLeft(items) {
	      
	      (old, item) => item.next match {
	        case Some(n:Nonterminal) => old ++ grammar.prods(n).map {
	          (sentence) => Item(n, sentence, 0)
	        }
	        case _ => old
	      }
	    }
	    
	    if (newItems == items)
	      items
	    else
	      closure(newItems)
	  }
	  
	  def goto(items: Set[Item], what: Rule) = 
	    closure(
	        items.map { (item) => item.goto(what) }.foldRight[Set[Item]](Set.empty) { 
	          (item, set) => item match {
	            case Some(i) => set + i
	            case _ => set
	          }
	        })
    
	  def canonicalSetOfLR(itemSets: Set[Set[Item]]): Set[Set[Item]] = {
	    
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
    }
  
	  val startItem = Item(Nonterminal('__Start__), List( start ), 0)
	  val startState = closure(Set(startItem))
	  val acceptingItem = Item(Nonterminal('__Start__), List( start ), 1)
	  val acceptingState = closure(Set(acceptingItem))
	  
	  val automaton = canonicalSetOfLR(Set(startState)).toList
	  
	  def parse(input: List[Terminal]): Boolean = {
	    
	    import collection.mutable.Stack
	    
	    // parser state
	    val stateStack = Stack(startState)
	    val symbolStack: Stack[Rule] = Stack()
	    var restInput = input
	    
	    def shift() {
	      symbolStack push restInput.head
	      restInput = restInput.tail
	    }
	    // there could be reduce / reduce conflicts...
	    def reduce() {
	      stateStack.pop.filter { (items) => items.reducable }.toList match {
	        case Nil => sys error "Serious error, trying to reduce - but not possible"
	        case Item(nonterm, prod, _) :: Nil => {
	          // pop off all items of the given production
	          prod.reverse.foreach { (rule) =>
	            if (symbolStack.pop != rule)
	              sys error "Cannot apply reduce here, because the given production does not match"
	            }
	          // push on the nonterminal again
	          symbolStack push nonterm
	          
	          // try to follow transition from old state
	          stateStack push goto(stateStack.head, nonterm)
	        }
	        case _ => sys error "reduce, reduce conflict"
	      }	      
	    }
	    
	    def prettyPrintStack(stack: Stack[Set[Item]]) = stack.map( (item) => automaton.indexOf(item) ) mkString "::"
	    
	    while(restInput.size > 0 || stateStack.head != acceptingState) {
	      
	      println("State: %s, Symbols: %s, Input: %s".format(prettyPrintStack(stateStack), symbolStack, restInput))
	      
	      // check if there is a transition
	      
	      if (restInput.isEmpty)
	        reduce()
	      else	      
		      goto( stateStack.head, restInput.head ) match {
		        
		        // a reduction can take place
		        case s if s.isEmpty => {
		          println("reducing")
		          reduce()	          
		        }
		        
		        case s => {
		          println("shifting")
		          stateStack push s
		          shift()
		        }
		      }	      
	    }
	    true
	  }
	  
  }
}
*/