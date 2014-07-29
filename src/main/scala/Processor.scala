package markup

import java.io.{Reader => JReader, File}
import java.awt.Color
import java.awt.Color._
import java.util.regex.Pattern

import util.parsing.input.Reader
import collection.mutable.{HashMap, ArrayBuffer, ListBuffer}//, ArrayStack, ArrayBuffer, HashMap, HashSet}
import collection.immutable.LinearSeq

import typesetter._


abstract class Processor extends Typesetter with MarkupReader
{
	type PrimitiveHandler = (Token, LinearSeq[Token]) => LinearSeq[Token]
	
	protected var typeset = false
	
	protected val resource: Class[_]
	protected val init: String
	
	protected def out( o: Any = '\n' )
	
	protected def function( clazz: String, method: String )( arg: Double ) =
		Class.forName( clazz ).getDeclaredMethod(method, classOf[Double]).invoke( null, arg.asInstanceOf[Object] ).
		asInstanceOf[Number].doubleValue

	protected class NonExecutableHandler extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) = MarkupReader.error( "non-executable marker", t )
	}
	
	protected class BasicHandler( action: => Unit ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
			action
			s
		}
	}
	
	protected class MidHandler( pre: => Unit, post: => Unit, endcode: (String => Nothing) => Unit ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
			if (s.isEmpty) MarkupReader.error( "argument was expected", t.rest )
			
			pre
			
		val rest = processToken( s, endcode )
		
			post
			rest
		}
	}
	
	protected class BoxArgHandler( action: (Symbol, Option[Box]) => Unit ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
		val (_, b, tail) = boxnumber( t, s )
		val sym = Symbol( b )
		
			action( sym, boxVariable(sym) )
			tail
		}
	}
	
	protected class ArgHandler( action: (List[Token], Token) => Unit ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
		val (arg, first, tail) = argument( s )
		
			action( arg, first )
			tail
		}
	}
	
	protected class Arg2Handler( action: (List[Token], List[Token]) => Unit ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
		val (args, tail) = arguments( s, 2 )
		
			action( args(0), args(1) )
			tail
		}
	}

	protected class StrMidHandler[T]( pre: T => Unit, endcode: (String => Nothing) => Unit, post: => Unit, err: String )( implicit conv: String => Option[T] ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
			if (s.isEmpty) MarkupReader.error( err, t.rest )
			
			s.head match
			{
				case t@(QStr( _ ) | Str( _ )) =>
					pre( string(t, err) )
					
					if (skipSpace(s.tail).isEmpty) MarkupReader.error( "argument was expected", t.rest )
					
				val rest = processToken( skipSpace(s.tail), endcode )
				
					post
					rest
				case t => MarkupReader.error( err, t )
			}
		}
	}

	protected class Str2MidHandler[T1, T2]( pre: (T1, T2) => Unit, endcode: (String => Nothing) => Unit, post: => Unit, err1: String, err2: String )( implicit c1: String => Option[T1], c2: String => Option[T2] ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
			if (s.isEmpty) MarkupReader.error( err1, t.rest )
			
			s.head match
			{
				case t1@(QStr( _ ) | Str( _ )) =>
					val s1: T1 = string( t1, err1 )
					val tail = skipSpace( s.tail )
					
					if (tail.isEmpty) MarkupReader.error( err2, t.rest )
					
					tail.head match
					{
						case t2@(QStr( _ ) | Str( _ )) =>
							pre( s1, string(t2, err2) )
					
							if (skipSpace(tail.tail).isEmpty) MarkupReader.error( "group was expected", t2.rest )
					
						val rest = processToken( skipSpace(tail.tail), endcode )
						
							post
							rest
						case t => MarkupReader.error( err2, t )
					}
				case t => MarkupReader.error( err1, t )
			}
		}
	}

	protected def stringToken( t: Token, s: LinearSeq[Token], err: String ) =
	{
		if (s.isEmpty)
			if (t eq null)
				sys.error( err )
			else
				MarkupReader.error( err, t.rest )
			
	val v = s.head match
		{
			case QStr( s ) => s
			case Str( s ) => s
			case t => MarkupReader.error( err, s.head )
		}
	
		(v, skipSpace( s.tail ), s.head.rest)
	}
	
	protected class OperationHandler( action: (Symbol, Any) => Unit ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
		val (name, tail, rest) = stringToken( t, s, "variable name expected" )
		val err = "operand was expected"
			
			if (tail.isEmpty)
				MarkupReader.error( err, rest )
		
		val sym = Symbol( name )
		
			tail.head match
			{
				case t1@(QStr( _ ) | Str( _ )) =>
					action( sym, string[Dimen](t1, err) )
					skipSpace( tail ).tail
				case t => MarkupReader.error( err, t )
			}
		}
	}
	
	protected class BoxMidHandler( pre: Box => Unit, post: => Unit, endcode: (String => Nothing) => Unit ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
			if (s.isEmpty)
				MarkupReader.error( "expected a box argument", t.rest )
				
			list
			
		val tail = processToken( s )
		val args = arg
		
			if (args.length != 1 || !args.head.isBox)
				MarkupReader.error( "expected a box argument", t.rest )
				
				if (skipSpace(tail).isEmpty) sys.error( "argument was expected" )
			
				pre( args.head.asBox )
				
			val rest = processToken( skipSpace(tail), endcode )
				post
				rest
		}
	}
	
	protected class MidStrHandler[T]( pre: => Unit, post: T => Unit, err: String )( implicit conv: String => Option[T] ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
			if (s.isEmpty) MarkupReader.error( "argument was expected", t.rest )
			
			s.head match
			{
				case t@(QStr( _ ) | Str( _ )) =>
					pre
					
					if (skipSpace(s.tail).isEmpty) MarkupReader.error( "argument was expected", t.rest )
					
				val rest = processToken( skipSpace(s.tail), _ => {} )
				
					post( string(t, "string argument was expected") )
					rest
				case t => MarkupReader.error( "string argument was expected", t )
			}
		}
	}
	
	protected class CustomGroupHandler( pre: => Unit, post: => Unit ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
			if (s.isEmpty) MarkupReader.error( "end group character was expected", t.rest )
			
		val egroup = s.head match
			{
				case QStr( s ) if s.length == 1 => s
				case Str( s ) if s.length == 1 => s
				case t => MarkupReader.error( "end group character was expected", t )
			}
		
			dup
			pre
			specialChar( '}', egroup.head )
			
		val tail = skipSpace( s.tail )
		
			if (tail.isEmpty) MarkupReader.error( "text was expected", t.rest )

		val rest = processGroup( tail, _ => {} )
		
			post
			rest
		}
	}
	
	protected class MarkerArgHandler( action: String => Unit, err: String ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
			if (s.isEmpty) MarkupReader.error( err, t.rest )
			
			s.head match
			{
				case Marker( m ) =>
					action( m )
					skipSpace( s.tail )
				case t => MarkupReader.error( err, t )
			}
		}
	}
	
	protected class StrArgHandler[T]( action: T => Unit, valid: T => Boolean, err: String )( implicit conv: String => Option[T] ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
			if (s.isEmpty) MarkupReader.error( err, t.rest )
			
			s.head match
			{
				case t@(QStr( _ ) | Str( _ )) =>
					val v = string( t, err )
					
					if (!valid( v )) MarkupReader.error( err, t )
					
					action( v )
					skipSpace( s.tail )
				case t => MarkupReader.error( err, t )
			}
		}
	}
	
	protected class StrNHandler( action: IndexedSeq[Any] => Unit, args: List[(String, String => Option[_])] ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
		val buf = new ArrayBuffer[Any]
		
			def arg( rest: Reader[Char], seq: LinearSeq[Token], argslist: List[(String, String => Option[_])] ): LinearSeq[Token] =
			{
				if (argslist.isEmpty)
					seq
				else
				{
					if (seq.isEmpty) MarkupReader.error( argslist.head._1, rest )
				
					seq.head match
					{
						case t@(QStr( _ ) | Str( _ )) =>
							buf += string[Any]( t, argslist.head._1 )( argslist.head._2 )
							arg( t.rest, skipSpace(seq.tail), argslist.tail )
						case t => MarkupReader.error( argslist.head._1, t )
					}
				}
			}
		
		val res = arg( t.rest, s, args )
		
			action( buf.toIndexedSeq )
			res
		}
	}
	
	protected class Str2Handler[T1, T2]( action: (T1, T2) => Unit, err1: String, err2: String )( implicit c1: String => Option[T1], c2: String => Option[T2] ) extends PrimitiveHandler
	{
		def apply( t: Token, s: LinearSeq[Token] ) =
		{
			if (s.isEmpty) MarkupReader.error( err1, t.rest )
			
			s.head match
			{
				case str@(QStr( _ ) | Str( _ )) =>
					val s1: T1 = string( str, err1 )
					val tail = skipSpace( s.tail )
					
					if (tail.isEmpty) MarkupReader.error( err2, t.rest )
					
					tail.head match
					{
						case t@(QStr( _ ) | Str( _ )) =>
							action( s1, string(t, err2) )
							skipSpace( tail.tail )
						case t => MarkupReader.error( err2, t )
					}
				case t => MarkupReader.error( err1, t )
			}
		}
	}
	
	protected def string[T]( t: Token, err: String )( implicit conv: String => Option[T] ): T =
		(t match
		{
		case Str( s ) => conv( s )
		case QStr( s ) => conv( s )
		case _ => MarkupReader.error( err, t )
		}) match
		{
			case None => MarkupReader.error( err, t )
			case Some( r ) => r
		}
	
	protected implicit def string2int( s: String ) =
	{
		def convert( s: String, radix: Int ) =
			if (s matches (if (radix == 10) "[0-9]+" else if (radix == 8) "[0-7]+" else "[0-9a-fA-F]+"))
				Some( Integer.parseInt(s, radix) )
			else
				None

		if (s.startsWith( "'" ))
			convert( s.substring(1), 8 )
		else if (s.startsWith( "0x" ))
			convert( s.substring(2), 16 )
		else
			convert( s, 10 )
	}

	protected def integer( t: Token ): Int = string( t, "integer was expected" )
	
	protected implicit def string2path( s: String ) =
	{
	val f = new File( s )
	
		if (f.isFile)
			Some( f )
		else
			None
	}
	
//	protected implicit def string2style( s: String ) = Font.styleMap.get( s )

	private val FONT_PATTERN = Pattern.compile( "(.*)-(plain|slant|smallcaps|bold)-([0-9]+)" )

	protected implicit def string2font( s: String ) =
	{
	val matcher = FONT_PATTERN.matcher( s )

		if (matcher.matches)
			Font.fromName( resource, matcher.group(1), matcher.group(2), matcher.group(3).toInt )
		else
			variable.get( Symbol(s) ) match
			{
			case Some( f: Font ) => Some( f )
			case _ => None
			}
	}

	protected implicit def string2string( s: String ) = Some( s )

	protected implicit def string2function( s: String ): Option[Double => Double] =
		s.lastIndexOf( '.' ) match
		{
			case -1 => None
			case index => Some( function(s.substring(0, index), s.substring(index + 1)) )
		}
		
	private val COLOR_MAP =
		Map(
			(classOf[Color].getDeclaredFields
			filter (_.getType eq classOf[Color])
			filter (_.getName.head.isLower)
			map (f => (f.getName, f.get(null).asInstanceOf[Color]))): _*)
	
	protected implicit def string2color( s: String ) =
		if (s matches "[0-9a-fA-F]{6}")
			Some( new Color(Integer.parseInt(s, 16)) )
		else
			COLOR_MAP.get( s )
	
	protected implicit def string2glue( s: String ) = glue( s )
//		if (s matches """-?(?:\d+(?:\.\d*)?|\d*\.\d+)(?:E-?\d+)?""")
//			Some( Glue(s.toDouble) )
//		else
//			if (variable.contains( Symbol(s) ))
//				Some( glueVariable( Symbol(s) ) )
//			else
//				None
	
	protected implicit def string2dimen( s: String ) = dimen( s )
	
	protected def string2double( s: String ) =
		if (!s.matches( """-?(\d+(\.\d*)?|\d*\.\d+)""" ))
			None
		else
			Some( s.toDouble )
	
	protected def register( name: String, sym: Symbol, handler: PrimitiveHandler )
	{
		if (variable contains sym)
			sys.error( "already defined: <" + name + '>' )
			
		variable( sym, handler )
	}
	
	protected def registerMarker( marker: String, handler: PrimitiveHandler ) =
		register( marker, Symbol('\\' + marker), handler )

	protected def registerActive( ch: Char, handler: PrimitiveHandler ) =
		register( ch.toString, Symbol("~" + ch), handler )
	
//	protected def deregister( marker: String )
//	{
//		if (!(markerHandlers contains marker))
//			sys.error( "marker not defined: " + marker )
//
//		markerHandlers -= marker
//	}
	
	protected def defineOperation( marker: String )( action: (Symbol, Any) => Unit ) = registerMarker( marker, new OperationHandler(action) )
	
	protected def defineCustomGroup( marker: String )( pre: => Unit )( post: => Unit ) = registerMarker( marker, new CustomGroupHandler(pre, post) )
	
	protected def define( marker: String )( action: => Unit ) = registerMarker( marker, new BasicHandler(action) )
	
	protected def defineActive( ch: Char )( action: => Unit ) = registerActive( ch, new BasicHandler(action) )
	
	protected def defineConst( marker: String, const: String ) = registerMarker( marker, new BasicHandler(string( const )) )
	
	protected def defineConstChar( marker: String, const: String ) = registerMarker( marker, new BasicHandler(glyph( const.head )) )
	
	protected def defineSpecialChar( marker: String ) = defineConst( marker, marker )
	
	protected def defineMarker( marker: String )( handler: PrimitiveHandler ) = registerMarker( marker, handler )
	
	protected def defineBox( marker: String )( action: (Symbol, Option[Box]) => Unit ) = registerMarker( marker, new BoxArgHandler(action) )
	
	protected def defineMid( marker: String )( pre: => Unit )( post: => Unit ) =
		registerMarker( marker, new MidHandler(pre, post, _ => {}) )
	
	protected def defineMidMode( marker: String )( pre: => Unit )( endcode: (String => Nothing) => Unit = _ => {} ) =
		registerMarker( marker, new MidHandler(pre, end, endcode) )
	
	protected def defineMidList( marker: String )( post: => Unit ) =
		registerMarker( marker, new MidHandler(list, post, _ => {}) )
	
	protected def defineMidLocal( marker: String )( pre: => Unit ) =
		registerMarker( marker, new MidHandler({dup; pre}, pop, _ => {}) )
	
	protected def defineStrMid[T]( marker: String, err: String )( pre: T => Unit )( post: => Unit )( implicit conv: String => Option[T] ) =
		registerMarker( marker, new StrMidHandler(pre, _ => {}, post, err) )
	
	protected def defineStrMidMode[T]( marker: String, err: String )( pre: T => Unit )( endcode: (String => Nothing) => Unit = _ => {} )( implicit conv: String => Option[T] ) =
		registerMarker( marker, new StrMidHandler(pre, endcode, end, err) )
	
	protected def defineStr2MidMode[T1, T2]( marker: String, err1: String, err2: String )( pre: (T1, T2) => Unit )( endcode: (String => Nothing) => Unit = _ => {} )( implicit c1: String => Option[T1], c2: String => Option[T2] ) =
		registerMarker( marker, new Str2MidHandler(pre, endcode, end, err1, err2) )
	
	protected def defineBoxMidMode( marker: String )( pre: Box => Unit )( endcode: (String => Nothing) => Unit = _ => {} ) =
		registerMarker( marker, new BoxMidHandler(pre, end, endcode) )
	
	protected def defineMidStr[T]( marker: String, err: String )( pre: => Unit )( post: T => Unit )( implicit conv: String => Option[T] ) =
		registerMarker( marker, new MidStrHandler(pre, post, err) )
	
	protected def defineStr[T]( marker: String, err: String )( action: T => Unit )( implicit conv: String => Option[T] ) =
		registerMarker( marker, new StrArgHandler(action, (_: T) => true, err) )
	
	protected def defineStrN( marker: String, args: (String, String => Option[_])* )( action: IndexedSeq[Any] => Unit ) =
		registerMarker( marker, new StrNHandler(action, args.toList) )
	
	protected def defineDimenStr( marker: String )( action: Dimen => Unit ) =
		registerMarker( marker, new StrArgHandler[Dimen](d => action(d), _ => true, "expected dimension") )
	
	protected def defineStr2[T1, T2]( marker: String, err1: String, err2: String )( action: (T1, T2) => Unit )( implicit c1: String => Option[T1], c2: String => Option[T2] ) =
		registerMarker( marker, new Str2Handler(action, err1, err2) )
	
	protected def defineDimenStr2( marker: String )( action: (Dimen, Dimen) => Unit ) =
		registerMarker( marker, new Str2Handler[Dimen, Dimen]((d1, d2) => action(d1, d2), "expected dimension", "expected dimension") )
	
	protected def defineGlueStr( marker: String )( action: Glue => Unit ) =
		registerMarker( marker, new StrArgHandler[Glue](g => action(g), _ => true, "expected glue") )
	
	protected def defineStrVar[T]( marker: String, default: T, err: String )( implicit c1: String => Option[T] ) =
	{
	val v = Symbol( marker )
	
		def assigner( c: T ) = variable( v, c )
	
		if (!assigned( v ))
			assigner( default )
			
		registerMarker( marker, new StrArgHandler[T](assigner, _ => true, err) )
	}
	
	protected def defineColorVar( marker: String ) =
		defineStrVar[Color]( marker, Color.BLACK, "expected color (RGB as a 6 character hex string)" )
	
	protected def defineSkipVar( marker: String ) = defineStrVar[Glue]( marker, ZGlue, "expected glue" )
	
	protected def defineDimenVar( marker: String ) = defineStrVar[Dimen]( marker, 0, "expected dimension" )
	
	protected def defineNumberVar( marker: String ) = defineStrVar[Double]( marker, 0, "expected number" )
	
	protected def defineFontVar( marker: String ) =
		defineStrVar[Font]( marker, Font(null, "Serif", "plain", 20), "expected font" )
		
	protected def defineDefine( marker: String, definer: String => Unit ) =
		registerMarker( marker, new MarkerArgHandler(m => definer(m), "expected marker to be defined") )
	
	protected def defineArg( marker: String )( action: (List[Token], Token) => Unit ) =
		registerMarker( marker, new ArgHandler(action) )
	
	protected def defineArg2( marker: String )( action: (List[Token], List[Token]) => Unit ) =
		registerMarker( marker, new Arg2Handler(action) )
	
	protected def defineNonExecutable( marker: String ) =
		registerMarker( marker, new NonExecutableHandler )
	
	protected def specialChar( name: Char, c: Char ) {variable( Symbol(name.toString), c )}
	
	protected def specialChar( c: Char ) {specialChar( c, c )}
	
	protected val INTEGER_PATTERN = """\d+""".r.pattern
	
	protected def boxnumber( m: Token, s: LinearSeq[Token] ) =
	{
	val err = "expected box number"
		
		if (s.isEmpty)
			MarkupReader.error( err, m )
		
	val number =
		s.head match
		{
		case Str( s ) => s
		case QStr( s ) => s
		case _ => MarkupReader.error( err, s.head )
		}
	
		if (INTEGER_PATTERN.matcher(number).matches)
			(s.head.rest, number, s.tail)
		else
			MarkupReader.error( err, s.head )
	}
	
	protected def split[A]( l: List[A], s: List[A] ) =
	{
	val resll = new ListBuffer[List[A]]
	
		def _split( l: List[A] )
		{
			if (!l.isEmpty)
			{
			val resl = new ListBuffer[A]
			
				def __split( l: List[A] ): (List[A], Boolean) =
					if (l.isEmpty)
						(l, false)
					else if (l startsWith s)
						(l drop s.length, true)
					else
					{
						resl += l.head
						
						__split( l.tail )
					}
			
			val (rest, sep) = __split( l )
			
				resll += resl.toList
				
				if (sep && rest.isEmpty)
					resll += Nil
					
				_split( rest )
			}
		}
	
		_split( l )
		resll.toList
	}

	protected def processToken( s: LinearSeq[Token], endcode: (String => Nothing) => Unit = _ => {} ) =	
		s.head match
		{
			case m@Marker( n ) =>
				variable get Symbol('\\' + n) match
				{
					case Some( h: PrimitiveHandler ) => h( m, s.tail )
					case _ => MarkupReader.error( "unrecognized marker " + '"' + n + '"', m )
				}
			case a@Active( ch ) =>
				variable get Symbol("~" + ch) match
				{
					case Some( h: PrimitiveHandler ) => h( a, s.tail )
					case _ => MarkupReader.error( "unrecognized active character " + '"' + ch + '"', a )
				}
			case t@Str( w ) =>
				if (typeset)
					string( w, MarkupReader.error(_, t) )

				s.tail
			case QStr( w ) =>
				qstring( w )
				s.tail
			case Space() =>
				if (typeset)
					space

				s.tail
			case Begin() =>
				dup
				processGroup( s.tail, endcode )
			case t@Param() =>
				MarkupReader.error( "misplaced parameter place holder", t )
			case t@Align() =>
				MarkupReader.error( "misplaced alignment separator", t )
			case t@End() =>
				MarkupReader.error( "illegal end of group", t )
		}
	
	protected def processGroup( s: LinearSeq[Token], endcode: (String => Nothing) => Unit ): LinearSeq[Token] =
		if (s.isEmpty)
			sys.error( "unclosed group at end of document" )
		else if (s.head == End())
		{
			endcode( MarkupReader.error(_, s.head) )
			pop
			s.tail
		}
		else
			processGroup( processToken(s, endcode), endcode )

	protected def processList( s: LinearSeq[Token] ) =
	{
		list
		process( s )
		arg
	}
	
	protected def process( s: LinearSeq[Token] ): LinearSeq[Token] =
		if (s.isEmpty)
			s
		else
			process( processToken(s, _ => {}) )
	
	def processDocumentFromFile( resource: Class[_], doc: String ) = processDocument( readFromFile(resource, doc) )
	
	def processDocument( doc: String ): Box = processDocument( read(doc) )

	def processDocument( doc: JReader ): Box = processDocument( read(doc) )

	def processDocument( doc: => LinearSeq[Token] ): Box =
	{
		vertical
		typeset = false
		process( read(init) )
		typeset = true
		process( doc )
		par
		box
	}
}

/*
object HammingTest {
  // A convenience object for stream pattern matching
  object #:: {
    class TailWrapper[+A](s: Stream[A]) {
      def unwrap = s.tail
    }
    object TailWrapper {
      implicit def unwrap[A](wrapped: TailWrapper[A]) = wrapped.unwrap
    }
    def unapply[A](s: Stream[A]): Option[(A, TailWrapper[A])] = {
      if (s.isEmpty) None
      else {
        Some(s.head, new TailWrapper(s))
      }
    }
  }

  def merge(a: Stream[BigInt], b: Stream[BigInt]): Stream[BigInt] =
    (a, b) match {
      case (x #:: xs, y #:: ys) =>
        if (x < y) x #:: merge(xs, b)
        else if (y < x) y #:: merge(a, ys)
        else x #:: merge(xs, ys)
    }                                             //> merge: (a: Stream[BigInt], b: Stream[BigInt])Stream[BigInt]

  lazy val numbers: Stream[BigInt] =
    1 #:: merge(numbers map { _ * 2 }, merge(numbers map { _ * 3 }, numbers map { _ * 5 }))
                                                  //> numbers  : Stream[BigInt] = <lazy>
  numbers.take(10).toList                         //> res0: List[BigInt] = List(1, 2, 3, 4, 5, 6, 8, 9, 10, 12)
}
*/