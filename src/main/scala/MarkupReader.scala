package markup

import java.io.{Reader => JReader, File, FileReader, StringReader, BufferedReader, InputStreamReader}

import util.parsing.input.{StreamReader, Reader, Position, OffsetPosition}
import collection.mutable.{Buffer, ArrayBuffer, ListBuffer, HashMap}

import typesetter.Util


object MarkupReader
{
	def error( msg: String, pos: Position ) = sys.error( msg + " at " + pos + '\n' + pos.longString )
	
	def error( msg: String, tok: Token ): Nothing = error( msg, tok.pos )
	
	def error( msg: String, r: Reader[_] ): Nothing = error( msg, r.pos )
}

trait MarkupReader
{
	type TranslationHandler = Reader[Token] => Reader[Token]
	
	protected def variable: Map[Symbol, Any]
	
	protected def variable( key: Symbol, value: Any )
	
	protected def paragraphMarker: Option[String]
	
	protected def isEscapeChar( c: Char ): Boolean
	
	protected def isLineCommentChar( c: Char ): Boolean
	
	protected def isLineCommentCharFirst( c: Char ): Boolean
	
	protected def isLineCommentCharSecond( c: Char ): Boolean
	
	protected def isMultiLineCommentCharFirst( c: Char ): Boolean
	
	protected def isMultiLineCommentCharSecond( c: Char ): Boolean

	protected def isStringChar( c: Char ): Boolean

	protected def isBeginChar( c: Char ): Boolean

	protected def isEndChar( c: Char ): Boolean

	protected def isAlignChar( c: Char ): Boolean

	protected def isParamChar( c: Char ): Boolean

	protected def isIgnoredChar( c: Char ) = c == '\r'

	protected def isActiveChar( ch: Char ): Boolean

	protected def tokenReader( _first: Token, _rest:=> Reader[Token], _pos: Position ) =
		new Reader[Token]
		{
			val atEnd = false
			
			val first = _first
			
			val rest = _rest
			
			val pos = _pos
		}
		
	protected def endReader( _pos: Position ) =
		new Reader[Token]
		{
			val atEnd = true
			
			def first = throw new NoSuchMethodError( "at end" )
			
			def rest = throw new NoSuchMethodError( "at end" )
			
			val pos = _pos
		}
		
	protected def bumpPosition( p: Position ) =
	{
		new Position
		{
			 def line = p.line

			 def column = p.column + 1
			 
			 def lineContents = p.longString takeWhile (_ != '\n')
		}
	}

	protected def isSpace( r: Reader[Char] ) = r.first.isWhitespace && (r.first != '\n' || !isActiveChar( '\n' ))
	
	protected def skipSpaceOnLine( r: Reader[Char] ): Reader[Char] =
		if (isSpace( r ) && r.first != '\n')
			skipSpaceOnLine( r.rest )
		else
			r

	protected def paragraph( r: Reader[Char] ) =
		paragraphMarker != None && r.first == '\n' && !isActiveChar( '\n' ) && !r.rest.atEnd &&
		!skipSpaceOnLine( r.rest ).atEnd && skipSpaceOnLine( r.rest ).first == '\n'

	protected def skipSpace( r: Reader[Char] ): Reader[Char] =
		if (isSpace( r ))
			if (paragraph( r ))
				r
			else
				skipSpace( r.rest )
		else
			r

	protected def skipLine( r: Reader[Char] ): Reader[Char] =
		if (r.atEnd)
			r
		else if (r.first == '\n')
			r.rest
		else
			skipLine( r.rest )

	protected def skipMultiLineComment( r: Reader[Char] ): Reader[Char] =
		if (r.atEnd)
			r
		else if (isMultiLineCommentCharSecond( r.first ) && !r.rest.atEnd && isMultiLineCommentCharFirst( r.rest.first ))
			r.rest.rest
		else
			skipMultiLineComment( r.rest )
	
	def printTokens( s: String ) = println( read(s).mkString(", ") )

	def readFromFile( resource: Class[_], file: String ) = read( new BufferedReader(new InputStreamReader(Util.stream(resource, file), "UTF-8")) )
	
	def read( r: JReader ): Reader[Token] = read( StreamReader(r) )
	
	def read( s: String ): Reader[Token] = read( new StringReader(s) )
	
	def read( r: Reader[Char] ): Reader[Token] = expandTokenStream( readTokenStream(r) )

	def expandTokenStream( s: Reader[Token] ): Reader[Token] =
		if (s.atEnd)
			s
		else
		{
			s.first match
			{
				case Marker( name ) =>
					variable get Symbol( '\\' + name ) match
					{
						case Some( handler: TranslationHandler ) => return expandTokenStream( handler(s.rest) )
						case _ =>
					}
				case Active( ch ) =>
					variable get Symbol( "~" + ch ) match
					{
						case Some( handler: TranslationHandler ) => return expandTokenStream( handler(s.rest) )
						case _ =>
					}
				case _ => 
			}

			tokenReader( s.first, expandTokenStream(s.rest), s.pos )
		}
	
	protected def skipSpace( s: Reader[Token] ) = if (!s.atEnt && s.first == Space()) s.rest else s
	
	class MarcroTranslation( body: Reader[Token], argc: Int ) extends TranslationHandler
	{
		def apply( s: Reader[Token] ): Reader[Token] =
		{
		val (args, rest) = arguments( s, argc )
		
			Stream.concat( replaceParameters(body, args, true), rest )
		}
	}

	protected def argument( s: Reader[Token] ) =	
	{
	val buf = new ListBuffer[Token]
	val (end, rest) = tokenList( buf, 0, s )
		
		(buf.toList, end, rest)
	}
	
	protected def tokenList( buf: Buffer[Token], count: Int, s: Reader[Token] ): (Token, Reader[Token]) =
		if (s.isEmpty)
			sys.error( "unexpected end of input - unclosed group" )
		else
			s.head match
			{
				case t@Begin() =>
					if (count > 0)
						buf += t
						
					tokenList( buf, count + 1, s.tail )
				case t@End() =>
					if (count == 0)
						MarkupReader.error( "illegal end of group", t )
					else
						if (count > 1)
						{
							buf += t
							tokenList( buf, count - 1, s.tail )
						}
						else
							(t, s.tail)
				case t =>
					buf += t
					
					if (count > 0)
						tokenList( buf, count, s.tail )
					else
						(t, s.tail)
			}
	
	protected def arguments( s: Reader[Token], argc: Int ) =
	{
	val args = new Array[List[Token]]( argc )
	var next = s
	
		for (i <- 0 until argc)
		{
		val (arg, _, tail) = argument( next )
		
			args(i) = arg
			next =
				if (i < argc - 1)
					skipSpace( tail )
				else
					tail
		}
	
		(args.toIndexedSeq, next)
	}

	protected def argumentGroup( s: Reader[Token] ) =	
	{
		if (s.head != Begin()) MarkupReader.error( "argument group expected", s.head )
		
		argument( s )
	}
	
	def replaceParameters( s: Reader[Token], replacements: IndexedSeq[List[Token]], numbered: Boolean ) =
	{
	val buf = new ListBuffer[Token]
	
		def _replace( s: Reader[Token] )
		{
			if (!s.isEmpty)
				s.head match
				{
					case Param() =>
						if (numbered)
						{
							if (s.tail.isEmpty)
								MarkupReader.error( "expected parameter number (1-9)", s.head.rest )
						
							s.tail.head match
							{
								case t@Str( str ) =>
									val c = str.head
									
									if (!c.isDigit || c == '0')
										MarkupReader.error( "expected parameter number (1-9)", t )
					
									buf ++= replacements( c - '0' - 1 )
									
									if (str.length > 1)
										buf += Str( str.substring(1) ) pos bumpPosition( t.pos )
									
									_replace( s.tail.tail )
								case t => MarkupReader.error( "expected parameter number (1-9)", t )
							}
						}
						else
						{
							buf ++= replacements( 0 )
							_replace( s.tail )
						}
					case t =>
						buf += t
						_replace( s.tail )
				}
		}
	
		_replace( s )
		buf.toList
	}
	
	protected def readMacroDefinition( r: Reader[Char] ) =
	{
	val toks = readTokenStream( r )
		
		if (toks.isEmpty) MarkupReader.error( "expected marker", r )
		
	val marker = toks.head
		
		if (!marker.isInstanceOf[Marker]) MarkupReader.error( "expected marker", marker )
		
	val (toks1, n) =
		toks.tail.head match
		{
		case t@Str( s ) =>
			if (!s.matches( "\\d+" )) MarkupReader.error( "expected number of parameters", t )
			
			(skipSpace(toks.tail.tail), s.toInt)
		case _ => (toks.tail, 0)
		}
		
	val (a, _, rest) = argumentGroup( toks1 )
		
		variable( Symbol('\\' + marker.asInstanceOf[Marker].name), new MarcroTranslation(a, n) )
		expandTokenStream( rest )
	}
	
	protected def readTokenStream( r: Reader[Char] ): Stream[Token] =
		readToken( r ) match
		{
			case None =>
				Stream.Empty
			case Some( t ) =>
				Stream.cons( t, readTokenStream(t.rest) )
		}
	
	protected def readToken( r: Reader[Char] ): Option[Token] =
		if (r.atEnd)
			None
		else if (isActiveChar( r.first ))
			Some( Active(r.first) pos r.pos rest r.rest )
		else if (isIgnoredChar( r.first ))
			readToken( r.rest )
		else if (isEscapeChar( r.first ))
		{
		val (s, rest) = readMarker( r.rest )
		
			Some( Marker(s) pos r.pos rest skipSpace(rest) )
		}
		else if (isLineCommentChar( r.first ))
			readToken( skipLine(r.rest) )
		else if (isLineCommentCharFirst( r.first ) && !r.rest.atEnd && isLineCommentCharSecond( r.rest.first ))
			readToken( skipLine(r.rest.rest) )
		else if (isMultiLineCommentCharFirst( r.first ) && !r.rest.atEnd && isMultiLineCommentCharSecond( r.rest.first ))
			readToken( skipMultiLineComment(r.rest.rest) )
		else if (isSpace( r ))
			if (paragraph( r ))
				Some( Marker(paragraphMarker.get) pos r.pos rest skipSpaceOnLine(r.rest).rest )
			else
				Some( Space() pos r.pos rest skipSpace(r.rest) )
		else if (isBeginChar( r.first ))
			Some( Begin() pos r.pos rest r.rest )
		else if (isEndChar( r.first ))
			Some( End() pos r.pos rest r.rest )
		else if (isAlignChar( r.first ))
		{
		val rest = skipSpace( r.rest )
			
			Some( Align() pos r.pos rest rest )
		}
		else if (isParamChar( r.first ))
			Some( Param() pos r.pos rest r.rest )
		else if (isStringChar( r.first ))
		{
		val (s, rest) = readUntil( r => r.atEnd || r.first == '\n' || isStringChar(r.first), r.rest )

			if (rest.atEnd || rest.first == '\n') MarkupReader.error( "unterminated quoted string", rest )
			
			Some( QStr(s) pos r.pos rest rest.rest )
		}
		else
		{
		val (s, rest) = readChunk( r )

			Some( Str(s) pos r.pos rest rest )
		}

	protected def readUntil( cond: Reader[Char] => Boolean, r: Reader[Char] ) =
	{
	val tok = new StringBuilder

		def readString( r: Reader[Char] ): Reader[Char] =
			if (cond( r ))
				r
			else
			{
				tok += r.first
				readString( r.rest )
			}

		val rest = readString( r )
		
			(tok.toString, rest)
	}
	
	protected def readChunk( r: Reader[Char] ) =
	{
		if (r.atEnd)
			("", r)
		else
			readUntil(
				r => r.atEnd ||
					isEscapeChar( r.first ) ||
					isSpace( r ) ||
					isBeginChar( r.first ) ||
					isEndChar( r.first ) ||
					isAlignChar( r.first ) ||
					isParamChar( r.first ) ||
					isStringChar( r.first ) ||
					isActiveChar(r.first) ||
					isMultiLineCommentCharFirst( r.first ) ||
					isLineCommentCharFirst( r.first ) ||
					isLineCommentChar( r.first ), r )
	}
	
	protected def readAlpha( r: Reader[Char] ) = readUntil( r => r.atEnd || !r.first.isLetter || isActiveChar(r.first), r )

	protected def readMarker( r: Reader[Char] ) =
		if (r.atEnd || r.first == '\r' || r.first == '\n')
			("", r.rest)
		else if (!r.first.isLetter)
			(r.first.toString, r.rest)
		else
			readAlpha( r )
}

trait TeXStyleReader extends MarkupReader
{
//	protected var specialsExceptEndEnabled = true
//	protected var endChar = '}'
	
	protected def isChar( name: Char, c: Char ) =
		variable get Symbol( name.toString ) match
		{
		case None => false
		case Some( ch ) => ch == c
		}
	
	protected def paragraphMarker = Some( "par" )
	
	protected def isEscapeChar( c: Char ) = isChar( '\\', c )
	
	protected def isLineCommentChar( c: Char ) = isChar( '%', c )

	protected def isBeginChar( c: Char ) = isChar( '{', c )

	protected def isEndChar( c: Char ) = isChar( '}', c )

	protected def isAlignChar( c: Char ) = isChar( '&', c )

	protected def isParamChar( c: Char ) = isChar( '#', c )
}

trait YAMLReader extends TeXStyleReader
{
	protected def isLineCommentCharFirst( c: Char ) = isChar( '/', c )
	
	protected def isLineCommentCharSecond( c: Char ) = isChar( '/', c )
	
	protected def isMultiLineCommentCharFirst( c: Char ) = isChar( '/', c )
	
	protected def isMultiLineCommentCharSecond( c: Char ) = isChar( '*', c )

	protected def isStringChar( c: Char ) = isChar( '"', c )

	protected def isActiveChar( ch: Char ) = variable contains Symbol( "~" + ch )
}

//object YAMLReaderTest extends App with YAMLReader
//{
//	printTokens(
//"""
//\abc123 {asdf}%asdf
//""" )
//}
