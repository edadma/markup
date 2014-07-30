package markup

import util.parsing.input.{Reader, Position}


abstract class Token
{
	private [markup] var _pos: Position = null
	private [markup] var _rest: Reader[Char] = null
	
	def pos = _pos
	
	def pos( p: Position ) =
	{
		_pos = p
		this
	}
	
	def rest = _rest
	
	def rest( r: Reader[Char] ) =
	{
		_rest = r
		this
	}
	
	def error( msg: String ) = sys.error( msg + " at " + _pos + '\n' + _pos.longString )
}

case class Str( s: String ) extends Token
case class QStr( s: String ) extends Token
case class Marker( name: String ) extends Token
case class Begin() extends Token
case class End() extends Token
case class Align() extends Token
case class Param() extends Token
case class Active( ch: Char ) extends Token
//case class LetterChar( ch: Char ) extends Token
//case class OtherChar( ch: Char ) extends Token
case class Space() extends Token
