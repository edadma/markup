package markup

import java.awt.Font._
import java.awt.{Color, BasicStroke}
import java.awt.Color._
import java.io.{File, FileInputStream}
import javax.imageio.ImageIO

import collection.immutable.LinearSeq
import collection.mutable.{ArrayBuffer, ListBuffer}

import typesetter._


abstract class YAML extends Processor with YAMLReader
{
	val init =
		"""
		basic setup
		-----------
		
		\newskip\smallskipamount
		\smallskipamount 3pt// plus1pt minus1pt
		\newskip\medskipamount
		\medskipamount 6pt// plus2pt minus2pt
		\newskip\bigskipamount
		\bigskipamount 12pt// plus4pt minus4pt

		
		Bible quotation macros
		----------------------
		
		\def\nkjv 2 {\biblequote{#1} NKJV {#2}}
		\def\amp 2 {\biblequote{#1} AMP {#2}}
		\def\biblequote 3 {\m\b{#1} (#2)\m\vv{#3}}
		\def\ltnkjv 2 {\lowerthird{\nkjv {#1} {#2}}}
		\def\lowerthird 1 {\vskip 480 \translucent 5 \vbt 230 {\hsize1270 #1 \vfil}}
		\def\lhnkjv 2 {\lowerhalf{\nkjv {#1} {#2}}}
		\def\lowerhalf 1 {\vskip 360 \translucent 5 \vbt 350 {\hsize1270 #1 \vfil}}
		
		
		useful macros
		-------------
		
		\def\leaderfill{\leaders\hbt 30 {\hss.\hss}\hfil}
		\def\hrulefill{ \leaders\hrule\hfil\ }
		\def\leftline1{\line{#1\hss}}
		\def\rightline1{\line{\hss#1}}
		\def\quad{\hskip1em }
		\def\qquad{\hskip2em }
		\def\thinspace{\kern.1667em }
		\def\enskip{\hskip.5em }
		\def\enspace{\kern.5em }
		
		\def\smallskip{\vskip smallskipamount}
		\def\medskip{\vskip medskipamount}
		\def\bigskip{\vskip bigskipamount}

		\def\hang{\hangindent parindent}
		\def\item{\par\hang\textindent}
		\def\itemitem{\par\indent\hangindent2parindent\textindent}
		\def\itemitemitem{\par\indent\indent\hangindent3parindent\textindent}
		\def\textindent1{\indent\llap{#1\enspace}\ignorespaces}
		\def\narrower{\advance leftskip parindent\advance rightskip parindent}

		\def\title1{\centerline{\b{#1}}\vskip10}
		\def\heading1{\vskip15\hb{\b{#1}}\vskip5\m}
		
		\def\YAMLL{\hb{\font "resources/cm-unicode-0.7.0/cmunrm.ttf-plain-30" Y\kern-10{\font "resources/cm-unicode-0.7.0/cmunrm.ttf-plain-24" A}\kern-5 M\kern-10 L}}
		\def\YAML{\hb{\font "resources/cm-unicode-0.7.0/cmunrm.ttf-plain-24" Y\kern-8{\font "resources/cm-unicode-0.7.0/cmunrm.ttf-plain-20" A}\kern-4 M\kern-8 L}}
		\def\YAMLb{\hb{\font "resources/cm-unicode-0.7.0/cmunrb.ttf-plain-24" Y\kern-8{\font "resources/cm-unicode-0.7.0/cmunrb.ttf-plain-20" A}\kern-4 M\kern-9 L}}
		\def\TeX{T\kern-.1667em\lower.5ex E\kern-.125em X}
		"""

	specialChar( '\\' )
	specialChar( '%' )
	specialChar( '{' )
	specialChar( '}' )
	specialChar( '&' )
	specialChar( '#' )
	specialChar( '/' )
	specialChar( '*' )
	specialChar( '"' )
	
	defineActive( '~' ) {string( " " )}

	def obeylines = defineActive( '\n' ) {par}
	
	define( "obeylines" ) {obeylines}
	
	defineCustomGroup( "code" )
	{
		vskip( 6*pt )
		font( fontVariable('tbfont) )
		obeylines
	}
	{
		vskip( 6*pt )
	}
	
	defineStr[String]( "input", "expected input file" ) {file => process( readFromFile(resource, file) )}
	
	defineStr[File]( "image", "expected image file" ) {path => add( new ImageBox(ImageIO.read(path), alpha) )}
	
	defineMidMode( "draw" ) {draw( None )} ()
	
	defineMidMode( "path" ) {path} {mode.asInstanceOf[PathMode].end( _ )}
	
	def plot( f: Double => Double, start: Double, limit: Double, xscale: Double, yscale: Double )
	{
	val JUMP = 5
	val step = JUMP/xscale
		
		def applyf( v: Double )
		{
			string( (v*xscale).toString )
			space
			string( (f(v)*yscale).toString )
		}
		
		string( "move" )
		space
		
	var cur = start
	
		while (cur < limit)
		{
			applyf( cur )
			space
			string( "line" )
			space
			cur += step
		}
		
		applyf( limit )
	}
	
//	def plot( f: Double => Double, start: Double, limit: Double, xscale: Double, yscale: Double )
//	{
//	val JUMP = 7
//	val step = JUMP/xscale
//	val array = new ArrayBuffer[String]
//	
//		def applyf( v: Double )
//		{
//			array += (v*xscale).toString
//			array += (f(v)*yscale).toString
//		}
//		
//	var cur = start
//	
//		while (cur < limit)
//		{
//			applyf( cur )
//			cur += step
//		}
//		
//		applyf( limit )
//		
//		if (array.length < 8) sys.error( "function plot has too few points" )
//		
//		(array.length - 2)%6 match
//		{
//			case 0 =>
//			case 2 =>
//				array.remove( array.length - 4, 2 )
//			case 4 =>
//				array.remove( array.length - 4, 2 )
//				array.remove( array.length - 6, 2 )
//		}
//		
//		string( "move" )
//		space
//		string( array(0) )
//		space
//		string( array(1) )
//		
//		for (i <- 2 until array.length by 6)
//		{
//		space
//		string( "curve" )
//		space
//		string( array(i) )
//		space
//		string( array(i + 1) )
//		space
//		string( array(i + 2) )
//		space
//		string( array(i + 3) )
//		space
//		string( array(i + 4) )
//		space
//		string( array(i + 5) )
//		}
//	}
	
	// \plot <function> <from> <to> <x-scale> <y-scale>
	//
	defineStrN( "plot",
		("expected fully qualified Scala object method (Double => Double) name", string2function),
		("expected starting value", string2double),
		("expected limiting value", string2double),
		("expected x-scale dimension", string2dimen),
		("expected y-scale dimension", string2dimen) )
	{
		a =>
			plot( a(0).asInstanceOf[Double => Double], a(1).asInstanceOf[Double], a(2).asInstanceOf[Double],
				a(3).asInstanceOf[Double], a(4).asInstanceOf[Double] )
	}
	
	defineStr2MidMode[Dimen, Dimen]( "drawd", "expected draw box width", "expected draw box height" ) {(d1, d2) => draw( Some(d1, d2) )} {_ => {}}
	
	defineStrN( "arc",
		("expected width", string2dimen),
		("expected height", string2dimen),
		("expected start angle", string2double),
		("expected extent angle", string2double),
		("expected arc type (one of open/chord/pie)", string2string ) )
	{
		a =>
			arc( a(0).asInstanceOf[Double], a(1).asInstanceOf[Double],
				a(2).asInstanceOf[Double], a(3).asInstanceOf[Double], a(4).toString, false )
	}
	
	defineDimenStr2( "empty" ) {(d1, d2) => empty( d1, d2 )}
	
	defineDimenStr( "circle" ) {d => circle( d, false )}
	
	defineDimenStr2( "rectangle" ) {(w, h) => rectangle( w, h, false )}
	
	defineDimenStr2( "ellipse" ) {(w, h) => ellipse( w, h, false )}
	
	defineDimenStr( "circlef" ) {d => circle( d, true )}
	
	defineDimenStr2( "rectanglef" ) {(w, h) => rectangle( w, h, true )}
	
	defineDimenStr2( "ellipsef" ) {(w, h) => ellipse( w, h, true )}
	
	defineStr[Dimen]( "thickness", "expected line thickness" ) {t => stroke( new BasicStroke(t.toFloat) )}
	
	define( "space" ) {space}
	
	defineStr[String]( "load", "expected path to font file" )
	{
		path => if (Font.load( resource, path ) == None) sys.error( "font resource not found: " + path )
	}
	
	define( " " )
	{
		paragraph
		interword
	}
	
	defineSpecialChar( "#" )
	
	defineSpecialChar( "$" )
	
	defineSpecialChar( "%" )
	
	defineSpecialChar( "&" )
	
	defineSpecialChar( "_" )
	
	defineSpecialChar( "{" )
	
	defineSpecialChar( "}" )
	
	defineSpecialChar( "\\" )
	
	defineConst( "degree", Typesetter.`DEGREE SIGN` )
	
	defineConst( "bullet", Typesetter.BULLET )
	
	defineConst( "invexclamation", Typesetter.`INVERTED EXCLAMATION MARK` )
	
	defineConst( "invquestion",  Typesetter.`INVERTED QUESTION MARK` )
	
	defineConst( "times", Typesetter.`MULTIPLICATION SIGN` )
	
	defineConst( "pm", Typesetter.`PLUS-MINUS SIGN` )
	
	defineConst( "div", Typesetter.`DIVISION SIGN` )
	
	defineConst( "midpoint", Typesetter.`MIDDLE DOT` )

	defineConst( "dots", Typesetter.`HORIZONTAL ELLIPSIS` )
	
	defineConst( "copyright", Typesetter.`COPYRIGHT SIGN` )
	
//	defineConstChar( "lt", Typesetter.`LESS-THAN SIGN` )
//	
//	defineConstChar( "gt", Typesetter.`GREATER-THAN SIGN` )
	
	defineConst( "O", Typesetter.`LATIN CAPITAL LETTER O WITH STROKE` )
	
	defineConst( "o", Typesetter.`LATIN SMALL LETTER O WITH STROKE` )
	
	defineConst( "L", Typesetter.`LATIN CAPITAL LETTER L WITH STROKE` )
	
	defineConst( "l", Typesetter.`LATIN SMALL LETTER L WITH STROKE` )
	
	defineConst( "oe", Typesetter.`LATIN SMALL LIGATURE OE` )
	
	defineConst( "OE", Typesetter.`LATIN CAPITAL LIGATURE OE` )
	
	defineConst( "ae", Typesetter.`LATIN SMALL LETTER AE` )
	
	defineConst( "AE", Typesetter.`LATIN CAPITAL LETTER AE` )
	
	defineConst( "pounds", Typesetter.`POUND SIGN` )
	
//	defineConst( "tbullet", Typesetter.`TRIANGULAR BULLET` )
	
	defineStr[String]( "'", "expected letter to be accented with accute" ) {s => string( Typesetter.commonAccent(s, Typesetter.`COMBINING ACUTE ACCENT`) )}
	
	defineStr[String]( "`", "expected letter to be accented with grave" ) {s => string( Typesetter.commonAccent(s, Typesetter.`COMBINING GRAVE ACCENT`) )}
	
	defineStr[String]( "^", "expected letter to be accented with circumflex" ) {s => string( Typesetter.commonAccent(s, Typesetter.`COMBINING CIRCUMFLEX ACCENT`) )}
	
	defineStr[String]( "~", "expected letter to be accented with tilde" ) {s => string( Typesetter.commonAccent(s, Typesetter.`COMBINING TILDE`) )}
	
	defineStr[String]( "\"", "expected letter to be accented with diaeresis" ) {s => string( Typesetter.commonAccent(s, Typesetter.`COMBINING DIAERESIS`) )}
	
	defineStr[String]( "?", "expected letter to be accented with cedilla" ) {s => string( Typesetter.commonAccent(s, Typesetter.`COMBINING CEDILLA`) )}
	
	defineStr[String]( "@", "expected letter to be accented with ring" ) {s => string( Typesetter.commonAccent(s, Typesetter.`COMBINING RING ABOVE`) )}
	
	defineStr[String]( "=", "expected letter to be accented with macron" ) {s => string( Typesetter.macronAccent(s) )}
	
	defineMidLocal( "rm" ) {font( fontVariable('rmfont) )}
	
	defineMidLocal( "b" ) {font( fontVariable('bfont) )}
	
	defineMidLocal( "sc" ) {font( fontVariable('scfont) )}
	
	defineMidLocal( "i" ) {font( fontVariable('ifont) )}
	
	defineMidLocal( "s" ) {font( fontVariable('sfont) )}
	
	defineMidLocal( "t" ) {font( fontVariable('tfont) )}
	
	defineMidLocal( "tb" ) {font( fontVariable('tbfont) )}
	
	defineMarker( "ignorespaces" ) {(_, s) => skipSpace( s )}
	
	defineMarker( "setbox" )
	{
		(m, s) =>
		val (rest, b, t1) = boxnumber( m, s )
		val t2 = skipSpace( t1 )
		
			if (t2.isEmpty)
				MarkupReader.error( "expected box", rest )
				
			list
			
		val t3 = processToken( t2 )
		val args = arg
		
			if (args.length != 1 || !args.head.isBox)
				MarkupReader.error( "one box was expected", t2.head )
			
			variable( Symbol(b), args.head )
			t3
	}
	
	defineBox( "box" )
	{
		(s, b) =>
			if (b != None)
			{
				add( b get )
				remove( s )
			}
	}
	
	defineBox( "copy" )
	{
		(_, b) => b map add
	}
	
	defineMarker( "leaders" )
	{
		(m, s) =>
			if (s.isEmpty)
				MarkupReader.error( "expected box", m.rest )
			
			list
			
		val tail = processToken( s )
		
			if (tail.isEmpty)
				sys.error( "expected skip at end of input" )
			
		val tail2 = processToken( tail )
		val args = arg
		
			if (args.length != 2)
				MarkupReader.error( "two arguments were expected", m.rest )
			
		val skip = args(1).asSkip
		
			skip leader args(0).asMeasurable
			add( skip )
			tail2
	}
	
	defineOperation( "advance" )
	{
		(sym, value) => 
			variable( sym, glueVariable(sym) add value.asInstanceOf[Double] )
	}

	defineMarker( "def" ) {(m, _) => readMacroDefinition( m.rest )}
	
	defineStr[Int]( "char", "expected Unicode character value" ) {c => glyph( c.toChar )}
	
	define( "hrule" ) {hrule( 2 )}
	
	define( "vrule" ) {vrule( 2 )}
	
	define( "indent" ) {indent}
	
	define( "m" )
	{
		par
		horizontal
	}
	
	defineDimenStr( "kern" ) {d => kern( d )}
	
	defineGlueStr( "hskip" ) {g => hskip( g )}
	
	defineGlueStr( "vskip" ) {g => vskip( g )}
	
	define( "hfil" ) {hfil}
	
	define( "vfil" ) {vfil}
	
	define( "hss" ) {hss}
	
	define( "vss" ) {vss}
	
	defineMidList( "u" ) {underline( arg, 3, 2 )}
	
	defineMidList( "frame" ) {frame( arg, 3, 1 )}
	
	defineMidList( "centerline" ) {centerline( arg )}
	
	defineMidList( "llap" ) {llap( arg )}
	
	defineMidList( "rlap" ) {rlap( arg )}
	
	defineMidMode( "line" ) {line} ()
	
	defineMidStr[Dimen]( "lower", "expected 'lower' dimension" ) {hbox} {d => shift( box, -d )}
	
	defineMidStr[Dimen]( "raise", "expected 'raise' dimension" ) {hbox} {d => shift( box, d )}

	defineMidStr[Dimen]( "translucent", "expected 'margin' dimension" ) {hbox} {d => translucent( box, d )}

	define( "par" ) {par}
	
	defineMidMode( "hb" ) {hbox} ()
	
	defineStrMidMode[Dimen]( "hbt", "expected 'to' dimension" ) {d => hbox( To(d) )} ()
	
	defineStrMidMode[Dimen]( "hbs", "expected 'spread' dimension" ) {d => hbox( Spread(d) )} ()
	
	defineMidMode( "vb" ) {vbox} {_ => par}
	
	defineStrMidMode[Dimen]( "vbt", "expected 'to' dimension" ) {d => vbox( To(d) )} {_ => par}
	
	defineStrMidMode[Dimen]( "vbs", "expected 'spread' dimension" ) {d => vbox( Spread(d) )} {_ => par}
	
	defineMidMode( "vt" ) {vtop} {_ => par}
	
	defineStrMidMode[Dimen]( "vtt", "expected 'to' dimension" ) {d => vtop( To(d) )} {_ => par}
	
	defineStrMidMode[Dimen]( "vts", "expected 'spread' dimension" ) {d => vtop( Spread(d) )} {_ => par}
	
	defineNonExecutable( "cr" )
	
	defineDefine( "newskip", defineSkipVar )
	
	defineDefine( "newdimen", defineDimenVar )
	
	defineDefine( "newnumber", defineNumberVar )
	
	defineDefine( "newcolor", defineColorVar )
	
	defineDefine( "newfont", defineFontVar )
	
	defineNumberVar( "dpi" )
	
	defineSkipVar( "tabskip" )
	
	defineSkipVar( "leftkip" )
	
	defineSkipVar( "rightskip" )
	
	defineSkipVar( "parskip" )
	
	defineSkipVar( "parfillskip" )
	
	defineDimenVar( "hsize" )
	
	defineDimenVar( "vsize" )
	
	defineDimenVar( "hangindent" )
	
	defineDimenVar( "parindent" )
	
	defineColorVar( "color" )
	
	defineColorVar( "vcolor" )
	
	defineFontVar( "font" )
	
	defineFontVar( "rmfont" )
	
	defineFontVar( "bfont" )
	
	defineFontVar( "ifont" )
	
	defineFontVar( "sfont" )
	
	defineFontVar( "scfont" )
	
	defineFontVar( "tfont" )
	
	defineFontVar( "tbfont" )
	
	defineFontVar( "vfont" )
	
	defineMarker( "halign" )
	{
		(m, _) =>
			val margin = glueVariable( 'tabskip )
			val toks = readTokenStream( m.rest )
			val (arg, end, rest) = argumentGroup( toks )
			val rows = split( arg, List(Marker("cr")) )
			
			if (rows.isEmpty || rows.last != List())
				MarkupReader.error( """missing \cr at end of \halign specification""", end )

			val initialbuf = new ListBuffer[(List[Token], Glue)]
			val repeatbuf = new ListBuffer[(List[Token], Glue)]
			val buf = new ListBuffer[Token]
			val NORMAL = 0
			val PARAM = 1
			val ALIGN = 2
			var state = NORMAL
			var bufptr = initialbuf
			var initial = true
			
			def readTemplate( l: LinearSeq[Token] ): Unit =
				if (l.isEmpty)
				{
					if (state != PARAM)
						MarkupReader.error( "missing parameter character (#)", arg find (_ == Marker("cr")) get )
						
					bufptr += ((buf.toList, glueVariable('tabskip)))
				}
				else
				{
					l.head match
					{
					case Marker( "tabskip" ) =>
						readTemplate( processToken( l ) )
					case t@Param() =>
						if (state == PARAM)
							MarkupReader.error( "expected exactly one appearance of the parameter character (#)", t )
							
						buf += t
						state = PARAM
						readTemplate( l.tail )
					case t@Align() =>
						if (state == ALIGN)
							if (initial)
							{
								bufptr = repeatbuf
								initial = false
								state = NORMAL
							}
							else
								MarkupReader.error( "only one double alignment separator (&&) is allowed in the template", t )
						else if (state == PARAM)
						{
							bufptr += ((buf.toList, glueVariable('tabskip)))
							buf.clear
							state = ALIGN
						}
						else
							MarkupReader.error( "missing parameter character (#)", t )
							
						readTemplate( l.tail )
					case t =>
						buf += t
						readTemplate( l.tail )
					}					
				}
			
			dup
			readTemplate( rows.head )
			
			val tempbuf = new ListBuffer[(List[Token], Glue)]

			tempbuf ++= initialbuf
			
			var current: List[Token] = null
			val tabbuf = new ListBuffer[List[List[Token]]]
			var widest = 0
			
			for (r <- rows.tail dropRight 1 map (rr => {current = rr; split( rr, List(Align()) )}))
			{
			val len = r.length
			
				widest = widest max len
				
				if (len > tempbuf.length)
					if (repeatbuf.isEmpty)
						MarkupReader.error( "too many columns in this row", current.head )
					else
						for (i <- 0 to (len - tempbuf.length)/repeatbuf.length)
							tempbuf ++= repeatbuf

				tabbuf += r
			}

			val templates = tempbuf.toList
			val inserts = tabbuf.toList
			val table = inserts map (r => (r zip templates) map
				(c => processList(expandTokenStream(replaceParameters(c._2._1, IndexedSeq(c._1), false)))))
			
			pop
			halign( Natural, table, margin, templates take widest map (_._2) )
			expandTokenStream( rest )
	}

	def phant( h: Boolean, v: Boolean )
	{
	val b = box
	
		add( new SpaceBox(true, if (h) b.width else 0, if (v) b.ascent else 0, if (v) b.descent else 0, null) )
	}
	
	defineMid( "vphantom" ) {hbox} {phant(false, true)}
	
	defineMid( "hphantom" ) {hbox} {phant(true, false)}
	
	defineMid( "phantom" ) {hbox} {phant(true, true)}
	
	defineArg( "print" )
	{
		(a, _) =>
			def p( l: List[Token] ): Unit =
				if (!l.isEmpty)
				{
					l.head match
					{
					case Str( s ) => out( s )
					case QStr( s ) => out( s )
					case Space() => out( " " )
					}
					
					p( l.tail )
				}
			
			p( a )
			out()
	}
	
	defineStr( "v", "expected verse number" )
	{
		v: String =>
			horizontal
			dup
			
//		val baselineskip = glueVariable( 'baselineskip ).natural
		val capital = font.capital
		
			font( fontVariable('vfont) )
			color( colorVariable('vcolor) )
			
			paragraph
			hbox
			glyphs( v, false )
		
		val b = box
		
			shift( b, capital - b.height )
			
//			shift( v, baselineskip*.3 )
			pop
			hspace( 1 )
	}

	defineArg( "vv" ) {(a, _) => process( pp(a) )}
	
	define( "vars" ) {vars}
	
	defineArg( "show" ) {(a, _) => out( a )}
	
	define( "fonts" ) {println( Font.jfonts.mkString(", ") )}
	
	private def pp( l: List[Token] ): List[Token] =
		l match
		{
			case Nil =>
				Nil
			case Space() :: t =>
				_pp( t )
			case _ => _pp( l )
		}

	private def _pp( l: List[Token] ): List[Token] =
		l match
		{
			case Nil =>
				Nil
			case Str(s) :: Space() :: t if (s matches "\\d+") =>
				Marker( "v" ) :: Str( s ) :: _pp( t )
			case (tok@Str(s)) :: t if (s matches ".+\\[.+\\].*|.*\\[.+\\].+") =>
				Str( s.replaceAll("\\[.+\\]", "") ).pos( tok.pos ) :: _pp( t )
			case h :: t =>
				h :: _pp( t )
		}
}