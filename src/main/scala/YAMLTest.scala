package markup

import java.awt.{AlphaComposite}
import java.awt.RenderingHints._
import java.awt.Color._
import java.awt.image.{BufferedImage}
import javax.imageio.{ImageIO}
import java.io._

import swing._
import math._


object YAMLTest extends MainFrame with App
{
//	val yaml =
//		new YAML
//		{
//		val resource = getClass
//			
//		}
//	val p = yaml processDocument
//"""asdf"""

//	def f( x: Double ) = sin(4*x)/* + sin(2*x)*/
//	
//\frame \draw {mc \circlef 5 org \path {\plot markup.YAMLTest.f -3.14 3.14 50 50}}

//	\tabskip20pt
//	\halign
//		{\hfil#\hfil&\hfil#&&\b#\hfil\cr
//		one&two&un&deux\cr
//		three&four&throis&quatre\cr
//		five&six&cinq&six\cr
//		seven&eight&sept&huit\cr
//		}

//\drawd 0 0 {\image image.jpg}
//\ltnkjv {Psalm 37:39-40}
//	{
//	39 But the salvation of the righteous is from the \sc Lord;
//	He is their strength in the time of trouble.
//	40 And the \sc Lord shall help them and deliver them;
//	He shall deliver them from the wicked,
//	and save them,
//	because they trust in Him.
//	}

	val IMAGE_WIDTH = 1280
	val IMAGE_HEIGHT = 720
	val fgimg = new BufferedImage( IMAGE_WIDTH, IMAGE_HEIGHT, BufferedImage.TYPE_INT_ARGB_PRE )
	val fg = fgimg.getGraphics.asInstanceOf[Graphics2D]
	val TRANSPARENT = AlphaComposite.getInstance( AlphaComposite.SRC, 0 )

		fg setColor BLACK
		fg setComposite TRANSPARENT
		fg.fillRect( 0, 0, IMAGE_WIDTH, IMAGE_HEIGHT )
		fg.setRenderingHint( KEY_ANTIALIASING, VALUE_ANTIALIAS_ON )
//		p.draw( fg, 0, 0 )
//		ImageIO.write( fgimg, "PNG", new File("wallpaper.png") )

	contents =
		new Panel
		{
			preferredSize = new Dimension( IMAGE_WIDTH, IMAGE_HEIGHT )
			
			override def paint( g: Graphics2D )
			{
				super.paint( g )
				g.drawImage( fgimg, 0, 0, null )
//				p.box( g, MARGIN, MARGIN, CYAN )
			}
		}
	visible = true
}
