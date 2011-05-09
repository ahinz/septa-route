package org.hinz.septa;

import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.SQLException
import java.sql.Statement

import scala.xml._
import scala.io.Source

class DBWriter(db:String) {
  Class.forName("org.sqlite.JDBC")

  def writeSinglePoint(statement:Statement, rid:Int,  lat1: Double, lon1: Double, dist: Double) =
    statement.executeUpdate("insert into route_data (route_id, lat, lon, ref) values (" +
                            rid + ", " + lat1 + ", " + lon1 + ", " + dist + ")")

  def writePoints(routeid:Int, pts: List[(Double,Double)]) = {
    val connection = DriverManager.getConnection("jdbc:sqlite:/Users/ahinz/src/scala/septa/devdb.db")
    val statement = connection.createStatement();
    
    addDistanceToPts(pts).map(x => writeSinglePoint(statement, routeid, x._1,x._2,x._3))
  }

  def addDistanceToPts(l: List[(Double,Double)], acc: List[(Double,Double,Double)]=Nil):List[(Double,Double,Double)] = l match {
    case Nil => acc.reverse
    case x::xs => 
      if (acc.length == 0)
        addDistanceToPts(xs, (x._1,x._2,0.0) :: acc)
      else
        addDistanceToPts(xs, (x._1,x._2,acc.head._3 + 
                              GIS.distanceCalculator(x._1,x._2,acc.head._1,acc.head._2)) :: acc)
  }
        
}


class KMLParser {

  val file = XML.loadString(Source.fromFile("/Users/ahinz/Downloads/23.kml").getLines.mkString("\n"))
  
  def coords(xml:NodeSeq) = {
    (xml \\ "coordinates").map(_.text.split(" ").toList.map(llpair => {
      val ll = llpair.split(",")
      if (ll.length == 3) {
        Some((ll(1).toDouble, ll(0).toDouble))
      } else {
        None
      }
    }).flatMap(x => x)).toList
  }

  val pts = coords(file) //splitIntersections(coords(file)).flatMap(x => x)

  // Intersert
  val pts2 = List(pts(2).reverse,pts(0).reverse,pts(5).reverse,pts(6)).flatten

  //new DBWriter("2").writePoints(1,pts2)

  def test() = {
    val lines = coords(file)
    lines
  }
}

import swing._
import scala.swing.event._
import java.awt.geom._
import java.awt.Color

class GISPanel(k: KMLParser) extends Panel {
 
  var sel:List[(Double,Double)] = null
  val pts = k.pts
  val lats = pts.flatMap(_.map(_._1))
  val lons = pts.flatMap(_.map(_._2))

  val boundsLat = (lats.max, lats.min)
  val boundsLon = (lons.max, lons.min)

  val xScale:Double = 950.0 //size.width
  val yScale:Double = 550.0 //size.height

  val scaleTransform = AffineTransform.getScaleInstance(
    xScale / (boundsLat._1 - boundsLat._2),
    yScale/ (boundsLon._2 - boundsLon._1))

  val translateTransform = AffineTransform.getTranslateInstance(
    - boundsLat._2,  - boundsLon._1)

  translateTransform.preConcatenate(scaleTransform)

  val transform = translateTransform

  listenTo(mouse.clicks)

  def d(x1: Double, y1:Double, x2:Double, y2: Double) =
    math.sqrt((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1-y2))

  def doClick(p: Point) = {
    val latlon = transform.inverseTransform(p, null)

    // Find nearest point
    val minpt = pts.flatMap(x => x.map(a => (a,d(a._1,a._2, latlon.getX, latlon.getY)))).sortWith(_._2 < _._2).head._1

    // Find that row
    sel = pts.filter(_.contains(minpt)).head

    // Output the index
    println("Selected index: " + pts.indexOf(sel))

    repaint()
  }

  reactions += {
    case MouseClicked(_,p,_,_,_) => doClick(p)
  }

  override def paint(g: Graphics2D) = {

    val colors = List(Color.BLUE,Color.GREEN)

    var k = 0

    // 2,0,5,6
    val pts2 = List(List(pts(2).reverse,pts(0).reverse,pts(5).reverse,pts(6)).flatten)

    var ptx:Point2D = null

    g.setColor(Color.WHITE)
    g.fillRect(0,0,xScale.toInt,yScale.toInt)

    pts2.map( p => {

      if (p == sel) {
        g.setColor(Color.RED)
      } else {
        g.setColor(colors(k % colors.length))
        k += 1
      }

      p.zip(p.tail).map(p => {
        val px = new Point2D.Double(p._1._1,p._1._2);
        val p1:Point2D = transform.transform(new Point2D.Double(p._1._1,p._1._2), null)
        val p2:Point2D = transform.transform(new Point2D.Double(p._2._1,p._2._2), null)

        g.drawLine(p1.getX.toInt, p1.getY.toInt, p2.getX.toInt, p2.getY.toInt)
      })

      val pt = p.head //last
      val p1:Point2D = transform.transform(new Point2D.Double(pt._1,pt._2), null)
     
      g.fillOval(p1.getX.toInt, p1.getY.toInt, 5, 5)

      if (p == sel) {
        ptx = p1
      }

      val pt2 = p.head
      val p2:Point2D = transform.transform(new Point2D.Double(pt2._1,pt2._2), null)

//      g.fillRect(p2.getX.toInt,p2.getY.toInt,5,10)
    });

    g.setColor(Color.RED)
    if (ptx != null) {
      g.fillOval(ptx.getX.toInt, ptx.getY.toInt, 10,10)
    }
  }
}

object HelloWorld extends SimpleSwingApplication {
  def top = new MainFrame {
    size = new java.awt.Dimension(1000,600)
    preferredSize = new Dimension(1000,600)
    title = "Hello, World!"
    contents = new GISPanel(new KMLParser())
  }
}

/*
object Main {

  def main(args: Array[String]) = {
    println(KMLParser.test())

  }
}*/

object T {}
