package geoslick

import org.scalatest._

import PostgisDriver.simple._
import Database.threadLocalSession

import com.vividsolutions.jts.geom.{Point,Geometry,GeometryFactory,PrecisionModel}
import com.vividsolutions.jts.io.WKTReader

import util._

class PostgisSpec extends FlatSpec {

  object SimpleCity extends PostgisTable[(Int,String)]("cities") {
    
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")

    def * = id ~ name
    def forInsert = name
  }

  object City extends PostgisTable[(Int,String,Geometry)]("cities") {
      
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def geom = geoColumn[Point]("geom", 4326)

    def * = id ~ name ~ geom
    def forInsert = name ~ geom
  }

  val pguser = scala.util.Properties.envOrElse("PGUSER","postgres")
  val pgpass = scala.util.Properties.envOrElse("PGPASS","postgres")
  val pgdb = scala.util.Properties.envOrElse("PGDB","slick")
  val pghost = scala.util.Properties.envOrElse("PGHOST","localhost:5432")

  val db = Database.forURL("jdbc:postgresql://" + pghost + "/" + pgdb,
                           driver="org.postgresql.Driver",
                           user=pguser,
                           password=pgpass)

  "Environment" should "be sane" in {    
    db withSession {
      // Make sure things are clean
      // we probably shouldn't need this
      try { SimpleCity.ddl.drop } catch { case e: Throwable =>  }

      val cities = Seq("washington","london","paris")

      SimpleCity.ddl.create
      SimpleCity.forInsert.insertAll(cities:_*)

      val q = for { c <- SimpleCity } yield c.name

      assert(q.list==cities)

      val q2 = for { c <- SimpleCity if c.id > 1 } yield c
      q2.delete

      assert({ for { c <- SimpleCity } yield c }.list.length == 1)

      val q3 = for { c <- SimpleCity } yield c
      q3.delete

      assert({ for { c <- SimpleCity } yield c }.list.length == 0)

      SimpleCity.ddl.drop
    }
  }

  "Postgis driver" should "be able to insert geoms" in {
    db withSession {
      // Make sure things are clean
      // we probably shouldn't need this
      try { City.ddl.drop } catch { case e: Throwable =>  }

      City.ddl.create
      City.forInsert.insertAll(data:_*)

      val q = for { c <- City } yield (c.name, c.geom.asEWKB())
      
      info(q.selectStatement)
      info(q.list.toString)
      info(data.toList.toString)
      assert(q.list==data.toList)
                                  
      City.ddl.drop
    }
  }

  it should "be able to delete all" in {
    db withSession {
      // Make sure things are clean
      // we probably shouldn't need this
      try { City.ddl.drop } catch { case e: Throwable =>  }

      City.ddl.create
      City.forInsert.insertAll(data:_*)

      val q1 = for { c <- City } yield c
      assert(q1.list.length == data.length)

      val q2 = for { c <- City } yield c
      q2.delete

      val q3 = for { c <- City } yield c
      assert(q3.list.length == 0)
                                  
      City.ddl.drop    
    }
  }

  it should "be able to delete with geom where clause" in {
    db withSession {
      // Make sure things are clean
      // we probably shouldn't need this
      try { City.ddl.drop } catch { case e: Throwable =>  }

      City.ddl.create
      City.forInsert.insertAll(data:_*)

      // 40.30, 78.32 -> Altoona,PA
      val bbox = bboxBuffer(78.32, 40.30, 0.01)
      
      val q = for { c <- City if c.geom && bbox } yield c
      q.delete

      val q2 = for { c <- City } yield c.name
      assert(q2.list == data.map(_._1).filter(_ != "Altoona,PA").toList)

      City.insert(4000, "ATown",pt(-55.1,23.3))

      val q3 = for { c <- City if c.id =!= 4000 } yield c
      q3.delete

      val q4 = for { c <- City } yield c.name
      assert(q4.list == List("ATown"))
                                  
      City.ddl.drop    
    }
  }

  it should "be able to query with geo fcns" in {
    db withSession {
      // Make sure things are clean
      // we probably shouldn't need this
      try { City.ddl.drop } catch { case e: Throwable =>  }

      City.ddl.create
      City.ddl.createStatements.foreach(info(_))
      City.forInsert.insertAll(data:_*)

      // 40.30, 78.32 -> Altoona,PA
      val bbox = bboxBuffer(78.32, 40.30, 0.01)
      info("a")
      // Operator
      val q = for {
          c <- City if c.geom && bbox // && -> overlaps
        } yield c.name

      assert(q.list == List("Altoona,PA"))
      info("a")
      // Function
      val dist = 0.5
      val q2 = for {
          c1 <- City
          c2 <- City if c1.geom.distance(c2.geom) < dist && c1.name =!= c2.name
        } yield (c1.name, c2.name, c1.geom.distance(c2.geom))

      info(q2.selectStatement)
        
      val q2format = q2.list map {
          case (n1,n2,d) => (n1,n2,"%1.4f" format d)
        }
      info("a")
      val jts = for {
          j1 <- data
          j2 <- data if j1._2.distance(j2._2) < dist && j1._1 != j2._1
        } yield (j1._1, j2._1, "%1.4f" format j1._2.distance(j2._2))

      assert(q2format == jts.toList)
          
      // Output function
      val q3 = for {
          c <- City if c.name === "Reading,PA"
        } yield c.geom.asGeoJson()

      info("a")
      assert(q3.first == """{"type":"Point","coordinates":[76,40.4]}""")

      City.ddl.drop
    }
  }

  object OptCity extends PostgisTable[(Int,String,Option[Geometry])]("cities") {
      
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def geom = geoColumn[Option[Point]]("geom", 4326)

    def * = id ~ name ~ geom
    def forInsert = name ~ geom
  }

  it should "be able to handle optional fields" in {
    db withSession {
      try { OptCity.ddl.drop } catch { case e: Throwable => }

      OptCity.ddl.create

      val cities = Seq(("washington",Some(pt(-77.02,38.53))),
                       ("london", None),
                       ("paris", Some(pt(2.3470,48.8742))))

      OptCity.forInsert.insertAll(cities:_*)

      val q1 = for {
          c <- OptCity if c.geom isNull
        } yield (c.name, c.geom)

      assert(q1.list == List(("london",None)))

      val q2 = for {
          c <- OptCity if c.geom isNotNull
        } yield c.name

      assert(q2.list == List("washington","paris"))

      OptCity.ddl.drop
    }
  }

  it should "be able to query with geo fcns on null fields" in {
    db withSession {
      // Make sure things are clean
      // we probably shouldn't need this
      try { OptCity.ddl.drop } catch { case e: Throwable =>  }

      val data2 = data.map {
          case (s,g) => (s, Some(g))
        }

      OptCity.ddl.create
      OptCity.forInsert.insertAll(data2:_*)

      // 40.30, 78.32 -> Altoona,PA
      val bbox = bboxBuffer(78.32, 40.30, 0.01)
      
      val q = for {
          c <- OptCity if c.geom && bbox // && -> overlaps
        } yield c.name

      assert(q.list == List("Altoona,PA"))

      OptCity.ddl.drop
    }
  }


  it should "be able to handle generic geom fields" in {
    // if this compiles we're golden
    object Foo extends PostgisTable[(Int,String,Option[Geometry])]("foo") {
      
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def geom = geoColumn[Option[Geometry]]("geom", 4326)

      def * = id ~ name ~ geom
      def forInsert = name ~ geom
    }

    object Bar extends PostgisTable[(Int,String,Geometry)]("bar") {
      
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def geom = geoColumn[Geometry]("geom", 4326)

      def * = id ~ name ~ geom
      def forInsert = name ~ geom
    }

  }
}
