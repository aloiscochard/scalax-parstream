package scalax

import org.specs2.mutable._
import ParStream._

// TODO [aloiscochard] Add some sense to this pseudo-benchmark

class WikipediaBenchmark extends Specification {
  def stopwatch[T](f: => T) = { val start = System.currentTimeMillis; val r = f; (r, System.currentTimeMillis - start) }

  val folderPath = "/tmp/"

  "Wikipedia" should {

    "benchmark - nopar" in {
      val (size, time) = stopwatch { process(images) }
      println("nopar:\t\t %sms (%s)".format(time, size))
      true mustEqual true
    }

    "benchmark - par" in {
      val (size, time) = stopwatch { process(images.toList.par.iterator) }
      println("par:\t\t %sms (%s)".format(time, size))
      true mustEqual true
    }

    "benchmark - parstream" in {
      val (size, time) = stopwatch { process(images.parstream) }
      println("parstream:\t %sms (%s)".format(time, size))
      true mustEqual true
    }

  }

  def process(images: Iterator[String]) =
    images.map(download(_)).map(write(_)).sum


  def images = WikipediaClient.randomImages()

  def download(urlImage: String) = {
    val name = urlImage.splitAt(urlImage.lastIndexOf("/") + 1)._2
    val data = WikipediaClient.download(urlImage)
    (name, data)
  }

  def write(image: (String, Array[Byte])) = {
    val path = folderPath + image._1
    val f = new java.io.File(path)
    f.delete
    val fos = new java.io.FileOutputStream(f)
    fos.write(image._2)
    fos.close
    image._2.length
  }
}


import dispatch._
import dispatch.liftjson.Js._
import net.liftweb.json.JsonAST._

object WikipediaClient {
  class HttpNoLog extends Http {
    override def make_logger = new Logger {
      def info(msg: String, items: Any*) {}
      def warn(msg: String, items: Any*) {}
    }
  }

  def download(urlImage: String) = {
    val h = new HttpNoLog
    val stream = new java.io.ByteArrayOutputStream
    h(url(urlImage) >>> stream)
    h.shutdown
    stream.toByteArray
  }

  def randomImages(limit: Int = 40) = new Iterator[String] {
    val h = new HttpNoLog

    def hasNext: Boolean = {
      val n = (total < limit)
      if (!n) h.shutdown
      n
    }

    def next: String = { total = total +1; read }

    private var total: Int = 0
    private var urlNext = urlImage

    private def read = {
      val json = h(urlNext ># { json => json.children.flatMap(_ match {
        case JField("query",JObject(List(JField("allimages",JArray(List(JObject(list))))))) =>
          list.flatMap(_ match {
            case JField("url",JString(imageUrl)) => Some(imageUrl)
            case _ => None
          })
        case JField("query-continue",JObject(List(JField("allimages",JObject(List(JField("aifrom",JString(nextUrl)))))))) =>
          Some(nextUrl)
        case _ => None
      })})

      urlNext = urlImageNext(json(1))
      json(0)
    }
  }
  
  private final val urlImage = :/("en.wikipedia.org") / "w/api.php" <<?
    Map("action" -> "query",
      "list" -> "allimages",
      "aiprop" -> "url",
      "ailimit" -> "1",
      "format" -> "json") <:<
    Map("User-Agent" -> "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Win64; x64; Trident/5.0)")

  private final val urlImageNext = (from: String) => urlImage <<? Map("aifrom" -> from)
}


