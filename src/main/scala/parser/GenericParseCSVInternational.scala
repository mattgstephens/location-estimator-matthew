package parser

import java.io.{BufferedReader, FileReader}

object GenericParseCSVInternational extends App {

  def parseCSVInternational(filename: String): List[String] = {
//    try {
//      val br: BufferedReader = new BufferedReader(new FileReader(filename))
//      val string_list = LazyList.continually(br.readLine()).takeWhile(_ != null)
//      string_list.toList
//    } catch {
//      case e: FileNotFoundException => System.out.println(e.getMessage)
//    }
    val br: BufferedReader = new BufferedReader(new FileReader(filename))
    val string_list = LazyList.continually(br.readLine()).takeWhile(_ != null)
    string_list.toList
  }

}
