// #1
//input = 'http://holidaycheck.com/'
//output = { protocol: 'http', 'domain': 'holidaycheck.com' }
/*
// #2
input = 'https://holidaycheck.com/passions?q=yoga'
output = { protocol: 'http', 'domain': 'holidaycheck.com',
path: 'passions', query: {q: 'yoga'} */


package URLParser

object URLParser {

 def parseUrl(url: String): Map[String, AnyRef] = {

    def parseBaseUrl(url: String): (Map[String, String], String) = {
      val (protocol, domainPathQuery) = url.split("://", 2) match {
          case Array(prot, dpq) => (prot, dpq)
          case _ => ("error", "error")
      }
      val (domain, pathQuery) = domainPathQuery.stripSuffix("/").split("/", 2) match {
        case Array(dom, pq) => (dom, pq)
        case Array(dom) => (dom, "")
      }
      (Map("protocol" -> protocol, "domain" -> domain), pathQuery)
    }

    def parseExtendedUrl(pathAndQuery: String): Map[String, AnyRef] = {
      val (path, query) = pathAndQuery.split("\\?", 2) match {
        case Array(pth, qry) => (pth, qry)
        case Array(pth) => (pth, "")
      }
      lazy val queryComponents: Map[String, String] = {
        (for (q <- query.split("&").flatMap(str => str.split("=", 2)).grouped(2)) yield (q(0) -> q(1))).toMap
      }
      val extendedUrl : Map[String, AnyRef] = (path, query) match {
        case (path, query) if path.isEmpty => Map() 
        case (path, query) if query.isEmpty => Map("path" -> path)
        case (path, query) if !query.contains('=') => Map("path" -> path, "query" -> query)
        case (path, query) => Map("path" -> path, "query" -> queryComponents)
      }
      extendedUrl
    }
    
    val (baseUrl, pathAndQuery) = parseBaseUrl(url)
    baseUrl ++ parseExtendedUrl(pathAndQuery)
  }



  def main(args: Array[String]) {
    assert(parseUrl("http://holidaycheck.com") == 
      Map("protocol" -> "http", "domain" -> "holidaycheck.com"))
    assert(parseUrl("http://holidaycheck.com/") == 
      Map("protocol" -> "http", "domain" -> "holidaycheck.com"))
    assert(parseUrl("http://holidaycheck.com/passions/index.html") == 
      Map("protocol" -> "http", "domain" -> "holidaycheck.com", "path" -> "passions/index.html"))
    assert(parseUrl("https://holidaycheck.com/passions?q=yoga") == 
      Map("protocol" -> "https", "domain" -> "holidaycheck.com", "path" -> "passions", "query" -> Map("q" -> "yoga")))
    assert(parseUrl("https://holidaycheck.com/passions?q=yoga?oder?Pilates&place=Konstanz") ==
      Map("protocol" -> "https", "domain" -> "holidaycheck.com", "path" -> "passions", "query" -> Map("q" -> "yoga?oder?Pilates", "place" -> "Konstanz")))
    assert(parseUrl("https://holidaycheck.com/passions?all") == 
      Map("protocol" -> "https", "domain" -> "holidaycheck.com", "path" -> "passions", "query" -> "all"))
    assert(parseUrl("https//holidaycheck.com/passions?all") == 
      Map("protocol" -> "error", "domain" -> "error"))

    
    println(parseUrl("http://holidaycheck.com/"))
    println(parseUrl("https://holidaycheck.com/passions?q=yoga"))
    //println(parseUrl("https://holidaycheck.com/passions?q=yoga?oder?Pilates&place=Konstanz"))
    //println(parseUrl("http://holidaycheck.com/passions/index.htm"))
    //println(parseUrl("https//holidaycheck.com/passions?all"))

  }
}
