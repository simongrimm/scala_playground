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

    val baseComponents = url.split("://", 2).map(str => str.stripSuffix("/"))
      .flatMap(str => str.split("/", 2)).toList


    val (baseUrl : Map[String, String], pathAndQuery: String) = baseComponents match {
      case List(pro, dom) => (Map("protocol" -> pro, "domain" -> dom), "")
      case List(pro, dom, res) => (Map("protocol" -> pro, "domain" -> dom), res)
    }

    def parseExtendedUrl(): Map[String, AnyRef] = {

      def parsePathAndQuery(): Map[String, AnyRef] = {
        val (path, query) = pathAndQuery.split("\\?", 2).toList match {
          case List(pth, qry) => (pth, qry)
        }

        lazy val queryComponents: Map[String, String] = {
          (for (q <- query.split("&")
            .flatMap(str => str.split("=", 2)).grouped(2)) yield (q(0) -> q(1))).toMap
        }

        val urlMappingExtension = query match {
          case query if query.contains('=') => Map("path" -> path, "query" -> queryComponents)
          case _ => Map("path" -> path, "query" -> query)
        }
        urlMappingExtension
      }

      val extendedUrl = pathAndQuery match {
        case _ if pathAndQuery.isEmpty => baseUrl
        case _ if pathAndQuery.contains('?') => baseUrl ++ parsePathAndQuery()
        case _ => baseUrl + ("path" -> pathAndQuery)
      }
      extendedUrl
    }

    //baseUrl
    parseExtendedUrl()
  }



  def main(args: Array[String]) {
    assert(parseUrl("http://holidaycheck.com/") == Map("protocol" -> "http", "domain" -> "holidaycheck.com"))
    assert(parseUrl("https://holidaycheck.com/passions?q=yoga") == Map("protocol" -> "https", "domain" -> "holidaycheck.com", "path" -> "passions", "query" -> Map("q" -> "yoga")))
    assert(parseUrl("https://holidaycheck.com/passions?q=yoga?oder?Pilates&place=Konstanz")  == Map("protocol" -> "https", "domain" -> "holidaycheck.com", "path" -> "passions", "query" -> Map("q" -> "yoga?oder?Pilates", "place" -> "Konstanz")))
    assert(parseUrl("https://holidaycheck.com/passions?all")  == Map("protocol" -> "https", "domain" -> "holidaycheck.com", "path" -> "passions", "query" -> "all"))

    println(parseUrl("https://holidaycheck.com/passions?q=yoga"))
    println(parseUrl("http://holidaycheck.com/"))

  }
}
