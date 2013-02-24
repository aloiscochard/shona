package shona

import org.specs2.mutable._

import org.json4s._
//import org.json4s.JsonAST._
import org.json4s.native.JsonMethods._

class ShonaJsonSpec extends Specification {
  val json = parse("""
    {
      "name": "Alois",
      "contact": {
        "skype": "alois.cochard"
      },
      "accounts": [{
        "name": "GitHub",
        "address": "github.com/aloiscochard",
        "type": "code"
      }, {
        "name": "Blogger",
        "address": "aloiscochard.blogspot.com",
        "type": "blog"
      }]
    }
  """).asInstanceOf[JObject]

  "ShonaJson" should {
    "extract expression" in {
      def extract(expression: String) = Expression(expression).right.toOption.flatMap(Json(json))

      extract("name") === Some(JString("Alois"))
      extract("contact.skype") === Some(JString("alois.cochard"))
      extract("accounts.name") === Some(JArray(List(JString("GitHub"), JString("Blogger"))))
      extract("accounts(type, address)") === Some(JArray(List(JObject(List(("type",JString("code")), ("address",JString("github.com/aloiscochard")))), JObject(List(("type",JString("blog")), ("address",JString("aloiscochard.blogspot.com")))))))
    }
  }
}

