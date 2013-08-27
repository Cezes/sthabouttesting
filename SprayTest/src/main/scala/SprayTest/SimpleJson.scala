package SprayTest


import spray.httpx.unmarshalling.{Unmarshaller, pimpHttpEntity}
import spray.json._


case class Person(name: String, age: Int, sex: String, address: String)


trait MyJsonProtocol extends DefaultJsonProtocol {
  implicit val PersonFormat = jsonFormat(Person, "name", "age", "sex", "address")
}
