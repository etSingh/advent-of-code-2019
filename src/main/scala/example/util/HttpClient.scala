package example.util

import sttp.client._


object HttpClient {

  val sessionID: String = Configuration.sessionID

  implicit val backend: SttpBackend[Identity, Nothing, NothingT]
  = HttpURLConnectionBackend()

  def get(uri: String): Either[String, String] = {
    if (sessionID.isEmpty) {
      Left("Please set your session id in application.conf")
    }
    else {
      val request = basicRequest
        .cookie("session", sessionID)
        .get(uri"$uri")
      val response = request.send()
      response.body
    }
  }
}