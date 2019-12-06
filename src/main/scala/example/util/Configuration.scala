package example.util

import com.typesafe.config.ConfigFactory

object Configuration {

  private val conf = ConfigFactory.load().getConfig("advent-of-code")

  def sessionID = conf.getString("session-id")

}
