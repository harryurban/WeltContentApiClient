package de.welt.contentapi.core.client.services.configuration

import javax.inject.{Inject, Singleton}

import de.welt.contentapi.core.client.services.exceptions.BadConfigurationException
import play.api.Configuration

case class ServiceConfiguration(serviceName: String, host: String, endpoint: String, username: String, password: String)

object ServiceConfiguration {
  def fromConfig(serviceName: String, config: Configuration): Option[ServiceConfiguration] =
    for {
      host ← config.getString("host")
      endpoint ← config.getString("endpoint")
      username ← config.getString("credentials.username")
      password ← config.getString("credentials.password")
    } yield ServiceConfiguration(serviceName, host, endpoint, username, password)
}

sealed trait ContentClientConfig {

  def configuration: Configuration

  def getServiceConfig(serviceName: String): ServiceConfiguration = configuration.getConfig(s"welt.api.$serviceName")
    .flatMap(cfg => ServiceConfiguration.fromConfig(serviceName, cfg))
    .getOrElse(throw BadConfigurationException(s"Service at $serviceName was not properly configured"))

  object aws {
    private lazy val s3Config = configuration.getConfig("welt.aws.s3")
    lazy val endpoint: Option[String] = s3Config.flatMap(_.getString("endpoint"))

    object s3 {
      object janus {
        lazy val bucket: String = s3Config.flatMap(_.getString("janus.bucket"))
          .getOrElse(throw BadConfigurationException("'welt.aws.s3.janus.bucket' not configured"))
        lazy val file: String = s3Config.flatMap(_.getString("janus.file"))
          .getOrElse(throw BadConfigurationException("'welt.aws.s3.janus.file' not configured"))

      }
      object pressed {
        lazy val bucket: String = s3Config.flatMap(_.getString("pressed.bucket"))
          .getOrElse(throw BadConfigurationException("'welt.aws.s3.pressed.bucket' not configured"))
        lazy val file: String = s3Config.flatMap(_.getString("pressed.file"))
          .getOrElse(throw BadConfigurationException("'welt.aws.s3.pressed.file' not configured"))
      }

      object sectionMetadata {
        lazy val bucket: Option[String] = s3Config.flatMap(_.getString("sectionMetadata.bucket"))
        lazy val file: Option[String] = s3Config.flatMap(_.getString("sectionMetadata.file"))
      }
    }
  }

  object digger {
    private lazy val diggerConfig = configuration.getConfig("welt.digger")
    lazy val host: String = diggerConfig.flatMap(_.getString("host"))
      .getOrElse(throw BadConfigurationException("'welt.digger.host' not configured"))
    lazy val endpoint: String = diggerConfig.flatMap(_.getString("endpoint"))
      .getOrElse(throw BadConfigurationException("'welt.digger.endpoint' not configured"))
    lazy val username: String = diggerConfig.flatMap(_.getString("username"))
      .getOrElse(throw BadConfigurationException("'welt.digger.username' not configured"))
    lazy val password: String = diggerConfig.flatMap(_.getString("password"))
      .getOrElse(throw BadConfigurationException("'welt.digger.password' not configured"))
  }

  object datadog {
    lazy val statsdHost: Option[String] = configuration.getString("datadog.statsdHost")
  }
}

@Singleton
class ContentClientConfigImpl @Inject()(override val configuration: Configuration) extends ContentClientConfig
