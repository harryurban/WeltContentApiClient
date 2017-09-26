package de.welt.contentapi.raw.client.services

import javax.inject.{Inject, Singleton}

import de.welt.contentapi.core.client.services.CapiExecutionContext
import de.welt.contentapi.core.client.services.s3.S3Client
import de.welt.contentapi.raw.models.RawChannel
import de.welt.contentapi.utils.Env.{Env, Live, Preview}
import de.welt.contentapi.utils.Loggable
import play.api.libs.json.{JsError, JsSuccess, Json}
import play.api.{Configuration, Environment, Mode}

import scala.concurrent.duration._

trait RawTreeService {
  def root(env: Env): Option[RawChannel]
}

@Singleton
class RawTreeServiceImpl @Inject()(s3Client: S3Client,
                                   config: Configuration,
                                   environment: Environment,
                                   implicit val capiContext: CapiExecutionContext) extends RawTreeService with Loggable {

  import RawTreeServiceImpl._

  @volatile private var data: Map[Env, RawChannel] = Map.empty
  protected[services] val bucket: String = config.get[String](bucketConfigKey)
  protected[services] val folder: String = config.get[String](folderConfigKey)

  // start cron to update the tree automatically
  capiContext.actorSystem.scheduler.schedule(100.milliseconds, 1.minute, () ⇒ update())

  override def root(env: Env): Option[RawChannel] = data.get(env)

  /**
    * prod/dev/local-dev mode
    * This is only a sub folder with the Live/Preview raw tree
    */
  private val mode: String = config.getOptional[String](modeConfigKey).getOrElse {
    // playMode is a fallback for api-client-version >0.6.x
    environment.mode match {
      case Mode.Prod ⇒ "prod"
      case _ ⇒ "dev"
    }
  }

  protected def objectKeyForEnv(env: Env): String = s"$folder/$mode/${env.toString}/config.json"

  protected[services] def update(): Unit = {
    val treeByEnv = for {env ← Seq(Live, Preview)} yield {
      s3Client.get(bucket, objectKeyForEnv(env)).flatMap { tree ⇒
        Json.parse(tree).validate[RawChannel](de.welt.contentapi.raw.models.RawReads.rawChannelReads) match {
          case JsSuccess(parsedTree, _) ⇒
            log.info(s"Loaded/Refreshed raw tree for $env")
            parsedTree.updateParentRelations()
            val someTuple: Option[(Env, RawChannel)] = Some(env → parsedTree)
            someTuple
          case e: JsError ⇒
            log.error(f"JsError parsing S3 file: '$bucket/$folder'. " + JsError.toJson(e).toString())
            None
        }
      }
    }
    data = treeByEnv.collect {
      case Some(y) ⇒ y
    }.toMap
  }
}

object RawTreeServiceImpl {
  val bucketConfigKey = "welt.aws.s3.rawTree.bucket"
  val folderConfigKey = "welt.aws.s3.rawTree.folder"
  val modeConfigKey = "welt.aws.s3.rawTree.mode"
}
