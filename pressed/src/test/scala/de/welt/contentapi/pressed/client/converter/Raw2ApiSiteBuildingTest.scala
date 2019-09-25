package de.welt.contentapi.pressed.client.converter

import de.welt.contentapi.raw.models._
import de.welt.testing.TestHelper.raw.channel.{emptyWithId, emptyWithIdAndChildren}
import org.mockito.Mockito
import org.scalatestplus.play.PlaySpec

//noinspection ScalaStyle
class Raw2ApiSiteBuildingTest extends PlaySpec {

  val rawToApiConverter: RawToApiConverter = new RawToApiConverter(new InheritanceCalculator())

  //noinspection ScalaStyle
  trait SiteBuildingTreeScope {
    def spyRawToApiConverter = Mockito.spy(rawToApiConverter)
    // @formatter:off

    /**
      * *S* = nonEmpty site building
      * *s* = site building equals the default constructor
      * *M* = is explicit master
      * *m* = is implicit master, because 1st level child of root
      *
      *
      *         (    0[root]   *S*    )
      *         |            \
      *      (10 *SM*)    (20 *M*)
      *        |              \
      *      (100)         (200 *S*)
      *       |                \
      *     (1000 *s*)     (2000 *S*)
      */

    // @formatter:on

    val node100 = emptyWithIdAndChildren(100, Seq.empty)

    val node10 = emptyWithIdAndChildren(10, Seq(node100))
    node10.config = RawChannelConfiguration(siteBuilding = Some(RawChannelSiteBuilding(
      fields = Some(Map("key1" -> "value2", "key2" -> "value2")),
      sub_navigation = Some(Seq(RawSectionReference(Some("Label"), Some("/Path/")))),
      elements = Some(Seq(
        RawElement(
          id = RawChannelElement.IdDefault,
          `type` = "mood",
          assets = Some(List(
            RawAsset(
              `type` = "image",
              fields = Some(Map("key1" -> "value2", "key2" -> "value2"))
            )
          ))
        )
      ))
    )), master = true)

    /** 2XXX */
    val node2000 = emptyWithId(200)
    node2000.config = RawChannelConfiguration(siteBuilding = None)

    val node200 = emptyWithIdAndChildren(200, Seq(node2000))
    node200.config = RawChannelConfiguration()

    val node20 = emptyWithIdAndChildren(20, Seq(node200))
    node20.config = RawChannelConfiguration(siteBuilding = Some(RawChannelSiteBuilding(
      fields = Some(Map(
        "header_slogan" -> "Header Slogan",
        "sponsoring_slogan" -> "Sponsoring Slogan"
      )),
      sub_navigation = Some(Seq(RawSectionReference(Some("Label"), Some("/Path/")))),
      elements = Some(Seq(RawElement(id = "element id", `type` = "element type")))
    )))

    /** root */
    val root = emptyWithIdAndChildren(0, Seq(node10, node20))
    root.updateParentRelations()

    import de.welt.contentapi.core.models.testImplicits.pathUpdater

    root.updatePaths()
  }

  "calculateSiteBuilding()" should {

    "look for a master and inherit `sponsoring_` values if channel.isMasterInheritanceEligible is true because no `sponsoring_` values are present" in new SiteBuildingTreeScope {
      node200.config = RawChannelConfiguration(siteBuilding = Some(RawChannelSiteBuilding(
        fields = Some(Map(
          "header_slogan" -> "Header Slogan"
        )),
        sub_navigation = Some(Seq(RawSectionReference(Some("Label"), Some("/Path/")))),
        elements = Some(Seq(RawElement(id = "element id", `type` = "element type")))
      )))

      spyRawToApiConverter.calculateSiteBuilding(node200).get.unwrappedFields.get("sponsoring_slogan") mustBe Some("Sponsoring Slogan")
      Mockito.verify(spyRawToApiConverter, Mockito.atMostOnce()).calculateMasterChannel(node200)
    }

    "look for a master and inherit `header_` values if channel.isMasterInheritanceEligible is true because no `header` values are present" in new SiteBuildingTreeScope {
      node200.config = RawChannelConfiguration(siteBuilding = Some(RawChannelSiteBuilding(
        fields = Some(Map(
          "sponsoring_slogan" -> "Sponsoring Slogan"
        )),
        sub_navigation = Some(Seq(RawSectionReference(Some("Label"), Some("/Path/")))),
        elements = Some(Seq(RawElement(id = "element id", `type` = "element type")))
      )))

      spyRawToApiConverter.calculateSiteBuilding(node200).get.unwrappedFields.get("header_slogan") mustBe Some("Header Slogan")
      Mockito.verify(spyRawToApiConverter, Mockito.atMostOnce()).calculateMasterChannel(node200)
    }

    "look for a master and inherit `elements` if channel.isMasterInheritanceEligible is true because no elements are present" in new SiteBuildingTreeScope {
      node200.config = RawChannelConfiguration(siteBuilding = Some(RawChannelSiteBuilding(
        fields = Some(Map(
          "header_slogan" -> "Header Slogan",
          "sponsoring_slogan" -> "Sponsoring Slogan"
        )),
        sub_navigation = Some(Seq(RawSectionReference(Some("Label"), Some("/Path/")))),
        elements = None
      )))

      spyRawToApiConverter.calculateSiteBuilding(node200).get.unwrappedElements.size mustBe 1
      Mockito.verify(spyRawToApiConverter, Mockito.atMostOnce()).calculateMasterChannel(node200)
    }

    "look for a master and inherit `sub_navigation` if channel.isMasterInheritanceEligible is true because no sub_navigation is present" in new SiteBuildingTreeScope {
      node200.config = RawChannelConfiguration(siteBuilding = Some(RawChannelSiteBuilding(
        fields = Some(Map(
          "header_slogan" -> "Header Slogan",
          "sponsoring_slogan" -> "Sponsoring Slogan"
        )),
        sub_navigation = None,
        elements = Some(Seq(RawElement(id = "element id", `type` = "element type")))
      )))

      spyRawToApiConverter.calculateSiteBuilding(node200).get.unwrappedSubNavigation.size mustBe 1
      Mockito.verify(spyRawToApiConverter, Mockito.atMostOnce()).calculateMasterChannel(node200)
    }

    "never look for a master and inherit values if channel.isMasterInheritanceEligible is false" in new SiteBuildingTreeScope {
      node200.config = RawChannelConfiguration(siteBuilding = Some(RawChannelSiteBuilding(
        fields = Some(Map(
          "header_slogan" -> "Header Slogan",
          "sponsoring_slogan" -> "Sponsoring Slogan"
        )),
        sub_navigation = Some(Seq(RawSectionReference(Some("Label"), Some("/Path/")))),
        elements = Some(Seq(RawElement(id = "element id", `type` = "element type")))
      )))

      spyRawToApiConverter.calculateSiteBuilding(node200).isDefined mustBe true
      Mockito.verify(spyRawToApiConverter, Mockito.never()).calculateMasterChannel(node200)
    }



  }

  "mergeSitebuildings()" should {

    "add `header_` fields from master if channel itself has no `header_` fields" in {
      val channel = RawChannelSiteBuilding(fields = Some(Map(
        "sponsoring_slogan" -> "Winamp - it really whips the llama's ass",
        "sponsoring_hidden" -> "true"
      )))

      val master = RawChannelSiteBuilding(fields = Some(Map(
        "header_logo" -> "zukunftsfond",
        "header_hidden" -> "true"
      )))
      rawToApiConverter.mergeSitebuildings(channel, master) mustBe RawChannelSiteBuilding(fields = Some(Map(
        "sponsoring_slogan" -> "Winamp - it really whips the llama's ass",
        "sponsoring_hidden" -> "true",
        "header_logo" -> "zukunftsfond",
        "header_hidden" -> "true"
      )))
    }

    "add `header_` fields from master if channel itself has `header_hidden = false` as only `header` field" in {
      val channel = RawChannelSiteBuilding(fields = Some(Map(
        "sponsoring_slogan" -> "Winamp - it really whips the llama's ass",
        "sponsoring_hidden" -> "true",
        "header_hidden" -> "false"
      )))

      val master = RawChannelSiteBuilding(fields = Some(Map(
        "header_logo" -> "zukunftsfond",
        "header_hidden" -> "true"
      )))
      rawToApiConverter.mergeSitebuildings(channel, master) mustBe RawChannelSiteBuilding(fields = Some(Map(
        "sponsoring_slogan" -> "Winamp - it really whips the llama's ass",
        "sponsoring_hidden" -> "true",
        "header_logo" -> "zukunftsfond",
        "header_hidden" -> "true"
      )))
    }

    "add `sponsoring_` fields from master if channel itself has no `sponsoring_` fields" in {
      val channel = RawChannelSiteBuilding(fields = Some(Map(
        "header_logo" -> "zukunftsfond"
      )))

      val master = RawChannelSiteBuilding(fields = Some(Map(
        "header_logo" -> "zukunftsfond",
        "sponsoring_slogan" -> "Winamp - it really whips the llama's ass",
        "sponsoring_hidden" -> "true"
      )))
      rawToApiConverter.mergeSitebuildings(channel, master) mustBe RawChannelSiteBuilding(fields = Some(Map(
        "sponsoring_slogan" -> "Winamp - it really whips the llama's ass",
        "sponsoring_hidden" -> "true",
        "header_logo" -> "zukunftsfond"
      )))
    }

    "add elements from master if channel itself has no elements" in {
      val channel = RawChannelSiteBuilding(elements = None)
      val master = RawChannelSiteBuilding(elements = Some(Seq(RawElement())), fields = Some(Map.empty))
      rawToApiConverter.mergeSitebuildings(channel, master) mustBe master
    }

    "add sub navi from master if channel itself has no sub navi" in {
      val channel = RawChannelSiteBuilding(elements = None)
      val master = RawChannelSiteBuilding(sub_navigation = Some(Seq(RawSectionReference())), fields = Some(Map.empty))
      rawToApiConverter.mergeSitebuildings(channel, master) mustBe master
    }

  }

}
