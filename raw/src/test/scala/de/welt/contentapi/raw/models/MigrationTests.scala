package de.welt.contentapi.raw.models

import org.scalatestplus.play.PlaySpec
import play.api.libs.json.Json

class MigrationTests extends PlaySpec {

  import RawReads._

  private final val emptyJson = "{}"

  "Migration" must {
    "migrate old header values to sitebuilding fields" in {
      val header =
        """{
          |      "logo": "kompakt",
          |      "slogan": "header_old_section_name_slogan",
          |      "label": "header_old_section_name_label",
          |      "hidden": false,
          |      "adIndicator": true,
          |      "sloganReference": {
          |        "path": "/header_old_section_name_slogan_link/"
          |      },
          |      "headerReference": {
          |        "path": "/header_old_section_name_header_link/"
          |      }
          |    }
        """.stripMargin
      val mheader = Json.parse(header).validate[RawChannelHeader].asOpt
      val sitebuilding = RawReads.sitebuildingMigration(mheader, None, None).get
      val migratedFields = collection.Map(
        "header_slogan_href" -> "/header_old_section_name_slogan_link/",
        "header_slogan" -> "header_old_section_name_slogan",
        "sponsoring_ad_indicator" -> "true",
        "header_logo" -> "kompakt",
        "header_hidden" -> "false",
        "header_label" -> "header_old_section_name_label",
        "header_href" -> "/header_old_section_name_header_link/"
      )
      sitebuilding.fields.getOrElse(Map.empty) mustBe migratedFields
    }

    "migrate old header references to sitebuilding references" in {
      val header =
        """{
          |     "sectionReferences": [
          |        {
          |          "label": "header_old_subnavi_label_1",
          |          "path": "/header_old_subnavi_label_1_url/"
          |        },
          |        {
          |          "label": "header_old_subnavi_label_2",
          |          "path": "/header_old_subnavi_label_2_url/"
          |        }
          |      ]
          |    }
        """.stripMargin
      val mheader = Json.parse(header).validate[RawChannelHeader].asOpt
      val sitebuilding = RawReads.sitebuildingMigration(mheader, None, None).get
      sitebuilding.sub_navigation.map(_.size) mustBe Some(2)
      sitebuilding.sub_navigation mustBe mheader.flatMap(_.sectionReferences)
    }

    "migrate old sponsoring to sitebuilding" in {
      val json =
        """{
          |      "hidden": false,
          |      "logo": "70-jahre-wams",
          |      "slogan": "sponsoring_old_sponsoring_slogan",
          |      "brandstation": "presented",
          |      "link": {
          |        "path": "/sponsoring_old_sponsoring_logo_link/"
          |      }
          |    }
        """.stripMargin
      val sponsoring = Json.parse(json).validate[RawSponsoringConfig].asOpt
      val sitebuilding = RawReads.sitebuildingMigration(None, sponsoring, None).get
      sitebuilding.fields.getOrElse(Map.empty) mustBe Map(
        "sponsoring_logo" -> "70-jahre-wams",
        "sponsoring_logo_href" -> "/sponsoring_old_sponsoring_logo_link/",
        "sponsoring_slogan" -> "sponsoring_old_sponsoring_slogan",
        "sponsoring_hidden" -> "false",
        "sponsoring_enclosure" -> "presented")
    }

    "sitebuilding references win over old ones" in {
      val header =
        """{
          |     "sectionReferences": [
          |        {
          |          "label": "header_old_subnavi_label_1",
          |          "path": "/header_old_subnavi_label_1_url/"
          |        },
          |        {
          |          "label": "header_old_subnavi_label_2",
          |          "path": "/header_old_subnavi_label_2_url/"
          |        },
          |        {
          |          "label": "unique_old",
          |          "path": "/unique_old/"
          |        }
          |      ]
          |    }
        """.stripMargin

      val siteBuilding =
        """{
          |"sub_navigation": [
          |        {
          |          "label": "header_old_subnavi_label_1",
          |          "path": "/header_old_subnavi_label_1_url/"
          |        },
          |        {
          |          "label": "header_old_subnavi_label_2",
          |          "path": "/header_old_subnavi_label_2_url/"
          |        },
          |        {
          |          "label": "unique_new",
          |          "path": "/unique_new/"
          |        }
          |      ]
          |}
        """.stripMargin

      val mheader = Json.parse(header).validate[RawChannelHeader].asOpt
      val msiteb = Json.parse(siteBuilding).validate[RawChannelSiteBuilding].asOpt

      val sitebuilding = RawReads.sitebuildingMigration(mheader, None, msiteb).get

      sitebuilding.sub_navigation mustBe Some(Vector(
        RawSectionReference(Some("header_old_subnavi_label_1"), Some("/header_old_subnavi_label_1_url/")),
        RawSectionReference(Some("header_old_subnavi_label_2"), Some("/header_old_subnavi_label_2_url/")),
        RawSectionReference(Some("unique_new"), Some("/unique_new/"))
      ))
    }

    "old references are migrated if sitebuilding is empty" in {
      val header =
        """{
          |     "sectionReferences": [
          |        {
          |          "label": "header_old_subnavi_label_1",
          |          "path": "/header_old_subnavi_label_1_url/"
          |        },
          |        {
          |          "label": "header_old_subnavi_label_2",
          |          "path": "/header_old_subnavi_label_2_url/"
          |        },
          |        {
          |          "label": "unique_old",
          |          "path": "/unique_old/"
          |        }
          |      ]
          |    }
        """.stripMargin

      val siteBuilding =
        """{
          |"sub_navigation": []
          |}
        """.stripMargin

      val mheader = Json.parse(header).validate[RawChannelHeader].asOpt
      val msiteb = Json.parse(siteBuilding).validate[RawChannelSiteBuilding].asOpt

      val sitebuilding = RawReads.sitebuildingMigration(mheader, None, msiteb).get

      sitebuilding.sub_navigation mustBe Some(Vector(
        RawSectionReference(Some("header_old_subnavi_label_1"), Some("/header_old_subnavi_label_1_url/")),
        RawSectionReference(Some("header_old_subnavi_label_2"), Some("/header_old_subnavi_label_2_url/")),
        RawSectionReference(Some("unique_old"), Some("/unique_old/"))
      ))
    }

    "migrate old header dropdown to sitebuilding dropdown logo" in {
      val json =
        """
          |{
          |      "logo": "kompakt"
          |}
        """.stripMargin
      val mHeader = Json.parse(json).validate[RawChannelHeader].asOpt
      val json2 =
        """{
          |      "fields": {
          |        "header_logo": "header_logo_escenic"
          |      },
          |      "elements": [
          |        {
          |          "id": "channel_element",
          |          "type": "header_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-header_logo_escenic.jpg"
          |              }
          |            }
          |          ]
          |        }
          |      ]
          |}""".stripMargin
      val mSiteb = Json.parse(json2).validate[RawChannelSiteBuilding].asOpt
      val sitebuilding: RawChannelSiteBuilding = RawReads.sitebuildingMigration(mHeader, None, mSiteb).get
      sitebuilding.unwrappedFields("header_logo") mustBe "kompakt"
      sitebuilding.unwrappedFields("header_logo_escenic") mustBe "header_logo_escenic"
      sitebuilding.elements.size mustBe 1

    }
    "migrate old sponsoring dropdown to sitebuilding dropdown logo" in {
      val json =
        """
          |{
          |      "logo": "kompakt"
          |}
        """.stripMargin
      val mSponsoring = Json.parse(json).validate[RawSponsoringConfig].asOpt
      val json2 =
        """{
          |      "fields": {
          |        "sponsoring_logo": "sponsoring_logo_escenic"
          |      },
          |      "elements": [
          |        {
          |          "id": "channel_element",
          |          "type": "header_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-header_logo_escenic.jpg"
          |              }
          |            }
          |          ]
          |        }
          |      ]
          |}""".stripMargin
      val mSiteb = Json.parse(json2).validate[RawChannelSiteBuilding].asOpt
      val sitebuilding = RawReads.sitebuildingMigration(None, mSponsoring, mSiteb).get
      sitebuilding.unwrappedFields("sponsoring_logo") mustBe "kompakt"
      sitebuilding.unwrappedFields("sponsoring_logo_escenic") mustBe "sponsoring_logo_escenic"
      sitebuilding.elements.size mustBe 1

    }


  }

  "Migration json compare" should {

    "old completely filled, sitebuilding empty" in {
      val json =
        """{
          |    "metadata": {
          |      "title": "",
          |      "description": "",
          |      "sectionRobots": {
          |        "noIndex": false,
          |        "noFollow": false
          |      },
          |      "contentRobots": {
          |        "noIndex": false,
          |        "noFollow": false
          |      },
          |      "sectionBreadcrumbDisabled": false,
          |      "keywords": []
          |    },
          |    "commercial": {
          |      "contentTaboola": {
          |        "showNetwork": true,
          |        "showNews": true,
          |        "showWeb": true,
          |        "showWebExtended": true
          |      },
          |      "definesAdTag": false,
          |      "definesVideoAdTag": false,
          |      "showFallbackAds": true,
          |      "disableAdvertisement": false
          |    },
          |    "articlePromotions": [
          |      {
          |        "contentId": "",
          |        "type": "Advertorial"
          |      }
          |    ],
          |    "header": {
          |      "logo": "kompakt",
          |      "slogan": "header_old_section_name_slogan",
          |      "label": "header_old_section_name_label",
          |      "sectionReferences": [
          |        {
          |          "label": "header_old_subnavi_label_1",
          |          "path": "/header_old_subnavi_label_1_url/"
          |        },
          |        {
          |          "label": "header_old_subnavi_label_2",
          |          "path": "/header_old_subnavi_label_2_url/"
          |        }
          |      ],
          |      "hidden": false,
          |      "adIndicator": true,
          |      "sloganReference": {
          |        "path": "/header_old_section_name_slogan_link/"
          |      },
          |      "headerReference": {
          |        "path": "/header_old_section_name_header_link/"
          |      }
          |    },
          |    "sponsoring": {
          |      "hidden": false,
          |      "logo": "70-jahre-wams",
          |      "slogan": "sponsoring_old_sponsoring_slogan",
          |      "brandstation": "sponsored",
          |      "link": {
          |        "path": "/sponsoring_old_sponsoring_logo_link/"
          |      }
          |    },
          |    "siteBuilding": {
          |      "fields": {
          |        "header_label": "",
          |        "header_logo": "",
          |        "header_href": "",
          |        "header_slogan": "",
          |        "header_slogan_href": "",
          |        "header_hidden": "",
          |        "partner_header_button_label": "",
          |        "partner_header_button_href": "",
          |        "partner_header_mood_image": "",
          |        "partner_header_logo": "",
          |        "partner_header_hidden": "",
          |        "sponsoring_logo": "",
          |        "sponsoring_logo_href": "",
          |        "sponsoring_slogan": "",
          |        "sponsoring_ad_indicator": "",
          |        "sponsoring_hidden": "",
          |        "sponsoring_enclosure_hidden": "",
          |        "sponsoring_enclosure": "",
          |        "footer_button_label": "",
          |        "footer_button_href": "",
          |        "footer_slogan_label": "",
          |        "footer_slogan_href": "",
          |        "footer_mood_image": "",
          |        "footer_logo": "",
          |        "footer_legal": "",
          |        "footer_hidden": "",
          |        "footer_body": "",
          |        "partner_header_body": ""
          |      },
          |      "sub_navigation": [],
          |      "elements": []
          |    },
          |    "master": false,
          |    "brand": false,
          |    "theme": {
          |      "name": ""
          |    },
          |    "content": {}
          |  }""".stripMargin

      val rawConfig: Option[RawChannelConfiguration] = Json.parse(json).validate[RawChannelConfiguration](RawReads.rawChannelConfigurationReads).asOpt
      val sitebuilding = rawConfig.flatMap(_.siteBuilding).orNull
      val expected =
        """{
          |    "fields": {
          |      "header_label": "header_old_section_name_label",
          |      "header_logo": "kompakt",
          |      "header_logo_escenic": "",
          |      "header_href": "/header_old_section_name_header_link/",
          |      "header_slogan": "header_old_section_name_slogan",
          |      "header_slogan_href": "/header_old_section_name_slogan_link/",
          |      "header_hidden": "false",
          |      "partner_header_button_label": "",
          |      "partner_header_button_href": "",
          |      "partner_header_mood_image": "",
          |      "partner_header_logo": "",
          |      "partner_header_hidden": "",
          |      "sponsoring_logo": "70-jahre-wams",
          |      "sponsoring_logo_escenic": "",
          |      "sponsoring_logo_href": "/sponsoring_old_sponsoring_logo_link/",
          |      "sponsoring_slogan": "sponsoring_old_sponsoring_slogan",
          |      "sponsoring_ad_indicator": "true",
          |      "sponsoring_hidden": "false",
          |      "sponsoring_enclosure_hidden": "",
          |      "sponsoring_enclosure": "sponsored",
          |      "footer_button_label": "",
          |      "footer_button_href": "",
          |      "footer_slogan_label": "",
          |      "footer_slogan_href": "",
          |      "footer_mood_image": "",
          |      "footer_logo": "",
          |      "footer_legal": "",
          |      "footer_hidden": "",
          |      "footer_body": "",
          |      "partner_header_body": ""
          |    },
          |    "sub_navigation": [
          |      {
          |        "label": "header_old_subnavi_label_1",
          |        "path": "/header_old_subnavi_label_1_url/"
          |      },
          |      {
          |        "label": "header_old_subnavi_label_2",
          |        "path": "/header_old_subnavi_label_2_url/"
          |      }
          |    ],
          |    "elements": []
          |  }""".stripMargin

      val expectedSitebuilding = Json.parse(expected).validate[RawChannelSiteBuilding].get
      expectedSitebuilding mustBe sitebuilding
    }

    "sitebuilding completely filled, old empty" in { // todo: here
      val json =
        """{
          |    "metadata": {
          |      "title": "",
          |      "description": "",
          |      "sectionRobots": {
          |        "noIndex": false,
          |        "noFollow": false
          |      },
          |      "contentRobots": {
          |        "noIndex": false,
          |        "noFollow": false
          |      },
          |      "sectionBreadcrumbDisabled": false,
          |      "keywords": []
          |    },
          |    "commercial": {
          |      "contentTaboola": {
          |        "showNetwork": true,
          |        "showNews": true,
          |        "showWeb": true,
          |        "showWebExtended": true
          |      },
          |      "definesAdTag": false,
          |      "definesVideoAdTag": false,
          |      "showFallbackAds": true,
          |      "disableAdvertisement": false
          |    },
          |    "articlePromotions": [],
          |    "header": {
          |      "logo": "",
          |      "slogan": "",
          |      "label": "",
          |      "sectionReferences": [],
          |      "hidden": false,
          |      "adIndicator": false
          |    },
          |    "sponsoring": {
          |      "hidden": false
          |    },
          |    "siteBuilding": {
          |      "fields": {
          |        "header_label": "sitebuilding_custom_header_label",
          |        "header_logo": "sitebuilding_custom_header_logo_ece_id",
          |        "header_href": "/sitebuilding_custom_header_link/",
          |        "header_slogan": "sitebuilding_custom_header_slogan",
          |        "header_slogan_href": "/sitebuilding_custom_header_slogan_link/",
          |        "header_hidden": "false",
          |        "partner_header_button_label": "sitebuilding_partner_header_module_cta_button",
          |        "partner_header_button_href": "/sitebuilding_partner_header_module_cta_button_href/",
          |        "partner_header_mood_image": "sitebuilding_partner_header_module_mood_ece_id",
          |        "partner_header_logo": "sitebuilding_partner_header_module_logo_ece_id",
          |        "partner_header_hidden": "false",
          |        "sponsoring_logo": "sitebuilding_header_sponsoring_logo_ece_id",
          |        "sponsoring_logo_href": "/sitebuilding_header_sponsoring_logo_link/",
          |        "sponsoring_slogan": "sitebuilding_header_sponsoring_slogan",
          |        "sponsoring_ad_indicator": "true",
          |        "sponsoring_hidden": "false",
          |        "sponsoring_enclosure_hidden": "false",
          |        "sponsoring_enclosure": "presented",
          |        "footer_button_label": "sitebuilding_partner_footer_modul_cta_button",
          |        "footer_button_href": "/sitebuilding_partner_footer_modul_cta_button_href/",
          |        "footer_slogan_label": "sitebuilding_partner_footer_modul_slogan",
          |        "footer_slogan_href": "",
          |        "footer_mood_image": "sitebuilding_partner_footer_modul_mood_ece_id",
          |        "footer_logo": "sitebuilding_partner_footer_modul_logo_ece_id",
          |        "footer_legal": "sitebuilding_partner_footer_modul_legal_text",
          |        "footer_hidden": "false",
          |        "general_breadcrumb_hidden": "",
          |        "footer_body": "sitebuilding_partner_footer_modul_body_text",
          |        "partner_header_body": "sitebuilding_partner_header_module_body_text"
          |      },
          |      "sub_navigation": [
          |        {
          |          "label": "sitebuilding_subnavi_label_1",
          |          "path": "/sitebuilding_subnavi_href_1/"
          |        },
          |        {
          |          "label": "sitebuilding_subnavi_label_2",
          |          "path": "/sitebuilding_subnavi_href_2/"
          |        }
          |      ],
          |      "elements": [
          |        {
          |          "id": "channel_element",
          |          "type": "header_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_custom_header_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "sponsoring_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_header_sponsoring_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "partner_header_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_partner_header_module_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "partner_header_mood_image",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "94x35",
          |                "source": "https://www.welt.de/img/mobilesitebuilding_partner_header_module_mood_ece_id/0096585617-ci94x35-wWIDTH/image.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "footer_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_partner_footer_modul_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "footer_mood_image",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "16x9",
          |                "source": "https://www.welt.de/img/mobilesitebuilding_partner_footer_modul_mood_ece_id/2281353017-ci16x9-wWIDTH/image.jpg"
          |              }
          |            }
          |          ]
          |        }
          |      ]
          |    },
          |    "master": false,
          |    "brand": false,
          |    "theme": {
          |      "name": ""
          |    },
          |    "content": {}
          |  }""".stripMargin

      val rawConfig: Option[RawChannelConfiguration] = Json.parse(json).validate[RawChannelConfiguration](RawReads.rawChannelConfigurationReads).asOpt
      val sitebuilding = rawConfig.flatMap(_.siteBuilding).orNull
      val expected =
        """{
          |      "fields": {
          |        "migrated": "true",
          |        "header_label": "sitebuilding_custom_header_label",
          |        "header_logo": "",
          |        "header_logo_escenic": "sitebuilding_custom_header_logo_ece_id",
          |        "header_href": "/sitebuilding_custom_header_link/",
          |        "header_slogan": "sitebuilding_custom_header_slogan",
          |        "header_slogan_href": "/sitebuilding_custom_header_slogan_link/",
          |        "header_hidden": "false",
          |        "partner_header_button_label": "sitebuilding_partner_header_module_cta_button",
          |        "partner_header_button_href": "/sitebuilding_partner_header_module_cta_button_href/",
          |        "partner_header_mood_image": "sitebuilding_partner_header_module_mood_ece_id",
          |        "partner_header_logo": "sitebuilding_partner_header_module_logo_ece_id",
          |        "partner_header_hidden": "false",
          |        "sponsoring_logo": "",
          |        "sponsoring_logo_escenic": "sitebuilding_header_sponsoring_logo_ece_id",
          |        "sponsoring_logo_href": "/sitebuilding_header_sponsoring_logo_link/",
          |        "sponsoring_slogan": "sitebuilding_header_sponsoring_slogan",
          |        "sponsoring_ad_indicator": "true",
          |        "sponsoring_hidden": "false",
          |        "sponsoring_enclosure_hidden": "false",
          |        "sponsoring_enclosure": "presented",
          |        "footer_button_label": "sitebuilding_partner_footer_modul_cta_button",
          |        "footer_button_href": "/sitebuilding_partner_footer_modul_cta_button_href/",
          |        "footer_slogan_label": "sitebuilding_partner_footer_modul_slogan",
          |        "footer_slogan_href": "",
          |        "footer_mood_image": "sitebuilding_partner_footer_modul_mood_ece_id",
          |        "footer_logo": "sitebuilding_partner_footer_modul_logo_ece_id",
          |        "footer_legal": "sitebuilding_partner_footer_modul_legal_text",
          |        "footer_hidden": "false",
          |        "general_breadcrumb_hidden": "",
          |        "footer_body": "sitebuilding_partner_footer_modul_body_text",
          |        "partner_header_body": "sitebuilding_partner_header_module_body_text"
          |      },
          |      "sub_navigation": [
          |        {
          |          "label": "sitebuilding_subnavi_label_1",
          |          "path": "/sitebuilding_subnavi_href_1/"
          |        },
          |        {
          |          "label": "sitebuilding_subnavi_label_2",
          |          "path": "/sitebuilding_subnavi_href_2/"
          |        }
          |      ],
          |      "elements": [
          |        {
          |          "id": "channel_element",
          |          "type": "header_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_custom_header_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "sponsoring_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_header_sponsoring_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "partner_header_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_partner_header_module_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "partner_header_mood_image",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "94x35",
          |                "source": "https://www.welt.de/img/mobilesitebuilding_partner_header_module_mood_ece_id/0096585617-ci94x35-wWIDTH/image.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "footer_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_partner_footer_modul_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "footer_mood_image",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "16x9",
          |                "source": "https://www.welt.de/img/mobilesitebuilding_partner_footer_modul_mood_ece_id/2281353017-ci16x9-wWIDTH/image.jpg"
          |              }
          |            }
          |          ]
          |        }
          |      ]
          |    }""".stripMargin

      val expectedSitebuilding = Json.parse(expected).validate[RawChannelSiteBuilding].get
      expectedSitebuilding mustBe sitebuilding
    }

    "both completely filled" in { // todo: here
      val json =
        """{
          |    "metadata": {
          |      "title": "",
          |      "description": "",
          |      "sectionRobots": {
          |        "noIndex": false,
          |        "noFollow": false
          |      },
          |      "contentRobots": {
          |        "noIndex": false,
          |        "noFollow": false
          |      },
          |      "sectionBreadcrumbDisabled": false,
          |      "keywords": []
          |    },
          |    "commercial": {
          |      "contentTaboola": {
          |        "showNetwork": true,
          |        "showNews": true,
          |        "showWeb": true,
          |        "showWebExtended": true
          |      },
          |      "definesAdTag": false,
          |      "definesVideoAdTag": false,
          |      "showFallbackAds": true,
          |      "disableAdvertisement": false
          |    },
          |    "articlePromotions": [],
          |    "header": {
          |      "logo": "kompakt",
          |      "slogan": "header_old_section_name_slogan",
          |      "label": "header_old_section_name_label",
          |      "sectionReferences": [
          |        {
          |          "label": "header_old_subnavi_label_1",
          |          "path": "/header_old_subnavi_label_1_url/"
          |        },
          |        {
          |          "label": "header_old_subnavi_label_2",
          |          "path": "/header_old_subnavi_label_2_url/"
          |        }
          |      ],
          |      "hidden": false,
          |      "adIndicator": true,
          |      "sloganReference": {
          |        "path": "/header_old_section_name_slogan_link/"
          |      },
          |      "headerReference": {
          |        "path": "/header_old_section_name_header_link/"
          |      }
          |    },
          |    "sponsoring": {
          |      "hidden": false,
          |      "logo": "70-jahre-wams",
          |      "slogan": "sponsoring_old_sponsoring_slogan",
          |      "brandstation": "presented",
          |      "link": {
          |        "path": "/sponsoring_old_sponsoring_logo_link/"
          |      }
          |    },
          |    "siteBuilding": {
          |      "fields": {
          |        "header_label": "sitebuilding_custom_header_label",
          |        "header_logo": "sitebuilding_custom_header_logo_ece_id",
          |        "header_href": "/sitebuilding_custom_header_link/",
          |        "header_slogan": "sitebuilding_custom_header_slogan",
          |        "header_slogan_href": "/sitebuilding_custom_header_slogan_link/",
          |        "header_hidden": "false",
          |        "partner_header_button_label": "sitebuilding_partner_header_module_cta_button",
          |        "partner_header_button_href": "/sitebuilding_partner_header_module_cta_button_href/",
          |        "partner_header_mood_image": "sitebuilding_partner_header_module_mood_ece_id",
          |        "partner_header_logo": "sitebuilding_partner_header_module_logo_ece_id",
          |        "partner_header_hidden": "false",
          |        "sponsoring_logo": "sitebuilding_header_sponsoring_logo_ece_id",
          |        "sponsoring_logo_href": "/sitebuilding_header_sponsoring_logo_link/",
          |        "sponsoring_slogan": "sitebuilding_header_sponsoring_slogan",
          |        "sponsoring_ad_indicator": "true",
          |        "sponsoring_hidden": "false",
          |        "sponsoring_enclosure_hidden": "false",
          |        "sponsoring_enclosure": "presented",
          |        "footer_button_label": "sitebuilding_partner_footer_modul_cta_button",
          |        "footer_button_href": "/sitebuilding_partner_footer_modul_cta_button_href/",
          |        "footer_slogan_label": "sitebuilding_partner_footer_modul_slogan",
          |        "footer_slogan_href": "",
          |        "footer_mood_image": "sitebuilding_partner_footer_modul_mood_ece_id",
          |        "footer_logo": "sitebuilding_partner_footer_modul_logo_ece_id",
          |        "footer_legal": "sitebuilding_partner_footer_modul_legal_text",
          |        "footer_hidden": "false",
          |        "general_breadcrumb_hidden": "",
          |        "footer_body": "sitebuilding_partner_footer_modul_body_text",
          |        "partner_header_body": "sitebuilding_partner_header_module_body_text"
          |      },
          |      "sub_navigation": [
          |        {
          |          "label": "sitebuilding_subnavi_label_1",
          |          "path": "/sitebuilding_subnavi_href_1/"
          |        },
          |        {
          |          "label": "sitebuilding_subnavi_label_2",
          |          "path": "/sitebuilding_subnavi_href_2/"
          |        }
          |      ],
          |      "elements": [
          |        {
          |          "id": "channel_element",
          |          "type": "header_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_custom_header_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "sponsoring_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_header_sponsoring_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "partner_header_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_partner_header_module_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "partner_header_mood_image",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "94x35",
          |                "source": "https://www.welt.de/img/mobilesitebuilding_partner_header_module_mood_ece_id/3506586347-ci94x35-wWIDTH/image.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "footer_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_partner_footer_modul_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "footer_mood_image",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "16x9",
          |                "source": "https://www.welt.de/img/mobilesitebuilding_partner_footer_modul_mood_ece_id/0771358307-ci16x9-wWIDTH/image.jpg"
          |              }
          |            }
          |          ]
          |        }
          |      ]
          |    },
          |    "master": false,
          |    "brand": false,
          |    "theme": {
          |      "name": ""
          |    },
          |    "content": {}
          |  }""".stripMargin

      val rawConfig: Option[RawChannelConfiguration] = Json.parse(json).validate[RawChannelConfiguration](RawReads.rawChannelConfigurationReads).asOpt
      val sitebuilding = rawConfig.flatMap(_.siteBuilding).orNull
      val expected =
        """{
          |      "fields": {
          |        "migrated": "true",
          |        "header_label": "sitebuilding_custom_header_label",
          |        "header_logo": "kompakt",
          |        "header_logo_escenic": "sitebuilding_custom_header_logo_ece_id",
          |        "header_href": "/sitebuilding_custom_header_link/",
          |        "header_slogan": "sitebuilding_custom_header_slogan",
          |        "header_slogan_href": "/sitebuilding_custom_header_slogan_link/",
          |        "header_hidden": "false",
          |        "partner_header_button_label": "sitebuilding_partner_header_module_cta_button",
          |        "partner_header_button_href": "/sitebuilding_partner_header_module_cta_button_href/",
          |        "partner_header_mood_image": "sitebuilding_partner_header_module_mood_ece_id",
          |        "partner_header_logo": "sitebuilding_partner_header_module_logo_ece_id",
          |        "partner_header_hidden": "false",
          |        "sponsoring_logo": "70-jahre-wams",
          |        "sponsoring_logo_escenic": "sitebuilding_header_sponsoring_logo_ece_id",
          |        "sponsoring_logo_href": "/sitebuilding_header_sponsoring_logo_link/",
          |        "sponsoring_slogan": "sitebuilding_header_sponsoring_slogan",
          |        "sponsoring_ad_indicator": "true",
          |        "sponsoring_hidden": "false",
          |        "sponsoring_enclosure_hidden": "false",
          |        "sponsoring_enclosure": "presented",
          |        "footer_button_label": "sitebuilding_partner_footer_modul_cta_button",
          |        "footer_button_href": "/sitebuilding_partner_footer_modul_cta_button_href/",
          |        "footer_slogan_label": "sitebuilding_partner_footer_modul_slogan",
          |        "footer_slogan_href": "",
          |        "footer_mood_image": "sitebuilding_partner_footer_modul_mood_ece_id",
          |        "footer_logo": "sitebuilding_partner_footer_modul_logo_ece_id",
          |        "footer_legal": "sitebuilding_partner_footer_modul_legal_text",
          |        "footer_hidden": "false",
          |        "footer_body": "sitebuilding_partner_footer_modul_body_text",
          |        "partner_header_body": "sitebuilding_partner_header_module_body_text"
          |      },
          |      "sub_navigation": [
          |        {
          |          "label": "sitebuilding_subnavi_label_1",
          |          "path": "/sitebuilding_subnavi_href_1/"
          |        },
          |        {
          |          "label": "sitebuilding_subnavi_label_2",
          |          "path": "/sitebuilding_subnavi_href_2/"
          |        }
          |      ],
          |      "elements": [
          |        {
          |          "id": "channel_element",
          |          "type": "header_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_custom_header_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "sponsoring_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_header_sponsoring_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "partner_header_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_partner_header_module_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "partner_header_mood_image",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "94x35",
          |                "source": "https://www.welt.de/img/mobilesitebuilding_partner_header_module_mood_ece_id/3506586347-ci94x35-wWIDTH/image.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "footer_logo",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "orig",
          |                "source": "https://www.welt.de/bin/logo-sitebuilding_partner_footer_modul_logo_ece_id.jpg"
          |              }
          |            }
          |          ]
          |        },
          |        {
          |          "id": "channel_element",
          |          "type": "footer_mood_image",
          |          "assets": [
          |            {
          |              "type": "image",
          |              "fields": {
          |                "crop": "16x9",
          |                "source": "https://www.welt.de/img/mobilesitebuilding_partner_footer_modul_mood_ece_id/0771358307-ci16x9-wWIDTH/image.jpg"
          |              }
          |            }
          |          ]
          |        }
          |      ]
          |    }""".stripMargin

      val expectedSitebuilding: RawChannelSiteBuilding = Json.parse(expected).validate[RawChannelSiteBuilding].get
      expectedSitebuilding mustBe sitebuilding
    }
  }


  def getFields(stuff: Map[String,String]): Seq[(String, String)] = {
    stuff.toSeq.sortBy(_._1).filterNot(_._1 == "migrated")
  }
}
